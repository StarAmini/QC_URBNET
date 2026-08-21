# Corrected QC pipeline for Basel, using the fixed functions in "R Functions/".
#
# Runs on RAW observations only (raw_data/Basel/SEF_output/RAW), ignoring the
# released QC/Hourly folders. Tests are applied sequentially in the paper's
# order, each on the previous test's cleaned data, with the corrected
# Table 2 parameters:
#   L1 gross_errors                  (>60 / <=-40 degC)
#   L3 time_consistency              diff = resolution_diff(15) = 3 degC
#   L4 temporal_persistance          12-hour window, native interval
#   L5 climatic_outliers             4 x IQR per calendar month
#   L7 spatiotemporal_consistency    threshold = 0.9999
# Skipped (inputs not available in this repo, per the audit):
#   L2 out_of_range      (needs Basel's per-season ERA5-Land thresholds)
#   L6 spatial_consistency (needs land-use metadata missing from the release)
#
# Output (SEF-style .tsv, RAW values kept, flags in the Meta column):
#   corrected_QC/15min/<station>.tsv   flagged 15-min series
#   corrected_QC/Hourly/<station>.tsv  hourly means from RAW (80% rule),
#                                      QC re-applied at hourly resolution
#                                      (diff = 4.5 degC)
#
# Run from the repo root:
#   & "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" run_corrected_qc.R

.libPaths(c(file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6"),
            .libPaths()))
suppressMessages({ library(xts); library(zoo); library(geosphere) })

fn_dir <- "R Functions"
source(file.path(fn_dir, "gross_errors.R"))
source(file.path(fn_dir, "time_consistency.R"))
source(file.path(fn_dir, "time_persistence.R"))
source(file.path(fn_dir, "climatic_outliers.R"))
source(file.path(fn_dir, "spatiotemporal_consistency.R"))
source("validation/sef_utils.R")

raw_dir <- file.path("raw_data", "Basel", "SEF_output", "RAW")
out_15 <- file.path("corrected_QC", "15min")
out_hr <- file.path("corrected_QC", "Hourly")
dir.create(out_15, recursive = TRUE, showWarnings = FALSE)
dir.create(out_hr, recursive = TRUE, showWarnings = FALSE)

files <- list.files(raw_dir, pattern = "\\.tsv$", full.names = TRUE)
args <- commandArgs(trailingOnly = TRUE)  # optional: limit station count
if (length(args) >= 1) files <- files[seq_len(min(as.numeric(args[1]), length(files)))]
station_of <- function(f) sub("_AirTemp.*$", "", basename(f))
cat(length(files), "RAW station files\n")

# ---- helpers ---------------------------------------------------------------

flag_times <- function(res) {
  # which() drops NAs (flag series are NA wherever the data itself is NA)
  index(res$qc_data_flagged)[which(as.numeric(res$qc_data_flagged) == 1)]
}

# Sequential single-station tests (L1, L3, L4, L5); returns cleaned series
# plus a list of flagged times per test.
run_station_tests <- function(x, diff_degC) {
  flags <- list()
  run <- function(name, fun) {
    res <- tryCatch(fun(x), error = function(e) {
      cat("  ", name, "skipped:", conditionMessage(e), "\n"); NULL
    })
    if (!is.null(res)) {
      flags[[name]] <<- flag_times(res)
      x <<- res$qc_data
    }
  }
  run("gross_errors", gross_errors)
  run("time_consistency",
      function(v) time_consistency(v, dt = 3, diff = diff_degC))
  run("temporal_persistance", temporal_persistence)
  run("climatic_outliers", climatic_outliers)
  list(cleaned = x, flags = flags)
}

# Meta strings ("qc=a,b") for one station's full time index
build_meta <- function(idx, flags) {
  meta <- character(length(idx))
  for (test in names(flags)) {
    hit <- idx %in% flags[[test]]
    meta[hit] <- ifelse(meta[hit] == "", test, paste0(meta[hit], ",", test))
  }
  meta[meta != ""] <- paste0("qc=", meta[meta != ""])
  meta
}

write_sef_qc <- function(path, sef, x, flags, period, tests_note) {
  idx <- index(x)
  vals <- as.numeric(x)
  header <- c(
    "SEF\t1.0.0",
    paste0("ID\t", sef$id),
    paste0("Name\t", sef$name),
    paste0("Lat\t", sef$lat),
    paste0("Lon\t", sef$lon),
    "Alt\tNA",
    "Source\tmeteoblue",
    "Link\twww.meteoblue.com",
    "Vbl\tta",
    paste0("Stat\t", if (period == "hourly") "mean" else "point"),
    "Units\tC",
    paste0("Meta\tURBNET Project corrected QC |qc = ", tests_note),
    "Year\tMonth\tDay\tHour\tMinute\tPeriod\tValue\tMeta"
  )
  rows <- paste(
    format(idx, "%Y"), format(idx, "%m"), format(idx, "%d"),
    format(idx, "%H"), format(idx, "%M"), period,
    ifelse(is.na(vals), "NA", sprintf("%.2f", vals)),
    build_meta(idx, flags),
    sep = "\t"
  )
  writeLines(c(header, rows), path, useBytes = TRUE)
}

# Hourly means from RAW with the paper's 80%-completeness rule
to_hourly <- function(x, obs_per_hour = 4, completeness = 0.8) {
  # Group by character keys: grouping by POSIXct lets midnight stamps drop
  # their time part in factor levels, and as.POSIXct() then parses the whole
  # mixed-format vector with the first element's format (all -> midnight).
  hour_key <- format(index(x), "%Y-%m-%d %H:00:00")
  v <- as.numeric(x)
  n_ok <- tapply(!is.na(v), hour_key, sum)
  means <- tapply(v, hour_key, mean, na.rm = TRUE)
  means[n_ok / obs_per_hour < completeness] <- NA_real_
  idx <- as.POSIXct(names(means), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  h <- xts(as.numeric(means), order.by = idx)
  colnames(h) <- colnames(x)
  h
}

# Full pipeline at one resolution: per-station L1/L3/L4/L5 then network L7
run_pipeline <- function(series_list, diff_degC, label) {
  cleaned <- list(); flags <- list()
  for (st in names(series_list)) {
    r <- run_station_tests(series_list[[st]], diff_degC)
    cleaned[[st]] <- r$cleaned
    flags[[st]] <- r$flags
    cat(sprintf("[%s] %d/%d %s: %s\n", label,
                match(st, names(series_list)), length(series_list), st,
                paste(sprintf("%s=%d", names(r$flags),
                              lengths(r$flags)), collapse = " ")))
  }
  db <- do.call(cbind, cleaned)
  colnames(db) <- names(cleaned)
  for (st in colnames(db)) {
    res <- tryCatch(
      suppressWarnings(spatiotemporal_consistency(st, db, sto_meta)),
      error = function(e) NULL
    )
    if (!is.null(res) && length(res$rm_dates) > 0) {
      flags[[st]]$spatiotemporal_consistency <-
        as.POSIXct(res$rm_dates, tz = "UTC")
    }
  }
  n7 <- sum(vapply(flags, function(f)
    length(f$spatiotemporal_consistency), integer(1)))
  cat(sprintf("[%s] spatiotemporal_consistency: %d flags network-wide\n",
              label, n7))
  flags
}

# ---- load RAW --------------------------------------------------------------

sefs <- lapply(files, read_sef)
names(sefs) <- vapply(files, station_of, character(1))
raw_series <- lapply(names(sefs), function(nm) {
  s <- sefs[[nm]]
  x <- xts(s$data$Value, order.by = s$data$time)
  colnames(x) <- nm
  x
})
names(raw_series) <- names(sefs)
sto_meta <- data.frame(ID = names(sefs),
                       LON = vapply(sefs, `[[`, numeric(1), "lon"),
                       LAT = vapply(sefs, `[[`, numeric(1), "lat"))
tests_note <- paste("gross_errors,time_consistency,temporal_persistance,",
                    "climatic_outliers,spatiotemporal_consistency", sep = "")

# ---- hourly first (fast): aggregate RAW then QC at hourly resolution -------

cat("\n== Hourly pipeline (diff =", resolution_diff(60), "degC) ==\n")
hr_series <- lapply(raw_series, to_hourly)
hr_flags <- run_pipeline(hr_series, resolution_diff(60), "hourly")
for (st in names(hr_series)) {
  write_sef_qc(file.path(out_hr, paste0(st, "_AirTemp_hourly_UTC.tsv")),
               sefs[[st]], hr_series[[st]], hr_flags[[st]],
               "hourly", tests_note)
}
cat("wrote", length(hr_series), "files to", out_hr, "\n")

# ---- 15-min pipeline -------------------------------------------------------

cat("\n== 15-min pipeline (diff =", resolution_diff(15), "degC) ==\n")
flags15 <- run_pipeline(raw_series, resolution_diff(15), "15min")
for (st in names(raw_series)) {
  write_sef_qc(file.path(out_15, paste0(st, "_AirTemp_15min_UTC.tsv")),
               sefs[[st]], raw_series[[st]], flags15[[st]],
               "0", tests_note)
}
cat("wrote", length(raw_series), "files to", out_15, "\n")

# ---- summary ---------------------------------------------------------------

summarize <- function(flags, label) {
  tests <- unique(unlist(lapply(flags, names)))
  cat("\nFlag totals (", label, "):\n", sep = "")
  for (t in tests) {
    n <- sum(vapply(flags, function(f) length(f[[t]]), integer(1)))
    cat(sprintf("  %-28s %d\n", t, n))
  }
}
summarize(hr_flags, "hourly")
summarize(flags15, "15min")
cat("\nDone.\n")
