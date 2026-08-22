# Corrected QC + hourly aggregation for any city, from RAW SEF files only.
#
# Order of operations (paper Methods, "Aggregating sub-hourly values to
# hourly and daily data": "Data aggregation in the hourly step was applied
# to the raw data" is what the release actually did, and it's wrong -
# averaging in a bad sub-hourly reading (e.g. a -40 degC gross error next to
# three real ~25 degC readings) silently contaminates the hourly mean before
# any QC test ever sees it, and no later test can recover from that because
# they only see the already-blended hourly value. This script instead:
#
#   1. Runs the sub-hourly QC tests on the RAW native-resolution series
#      FIRST (gross_errors, out_of_range, native-resolution time_consistency,
#      temporal_persistance, climatic_outliers).
#   2. Sets flagged sub-hourly values to NA (excludes them from aggregation)
#      - the same rule the paper already applies when building DailyMax/Min
#        from QC'd sub-hourly data, just also applied to the hourly mean.
#   3. Aggregates the CLEANED sub-hourly series to hourly means, still with
#      the paper's >=80%-completeness rule (now counted against how many
#      clean, non-flagged values are available in that hour).
#   4. Runs the hourly-resolution tests on the resulting hourly series
#      (time_consistency at 4.5 degC, temporal_persistance, climatic_outliers
#      again since monthly IQR shifts once bad points are removed, and the
#      network-wide spatiotemporal_consistency test).
#
# L2 out_of_range: per-city seasonal thresholds from ERA5-Land 1995-2023
#   empirical extremes, +6 degC on the max (paper's stated correction for
#   ERA5-Land cold bias / UHI).
# Skipped: L6 spatial_consistency (needs land-use metadata, not in SEF).
#
# Output per city:
#   corrected_QC/<City>/15min/<station>_...tsv   sub-hourly QC'd (flags only,
#       values kept; Meta column lists which sub-hourly test(s) hit)
#   corrected_QC/<City>/Hourly/<station>_AirTemp_hourly_UTC.tsv  hourly means
#       from CLEANED sub-hourly data, with hourly-level QC flags in Meta
#   corrected_QC/<City>/<City>_hourly.csv   wide CSV (rows = hours,
#       columns = stations, values from cleaned sub-hourly aggregation)
#   corrected_QC/<City>/<City>_flag_summary.csv  flags per station per test
#       (subhourly_* and hourly_* counted separately)
#
# Run:  Rscript run_corrected_qc_city.R <City> [n_stations]

.libPaths(c(file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6"),
            .libPaths()))
suppressMessages({ library(xts); library(zoo); library(geosphere) })

fn_dir <- "R Functions"
source(file.path(fn_dir, "gross_errors.R"))
source(file.path(fn_dir, "out_of_range.R"))
source(file.path(fn_dir, "time_consistency.R"))
source(file.path(fn_dir, "time_persistence.R"))
source(file.path(fn_dir, "climatic_outliers.R"))
source(file.path(fn_dir, "spatiotemporal_consistency.R"))
source("validation/sef_utils.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript run_corrected_qc_city.R <City> [n]")
city <- args[1]

raw_dir <- file.path("raw_data", city, "SEF_output", "RAW")
era5_file <- file.path("raw_data", city, "era5", "era5land_timeseries_hourly.csv")
out_dir <- file.path("corrected_QC", city, "Hourly")
sub_out_dir <- file.path("corrected_QC", city, "15min")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(sub_out_dir, recursive = TRUE, showWarnings = FALSE)

files <- list.files(raw_dir, pattern = "\\.tsv$", full.names = TRUE)
if (length(args) >= 2) files <- files[seq_len(min(as.numeric(args[2]), length(files)))]
cat(city, ":", length(files), "RAW station files\n")

# ---- out_of_range thresholds from ERA5-Land (1995-2023) --------------------

era <- read.csv(era5_file)
era_t <- as.POSIXct(era$valid_time, tz = "UTC")
sel <- format(era_t, "%Y") >= "1995" & format(era_t, "%Y") <= "2023"
mon <- as.numeric(format(era_t[sel], "%m"))
t2m <- era$t2m[sel]  # ERA5-Land t2m is already in degC in these files
season_of <- function(m) c("winter", "winter", "spring", "spring", "spring",
                           "summer", "summer", "summer", "autumn", "autumn",
                           "autumn", "winter")[m]
seas <- season_of(mon)
season_thresholds <- lapply(
  c(winter = "winter", spring = "spring", summer = "summer", autumn = "autumn"),
  function(s) list(min_val = min(t2m[seas == s], na.rm = TRUE),
                   max_val = max(t2m[seas == s], na.rm = TRUE) + 6)
)
cat("out_of_range thresholds (ERA5-Land 1995-2023, max +6):\n")
for (s in names(season_thresholds)) {
  cat(sprintf("  %-6s %6.2f .. %6.2f degC\n", s,
              season_thresholds[[s]]$min_val, season_thresholds[[s]]$max_val))
}

# ---- helpers (as in run_corrected_qc.R) ------------------------------------

flag_times <- function(res) {
  index(res$qc_data_flagged)[which(as.numeric(res$qc_data_flagged) == 1)]
}

# Aggregates x (already QC-cleaned: flagged points must be NA) to hourly
# means. Completeness is checked against the number of clean values actually
# present, per the paper's 80% rule - a value removed by QC counts the same
# as a value that was never measured.
to_hourly <- function(x, completeness = 0.8) {
  d <- diff(as.numeric(index(x)) / 60)
  interval <- median(d[d > 0], na.rm = TRUE)
  obs_per_hour <- max(1, round(60 / interval))
  hour_key <- format(index(x), "%Y-%m-%d %H:00:00")
  v <- as.numeric(x)
  n_ok <- tapply(!is.na(v), hour_key, sum)
  means <- tapply(v, hour_key, mean, na.rm = TRUE)
  means[n_ok / obs_per_hour < completeness] <- NA_real_
  idx <- as.POSIXct(names(means), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  h <- xts(as.numeric(means), order.by = idx)
  colnames(h) <- colnames(x)
  list(series = h, interval_min = interval)
}

# Runs one named QC test, NA-ing out its flagged points before returning, so
# the next test (and eventually the aggregation step) never sees them again.
run_test <- function(x, name, fun) {
  res <- tryCatch(fun(x), error = function(e) NULL)
  if (is.null(res)) return(list(cleaned = x, flagged = as.POSIXct(character(0), tz = "UTC")))
  list(cleaned = res$qc_data, flagged = flag_times(res))
}

# Sub-hourly pass: gross errors -> out of range -> native-resolution time
# consistency -> temporal persistence -> climatic outliers. Each test sees
# only what survived the previous one, and the cleaned (NA'd) series is what
# gets aggregated to hourly - so a gross-error value can never be blended
# into an hourly mean.
run_subhourly_tests <- function(x, diff_native) {
  flags <- list()
  r <- run_test(x, "gross_errors", gross_errors); x <- r$cleaned; flags$gross_errors <- r$flagged
  r <- run_test(x, "out_of_range", function(v) out_of_range(v, season_thresholds))
  x <- r$cleaned; flags$out_of_range <- r$flagged
  r <- run_test(x, "time_consistency",
                function(v) time_consistency(v, dt = 3, diff = diff_native))
  x <- r$cleaned; flags$time_consistency <- r$flagged
  r <- run_test(x, "temporal_persistance", temporal_persistence)
  x <- r$cleaned; flags$temporal_persistance <- r$flagged
  r <- run_test(x, "climatic_outliers", climatic_outliers)
  x <- r$cleaned; flags$climatic_outliers <- r$flagged
  list(cleaned = x, flags = flags)
}

# Hourly pass, run on the hourly series built from already-cleaned
# sub-hourly data. time_consistency uses the hourly threshold (4.5 degC);
# persistence and climatic_outliers are re-run at hourly resolution since
# their own logic (stuck-value runs, monthly IQR) is resolution-dependent.
run_hourly_tests <- function(x) {
  flags <- list()
  r <- run_test(x, "time_consistency",
                function(v) time_consistency(v, dt = 3, diff = resolution_diff(60)))
  x <- r$cleaned; flags$time_consistency <- r$flagged
  r <- run_test(x, "temporal_persistance", temporal_persistence)
  x <- r$cleaned; flags$temporal_persistance <- r$flagged
  r <- run_test(x, "climatic_outliers", climatic_outliers)
  x <- r$cleaned; flags$climatic_outliers <- r$flagged
  list(cleaned = x, flags = flags)
}

build_meta <- function(idx, flags) {
  meta <- character(length(idx))
  for (test in names(flags)) {
    hit <- idx %in% flags[[test]]
    meta[hit] <- ifelse(meta[hit] == "", test, paste0(meta[hit], ",", test))
  }
  meta[meta != ""] <- paste0("qc=", meta[meta != ""])
  meta
}

write_sef_qc <- function(path, sef, x, flags, period = "hourly") {
  idx <- index(x); vals <- as.numeric(x)
  header <- c("SEF\t1.0.0", paste0("ID\t", sef$id), paste0("Name\t", sef$name),
              paste0("Lat\t", sef$lat), paste0("Lon\t", sef$lon), "Alt\tNA",
              "Source\tcorrected pipeline", "Link\t", "Vbl\tta",
              paste0("Stat\t", if (period == "hourly") "mean" else "point"),
              "Units\tC",
              paste0("Meta\tURBNET corrected QC |qc = gross_errors,",
                     "out_of_range,time_consistency,temporal_persistance,",
                     "climatic_outliers,spatiotemporal_consistency"),
              "Year\tMonth\tDay\tHour\tMinute\tPeriod\tValue\tMeta")
  rows <- paste(format(idx, "%Y"), format(idx, "%m"), format(idx, "%d"),
                format(idx, "%H"), format(idx, "%M"), period,
                ifelse(is.na(vals), "NA", sprintf("%.2f", vals)),
                build_meta(idx, flags), sep = "\t")
  writeLines(c(header, rows), path, useBytes = TRUE)
}

# ---- per-station: read native RAW, QC sub-hourly, aggregate, QC hourly ----

sefs_hdr <- list(); hourly <- list(); sub_flags <- list(); hr_flags <- list()
intervals <- c()
for (i in seq_along(files)) {
  f <- files[i]
  st <- sub("_AirTemp.*$", "", basename(f))
  sef <- read_sef(f)
  x <- xts(sef$data$Value, order.by = sef$data$time)
  colnames(x) <- st
  sub_index <- index(x)
  sefs_hdr[[st]] <- list(id = sef$id, name = sef$name, lat = sef$lat, lon = sef$lon)
  if (nrow(x) < 2 || all(is.na(x))) {
    cat(sprintf("[%s] %d/%d %s: empty or too short, kept as all-NA\n",
                city, i, length(files), st))
    hourly[[st]] <- NULL
    next
  }

  d <- diff(as.numeric(index(x)) / 60)
  interval <- median(d[d > 0], na.rm = TRUE)
  intervals[st] <- interval

  # 1) QC on the raw sub-hourly series - bad points become NA and are
  #    therefore excluded from the hourly mean computed next.
  sub_r <- run_subhourly_tests(x, resolution_diff(interval))
  sub_flags[[st]] <- sub_r$flags

  # 2) Aggregate the CLEANED sub-hourly series to hourly means (80% rule
  #    counted against clean values only).
  h <- to_hourly(sub_r$cleaned)

  # 3) QC on the resulting hourly series.
  hr_r <- run_hourly_tests(h$series)
  hourly[[st]] <- hr_r$cleaned
  hr_flags[[st]] <- hr_r$flags

  n_sub <- sum(lengths(sub_r$flags))
  n_hr <- sum(lengths(hr_r$flags))
  cat(sprintf("[%s] %d/%d %s (%gmin): subhourly_flags=%d hourly_flags=%d (%s | %s)\n",
              city, i, length(files), st, interval, n_sub, n_hr,
              paste(sprintf("%s=%d", names(sub_r$flags), lengths(sub_r$flags)), collapse = " "),
              paste(sprintf("%s=%d", names(hr_r$flags), lengths(hr_r$flags)), collapse = " ")))

  # Write the sub-hourly QC file (values kept, flags in Meta - same
  # convention as the released QC folder, but on the corrected code).
  write_sef_qc(file.path(sub_out_dir, basename(f)), sefs_hdr[[st]],
               xts(as.numeric(x), order.by = sub_index, dimnames = list(NULL, st)),
               sub_r$flags, period = "0")
}
cat("native intervals seen (min):",
    paste(names(table(intervals)), table(intervals), sep = "x", collapse = " "),
    "\n")

# ---- network-wide spatiotemporal test on hourly ----------------------------

db <- do.call(cbind, hourly)
colnames(db) <- names(hourly)
sto_meta <- data.frame(
  ID = names(hourly),
  LON = vapply(sefs_hdr[names(hourly)], `[[`, numeric(1), "lon"),
  LAT = vapply(sefs_hdr[names(hourly)], `[[`, numeric(1), "lat"))
for (st in colnames(db)) {
  res <- tryCatch(suppressWarnings(spatiotemporal_consistency(st, db, sto_meta)),
                  error = function(e) NULL)
  if (!is.null(res) && length(res$rm_dates) > 0) {
    hr_flags[[st]]$spatiotemporal_consistency <- as.POSIXct(res$rm_dates, tz = "UTC")
  }
}

# ---- write outputs ---------------------------------------------------------

for (st in names(hourly)) {
  write_sef_qc(file.path(out_dir, paste0(st, "_AirTemp_hourly_UTC.tsv")),
               sefs_hdr[[st]], hourly[[st]], hr_flags[[st]], period = "hourly")
}
wide <- data.frame(time_utc = format(index(db), "%Y-%m-%d %H:%M:%S"),
                   as.data.frame(db), check.names = FALSE)
write.csv(wide, file.path("corrected_QC", city, paste0(city, "_hourly.csv")),
          row.names = FALSE, na = "")

sub_tests <- unique(unlist(lapply(sub_flags, names)))
hr_tests <- unique(unlist(lapply(hr_flags, names)))
summ <- data.frame(station = names(hourly))
for (t in sub_tests) summ[[paste0("subhourly_", t)]] <- vapply(
  names(hourly), function(s) length(sub_flags[[s]][[t]]), integer(1))
for (t in hr_tests) summ[[paste0("hourly_", t)]] <- vapply(
  names(hourly), function(s) length(hr_flags[[s]][[t]]), integer(1))
write.csv(summ, file.path("corrected_QC", city,
                          paste0(city, "_flag_summary.csv")), row.names = FALSE)

cat("\nSub-hourly flag totals (", city, ", removed before aggregation):\n", sep = "")
for (t in sub_tests) cat(sprintf("  %-28s %d\n", t,
                                 sum(vapply(sub_flags, function(f) length(f[[t]]), integer(1)))))
cat("\nHourly flag totals (", city, ", on cleaned-aggregated data):\n", sep = "")
for (t in hr_tests) cat(sprintf("  %-28s %d\n", t,
                                sum(vapply(hr_flags, function(f) length(f[[t]]), integer(1)))))
cat("wrote", length(hourly), "stations to", out_dir, "and", sub_out_dir, "\n")
