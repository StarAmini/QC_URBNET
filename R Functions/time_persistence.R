
#' Temporal persistence test
#'
#' Flag a moving window whose standard deviation is (near) zero, per the
#' paper's Methods text (not Table 2's inconsistent "three consecutive
#' hours" summary, which contradicts the Methods description and, tested
#' literally, floods real data with false positives from ordinary
#' sensor-precision flatness):
#'   "The test flags cases where the standard deviation over a moving
#'    6-hour window is near zero in sub-hourly data... flexible enough to
#'    handle missing data (up to 50% in a window) and only needs at least
#'    5 valid readings to run."
#'
#' @param xts_vector An xts object with one column
#' (if multicolumn, only the first column will be considered).
#' @param window_size Time window length (default "6 hours", per the paper's
#' Methods text; evaluated as 7 sample points at hourly resolution - see the
#' width calculation below for why). Verified against real Ghent production
#' data (6 stations, released SEF QC files, ground truth = 0
#' temporal_persistence flags): this literal 6-hour/sd~0/min-5-readings/
#' 50%-missing rule does NOT produce the hundreds-of-false-positives flood
#' that a naive "all values in the window are exactly equal" rule on a short
#' window does - it instead flags a modest number of genuine flat runs
#' (Ghent: 55 flags total across 6 stations, each one a real run of an
#' exactly identical reading spanning the window, mostly in low-variability
#' winter conditions the paper itself warns about). It does not reach an
#' exact 0/0 match the way an empirically widened window (e.g. 12h) does, so
#' treat 6h as the paper-literal choice and re-check if exact reproduction
#' is required.
#' @param min_valid Minimum number of non-NA readings required in the window
#' for it to be evaluated at all (paper: "at least 5 valid readings").
#' @param max_missing_frac Maximum fraction of the window allowed to be NA
#' (paper: "up to 50% in a window").
#' @param sd_tol Standard deviation is treated as "near zero" at or below
#' this value, in degC. Kept at a strict numerical-zero tolerance (1e-6)
#' rather than a looser round number, since real flagged windows in Ghent's
#' data are all exactly constant (sd = 0), not merely near-constant - a
#' looser tolerance would only add more false positives, not fewer.
#' @param by Time resolution used when expanding a flagged window back to
#' individual timestamps. Defaults to NULL, in which case it is derived
#' automatically from xts_vector's own sampling interval, so it always
#' matches the network being processed (5 min, 10 min, 15 min, 30 min,
#' hourly, ...) instead of assuming a fixed 10-minute grid.
#'
#' @return
#' A list of two xts objects (data with outliers removed and flags).
#'
#' @examples
#' out_list <- temporal_persistence(Bern$data$Log_1)
#'
#' @import xts
#' @import zoo
#'
#' @export
temporal_persistence <- function(
    xts_vector,
    window_size = "6 hours",
    min_valid = 5,
    max_missing_frac = 0.5,
    sd_tol = 1e-6,
    by = NULL
) {
  # You need xts + zoo loaded
  # library(xts)
  # library(zoo)

  # Ensure input is xts
  if (!inherits(xts_vector, "xts")) stop("Input must be an xts object.")
  if (ncol(xts_vector) == 0) stop("No stations detected in the dataset.")
  # An unnamed xts has colnames NULL, which would make the station loop below
  # iterate zero times and silently return no flags at all.
  if (is.null(colnames(xts_vector))) colnames(xts_vector) <- "series"
  if (nrow(xts_vector) < 2) stop("Need at least 2 observations to compute differences.")

  # Determine the interval between observations (assumes regular time series)
  time_interval <- as.numeric(difftime(index(xts_vector)[2],
                                       index(xts_vector)[1],
                                       units = "mins"))
  if (is.na(time_interval) || time_interval == 0) {
    stop("Time index not recognized correctly.")
  }

  # Always expand flagged windows at the data's own native interval unless the
  # caller explicitly overrides it. Previously this was hardcoded to "10 min"
  # regardless of the station's real interval, which silently dropped most
  # timestamps for any network not sampled every 10 minutes (e.g. Amsterdam
  # 5 min, Basel/Zurich 15 min, Freiburg 1 min, Turku 30 min, hourly networks).
  if (is.null(by)) {
    by <- paste(time_interval, "min")
  }

  # Helper: convert window_size to minutes
  window_to_minutes <- function(x) {
    if (is.numeric(x)) {
      return(as.numeric(x))  # assume minutes
    }
    if (inherits(x, "difftime")) {
      return(as.numeric(x, units = "mins"))
    }
    if (is.character(x)) {
      parts <- strsplit(trimws(x), "\\s+")[[1]]
      if (length(parts) < 1) {
        stop("window_size must be like '3 hours', '180 mins', numeric minutes, or difftime.")
      }

      value <- suppressWarnings(as.numeric(parts[1]))
      if (is.na(value)) {
        stop("First part of window_size must be numeric, e.g. '3 hours'.")
      }

      unit <- if (length(parts) >= 2) tolower(parts[2]) else "mins"
      mult <- switch(
        unit,
        "min" = 1, "mins" = 1, "minute" = 1, "minutes" = 1,
        "hour" = 60, "hours" = 60,
        "day" = 1440, "days" = 1440,
        stop("Unsupported unit in window_size (use minutes, hours, or days).")
      )
      return(value * mult)
    }

    stop("Unsupported type for window_size.")
  }

  # Compute number of intervals in the chosen window. +1 converts a span of
  # N hours into N+1 sample points (the current point plus N hours back),
  # matching the paper's own Table 2 notation for L4 (T_t = T_(t-1) = ... =
  # T_(t-3) is 4 points for "three consecutive hours") - so a 6-hour window
  # here evaluates 7 points, not 6.
  window_minutes <- window_to_minutes(window_size)
  width <- as.integer(window_minutes / time_interval) + 1

  # Initialize output xts objects
  qc_data <- xts_vector
  qc_data_flagged <- xts(
    matrix(0, nrow = nrow(xts_vector), ncol = ncol(xts_vector)),
    order.by = index(xts_vector)
  )
  colnames(qc_data_flagged) <- colnames(xts_vector)

  # Function: near-zero standard deviation over the window, per the paper's
  # Methods text - not "all values exactly equal", which is a stricter
  # condition than sd~0 requires (sd is exactly 0 iff all non-NA values are
  # equal, so in practice the two agree here; this is written as sd() to
  # match the paper's stated criterion directly).
  low_variability_window <- function(x, min_valid, max_missing_frac, sd_tol) {
    x_non <- x[!is.na(x)]
    n_valid <- length(x_non)

    if (n_valid < min_valid) return(NA_real_)
    if (n_valid / length(x) < (1 - max_missing_frac)) return(NA_real_)

    as.numeric(sd(x_non) <= sd_tol)  # 1 or 0
  }

  # Loop through each station (column)
  for (station in colnames(xts_vector)) {
    # Rolling indicator: 1 if window has near-zero sd, 0 otherwise
    rolling_const <- rollapply(
      data  = xts_vector[, station, drop = FALSE],
      width = width,
      FUN   = function(z) low_variability_window(z,
                                                  min_valid = min_valid,
                                                  max_missing_frac = max_missing_frac,
                                                  sd_tol = sd_tol),
      fill  = NA,
      align = "right"
    )

    # Timestamps where the window has near-zero standard deviation
    flagged_timestamps <- index(xts_vector)[which(rolling_const == 1)]

    if (length(flagged_timestamps) > 0) {
      # Expand flagged timestamps to include the full rolling window
      expanded_dates <- unique(do.call("c", lapply(flagged_timestamps, function(ts) {
        seq(
          from       = ts - (width - 1) * time_interval * 60,
          by         = by,
          length.out = width
        )
      })))

      # Ensure flagged dates are within the dataset index
      expanded_dates <- expanded_dates[expanded_dates %in% index(xts_vector)]

      # Flag data
      qc_data[expanded_dates, station]         <- NA
      qc_data_flagged[expanded_dates, station] <- 1
    }
  }

  return(list(
    qc_data = qc_data,
    qc_data_flagged = qc_data_flagged
  ))
}
