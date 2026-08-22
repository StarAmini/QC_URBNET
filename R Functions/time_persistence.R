
#' Temporal persistence test
#' 
#' Flag equal or nearly equal consecutive values within a time window.
#' 
#' @param xts_vector An xts object with one column 
#' (if multicolumn, only the first column will be considered).
#' @param window_size Time window length in minutes.
#' @param by Time resolution (string such as "10 min").
#' @param na_tolerance Maximum number of NAs allowed in the window.
#' @param min_non_na Minimum number on non-NA values in the window.
#' @param threshold Minimum standard deviation allowed in the window.
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
    function(
    xts_vector,
     window_size = "5 hours",
     by = "10 min",
     na_tolerance = 5,
     min_non_na = 1
) {
  # You need xts + zoo loaded
  # library(xts)
  # library(zoo)
  
  # Ensure input is xts
  if (!inherits(xts_vector, "xts")) stop("Input must be an xts object.")
  if (ncol(xts_vector) == 0) stop("No stations detected in the dataset.")
  if (nrow(xts_vector) < 2) stop("Need at least 2 observations to compute differences.")
  
 # Determine the interval between observations (assumes regular time series)
  time_interval <- as.numeric(difftime(index(xts_vector)[2],
                                       index(xts_vector)[1],
                                       units = "mins"))
  if (is.na(time_interval) || time_interval == 0) {
    stop("Time index not recognized correctly.")
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
        stop("window_size must be like '5 hours', '180 mins', numeric minutes, or difftime.")
      }
      
      value <- suppressWarnings(as.numeric(parts[1]))
      if (is.na(value)) {
        stop("First part of window_size must be numeric, e.g. '5 hours'.")
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
  
  # Compute number of intervals in the chosen window
  window_minutes <- window_to_minutes(window_size)
  width <- as.integer(window_minutes / time_interval) + 1
  
  # Initialize output xts objects
  qc_data <- xts_vector
  qc_data_flagged <- xts(
    matrix(0, nrow = nrow(xts_vector), ncol = ncol(xts_vector)),
    order.by = index(xts_vector)
  )
  colnames(qc_data_flagged) <- colnames(xts_vector)
  
  # Function: is the window constant (all non-NA values equal)?
  constant_window <- function(x, na_tolerance, min_non_na) {
    na_count <- sum(is.na(x))
    
    # too many NAs, skip
    if (na_count > na_tolerance) {
      return(NA_real_)
    }
    
    x_non <- x[!is.na(x)]
    
    # not enough data
    if (length(x_non) < min_non_na) {
      return(NA_real_)
    }
    
    # all values equal -> return 1, else 0
    if (length(x_non) == 0) {
      return(NA_real_)
    }
    
    is_const <- (max(x_non) - min(x_non)) == 0
    as.numeric(is_const)  # 1 or 0
  }
  
  # Loop through each station (column)
  for (station in colnames(xts_vector)) {
    # Rolling indicator: 1 if window is constant (dT/dt = 0), 0 otherwise
    rolling_const <- rollapply(
      data  = xts_vector[, station, drop = FALSE],
      width = width,
      FUN   = function(z) constant_window(z,
                                          na_tolerance = na_tolerance,
                                          min_non_na   = min_non_na),
      fill  = NA,
      align = "right"
    )
    
    # Timestamps where the window is constant (all differences = 0)
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
