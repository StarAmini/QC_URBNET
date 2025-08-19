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
    xts_vector,
    window_size = 180,
    by = "10 min",
    na_tolerance = 5,
    min_non_na = 1,
    threshold = 0.001
) {
  # Ensure the input is an xts object
  if (!inherits(xts_vector, "xts")) {
    stop("Input must be an xts object")
  }
  # Ensure time series is not empty
  if (ncol(xts_vector) == 0) stop("No stations detected in the dataset.")
  
  # Determine the interval between observations (assumes regular time series)
  time_interval <- as.numeric(difftime(index(xts_vector)[2], index(xts_vector)[1], units = "mins"))
  if (is.na(time_interval) || time_interval == 0) stop("Time index not recognized correctly.")
  
  # Compute number of intervals in the time window
  width <- as.integer(window_size / time_interval) + 1
  
  # Initialize output xts objects
  qc_data <- qc_data_flagged <- xts_vector[, 1]
  qc_data_flagged[!is.na(qc_data_flagged)] <- 0
  
  # Custom SD function with NA tolerance
  custom_sd <- function(x, na_tolerance = 5, min_non_na = 1) {
    # na_tolerance is now the max number of NAs allowed in the window
    na_count <- sum(is.na(x))
    
    # skip if too many NAs
    if (na_count > na_tolerance) {
      return(NA_real_)
    }
    
    # Otherwise, require at least some non-NA data to compute SD
    if (sum(!is.na(x)) < min_non_na) {
      return(NA_real_)
    }
    
    sd(x, na.rm = TRUE)
  }
  
  # Compute rolling standard deviation
  rolling_sd <- rollapply(
    data  = xts_vector[, 1, drop = FALSE],
    width = width,
    FUN   = function(z) custom_sd(z, na_tolerance = 5, min_non_na = 5),
    fill  = NA,
    align = "right"
  )
  
  # Identify timestamps where SD is near zero
  flagged_timestamps <- index(xts_vector)[which(rolling_sd < threshold )]
  
  if (length(flagged_timestamps) > 0) {
    # Expand flagged timestamps to include the full rolling window
    expanded_dates <- unique(do.call("c", lapply(flagged_timestamps, function(ts) {
      seq(from = ts - (width - 1) * time_interval * 60, by = by, length.out = width)
    })))
    
    # Ensure flagged dates are within the dataset index
    expanded_dates <- expanded_dates[expanded_dates %in% index(xts_vector)]
    
    # Flag data
    qc_data[as.POSIXct(expanded_dates, tz="UTC")] <- NA
    qc_data_flagged[as.POSIXct(expanded_dates, tz="UTC")] <- 1
  }
  
  return(list(qc_data = qc_data, qc_data_flagged = qc_data_flagged))
}
