
#' Time consistency test
#' 
#' Flag too large deviations from a running median.
#' 
#' @param xts_vector An xts object with one column 
#' (if multicolumn, only the first column will be considered).
#' @param dt Time window half-width (number of time steps).
#' @param diff Maximum difference from running median allowed.
#' 
#' @return
#' A list of two xts objects (data with outliers removed and flags).
#' 
#' @examples 
#' out_list <- time_consistency(Bern$data$Log_1)
#' 
#' @import xts
#' @import zoo
#'
#' @export
time_consistency <- function(xts_vector, dt = 3, diff = 6) {
  # Ensure the input is an xts object
  if (!inherits(xts_vector, "xts")) {
    stop("Input must be an xts object")
  }
  # Ensure time series is not empty
  if (ncol(xts_vector) == 0) stop("No stations detected in the dataset.")
  
  time_index <- index(xts_vector)  # Extract time index
  
  station_names <- colnames(xts_vector)
  qc_data <- xts_vector
  qc_data_flagged <- xts_vector
  qc_data_flagged[] <- 0

  for (station in station_names) {
    X <- as.numeric(xts_vector[, station])
    n <- length(X)
    PO <- rep(FALSE, n)

    for (i in 1:n) {
      if (is.na(X[i])) next

      neighbors <- max(1, i - dt):min(n, i + dt)
      if (sum(!is.na(X[neighbors])) < (1 + dt)) next

      # Full window median
      full_median <- median(X[neighbors], na.rm = TRUE)
      if (abs(X[i] - full_median) < diff) next

      # Check before median
      before_idx <- max(1, i - dt):(i - 1)
      before_vals <- X[before_idx]
      before_vals <- before_vals[!is.na(before_vals)]
      if (length(before_vals) == 0 || abs(X[i] - median(before_vals)) < diff) next

      # Check after median
      after_idx <- (i + 1):min(n, i + dt)
      after_vals <- X[after_idx]
      after_vals <- after_vals[!is.na(after_vals)]
      if (length(after_vals) == 0 || abs(X[i] - median(after_vals)) < diff) next

      PO[i] <- TRUE
    }

    qc_data_flagged[PO, station] <- 1
    qc_data[PO, station] <- NA
  }

  return(list(
    qc_data = qc_data,
    qc_data_flagged = qc_data_flagged
  ))
}
