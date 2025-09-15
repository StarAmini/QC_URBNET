
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
  
  # Create output objects
  qc_data <- qc_data_flagged <- xts_vector[, 1]
  qc_data_flagged[!is.na(qc_data_flagged)] <- 0
  
  X <- as.numeric(xts_vector[, 1])  # Convert to numeric
  n <- length(X)
  PO <- rep(FALSE, n)  # Boolean vector for outliers
  
  # Identify outliers based on neighboring median
  for (i in 1:n) {
    neighbors <- max(1, i - dt):min(n, i + dt)
    if (!is.na(X[i]) && sum(!is.na(X[neighbors])) >= (1 + dt)) {
      PO[i] <- abs(X[i] - median(X[neighbors], na.rm = TRUE)) >= diff
    }
  }
  
  # Update flagged data
  qc_data_flagged[PO] <- 1  # Mark outliers
  qc_data[PO] <- NA  # Replace outliers with NA

  return(list(
    qc_data = qc_data,
    qc_data_flagged = qc_data_flagged
  ))
}
