#' Gross errors test
#' 
#' Flag impossible values.
#' 
#' @param xts_vector An xts object with one column 
#' (if multicolumn, only the first column will be considered).
#' 
#' @return
#' A list of two xts objects (data with outliers removed and flags).
#' 
#' @examples 
#' out_list <- gross_errors(Bern$data$Log_1)
#' 
#' @import xts
#' @import zoo
#' 
#' @export
gross_errors <- function(xts_vector) {
  # Ensure the input is an xts object
  if (!inherits(xts_vector, "xts")) {
    stop("Input must be an xts object")
  }
  # Ensure time series is not empty
  if (ncol(xts_vector) == 0) stop("No stations detected in the dataset.")
  
  # Identify dates with outliers (values > 60 or < -40)
  rm_dates <- time(xts_vector[xts_vector[,1] >= 60 | xts_vector[,1] <= -40])
  
  qc_data <- qc_data_flagged <- xts_vector[, 1]
  qc_data[as.POSIXct(rm_dates, tz="UTC")] <- NA
  qc_data_flagged[!is.na(qc_data_flagged)] <- 0
  qc_data_flagged[as.POSIXct(rm_dates, tz="UTC")] <- 1
  
  # Create the output list
  out <- list(
    qc_data = qc_data,
    qc_data_flagged = qc_data_flagged
  )
  
  return(out)
}
