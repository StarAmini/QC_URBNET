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
  row_mask <- apply(xts_vector, 1, function(x) any(x > 60 | x <= -40, na.rm = TRUE))

  rm_dates <- index(xts_vector)[row_mask]
  
  # Create a copy of the input data for cleaning and flagging
  qc_data <- qc_data_flagged <- xts_vector
  
  # Set outlier values to NA in qc_data
  if(length(rm_dates) > 0) {
    
    qc_data[rm_dates] <- NA
    
  }

  # Flag outliers in qc_data_flagged
  qc_data_flagged[!is.na(qc_data_flagged)] <- 0  # Default flag to 0

  if(length(rm_dates) > 0) {
    
    qc_data_flagged[rm_dates] <- 1  # Flag outliers as 1

  }
  
  # Create the output list
  out <- list(
    qc_data = qc_data,
    qc_data_flagged = qc_data_flagged
  )
  
  return(out)
}
