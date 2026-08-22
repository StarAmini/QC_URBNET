#' Climatic outliers test
#' 
#' Flag statistical outliers defined from the interquartile range (IQR).
#' 
#' @param xts_vector An xts object with one column 
#' (if multicolumn, only the first column will be considered).
#' @param ext_lim_factor Number of IQRs that must be exceeded.
#' 
#' @return
#' A list of two xts objects (data with outliers removed and flags).
#' 
#' @examples 
#' out_list <- climatic_outliers(Bern$data$Log_1)
#' 
#' @import xts
#' @import zoo
#'
#' @export
climatic_outliers <- function(xts_vector, ext_lim_factor = 4) {
  # Ensure the input is an xts object
  if (!inherits(xts_vector, "xts")) {
    stop("Input must be an xts object")
  }
  # Ensure time series is not empty
  if (ncol(xts_vector) == 0) stop("No stations detected in the dataset.")
  
  tavg_filter_l <- sapply(1:12, function(x) {
    xts_obj_sample <- xts_vector[as.numeric(format(time(xts_vector), "%m")) == x, 1]
    quantile(xts_obj_sample, .25, na.rm = TRUE) - ext_lim_factor * IQR(xts_obj_sample, na.rm = TRUE)
  })
  
  tavg_filter_h <- sapply(1:12, function(x) {
    xts_obj_sample <- xts_vector[as.numeric(format(time(xts_vector), "%m")) == x, 1]
    quantile(xts_obj_sample, .75, na.rm = TRUE) + ext_lim_factor * IQR(xts_obj_sample, na.rm = TRUE)
  })
  
  rm_dates <- do.call(c, sapply(1:12, function(x) {
    xts_obj_sample <- xts_vector[as.numeric(format(time(xts_vector), "%m")) == x, 1]
    if (length(xts_obj_sample) == 0) return(NULL)
    zoo::index(
      xts_obj_sample[xts_obj_sample <= tavg_filter_l[x] |
                       xts_obj_sample >= tavg_filter_h[x]]
    )
  }))
  
  qc_data <- qc_data_flagged <- xts_vector[, 1]
  qc_data[as.POSIXct(rm_dates, tz="UTC")] <- NA
  qc_data_flagged[!is.na(qc_data_flagged)] <- 0
  qc_data_flagged[as.POSIXct(rm_dates, tz="UTC")] <- 1
  
  out <- list(
    qc_data = qc_data,
    qc_data_flagged = qc_data_flagged
  )
  
  return(out)
  
}
