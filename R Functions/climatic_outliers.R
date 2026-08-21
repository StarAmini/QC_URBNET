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
  
  # Compute the month of every timestamp once (was: format(time(...), "%m")
  # re-run from scratch inside each of 3 separate 12-iteration sapply loops,
  # each re-subsetting the full xts object per month - O(3 x 12 x n) of
  # repeated, expensive xts subsetting. Grouping once via a plain numeric
  # vector + split-style indexing keeps the same per-month quartile/IQR
  # logic but only walks the series once.
  months <- as.numeric(format(time(xts_vector), "%m"))
  vals <- as.numeric(xts_vector[, 1])
  idx <- zoo::index(xts_vector)

  tavg_filter_l <- numeric(12)
  tavg_filter_h <- numeric(12)
  flagged <- logical(length(vals))
  for (x in 1:12) {
    sel <- months == x
    sample_vals <- vals[sel]
    if (!any(sel)) next
    q <- quantile(sample_vals, c(.25, .75), na.rm = TRUE)
    iqr <- IQR(sample_vals, na.rm = TRUE)
    tavg_filter_l[x] <- q[1] - ext_lim_factor * iqr
    tavg_filter_h[x] <- q[2] + ext_lim_factor * iqr
    flagged[sel] <- !is.na(sample_vals) &
      (sample_vals <= tavg_filter_l[x] | sample_vals >= tavg_filter_h[x])
  }
  rm_dates <- idx[flagged]
  
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
