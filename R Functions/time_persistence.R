
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
    window_size = "3 hours",
    by = "10 min",
    na_tolerance = 5,
    min_non_na = 1,
    diff_threshold = 0   # 0 = exact flatline, >0 = allow small wiggles
) {
  # You need xts + zoo loaded
  # library(xts)
  # library(zoo)
  
  # Ensure input is xts
  if (!inherits(xts_vector, "xts")) stop("Input must be an xts object.")
  if (ncol(xts_vector) == 0) stop("No stations detected in the dataset.")
  if (nrow(xts_vector) < 2) stop("Need at least 2 observations to compute differences.")
  
  # Determine interval between observations (minutes)
  time_interval <- as.numeric(difftime(index(xts_vector)[2], index(xts_vector)[1], units = "mins"))
  if (is.na(time_interval) || time_interval <= 0) stop("Time index not recognized correctly.")
  
  # Rolling window width (3 hours = 180 min)
  width <- as.integer(180 / time_interval) + 1
  
  # Output objects
  qc_data <- xts_vector
  qc_data_flagged <- xts(
    matrix(0, nrow = nrow(xts_vector), ncol = ncol(xts_vector)),
    order.by = index(xts_vector)
  )
  colnames(qc_data_flagged) <- colnames(xts_vector)
  
  # Loop per station
  for (station in colnames(xts_vector)) {
    
    x <- xts_vector[, station, drop = FALSE]
    
    # If there are fewer rows than the window, skip this station
    if (NROW(x) < width) next
    
    # --- Flatline persistence: rolling check where dT/dt = 0 ---
    flatline <- zoo::rollapply(
      data = x,
      width = width,
      FUN = function(z) {
        z <- as.numeric(z)
        
        # too many NAs?
        if (sum(is.na(z)) > na_tolerance) return(FALSE)
        
        # remove NAs
        z <- z[!is.na(z)]
        
        # need at least 2 points to compute diff reliably
        if (length(z) < max(min_non_na, 2)) return(FALSE)
        
        all(abs(diff(z)) <= diff_threshold)
      },
      fill = NA,         # safer than FALSE
      align = "right"
    )
    
    # flatline is an xts/zoo object; pull logical core data
    flatline_logical <- as.logical(coredata(flatline))
    flagged_rows <- which(flatline_logical)
    
    if (length(flagged_rows) == 0) next
    
    flagged_timestamps <- index(xts_vector)[flagged_rows]
    
    # Expand flagged timestamps across window
    expanded_dates <- unique(do.call("c", lapply(flagged_timestamps, function(ts) {
      seq(
        from = ts - (width - 1) * time_interval * 60,
        by = by,
        length.out = width
      )
    })))
    
    # Keep only timestamps that exist in the dataset
    expanded_dates <- expanded_dates[expanded_dates %in% index(xts_vector)]
    
    # Apply flags
    qc_data[expanded_dates, station] <- NA
    qc_data_flagged[expanded_dates, station] <- 1
  }
  
  return(list(
    qc_data = qc_data,
    qc_data_flagged = qc_data_flagged
  ))
}
