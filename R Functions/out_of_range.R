#' Out of range test
#' 
#' Flag the values that exceed custom thresholds based on climatology.
#' 
#' @param xts_vector An xts object with one column 
#' (if multicolumn, only the first column will be considered).
#' @param season_thresholds A list of lists of thresholds (one list per season, default values are for Bern).
#' 
#' @return
#' A list of two xts objects (data with outliers removed and flags).
#' 
#' @examples 
#' out_list <- out_of_range(Bern$data$Log_1)
#' 
#' @import xts
#' @import zoo
#'
#' @export
out_of_range <- function(
    xts_vector,
    season_thresholds = list(
      winter = list(min_val = -24.44, max_val = 23.54),
      spring = list(min_val = -21.05, max_val = 38.08),
      summer = list(min_val = 7.85, max_val = 46.26),
      autumn = list(min_val = -8.42, max_val = 40.87)
    )
) {
  # Ensure the input is an xts object
  if (!inherits(xts_vector, "xts")) {
    stop("Input must be an xts object")
  }
  # Ensure time series is not empty
  if (ncol(xts_vector) == 0) stop("No stations detected in the dataset.")
  
  # Helper function to determine the season of a given date
  get_season <- function(date) {
    month <- as.numeric(format(date, "%m"))
    if (month %in% c(12, 1, 2)) {
      return("winter")
    } else if (month %in% 3:5) {
      return("spring")
    } else if (month %in% 6:8) {
      return("summer")
    } else {
      return("autumn")
    }
  }
  
  # Get dates and initialize outputs
  dates <- index(xts_vector)
  seasons <- sapply(dates, get_season)  # Determine season for each date
  
  qc_data <- qc_data_flagged <- xts_vector[, 1]
  qc_data_flagged[!is.na(qc_data_flagged)] <- 0
  
  for (season in names(season_thresholds)) {
    # Retrieve seasonal thresholds
    thresholds <- season_thresholds[[season]]
    season_mask <- seasons == season  # Identify dates in the current season
    
    # Identify indices outside threshold range for this season
    outlier_indices <- which((xts_vector[, 1] < thresholds$min_val | 
                               xts_vector[, 1] > thresholds$max_val) &
                               season_mask)
    
    # Set flagged and cleaned data for the outliers
    qc_data[outlier_indices] <- NA
    qc_data_flagged[outlier_indices] <- 1
  }
  
  # Output the results as a list
  out <- list(
    qc_data = qc_data,
    qc_data_flagged = qc_data_flagged
  )
  
  return(out)
}
