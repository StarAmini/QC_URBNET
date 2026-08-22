#' Out of range test
#'
#' Flag the values that exceed custom thresholds based on climatology.
#'
#' @param xts_vector An xts object with one column
#' (if multicolumn, only the first column will be considered).
#' @param season_thresholds A list of lists of thresholds (one list per
#' season): winter/spring/summer/autumn, each with min_val/max_val. Per the
#' Methods text, these are derived per network from ERA5-Land seasonal
#' extremes (with a bias correction of up to +6 degC), so there is no single
#' default that is valid across networks - this must be supplied explicitly
#' for the city/network being processed. (Previously this argument had a
#' Bern-specific default baked in, which meant a call site that forgot to
#' pass season_thresholds for a different network would silently apply
#' Bern's climatology instead of erroring.)
#'
#' @return
#' A list of two xts objects (data with outliers removed and flags).
#'
#' @examples
#' bern_thresholds <- list(
#'   winter = list(min_val = -22.44, max_val = 23.54),
#'   spring = list(min_val = -20.05, max_val = 38.08),
#'   summer = list(min_val = 7.85, max_val = 46.26),
#'   autumn = list(min_val = -6.42, max_val = 40.87)
#' )
#' out_list <- out_of_range(Bern$data$Log_1, bern_thresholds)
#'
#' @import xts
#' @import zoo
#'
#' @export
out_of_range <- function(
    xts_vector,
    season_thresholds
) {
  if (missing(season_thresholds)) {
    stop(
      "`season_thresholds` must be supplied explicitly for this network - ",
      "there is no network-agnostic default (Table 2 / Methods: values are ",
      "derived per city from ERA5-Land seasonal extremes). Passing no ",
      "argument here used to silently fall back to Bern's climatology."
    )
  }
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

  qc_data <- xts_vector  # Initialize cleaned data
  qc_data_flagged <- xts_vector  # Initialize flagged data

  qc_data_flagged[] <- 0  # Default: no flags

  for (season in names(season_thresholds)) {
    # Retrieve seasonal thresholds
    thresholds <- season_thresholds[[season]]
    season_mask <- seasons == season  # Identify dates in the current season

    # Identify indices outside threshold range for this season
    outlier_indices <- which(xts_vector < thresholds$min_val |
                         xts_vector > thresholds$max_val)

     # Apply the season mask
     rm_dates <- outlier_indices[seasons[outlier_indices] == season]

    # Set flagged and cleaned data for the outliers
    qc_data[rm_dates] <- NA
    qc_data_flagged[rm_dates] <- 1
  }

  # Output the results as a list
  out <- list(
    qc_data = qc_data,
    qc_data_flagged = qc_data_flagged
  )

  return(out)
}
