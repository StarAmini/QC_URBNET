#' Spatio-temporal consistency test
#' 
#' Flag values based on neighboring stations and time evolution simultaneously.
#' 
#' @param xy_target ID of target station.
#' @param xts_database An xts object with temperature data.
#' @param xyz_metadata An xts object with spatial metadata.
#' @param params List of parameters lmt_xy_, lmt_n_, n_nearby_.
#' @param threshold Quantile threshold
#' 
#' @return
#' A list of two xts objects (data with outliers removed and flags for target station)
#' and a vector of flagged times.
#' 
#' @examples 
#' out_list <- spatiotemporal_consistency("Log_1", Bern$data, Bern$xyz)
#' 
#' @import xts
#' @import zoo
#'
#' @export
spatiotemporal_consistency <- function(
    xy_target,
    xts_database,
    xyz_metadata,
    params = list(
        lmt_xy_ = 2500,
        lmt_n_ = 5,
        n_nearby_ = 5
    ),
    threshold = 0.99
) {
  
  get_spatial_nearby_points <- function(
      xy_target,
      xyz_metadata,
      lmt_xy,
      lmt_n
  ) {
    
    pos_target <- match(xy_target, xyz_metadata$ID)
    
    target <- xyz_metadata[pos_target, c("LON", "LAT")]
    nearby <- xyz_metadata[, c("LON", "LAT")]
    
    out <- xyz_metadata[, c("ID", "LON", "LAT")]
    out$distance <- geosphere::distHaversine(target, nearby)
    out <- out[out$distance < lmt_xy, ]
    out <- out[order(out$distance), ]
    out <- out[1:(lmt_n + 1), ]
    out <- out[complete.cases(out), ]
    out <- as.character(out$ID)
    
    return(out)
    
  }
  
  # spatial check
  xy_nearby <- get_spatial_nearby_points(xy_target = xy_target,
                                         xyz_metadata = xyz_metadata,
                                         lmt_xy = params$lmt_xy_,
                                         lmt_n = params$lmt_n_)
  xy_nearby <- xy_nearby[-1]
  
  if (length(xy_nearby) < params$n_nearby_) {
    warning(paste("Not enough nearby stations for", xy_target, 
                  "- found:", length(xy_nearby), "- skipping spatial QC but preserving column."))
    
    zero_flags <- xts(rep(0, nrow(xts_database[, xy_target])), order.by = index(xts_database[, xy_target]))
    return(list(
      qc_data = xts_database[, xy_target],  # original data unchanged
      qc_data_flagged = zero_flags,
      rm_dates = character(0),  # empty date list
      neighbor_count = length(xy_nearby)
    ))
  }
  
  target_xts <- xts_database[, xy_target]
  nearby_xts <- xts_database[, xy_nearby]
  
  dif_target_nearby <- lapply(
    seq_len(ncol(nearby_xts)),
    function(idd) {
      
      dif_ts <- target_xts - nearby_xts[, idd]
      
      dif_099 <- quantile(dif_ts, threshold, na.rm = TRUE)
      flag_099_pos <- time(dif_ts[dif_ts >= dif_099])
      
      dif_1_099 <- quantile(dif_ts, 1 - threshold, na.rm = TRUE)
      flag_099_neg <- time(dif_ts[dif_ts <= dif_1_099])
      
      out_ts <- dif_ts
      out_ts[!is.na(out_ts)] <- 0
      out_ts[c(flag_099_pos, flag_099_neg)] <- 1
      
      out_ts
    }
  )
  
  dif_target_nearby <- do.call(cbind, dif_target_nearby)
  nr_measurements <- rowSums(!is.na(dif_target_nearby))
  nr_measurements[is.na(target_xts)] <- 5
  dif_target_nearby <- rowSums(dif_target_nearby, na.rm = TRUE)
  # time step in which at least 5 nearby points are flagged
  dif_target_nearby <- which(dif_target_nearby >= nr_measurements)
  # getting the dates
  dif_target_nearby <- time(target_xts)[dif_target_nearby]
  dif_target_nearby <- as.character(dif_target_nearby)
  
  # temporal check
  # previous date
  dif_target_target_pre <-  target_xts - stats::lag(target_xts, k = -1)
  quant_099_pre <- quantile(
    dif_target_target_pre,
    threshold,
    na.rm = TRUE
  )
  dif_target_target_pre <- dif_target_target_pre[
    dif_target_target_pre > quant_099_pre |
      dif_target_target_pre < -quant_099_pre
  ]
  dif_target_target_pre <- as.character(time(dif_target_target_pre))
  
  # next date
  dif_target_target_nex <-  stats::lag(target_xts, k = 1) - target_xts
  quant_099_next <- quantile(
    dif_target_target_nex[dif_target_target_nex > 0],
    threshold,
    na.rm = TRUE
  )
  dif_target_target_nex <- dif_target_target_nex[
    dif_target_target_nex > quant_099_next |
      dif_target_target_nex < -quant_099_next
  ]
  dif_target_target_nex <- as.character(time(dif_target_target_nex))
  dif_target_target <- intersect(dif_target_target_pre, dif_target_target_nex)
  
  # intersecting both checks
  rm_dates <- intersect(dif_target_target, dif_target_nearby)
  
  
  print("rm_dates:")
  print(rm_dates)
  print(paste("Number of dates in rm_dates: ", length(rm_dates)))
  
  qc_data <- qc_data_flagged <- target_xts
  qc_data[rm_dates] <- NA
  qc_data_flagged[!is.na(qc_data_flagged)] <- 0
  qc_data_flagged[rm_dates] <- 1
  
  out <- list(
    qc_data = qc_data,
    qc_data_flagged = qc_data_flagged,
    rm_dates = rm_dates # Add rm_dates here
  )
  
  return(out)
}
