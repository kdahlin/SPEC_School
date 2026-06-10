#' Function to classify unclassified points in a LAZ file as noise or canopy 
#' 
#' This function reads in a a LAS object and classifies  
#' any unclassified data in the table as either noise (7) or canopy (5) using 
#' the IQR on a localized, per-voxel scale. This allows for the user to adjust
#' the voxel resolution depending on the landscape; coarser resolution is more
#' suitable for flat landscapes, and finer resolution is more suitable for 
#' landscapes with variable topography. The function also allows the option to 
#' mask or include the ground & canopy points in the IQR calculations, and then 
#' overwrites them back to their original classifications after the calculation 
#' is finished. 
#' 
#' @param las A LAS object from the lidR package containing the LiDAR
#' point cloud data and associated attributes (e.g., X, Y, Z coordinates,
#' return number, intensity, and classification).
#' @param voxel_res The x,y resolution of the voxels for calculating the IQR.
#' @param outlier.k Outlier coefficient, typically set as 1.5 or 3.
#' @param min_iqr Minimum IQR for a column of voxels - if IQR is below this 
#' threshold, all points will be classified as valid canopy (5).
#' @param canopy_mask TRUE / FALSE statement that allows the user to select if they
#' want the canopy points to be used when calculated the IQR (FALSE) or if they 
#' want those points to be masked (TRUE)
#' @param ground_mask TRUE / FALSE statement that allows the user to select if they
#' want the ground points to be used when calculated the IQR (FALSE) or if they 
#' want those points to be masked (TRUE)
#' @return A LAS object with updated point classifications where outliers
#' are labeled as noise (7), valid canopy points as (5), and ground points
#' preserved as (2).
#' @export
#' 

flag.outliers.las <- function(las, voxel_res = 40, outlier.k = 3,
                              min_iqr = 1, canopy_mask = TRUE, 
                              ground_mask = TRUE) {
  
  # Load required packages
  for (pkg in c("data.table", "lidR")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop(paste0("Package '", 
                                                            pkg, "' is required."))
  }

  
  dt <- copy(las@data)
  
  # Add voxel columns to data table by user specified resolution 
  dt[, voxel_x := floor(X / voxel_res)]
  dt[, voxel_y := floor(Y / voxel_res)]
  
  # Assign new classification based on column-level IQR
  dt[, Classification := {
    
    # Number of points in voxel
    n <- .N
    cls_new <- rep(NA_integer_, n)
    
    # write ground points to new variable so we can restore them later if we 
    # don't mask them during the IQR calculation
    original_ground <- Classification == 2L
    original_canopy  <- Classification == 5L
    
    # Logical masks to optionally exclude pre classified ground and canopy points 
    ground_points <- if (ground_mask) original_ground else rep(FALSE, .N)
    canopy_points <- if (canopy_mask) original_canopy else rep(FALSE, .N)
    
    # Use only unmasked points to compute IQR - if ground / canopy mask = TRUE
    # it will exclude those points from the calculation. If ground / canopy mask 
    # = FALSE they will be included in the calculation 
    usable_points <- !(ground_points | canopy_points)
    z_vals <- Z[!(ground_points | canopy_points)]
    
    # Compute IQR
    q <- quantile(z_vals, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    
    if (!is.na(iqr) && iqr >= min_iqr) {
      t.lower <- q[1] - outlier.k * iqr
      t.upper <- q[2] + outlier.k * iqr
      
      # Assign outliers to unmasked points outside IQR
      cls_new[!(ground_points | canopy_points) & (Z < t.lower | Z > t.upper)] <- 7L
      
      # Assign valid to unmasked points inside IQR
      cls_new[!(ground_points | canopy_points) & Z >= t.lower & Z <= t.upper] <- 5L
    } else {
      
      # Small IQR: all unmasked points valid
      cls_new[!(ground_points | canopy_points)] <- 5L
    }
    
    # if you didn't mask the ground and canopy points, make sure to rewrite them back to 
    # their original classifications before exporting
    cls_new[original_ground] <- 2L
    cls_new[original_canopy] <- 5L
    
    # assign the new classifications to voxels
    cls_new
  }, by = .(voxel_x, voxel_y)]
  
  dt[, c("voxel_x", "voxel_y") := NULL]
  
  las@data <- dt
  
  return(las)
}




