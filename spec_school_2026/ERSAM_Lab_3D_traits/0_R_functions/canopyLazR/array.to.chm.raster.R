#' Convert a voxelized LiDAR array into height rasters 
#'
#' This function converts a voxelized LiDAR array into
#' canopy height model (CHM) rasters 
#'
#' @param laz.array List. Voxelized LiDAR array from laz.to.array
#' @param epsg.code Integer. EPSG code used to assign the raster projection.
#' @return A list containing:
#'        - CHM rasters
#' @details
#' The voxel array encodes canopy structure such that:
#' \itemize{
#'   \item values > 0 indicate occupied voxels
#'   \item values = 0 indicate valid empty voxels
#'   \item values = NA indicate padding above local canopy height
#' }
#'
#' Canopy height is estimated by identifying the highest occupied voxel in each
#' x-y column and mapping that voxel index to height using the z-bin definition.
#'
#' Ground is treated as approximately 0 due to prior height normalization of the
#' point cloud, and CHM is computed as canopy height minus ground height.
#'
#' @export

array.to.chm.raster <- function(laz.array, epsg.code) {
  
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' required but not installed.")
  }
  
  array <- laz.array$array
  z.bin <- laz.array$z.bin
  
  n_z <- dim(array)[1]
  n_y <- dim(array)[2]
  n_x <- dim(array)[3]
  
  # Identify occupied voxels - the return is logical where TRUE = occupied 
  # (a.k.a valid, non-zero returns.)
  # Reverse the z-axis so the highest occupied voxel in each column becomes
  # the first TRUE value encountered.
  # Then convert those reversed indices back to the original z indexing and
  # map them to canopy height using z.bin.
  occupied <- !is.na(array) & array > 0 
  
  # Reverse z so we can find the top-most TRUE 
  occupied_rev <- occupied[n_z:1, , ]
  
  # Find first TRUE along z (top-down)
  top_idx_rev <- apply(occupied_rev, c(2, 3), function(col) {
    idx <- which(col)[1]
    if (length(idx) == 0) return(NA_integer_)
    idx
  })
  
  # Convert back to original z indexing
  top_idx <- ifelse(
    is.na(top_idx_rev),
    NA_integer_,
    n_z - top_idx_rev + 1
  )
  
  # Convert voxel index to height
  canopy.mat <- matrix(NA_real_, nrow = n_y, ncol = n_x)
  canopy.mat[!is.na(top_idx)] <- z.bin[top_idx[!is.na(top_idx)]]
  
  # Ground = 0 where column exists
  # Note: Point clouds are height-normalized, but a tolerance is applied
  # (e.g., ±0.05 m) to retain valid ground points. Ground is therefore
  # treated as approximately zero rather than exactly zero.
  valid.cols <- apply(!is.na(array), c(2, 3), any)
  ground.mat <- matrix(NA_real_, nrow = n_y, ncol = n_x)
  ground.mat[valid.cols] <- 0
  
  # subtract ground.,at from canopy.mat to make CHM
  chm.mat <- canopy.mat - ground.mat
  
  # flip orientation of the matrix and convert to raster
  canopy.raster <- terra::rast(
    chm.mat[nrow(chm.mat):1, , drop = FALSE])
  
  # define spatial extent
  xmin <- laz.array$x.bin[1]
  xmax <- laz.array$x.bin[length(laz.array$x.bin)]
  ymin <- laz.array$y.bin[1]
  ymax <- laz.array$y.bin[length(laz.array$y.bin)]
  
  # set extent and CRS
  terra::ext(canopy.raster) <- terra::ext(xmin, xmax, ymin, ymax)
  terra::crs(canopy.raster) <- sprintf("EPSG:%g", epsg.code)
  
  # return the final rasters
  return(canopy.raster)
  
}