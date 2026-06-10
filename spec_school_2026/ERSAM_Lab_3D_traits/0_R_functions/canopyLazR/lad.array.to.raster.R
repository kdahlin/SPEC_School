#' Convert a voxelized LAD array into a raster stack
#'
#' This function converts the voxelized LAD array created with the 
#' machorn.lad.est function into a raster stack, where each raster 
#' represents a vertical slice of the canopy.
#'
#' @param lad.array List. Voxelized LAD array created with the 
#' machorn.lad.est function
#' @param laz.array List. Original LiDAR voxel array created with the 
#' laz.to.array function. This object provides the spatial extent and voxel grid
#' used to georeference the stack. 
#' @param epsg.code Integer. EPSG code used to assign the raster projection.
#' @return A raster stack of LAD estimates, where each layer corresponds to a 
#' vertical canopy slice. 
#' @details
#' The LAD array preserves the structure of the original voxel grid:
#' \itemize{
#'   \item numeric values represent LAD estimates
#'   \item NA values represent padding above local canopy height
#' }
#'
#' The resulting raster stack maintains vertical canopy structure and can be
#' used for layer-wise analysis or mosaicking across tiles with a shared grid.
#' 
#' @export

lad.array.to.raster <- function(lad.array, laz.array, epsg.code) {
  
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' required but not installed.")
  }
  
  # reorder from [z, y, x] -> [y, x, z]
  lad.testr <- aperm(lad.array$rLAD, c(2, 3, 1))
  
  # flip vertically
  lad.testr <- lad.testr[dim(lad.testr)[1]:1, , , drop = FALSE]
  
  # convert array to raster stack
  lad.rasters <- terra::rast(lad.testr)
  
  # add spatial reference
  crs(lad.rasters) <-  sprintf("EPSG:%g", epsg.code)
  
  # set spatial extent
  xmin <- laz.array$x.bin[1]
  xmax <- laz.array$x.bin[length(laz.array$x.bin)]
  ymin <- laz.array$y.bin[1]
  ymax <- laz.array$y.bin[length(laz.array$y.bin)]
  terra::ext(lad.rasters) <- terra::ext(xmin, xmax, ymin, ymax)
  
  # lets rename the layers now to make sense - first layer is 0-1 meters, etc.
  names(lad.rasters) <- paste0("m_", seq_len(nlyr(lad.rasters)))
  
  # return the final raster
  return(lad.rasters)
}