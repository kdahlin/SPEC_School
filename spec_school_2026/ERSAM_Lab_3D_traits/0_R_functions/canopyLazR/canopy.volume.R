#' Convert LAD estimates into fives rasters explaining the volume and total leaf area of the 
#' euphotic (portion of the canopy where 65% of the leaf material is located) 
#' and the oligophotic zone (the remaining canopy) and the volume
#' of empty space within the canopy (from top of the canopy the ground)
#'
#' This function reads in a the LAD estimates that were previously calculated,
#' calculates the height cutoff for the euphotic zone, calculates the volume and total leaf area of this
#' portion of the canopy and then does the same for the remaining canopy. It then returns 5 rasters
#'
#' These forest structure attributes are based off calculations from:
#'
#' Lefsky, M.A., Cohen, W.B., Acker, S.A., Parker, G.G., Spies, T.A., and Harding, D. (1999).
#' Lidar Remote Sensing of the Canopy Structure and Biophysical Properties of 
#' Douglas-Fir Western Hemlock Forests. Remote Sensing of the Environment, 70, 339-361. 
#' https://doi.org/10.1016/S0034-4257(99)00052-8
#'
#' @param lad.array LAD estimate array that was generated using the machorn.lad function.
#' @param laz.array Voxelized LiDAR array that was generated using the laz.to.array function. 
#' This contains spatial information for all arrays.
#' @param ht.cut Height that calculations will exclude. This is to remove understory LAD estimates from
#' further calculations. If 5 is entered then all voxels 5 meters and above will be included. Enter 0 if
#' you want to include all calculations
#' @param xy.res Horizontal resolution of each voxel - if it is 10x10 meters then just enter 10
#' @param z.res Vertical resolution of each voxel - if it is 1 meter then just enter 1
#' @param epsg.code EPSG code so that the rasters can be projected into the appropriate projection
#' @return A list containing the quantile and mean rasters.
#' @export

canopy.volume <- function(lad.array, laz.array, ht.cut, xy.res, z.res, epsg.code) {

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' required but not installed.")
  }
  

  # Get LAD array dimensions
  n_y <- dim(lad.array$rLAD)[2]  # y-dimension
  n_x <- dim(lad.array$rLAD)[3]  # x-dimension
  n_z <- dim(lad.array$rLAD)[1]  # vertical dimension

  # initiate empty matrices
  euphotic.volume.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)
  euphotic.tla.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)
  oligophotic.volume.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)
  oligophotic.tla.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)
  empty.volume.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)

  if(ht.cut < n_z){

    # loop through the lad estimates
    for (r in 1:n_y) {
      for (c in 1:n_x) {

        # pull out a slice of the column
        canopy.column <- lad.array$rLAD[(ht.cut + 1):n_z,r,c]

        # check to see if there is actually data in the column
        if (all(is.na(canopy.column)) == TRUE) {

          euphotic.volume.mat[r,c] <- NA
          euphotic.tla.mat[r,c] <- NA
          oligophotic.volume.mat[r,c] <- NA
          oligophotic.tla.mat[r,c] <- NA

        } else {

          # find the LAI
          foliage.sum <- round(sum(canopy.column, na.rm = TRUE), digits = 4)

          # find the total leaf area of the euphotic zone
          # a.k.a where 65% of the total leaf area is in the canopy
          euphotic.tla <- round(foliage.sum * 0.65, digits = 4)

          # remove the na values
          valid.profile <- canopy.column[!is.na(canopy.column)]

          if (foliage.sum == 0) {

            euphotic.volume.mat[r,c] <- 0
            euphotic.tla.mat[r,c] <- 0
            oligophotic.volume.mat[r,c] <- 0
            oligophotic.tla.mat[r,c] <- 0
            empty.volume.mat[r,c] <- length(valid.profile) * xy.res * xy.res * z.res

          } else {

            # we need to reverse the order of this list since we want the upper 
            # most 65% so we need to start our count from the 
            # top rather than the bottom
            canopy.lad.rev <- rev(valid.profile)

            # we need to remove all voxels that have a value of zero because 
            # those are empty zones and are not used in the calculation of 
            # euphotic or oligophotic zones - but we will save that information
            canopy.filled.voxels <- canopy.lad.rev[canopy.lad.rev > 0]

            empty.voxels <- sum(valid.profile == 0)
            
            # find the cumulative sum of all the filled voxels
            foliage.cumsum <- cumsum(canopy.filled.voxels)
            
            # find the depth where 65% of tla is located
            foliage.depth.65 <- which.min(abs(foliage.cumsum - euphotic.tla))
  
            # Calculate volume metrics by multiplying by voxel resolution 
            euphotic.volume <- foliage.depth.65 * xy.res * xy.res * z.res

            oligophotic.volume <- (length(canopy.filled.voxels) - foliage.depth.65) * xy.res * xy.res * z.res
            oligophotic.tla <- round(foliage.sum - euphotic.tla, digits = 4)

            empty.volume <- empty.voxels * xy.res * xy.res * z.res

            euphotic.volume.mat[r,c] <- euphotic.volume
            euphotic.tla.mat[r,c] <- euphotic.tla
            oligophotic.volume.mat[r,c] <- oligophotic.volume
            oligophotic.tla.mat[r,c] <- oligophotic.tla
            empty.volume.mat[r,c] <- empty.volume

          }
        }
      }
    }
  }else{
    print(sprintf("ht.cut value >= %g, return NA rasters", n_z))
  }
  
  # create named list of matrices 
  mat.list <- list(
    euphotic.volume.column.raster = euphotic.volume.mat,
    euphotic.tla.column.raster = euphotic.tla.mat,
    oligophotic.volume.column.raster = oligophotic.volume.mat,
    oligophotic.tla.column.raster = oligophotic.tla.mat,
    empty.volume.column.raster = empty.volume.mat
  )

  # set geographic extent
  xmin <- laz.array$x.bin[1]
  xmax <- laz.array$x.bin[length(laz.array$x.bin)]
  ymin <- laz.array$y.bin[1]
  ymax <- laz.array$y.bin[length(laz.array$y.bin)]
  
  # flip matrices vertically and create rasters 
  stack.rast <- lapply(mat.list, function(m){
    terra::rast(m[nrow(m):1, , drop = FALSE])
  })
  
  
  # set spatial extent & spatial reference
  stack.rast <- lapply(stack.rast, function(x){
    terra::ext(x) <- terra::ext(xmin, xmax, ymin, ymax)
    terra::crs(x) <- sprintf("EPSG:%g", epsg.code)
    
    return(x)
    }
  )

# return the rasters
  return(stack.rast)
}
