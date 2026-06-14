#' Convert LAD estimates into a column-wise vertical standard deviation raster
#'
#' Calculates the standard deviation of LAD values within each vertical x/y column.
#' This raster can be mosaicked and used as the input for a 3x3 focal standard
#' deviation to calculate within-canopy rugosity.
#' 
#' These forest structure attributes are based off calculations from:
#'
#' Hardiman, B. S., Bohrer, G., Gough, C. M., Vogel, C. S., & Curtis, P. S. (2011).
#' The role of canopy structural complexity in wood net primary production of a 
#' maturing northern deciduous forest. Ecology, 92, 1818-1827. 
#' https://doi.org/10.1890/10-2192.1
#'
#' @param lad.array LAD estimate array that was generated using the machorn.lad function.
#' @param laz.array Voxelized LiDAR array that was generated using the laz.to.array function. 
#' This contains spatial information for all arrays.
#' @param ht.cut Height that calculations will exclude. This is to remove understory LAD estimates 
#' from further calculations. If 5 is entered then all voxels 5 meters and above will be included. 
#' Enter 0 if you want to include all calculations
#' @param epsg.code EPSG code so that the rasters can be projected into the appropriate projection
#' @return A raster of the standard deviation of individual voxel columns.
#' @export

lad.vertical.sd <- function(lad.array, laz.array, ht.cut, epsg.code) {
  
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' required but not installed.")
  }
  
  # Get LAD array dimensions
  n_y <- dim(lad.array$rLAD)[2]  # y-dimension
  n_x <- dim(lad.array$rLAD)[3]  # x-dimension
  n_z <- dim(lad.array$rLAD)[1]  # vertical dimension

  #Lets create an empty matrix that corresponds with each final raster
  sd.lad.col.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)

  if (ht.cut < n_z){
    #loop through the lad array and calculate the standard deviation of each vertical column
    #within the canopy
    for (r in 1:n_y) {
      for (c in 1:n_x) {
        canopy.column <- lad.array$rLAD[(ht.cut + 1):n_z, r, c]
        sd.lad.col <- round(sd(canopy.column, na.rm = TRUE), digits = 4)
        sd.lad.col.mat[r,c] <- sd.lad.col
      }
    }
  }else{
    print(sprintf("ht.cut value >= %g, return NA rasters", n_z))
  }

    # convert sd.lad.col from matrix to raster
    sd.lad.raster <- terra::rast(
      sd.lad.col.mat[nrow(sd.lad.col.mat):1, , drop = FALSE])

    # set geographic extent
    xmin <- laz.array$x.bin[1]
    xmax <- laz.array$x.bin[length(laz.array$x.bin)]
    ymin <- laz.array$y.bin[1]
    ymax <- laz.array$y.bin[length(laz.array$y.bin)]

    # set spatial extent and reference
    crs(sd.lad.raster) <- sprintf("EPSG:%g", epsg.code)
    ext(sd.lad.raster) <- ext(xmin, xmax, ymin, ymax)


  #return the final rasters
  return(sd.lad.raster)

}
