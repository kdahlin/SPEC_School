#' Convert LAD estimates into two rasters - height of maximum LAD and maximum LAD within a column
#'
#' This function reads in a the LAD estimates that were previously calculated,
#' finds the maximum LAD value within each column of voxels, and then finds the 
#' height where that value occurs. 
#' The output is a list containing two rasters, one for each calculation.
#'
#' These forest structure attributes are based off calculations from:
#'
#' Hardiman, B., Bohrer, G., Gough, C., & Curtis, P. (2013).
#' Canopy structural changes following widespread mortality of canopy dominant trees.
#' Forests, 4, 537-552. https://doi.org/10.3390/f4030537
#'
#' @param lad.array LAD estimate array that was generated using the machorn.lad function.
#' @param laz.array Voxelized LiDAR array that was generated using the laz.to.array function. 
#' This contains spatial information for all arrays.
#' @param ht.cut Height that calculations will exclude. This is to remove understory 
#' LAD estimates from further calculations. If 5 is entered then all voxels 5 meters 
#' and above will be included. Enter 0 if you want to include all calculations
#' @param epsg.code EPSG code so that the rasters can be projected into the appropriate projection
#' @return A list containing max LAD and height of max LAD rasters.
#' @export

lad.ht.max <- function(lad.array, laz.array, ht.cut, epsg.code) {

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' required but not installed.")
  }
  

  # Get LAD array dimensions
  n_y <- dim(lad.array$rLAD)[2]  # y-dimension
  n_x <- dim(lad.array$rLAD)[3]  # x-dimension
  n_z <- dim(lad.array$rLAD)[1]  # vertical dimension


  # Create empty matrices to hold max LAD and height of max LAD
  max.lad.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)
  max.lad.ht.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)

  if (ht.cut < n_z){
    # Loop over each horizontal (x, y) cell in the LAD array
    for (r in 1:n_y) {

      for (c in 1:n_x) {

        # Extract the vertical column (z-axis) of LAD values, starting from ht.cut
        lad.column <- lad.array$rLAD[(ht.cut + 1):n_z,r,c]

        if (all(is.na(lad.column))) {
          # If the whole column is NA, record NA
          max.lad.mat[r, c] <- NA
          max.lad.ht.mat[r, c] <- NA

        } else {

          # Find the max LAD and the height(s) at which it occurs
          max.lad <- max(lad.column, na.rm = TRUE)
          max.lad.indices <- which(lad.column == max.lad) + ht.cut  # Correct for cut offset

          # Record max LAD and choose a representative height if the 
          # max occurs in multiple layers
          max.lad.mat[r, c] <- max.lad

          # If the max LAD value occurs at multiple hieghts: record the lowest height 
          # for max_lad == 0, record the highest height for max_lad > 0
          max.lad.ht.mat[r, c] <- if (max.lad > 0) {
            max(max.lad.indices) 
            { else { 
              min(max.lad.indices)
              }
        }
      }
    }
  } else {
      print(sprintf("ht.cut value >= %g, return NA rasters", n_z))
  }

  # create named list from our matrices
  mat.list <- list(
    max.lad.raster = max.lad.mat,
    max.lad.ht.raster = max.lad.ht.mat
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

  # Return both rasters in a list
  return(stack.rast)

}
