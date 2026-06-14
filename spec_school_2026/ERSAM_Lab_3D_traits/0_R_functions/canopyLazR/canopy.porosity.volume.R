#' Convert LAD estimates into two rasters - volume of filled canopy and volume of canopy porosity
#'
#' This function reads in a the LAD estimates that were previously calculated,
#' finds the volume of voxels in a given column that contain a LAD estimate and the volume of
#' voxels in a given column that are empty (i.e. no LAD estimates). The output is a list
#' containing two rasters, one for each calculation.
#'
#' These forest structure attributes are based off calculations from:
#'
#' Hardiman, B., Bohrer, G., Gough, C., & Curtis, P. (2013).
#' Canopy structural changes following widespread mortality of canopy dominant trees.
#' Forests, 4, 537-552. https://doi.org/10.3390/f4030537
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
#' @param xy.res Resolution of xy coordinates - if it is a 10x10 meter pixel then enter 10 here
#' @param z.res Vertical resolution of voxel - if it is 1 meter tall then enter 1
#' @param epsg.code EPSG code so that the rasters can be projected into the appropriate projection
#' @return A named list containing filled voxel volume and porosity volume rasters.
#' @export

canopy.porosity.volume <- function(lad.array, laz.array, ht.cut, xy.res, z.res, epsg.code) {

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' required but not installed.")
  }

  # Get LAD array dimensions
  n_y <- dim(lad.array$rLAD)[2]  # y-dimension
  n_x <- dim(lad.array$rLAD)[3]  # x-dimension
  n_z <- dim(lad.array$rLAD)[1]  # vertical dimension

  # Create empty matrices to hold porosity and filled ratio matrix
  porosity.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)
  filled.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)

  if (ht.cut < n_z){
    #loop through the array and calculate the volume of filled and empty voxels
    for (r in 1:n_y) {

      for (c in 1:n_x) {

        canopy.column <- lad.array$rLAD[(ht.cut + 1):n_z,r,c]

        filled <- sum(canopy.column > 0, na.rm = TRUE)
        porosity <- sum(canopy.column == 0, na.rm = TRUE)

        filled.volume <- round(filled * xy.res * xy.res * z.res, digits = 4)
        porosity.volume <- round(porosity * xy.res * xy.res * z.res, digits = 4)

        filled.mat[r,c] <- filled.volume
        porosity.mat[r,c] <- porosity.volume
      }
    }

  }else{
    print(sprintf("ht.cut value >= %g, return NA rasters", n_z))
  }
  
  # list matrices 
  mat.list <- list (
    filled.volume.raster = filled.mat,
    empty.volume.raster = porosity.mat
  )
  
  # flip the matrices vertically and convert to raster 
  stack.rast <- lapply(mat.list, function(m){
    terra::rast(m[nrow(m):1, , drop = FALSE])
  })
  
  # set lat/lon extent
  xmin <- laz.array$x.bin[1]
  xmax <- laz.array$x.bin[length(laz.array$x.bin)]
  ymin <- laz.array$y.bin[1]
  ymax <- laz.array$y.bin[length(laz.array$y.bin)]
  
  # set spatial extent & spatial reference
  stack.rast <- lapply(stack.rast, function(x){
    terra::ext(x) <- terra::ext(xmin, xmax, ymin, ymax)
    terra::crs(x) <- sprintf("EPSG:%g", epsg.code)
    
    return(x)
    }
  )
  
  # return the final rasters
  return(stack.rast)
}
