#' Convert LAD estimates into six rasters explaining the height distribution of LAD within the canopy
#'  - the height of the 75th, 25th, 50th, 75th, 90th quantiles as well as the mean
#'
#' This function reads in a the LAD estimates that were previously calculated,
#' calculates the height where a host of different quantiles and the mean occur and return a raster
#' showing the height where each quantile occurs within a given vertical column.
#'
#' These forest structure attributes are based off calculations from:
#'
#' Shi, Y., Wang, T., Skidmore, A.K., and Heurich, M. (2018). Important LiDAR metrics for 
#' discriminating forest tree species in Central Europe. 
#' ISPRS Journal of Photogrammetry and Remote Sensing, 137, 163-174. 
#' https://doi.org/75.7516/j.isprsjprs.2018.02.002
#'
#' @param lad.array LAD estimate array that was generated using the machorn.lad function.
#' @param laz.array Voxelized LiDAR array that was generated using the laz.to.array function. 
#' This contains spatial information for all arrays.
#' @param ht.cut Height that calculations will exclude. This is to remove understory LAD estimates from
#' further calculations. If 5 is entered then all voxels 5 meters and above will be included. 
#' Enter 0 if you want to include all calculations
#' @param epsg.code EPSG code so that the rasters can be projected into the appropriate projection
#' @return A list containing the quantile and mean rasters.
#' @export

lad.quantiles <- function(lad.array, laz.array, ht.cut, epsg.code) {

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' required but not installed.")
  }
  

  # Get LAD array dimensions
  n_y <- dim(lad.array$rLAD)[2]  # y-dimension
  n_x <- dim(lad.array$rLAD)[3]  # x-dimension
  n_z <- dim(lad.array$rLAD)[1]  # vertical dimension

  #create a bunch of empty matrices to store the data in
  foliage.10.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)
  foliage.25.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)
  foliage.50.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)
  foliage.75.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)
  foliage.90.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)
  foliage.mean.mat <- matrix(data = NA, nrow = n_y, ncol = n_x)

  if(ht.cut < n_z){

    #loop through the lad array
    for (r in 1:n_y) {

      for (c in 1:n_x) {

        # pull out an individual column
        canopy.column <- lad.array$rLAD[(ht.cut + 1):n_z,r,c]

        # check to see if there is data in the column
        if (all(is.na(canopy.column)) == TRUE) {

          #if there is not then give it an NA value
          foliage.10.mat[r,c] <- NA
          foliage.25.mat[r,c] <- NA
          foliage.50.mat[r,c] <- NA
          foliage.75.mat[r,c] <- NA
          foliage.90.mat[r,c] <- NA
          foliage.mean.mat[r,c] <- NA

        } else {

          # remove the NA values from the column
          canopy.column.no.na <- canopy.column[!is.na(canopy.column)]

          # flag those columns for review:
          if (any(canopy.column.no.na < 0)) 
            warning("Negative LAD in voxel at (", r, ",", c, ")")
          canopy.column.no.na[canopy.column.no.na < 0] <- 0

          # calculate cumulative LAD
          cum.lad <- cumsum(canopy.column.no.na)

          # calculate the quantiles
          foliage.quantile <- quantile(cum.lad, 
                                       probs = c(0.1,
                                                 0.25, 
                                                 0.5, 
                                                 0.75,
                                                 0.90), 
                                       na.rm = TRUE)

          #calculate the mean
          foliage.mean <- mean(cum.lad, na.rm = TRUE)

          #find the height at which these values occur
          foliage.10.ht <- which(abs(cum.lad - foliage.quantile[[1]]) ==
                                   min(abs(cum.lad - foliage.quantile[[1]])))

          foliage.25.ht <- which(abs(cum.lad - foliage.quantile[[2]]) ==
                                   min(abs(cum.lad - foliage.quantile[[2]])))

          foliage.50.ht <- which(abs(cum.lad - foliage.quantile[[3]]) ==
                                   min(abs(cum.lad - foliage.quantile[[3]])))

          foliage.75.ht <- which(abs(cum.lad - foliage.quantile[[4]]) ==
                                   min(abs(cum.lad - foliage.quantile[[4]])))

          foliage.90.ht <- which(abs(cum.lad - foliage.quantile[[5]]) ==
                                   min(abs(cum.lad - foliage.quantile[[5]])))

          foliage.mean.ht <- which(abs(cum.lad - foliage.mean) ==
                                     min(abs(cum.lad - foliage.mean)))

          #save these values to the matrix - if the value occurs between two points then take the average
          foliage.10.mat[r,c] <- sum(foliage.10.ht) / length(foliage.10.ht)
          foliage.25.mat[r,c] <- sum(foliage.25.ht) / length(foliage.25.ht)
          foliage.50.mat[r,c] <- sum(foliage.50.ht) / length(foliage.50.ht)
          foliage.75.mat[r,c] <- sum(foliage.75.ht) / length(foliage.75.ht)
          foliage.90.mat[r,c] <- sum(foliage.90.ht) / length(foliage.90.ht)
          foliage.mean.mat[r,c] <- sum(foliage.mean.ht) / length(foliage.mean.ht)
        }
      }
    }
  } else{
    print(sprintf("ht.cut value >= %g, return NA values", n_z))
  }
  
  # save all our matrices in a list 
  mat.list <- list(
    quantile.10.raster = foliage.10.mat, 
    quantile.25.raster = foliage.25.mat, 
    quantile.50.raster = foliage.50.mat, 
    quantile.75.raster = foliage.75.mat, 
    quantile.90.raster = foliage.90.mat, 
    mean.raster = foliage.mean.mat) 

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

  #return the final rasters
  return(stack.rast)


}
