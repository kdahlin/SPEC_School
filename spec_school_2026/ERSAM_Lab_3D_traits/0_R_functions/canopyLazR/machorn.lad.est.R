#' Convert a LiDAR array into an array of LAD estimates
#'
#' This function reads in the LiDAR array created with the laz.to.array function 
#' and uses the MacArthur-Horn method to estimate leaf area density (LAD)
#' within each voxel. The sum of each column is equal to the leaf area index 
#' (LAI) of that column of voxels.
#'
#' @param laz.array List. Array of normalized LiDAR returns calculated 
#' using the laz.to.array function.
#' @param z.resolution Numeric. The vertical resolution of the voxel. Default 1.
#' @param beer.lambert.constant Numeric. Beer Lambert coefficient used for
#' calibrated LAD values. If NULL, a default value of 1 is used. 
#' @return A list containing:
#'        - rLAD: a voxel array of relative leaf area density
#'        - shots.in: the number of pulses entering each voxel
#' @details
#' The input voxel array represents vertical profiles of LiDAR return counts:
#' \itemize{
#'   \item values > 0: observed returns
#'   \item values = 0: valid empty voxels
#'   \item values = NA: padding above local canopy height
#' }
#'
#' The function computes cumulative pulse attenuation from the top of the canopy
#' downward and applies the Beer-Lambert formulation of the MacArthur-Horn method:
#' \deqn{LAD = \frac{1}{k \cdot dz} \ln{\left(\frac{I_{in}}{I_{through}}\right)}}
#'
#' NA values are temporarily replaced with 0 during calculations and restored
#' afterward to preserve the structural padding of the array.
#'
#' @export

machorn.lad.est <- function(laz.array, z.resolution, beer.lambert.constant = NULL){
  
  # Extract pulse counts from voxel array
  voxel.N.pulse <- lidar.array$array
  
  # create a mask to preserve the original array structure 
  valid_mask <- !is.na(voxel.N.pulse)
  
  # Replace NAs with 0 to simplify math
  voxel.N.pulse[is.na(voxel.N.pulse)] <- 0
  
  n_z <- dim(voxel.N.pulse)[1] # define vertical dimension 
  
  # Initialize array to store cumulative pulse counts from top to ground
  pulse.accum <- array(0, dim = dim(voxel.N.pulse))
  
  # Accumulate pulses down the canopy (top layer to bottom)
  for(i in n_z:1) {
    
    # Top of the canopy layer: same as the number of pulses at the TOC
    if(i == n_z) {
      pulse.accum[i,,] <- voxel.N.pulse[i,,]
    } else {
      pulse.accum[i,,] <- pulse.accum[i + 1,,] + voxel.N.pulse[i,,]
    }
  }
  
  # Replicate total column pulse counts across all vertical layers
  pulse.all <- array(rep(pulse.accum[1,,], each = n_z), 
                     dim = dim(voxel.N.pulse))
  
  # Number of pulses that passed through（exit) each voxel 
  # (if there were 1000 ground hits and 100 hits at the TOC, 
  # then 900 pulses would have went through that first voxel)
  shots.through <- pulse.all - pulse.accum
  
  # Compute how many shots entered each voxel (inflow from above)
  shots.in <- array(0, dim = dim(voxel.N.pulse))
  if (n_z > 1) {
    shots.in[1:(n_z - 1), , ] <- shots.through[2:n_z, , ]
  }
  shots.in[n_z, , ] <- pulse.accum[1, , ] # Top-layer inflow is total 
                                          # pulses observed in the column
  
  # Use default Beer-Lambert constant if not provided
  k <- if (is.null(beer.lambert.constant)) {
    message("MacArthur-Horn constant is not set. Using k = 1.")
    1
  } else {
    message(sprintf("MacArthur-Horn constant k = %.2f", beer.lambert.constant))
    beer.lambert.constant
  }
  
  # Apply Beer-Lambert law to estimate Relative Leaf Area Density
  rLAD <- log(shots.in/shots.through) * (1 / (k * z.resolution))
  
  # remove invalid numerical values 
  rLAD[is.infinite(rLAD) | is.nan(rLAD)] <- NA
  rLAD[rLAD < 0] <- NA
  
  # Restore NA padding above local canopy
  rLAD[!valid_mask] <- NA
  shots.in[!valid_mask] <- NA
  
  out <- list(
    rLAD = rLAD,
    shots.in = shots.in
  )
  
  rm(pulse.accum, voxel.N.pulse)
  
  return(out)
  
}