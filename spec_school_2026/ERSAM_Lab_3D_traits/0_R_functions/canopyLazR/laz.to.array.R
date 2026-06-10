#' Convert a  LAS/LAZ object to a voxelized array
#'
#' This function converts a normalized LAS/LAZ point cloud into a 3D voxel array.
#' Optionally, it fills missing ground voxels using a focal mean.
#'
#' @param laz.file.path Character. Path to the normalized (ground ~ 0) LAS/LAZ files
#' @param global.z.max Numeric. Global maximum Z value across cleaned tiles,
#' used to standardize the vertical voxel grid.
#' @param voxel.resolution Numeric. The spatial resolution (x,y) you want the output voxel 
#' to be - this is a single number where both sides of the cell will be the same.
#' @param z.resolution Numeric. The vertical resolution of the voxel.
#' @param z.buffer Numeric. Buffer used for the global z value to be used across
#' all tiles. 
#' @param fill.ground Logical. Whether to fill missing ground voxels with 
#' neighborhood mean.
#' @param fw.size Integer. Focal window size for ground smoothing.
#' Must be an odd number. Default = NA. 
#' @return A list containing:
#'  - array: voxelized 3D array [z, y, x] (ground = layer 1)
#'  - x.bin, y.bin, z.bin: bin edges for each axis
#'  
#'  @details
#' The input point cloud is assumed to be height-normalized such that ground is
#' approximately 0, with a small tolerance retained to preserve valid ground returns.
#'
#' The resulting voxel array has dimensions [z, y, x] and is structured as:
#' \itemize{
#'   \item values > 0: number of LiDAR returns within a voxel
#'   \item values = 0: valid voxels within the local vertical extent that contain no returns
#'   \item values = NA: padding above the local maximum occupied height of each column
#' }
#'
#' A global Z maximum is used to standardize the vertical grid across tiles,
#' allowing voxel arrays to be compared and merged. Within each column,
#' NA padding preserves the distinction between structural extent and empty space.
#' 
#' @export

laz.to.array <- function(laz.file.path, 
                         global.z.max,
                         voxel.resolution = 5, 
                         z.resolution = 1,
                         z.buffer = 5,
                         fill.ground = FALSE,
                         fw.size = NA
                         ) {
  
  # --- Read LAZ file and convert to data table---
  laz <- readLAS(laz.file.path)
  dt <- as.data.table(laz@data)
  setnames(dt, c("X", "Y", "Z"), c("x", "y", "z"))
  
  # --- remove the laz object for data storage savings
  rm(laz)
  
  #---- Define x/y bins from point extents 
  x.range <- range(dt$x, na.rm = T)
  y.range <- range(dt$y, na.rm = T)
  
  # convert range values to ceiling and floor so they line up with voxel sizes
  x.bin <- seq(floor(x.range[1]/voxel.resolution)*voxel.resolution,
               ceiling(x.range[2]/voxel.resolution)*voxel.resolution,
               by = voxel.resolution)
  
  y.bin <- seq(floor(y.range[1]/voxel.resolution)*voxel.resolution,
               ceiling(y.range[2]/voxel.resolution)*voxel.resolution,
               by = voxel.resolution)
  
  #---- Define z bins
  # lower bound from clean tile 
  z.min <- floor(min(dt$z, na.rm = T) / z.resolution) * z.resolution
  
  # upper bound is standardized across all tiles 
  z.global <- ceiling((global.z.max + z.buffer) / z.resolution) * z.resolution
  
  # create z bin based on shared vertical grid across tiles 
  z.bin <- seq(z.min, z.global, by = z.resolution)
  
  #----- Create array dimensions
  n_x <- length(x.bin) - 1L
  n_y <- length(y.bin) - 1L
  n_z <- length(z.bin) - 1L
  
  #---- Voxel indexing 
  x0 <- x.bin[1]
  y0 <- y.bin[1]
  z0 <- z.bin[1]
  
  dt[, voxel_x := floor((x - x0) / voxel.resolution) + 1L]
  dt[, voxel_y := floor((y - y0) / voxel.resolution) + 1L]
  dt[, voxel_z := floor((z - z0) / z.resolution) + 1L]
  
  # Coerce upper-edge cases into the final valid voxel
  dt[voxel_x > n_x, voxel_x := n_x]
  dt[voxel_y > n_y, voxel_y := n_y]
  dt[voxel_z > n_z, voxel_z := n_z]
  
  # --- Identify columns with valid ground support
  # A column is valid only if it contains at least one point classified
  # as ground (2)
  
  ground_cols <- unique(
    dt[Classification == 2L, .(voxel_y, voxel_x)]
  )
  
  # Valid ground return matrix on voxel grid: [y, x]
  ground_mat <- matrix(FALSE, nrow = n_y, ncol = n_x)
  
  if (nrow(ground_cols) > 0) {
    ground_mat[cbind(ground_cols$voxel_y, ground_cols$voxel_x)] <- TRUE
  }
  
  # --- Optional: expand ground support using neighborhood fill
  # This step "fills" empty ground cells (e.g. no ground returns in the 
  # original LAZ file) if they are near cells that have valid ground
  # returns.
  
  if (fill.ground) {
    
    ground_count.dt <- dt[Classification == 2L, .N, by = .(voxel_y, voxel_x)]
    
    ground_counts <- matrix(0L, nrow = n_y, ncol = n_x)
    
    ground_counts[cbind(ground_count.dt$voxel_y, ground_count.dt$voxel_x)
                  ] <- ground_count.dt$N
  
    r <- (fw.size - 1L) %/% 2L
    
    filled_counts <- matrix(0, nrow = n_y, ncol = n_x)
    n_neighbors   <- matrix(0L, nrow = n_y, ncol = n_x)
    
    for (dy in -r:r) {
      for (dx in -r:r) {
        
        src_rows <- max(1L, 1L - dy):min(n_y, n_y - dy)
        src_cols <- max(1L, 1L - dx):min(n_x, n_x - dx)
        
        tgt_rows <- max(1L, 1L + dy):min(n_y, n_y + dy)
        tgt_cols <- max(1L, 1L + dx):min(n_x, n_x + dx)
        
        filled_counts[tgt_rows, tgt_cols] <-
          filled_counts[tgt_rows, tgt_cols] +
          ground_counts[src_rows, src_cols]
        
        n_neighbors[tgt_rows, tgt_cols] <-
          n_neighbors[tgt_rows, tgt_cols] + 1L
      }
    }
    
    neighborhood_mean <- filled_counts / n_neighbors
    
    ground_counts_filled <- ground_counts
    zero_cells <- ground_counts == 0L
    
    ground_counts_filled[zero_cells] <- neighborhood_mean[zero_cells]
    
    ground_mat <- ground_counts_filled > 0
  }
  
  # Convert ground matrix back to valid column table
  valid_ground <- which(ground_mat, arr.ind = TRUE)
  
  valid_cols <- data.table(
    voxel_y = valid_ground[, 1],
    voxel_x = valid_ground[, 2]
  )
  
  # --- Keep only points in valid ground-supported columns
  dt_valid <- dt[
    valid_cols,
    on = .(voxel_y, voxel_x),
    nomatch = 0
  ]
  
  # --- Initialize full array as NA
  voxel.array <- array(NA_real_, dim = c(n_z, n_y, n_x))
  
  # --- Determine local maximum occupied bin per x-y column
  # This is for padding the distance between column max z and global max z
  # with NA's so we can merge tiles and rasters upstream. 
  col_max <- dt_valid[, .(max_bin = max(voxel_z, na.rm = TRUE)), 
                      by = .(voxel_y, voxel_x)]
  
  # Fill valid part (below the max z of a given column) of each column with 0
  for (i in seq_len(nrow(col_max))) {
    voxel.array[
      1:col_max$max_bin[i],
      col_max$voxel_y[i],
      col_max$voxel_x[i]
    ] <- 0
  }
  
  # Save valid positions before creating count array
  valid_idx <- which(!is.na(voxel.array))
  
  # create a linear index formula for creating array
  lin_idx <- dt_valid$voxel_z +
    (dt_valid$voxel_y - 1L) * n_z +
    (dt_valid$voxel_x - 1L) * (n_z * n_y)
  
  # Fill array with pulse counts 
  voxel_counts <- tabulate(lin_idx, nbins = n_z * n_y * n_x)
  voxel.count.array <- array(voxel_counts, dim = c(n_z, n_y, n_x))
  
  # Overwrite valid voxels with actual counts, preserving NA padding above local canopy
  voxel.array[valid_idx] <- voxel.count.array[valid_idx]
  
  # Return array + bin edges
  return(list(
    array = voxel.array,
    x.bin = x.bin,
    y.bin = y.bin,
    z.bin = z.bin
  ))
}
