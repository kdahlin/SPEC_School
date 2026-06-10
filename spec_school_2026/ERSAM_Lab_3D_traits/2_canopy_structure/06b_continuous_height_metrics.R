# Title: 06b_continuous_height_metrics.R
# Date: 05/06/2026
# Author: TRG

# This script is for calculating continuous relative height at whatever x,y 
# voxel resolution you want. Use Prob = 0.1 to make a canopy height model. 
# NOTE: YOUR LIDAR NEEDS TO BE NORMALIZED (ground = 0)!!!!!!!!!!!!!!

library(lidR)
library(terra)
library(data.table)

#-------------------------
# USER DEFINED VARIABLES
#-------------------------

# read in config file with site info
source("0_config_files/config_base.R")  

# type in the date you processed the clean data so you can pull 
# from that directory
clean.date <- "20260330"

# define voxel resolution
vox.xy <- 10
vox.z <- 1

# define the RH metric you desire (.50, .75, .98, .1)
prob <- 0.5
metric <- prob * 100 # for file naming

#---------------------------
# set directories
#---------------------------

# set the input for the laz files
laz.files <- file.path(root, "shared_data", "NEON_proc_data", site, 
                       year, paste0("CleanLidarPointClouds_", clean.date))

# create output directories for individual tiles and full site 
out.indiv <- file.path(root, "shared_data", "NEON_proc_data", site, 
                       year, paste0("LidarMetrics_", vox.xy,'x', vox.xy,'x', vox.z), 
                       paste0("IndividualFiles_", wd), 
                       "unmasked_rasters", paste0("rh", metric))

# create the directory if it doesn't exist 
if (!dir.exists(out.indiv)){
  dir.create(out.indiv, recursive = TRUE)
  message("Directory created ", out.indiv)
} else {
  message("Directory exists ", out.indiv)
}

#list all the files in the path
laz.files.list <- list.files(laz.files, pattern=c("\\.laz$|.las$"))
print(laz.files.list)


#-------------------------------------------------------------------------------
# MAIN LOOP: 
# calculate RH metric for each raster in the directory 
#-------------------------------------------------------------------------------

for (i in seq_along(laz.files.list)) {
  
  print(paste("Processing:", basename(laz.files.list[i])))
  
  # --- Read LAZ file and convert to data table---
  laz <- readLAS(file.path(laz.files, laz.files.list[i]))
  dt <- as.data.table(laz@data)
  setnames(dt, c("X", "Y", "Z"), c("x", "y", "z"))
  
  # --- remove the laz object for data storage savings
  rm(laz)
  
  #---- Define x/y bins from point extents 
  x.range <- range(dt$x, na.rm = T)
  y.range <- range(dt$y, na.rm = T)
  
  # convert range values to ceiling and floor so they line up with voxel sizes
  x.bin <- seq(floor(x.range[1]/vox.xy)*vox.xy,
               ceiling(x.range[2]/vox.xy)*vox.xy,
               by = vox.xy)
  
  y.bin <- seq(floor(y.range[1]/vox.xy)*vox.xy,
               ceiling(y.range[2]/vox.xy)*vox.xy,
               by = vox.xy)
  
  
  #----- Create matrix dimensions
  n_x <- length(x.bin) - 1L
  n_y <- length(y.bin) - 1L
  
  #---- Matrix indexing 
  x0 <- x.bin[1]
  y0 <- y.bin[1]
  
  dt[, voxel_x := floor((x - x0) / vox.xy) + 1L]
  dt[, voxel_y := floor((y - y0) / vox.xy) + 1L]
  
  # Coerce upper-edge cases into the final valid matrix 
  dt[voxel_x > n_x, voxel_x := n_x]
  dt[voxel_y > n_y, voxel_y := n_y]
  
  # calculate RH profile in each cell
  rh_profile <- dt[, .(
    RH = as.numeric(quantile(z, prob, na.rm = TRUE))
  ), by = .(voxel_x, voxel_y)]
  
  mat <- matrix(NA_real_, nrow = n_y, ncol = n_x)
  mat[cbind(rh_profile$voxel_y, rh_profile$voxel_x)] <- rh_profile$RH
  
  # vertical flip 
  mat <- mat[nrow(mat):1, ]
  
  # convert to raster
  rast <- rast(mat)
  
  # assign extents
  mat.ext <- ext(min(x.bin), max(x.bin),min(y.bin), max(y.bin))
  
  ext(rast) <- mat.ext
  
  # assign crs 
  crs(rast) <- sprintf("EPSG:%s", epsg)
  
  # some file output name prep
  file.name <- laz.files.list[i]
  tile.numb <- strsplit(file.name, "_")[[1]][5:6]
  
  out.file <- file.path(out.indiv,
                        paste0(site, "_", tile.numb[1], 
                               "_", tile.numb[2], "_rh", metric,".tif")
  )
  
  writeRaster(
    rast,
    filename = out.file,
    filetype = "GTiff",
    overwrite = TRUE
  )
  
  rm(
     ext, 
     mat, 
     rast
  )
  
  print("Finished processing! Woo-hooooooo!")
  
}
  
  
  
  