# Title: 06_LAD_metrics.R
# Date: 09/16/2025
# Authors:  AGK, KMD, AWB, MS, AJP & TRG 
# This script for calculating LAD-derived metrics from NEON lidar after Beer-
# Lambert calibration in previous steps
#------------
# so far we have: 
# 1.) downloaded point clouds
# 2.) normalized the point clouds (ground points = 0) and removed outliers
# 3.) calculated uncalibrated LAD and LAI 
# 4.) calculated the Beer-Lambert coefficient (k) by creating linear 
# model of DHP LAI and LiDAR LAI
# 5.) used the K we calculated to rerun the machorn LAD calculation and 
# calibrate our LAD and LAI. 

# where we are now:
# We will now use our calibrated LAD estimates and original array 
# structure to calculate LAD ratio, volume, and texture metrics. 
#-----------

# Load libraries
library(tidyverse)
library(terra)
#-----------

# read in each of the functions (which are currently in process)
source("./0_R_functions/canopyLazR/lad.ht.max.R")
source("./0_R_functions/canopyLazR/canopy.porosity.ratio.R")
source("./0_R_functions/canopyLazR/canopy.porosity.volume.R")
source("./0_R_functions/canopyLazR/lad.vertical.sd.R")
source("./0_R_functions/canopyLazR/lad.quantiles.R")
source("./0_R_functions/canopyLazR/canopy.volume.R")

#-------------------------
# USER DEFINED VARIABLES
#-------------------------

# read in config file with site info
source("0_config_files/config_base.R")  

# date of "Individual Files" folder you want to use
indiv.date <- "20260522"

# define the resolution of your voxels
vox.xy <- 10
vox.z <- 1

# define the number of understory layers (z dimension) you want to remove
min.height <- 5

#---------------------------
# set directories
#---------------------------

# set the input for the laz array RDS files
laz.array.dir <- file.path(root, "shared_data", "NEON_proc_data", site, year,
                           paste0("LidarMetrics_", vox.xy,'x', vox.xy,'x', vox.z),
                           paste0("IndividualFiles_", indiv.date),
                           "Rdata")

#list all the files in the path
laz.array.list <- sort(list.files(laz.array.dir, 
                                  pattern="_laz_array\\.(RData|rds)$", 
                                  full.names = TRUE))
print(laz.array.list)

# set the input for the LAD estimates 
lad.estimates.dir <- file.path(file.path(root, "shared_data", "NEON_proc_data", site, year,
                                 paste0("LidarMetrics_", vox.xy,'x', vox.xy,'x', vox.z), 
                                 paste0("IndividualFiles_", indiv.date), 
                                 "Rdata"))
#list all the files in the path
lad.estimates.list <- sort(list.files(lad.estimates.dir, 
                                      pattern="_lad\\.(RData|rds)$", 
                                      full.names = TRUE))
print(lad.estimates.list)


# define base dir for tile metrics 
base.dir <- file.path(root, "shared_data", "NEON_proc_data", site, year,
                      paste0("LidarMetrics_", vox.xy, "x", vox.xy, "x", vox.z),
                      paste0("IndividualFiles_", indiv.date))

unmasked.dir <- file.path(base.dir, "unmasked_rasters")

# define list of metric sub directories
metric.subdirs <- c(
  "max_lad", 
  "max_lad_height",
  "filled_voxel_ratio",
  "porosity_ratio", 
  "sd_lad_column",
  "quantile_10",
  "quantile_25",
  "quantile_50",
  "quantile_75", 
  "quantile_90",
  "mean_lad_height",
  "euphotic_volume",
  "euphotic_tla", 
  "euphotic_depth",
  "oligophotic_volume",
  "oligophotic_tla",
  "empty_volume"
)

# create metric directories if they don't exist
dir.list <- file.path(unmasked.dir, metric.subdirs)


for (d in dir.list) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    message("Created directory: ", d)
  } else {
    message("Directory exists: ", d)
  }
}

# create list of subdirectory names to call later 
dirs <- setNames(as.list(file.path(unmasked.dir, metric.subdirs)), metric.subdirs)


# check strsplit
file.name <- basename(laz.array.list[1])

# this should give you the utm coordinates from the tile -> if not, fix in the
# loop below!
tile.numb <- strsplit(file.name, "_")[[1]][2:3]
print(tile.numb)

#-------------------------------------------------------------------------------
# MAIN LOOP:
# load the Rdata/RDS files of the .laz and LAD estimate arrays and 
# calculate LiDAR metrics
#-------------------------------------------------------------------------------

for (i in seq_along(laz.array.list)) {
  
  print(i)
  print(paste0("Processing: ", lad.estimates.list[i], "and", laz.array.list[i]))
  
    # load the files
    vox.array <- readRDS(laz.array.list[i])
    lad.estimates <- readRDS(lad.estimates.list[i])
    
    # Calculate max LAD and height of max LAD
    max.lad <- lad.ht.max(lad.array = lad.estimates,
                            laz.array = vox.array,
                            ht.cut = min.height,
                            epsg.code = epsg)
    
    # Calculate the ratio of filled and empty voxels in a given column of the canopy
    empty.filled.ratio <- canopy.porosity.ratio(lad.array = lad.estimates,
                                                         laz.array = vox.array,
                                                         ht.cut = min.height,
                                                         epsg.code = epsg)
    
    # Calculate the volume of filled and empty voxels in a given column of the canopy
    empty.filled.volume <- canopy.porosity.volume(lad.array = lad.estimates,
                                                           laz.array = vox.array,
                                                           ht.cut = min.height,
                                                           xy.res = vox.xy,
                                                           z.res = vox.z,
                                                           epsg.code = epsg)
    
    vertical.sd.lad <- lad.vertical.sd(lad.array = lad.estimates,
                                                laz.array = vox.array,
                                                ht.cut = min.height,
                                                epsg.code = epsg)
    
    # Calculate the heights of various LAD quantiles
    ht.quantiles <- lad.quantiles(lad.array = lad.estimates,
                                    laz.array = vox.array,
                                    ht.cut = min.height,
                                    epsg.code = epsg)
    
    # Calculate various canopy volume metrics from Lefsky
    canopy.volume.metrics <- canopy.volume(lad.array = lad.estimates,
                                  laz.array = vox.array,
                                  ht.cut = min.height,
                                  xy.res = vox.xy,
                                  z.res = vox.z,
                                  epsg.code = epsg)
    
    # We can calculate the depth of the euphotic zone by dividing by the volume 
    # of the voxel
    euphotic.depth <- (canopy.volume.metrics$euphotic.volume.column.raster / 
      (vox.xy * vox.xy * vox.z))
    
    #-----------------------------------------
    # Lets write all the needed files to disc
    #-----------------------------------------
    
    #some file output name prep
    file.name <- basename(laz.array.list[i])
    tile.numb <- strsplit(file.name, "_")[[1]][2:3]
    
    rasters.to.save <- list(
      max_lad = max.lad[[1]],
      max_lad_height = max.lad[[2]],
      filled_voxel_ratio = empty.filled.ratio[[1]],
      porosity_ratio = empty.filled.ratio[[2]],
      sd_lad_column = vertical.sd.lad,
      quantile_10 = ht.quantiles[[1]],
      quantile_25 = ht.quantiles[[2]],
      quantile_50 = ht.quantiles[[3]],
      quantile_75 = ht.quantiles[[4]],
      quantile_90 = ht.quantiles[[5]],
      mean_lad_height = ht.quantiles[[6]],
      euphotic_volume = canopy.volume.metrics[[1]],
      euphotic_tla = canopy.volume.metrics[[2]],
      euphotic_depth = euphotic.depth,
      oligophotic_volume = canopy.volume.metrics[[3]],
      oligophotic_tla = canopy.volume.metrics[[4]],
      empty_volume = canopy.volume.metrics[[5]]
    )
    
    #---------------------------
    # Write rasters automatically
    #---------------------------
    for (metric in names(rasters.to.save)) {
      terra::writeRaster(
        rasters.to.save[[metric]],
        filename = file.path(dirs[[metric]], 
                             paste0(site, "_", tile.numb[1], "_", tile.numb[2], 
                                    "_", metric,".tif")),
        filetype = "GTiff",
        overwrite = TRUE
      )
    }
    
    #---------------------------------------------------------------------------
    # # Lets clean up some memory so we can run another tile
    # #-------------------------------------------------------------------------
    
    gc()
    rm(vox.array,
       lad.estimates,
       max.lad,
       empty.filled.ratio,
       empty.filled.volume,
       vertical.sd.lad,
       ht.quantiles,
       can.volume,
       euphotic.depth)
    gc()
    
    print(paste0(length(laz.files.list) - i, " tiles remaining!"))
    
  }  
  

