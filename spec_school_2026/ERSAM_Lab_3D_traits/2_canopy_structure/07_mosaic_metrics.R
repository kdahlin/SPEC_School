# Title: 07_mosaic_metrics.R
# Date: 09/16/2025
# Author:  AJP, KMD, AWB, TRG
# This script for mosaicking the LAD metric rasters, and passing a 
# moving window average over the metrics that require neighborhood 
# context 
#------------
# so far we have: 
# 1.) downloaded point clouds
# 2.) normalized the point clouds (ground points = 0) and removed outliers
# 3.) calculated uncalibrated LAD and LAI 
# 4.) calculated the Beer-Lambert coefficient (k) by creating linear 
# model of DHP LAI and LiDAR LAI
# 5.) used the K we calculated to rerun the machorn LAD calculation and 
# calibrate our LAD and LAI. 
# 6.) calculated LAD metrics 

# where we are at now: 
# WE will now mosaic the full site of our rasters and calculate a moving window 
# average for several of the metrics taht require it.
#----------------------

# Load libraries
library(tidyverse)
library(terra)

#-----------

# read in each of the functions 
source("./0_R_functions/canopyLazR/open.gap.volume.R")

#-------------------------
# USER DEFINED VARIABLES
#-------------------------

# read in config file with site info
source("0_config_files/config_base.R")  

# date of "individual files" folder you want to use
indiv.date <- "20251103"

# define the resolution of your voxels
vox.xy <- 10
vox.z <- 1

# define the moving window size for your calculation.
# Note; the window must be an odd number. 
window.size <- 3

#---------------------------
# set directories
#---------------------------

# set the input for the processed metric folders
lad.metrics.dir <- file.path(root, "shared_data", "NEON_proc_data", site, year,
                           paste0("LidarMetrics_", vox.xy,'x', vox.xy,'x', vox.z),
                           paste0("IndividualFiles_", indiv.date), 
                           "unmasked_rasters")

fullsite.dir <- file.path(root, "shared_data", "NEON_proc_data", site, year,
                        paste0("LidarMetrics_", vox.xy,'x', vox.xy,'x', vox.z),
                        "full_site", indiv.date)

if (!dir.exists(fullsite.dir)) {
  dir.create(fullsite.dir, recursive = TRUE)
  message("Created directory: ", fullsite.dir)
} else {
  message("Directory exists: ", fullsite.dir)
}

#-------------------------------------------------------------------------------
# Let's merge together all the individual tiles into one giant raster
#-------------------------------------------------------------------------------

#list the folders that contain the rasters we want to stitch together
laz.folders <- list.dirs(lad.metrics.dir, recursive = FALSE)

for (i in seq_along(laz.folders)) {
  
  message("Merging rasters from: ", laz.folders[i] )
  
  #list all the files in the folder we want to process
  rasters.list <- list.files(laz.folders[i], pattern="\\.tif$", 
                             full.names = TRUE)
  
  #make an empty list
  rst.list <- list()
  
  #load all the rasters into the list
  for (q in 1:length(rasters.list)) {
    rst<- rast(rasters.list[q])
    
    rst.list[[q]] <- rst
  }
  
  # merge all the rasters together
  merged.raster <- do.call(terra::merge, rst.list)
  
  # lets make the file name
  file.name <- rasters.list[q]
  tile.type <- strsplit(file.name, "/")[[1]][9]
  
  # set the out file path 
  out_file <- file.path(fullsite.dir, paste0(site, "_", year, "_", tile.type, 
                                             ".tif"))
  
  # ORIGINAL:
  # write the raster to the folder
  terra::writeRaster(merged.raster, filename = out_file, 
                     filetype = "GTiff", overwrite = TRUE)
  
  message("Merged raster successfully saved to: ", out_file)
}


#-------------------------------------------------------------------------------
# Pass a 3x3 moving window over some of the rasters to make our last rasters
#-------------------------------------------------------------------------------

# within canopy rugosity
canopy.rugosity.rast <- terra::rast(paste0(fullsite.dir, "/", site, "_", 
                              year, "_sd_lad_column.tif"))

canopy.rugosity <- focal(canopy.rugosity.rast, w = window.size, fun = sd, 
                         na.rm = TRUE)

terra::writeRaster(canopy.rugosity, 
                   filename = paste0(fullsite.dir,  "/", 
                                     site, "_", year, 
                                     "_within_canopy_rugosity.tif"),
                   filetype = "GTiff", overwrite = TRUE)

# volume of empty space
empty.volume.rast <- terra::rast(paste0(fullsite.dir, "/", site,
                                  "_", year, "_empty_volume.tif"))

canopy.empty.vol <- focal(empty.volume.rast, w = window.size, 
                          fun = sum, na.rm = TRUE)

terra::writeRaster(canopy.empty.vol, 
                   filename = paste0(fullsite.dir, "/", site,
                                     "_", year, "_empty_canopy_volume.tif"),
                   filetype = "GTiff", overwrite = TRUE)

# volume of euphotic zone
euphotic.volume.rast <- terra::rast(paste0(fullsite.dir, "/", site, 
                                  "_", year, "_euphotic_volume.tif"))

canopy.euphotic.vol <- focal(euphotic.volume.rast, w = window.size, 
                             fun = sum, na.rm = TRUE)

terra::writeRaster(canopy.euphotic.vol, 
                   filename = paste0(fullsite.dir, "/", site, 
                                     "_", year, "_canopy_euphotic_volume.tif"),
                   filetype = "GTiff", overwrite = TRUE)

# volume of oligophotic zone
oligophotic.volume.rast <- terra::rast(paste0(fullsite.dir, "/", site, "_", 
                               year, "_oligophotic_volume.tif"))

canopy.oligophotic.vol <- focal(oligophotic.volume.rast, w = window.size, 
                                fun = sum, na.rm = TRUE)

terra::writeRaster(canopy.oligophotic.vol, 
                   filename = paste0(fullsite.dir, "/", site, 
                                     "_", year, "_canopy_oligophotic_volume.tif"),
                   filetype = "GTiff", overwrite = TRUE)

# total leaf area in the euphotic zone
euphotic.tla.rast <- terra::rast(paste0(fullsite.dir, "/", site, "_", 
                               year, "_euphotic_tla.tif"))

canopy.euphotic.tla <- focal(euphotic.tla.rast , w = window.size, 
                             fun = sum, na.rm = TRUE)

terra::writeRaster(canopy.euphotic.tla, 
                   filename = paste0(fullsite.dir, "/", site, 
                                     "_", year, "_canopy_euphotic_tla.tif"),
                   filetype = "GTiff", overwrite = TRUE)

# total leaf area in the oligophotic zone
oligophotic.tla.rast <- terra::rast(paste0(fullsite.dir, "/", site, "_", 
                               year, "_oligophotic_tla.tif"))

canopy.oligophotic.tla <- focal(oligophotic.tla.rast, w = window.size, 
                                fun = sum, na.rm = TRUE)

terra::writeRaster(canopy.oligophotic.tla, 
                   filename = paste0(fullsite.dir, "/", site, 
                                     "_", year, "_canopy_oligophotic_tla.tif"),
                   filetype = "GTiff", overwrite = TRUE)

# open gap volume at the top of the canopy
chm.rast <- terra::rast(paste0(fullsite.dir, "/", site, "_", 
                               year, "_chm.tif"))

toc.open.gap <- open.gap.volume(chm.rast, 
                                xy.res = vox.xy, 
                                z.res = vox.z,
                                window.size = window.size)

terra::writeRaster(toc.open.gap, 
                   filename = paste0(fullsite.dir, "/", site, 
                                     "_", year, "_toc_open_gap.tif"),
                   filetype = "GTiff", overwrite = TRUE)

