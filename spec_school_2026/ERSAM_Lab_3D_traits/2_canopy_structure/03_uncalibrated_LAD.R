# Title: 03_uncalibrated_LAD.R
# Date: 09/16/2025
# Authors:  AGK, KMD, AWB, MS, AJP & TRG 
# This script for checking for outliers in point clouds then calculating LAD, 
# LAI, DTM, DSM, and CHM from NEON lidar before the Beer-Lambert calculation
# (DTM, DSM, and CHM do not need to be re-calculated later, doing this here
# as a double-check on outliers or other point cloud weirdness)

# Load libraries
library(tidyverse)
library(terra)
library(lidR)
library(data.table)
#-----------

# read in each of the functions 
source("./0_R_functions/canopyLazR/laz.to.array.R")
source("./0_R_functions/canopyLazR/array.to.chm.raster.R")
source("./0_R_functions/canopyLazR/machorn.lad.est.R")
source("./0_R_functions/canopyLazR/lad.array.to.raster.R")

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

# define the number of understory layers (z dimension) you want to remove
min.height <-5

# load log file from cleaning point clouds and define global z value 
# for all the tiles you will be processing 
z.log <- read_csv(file.path(root, "shared_data", "NEON_proc_data", site, 
                            year, paste0("CleanLidarPointClouds_", clean.date), 
                            "clean_pointcloud_log-final.csv"))

global.z.max <- max(z.log$max_z, na.rm = TRUE)

# now define a buffer for max z values 
z.buffer <- 5 

#---------------------------
# set directories
#---------------------------

# set the input for the laz files
laz.files.dir <- file.path(root, "shared_data", "NEON_proc_data", site, 
                       year, paste0("CleanLidarPointClouds_", clean.date))

#list all the files in the path
laz.files.list <- list.files(laz.files.dir, pattern=c("\\.laz$|.las$"))
print(laz.files.list)

# create output directories for individual tiles and full site 
out.indiv <- file.path(root, "shared_data", "NEON_proc_data", site, 
                       year, paste0("LidarMetrics_", vox.xy,'x', vox.xy,'x', 
                                    vox.z), 
                       paste0("IndividualFiles_", wd), "unmasked_rasters")

out.site <- file.path(root, "shared_data", "NEON_proc_data", site, year, 
                      paste0("LidarMetrics_", vox.xy,'x', vox.xy,'x', vox.z), 
                      paste0("FullSite_", wd))

Rdata.dir <- file.path(file.path(root, "shared_data", "NEON_proc_data", site, 
                                 year, paste0("LidarMetrics_", vox.xy,'x', 
                                              vox.xy,'x', vox.z), 
                                 paste0("IndividualFiles_", wd), "Rdata"))

# define list of metric sub directories
metric.subdirs <- c("lai_uncalibrated", "chm")

# create metric directories if they don't exist
dir.list <- file.path(out.indiv, metric.subdirs)

dirs <- c(dir.list, out.site, Rdata.dir)

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    message("Created directory: ", d)
  } else {
    message("Directory exists: ", d)
  }
}

# create list of subdirectory names to call later 
sub.dirs <- setNames(as.list(file.path(out.indiv, metric.subdirs)), 
                     metric.subdirs)

# check strsplit
file.name <- laz.files.list[1]

# this should give you the utm coordinates from the tile -> if not, fix in the
# loop below!
tile.numb <- strsplit(file.name, "_")[[1]][5:6]
print(tile.numb)

#-------------------------------------------------------------------------------
# MAIN LOOP: 
# calculate LAD for each individual tile - this should run without input
#-------------------------------------------------------------------------------


for (i in 1:length(laz.files.list)) {
  
  print(i)
  print(paste0("Processing: ", laz.files.list[i]))
  
  # Convert .laz or .las file into a voxelized lidar array
  vox.array <- laz.to.array(
    laz.file.path = file.path(laz.files.dir, laz.files.list[i]), 
    global.z.max = global.z.max,
    z.buffer = z.buffer, 
    voxel.resolution = vox.xy, 
    z.resolution = vox.z,
    fill.ground = TRUE,
    fw.size = 3)
  
  #------------------------------------------------------
  
  # Estimate LAD for each voxel in leveled array
  lad.estimates <- machorn.lad.est(lidar.array = vox.array,
                                 z.resolution = vox.z,
                                 beer.lambert.constant = NULL)
  
  # Convert the LAD array into a single raster stack
  lad.raster <- lad.array.to.raster(lad.array = lad.estimates,
                                            laz.array = vox.array,
                                            epsg.code = epsg)
  
  # Convert the list of LAZ arrays into a chm raster
  chm.rasters <- array.to.chm.raster(laz.array = vox.array,
                                     epsg.code = epsg)
  
  # Let's put a fail safe in case the lad raster doesn't contain any voxels over 
  # 10 meters
  if (nlyr(lad.raster) > 10){
    
    print("Calculating forest attributes!")
    
    # remove the bottom specified layers of LAD to remove influence
    #  from the under story 
    lad.raster.subset <- subset(lad.raster, min.height:nlyr(lad.raster)) 
    
    # Create a single LAI raster from the LAD raster stack
    lai.raster <- terra::app(lad.raster.subset, fun = sum, na.rm = TRUE)
    
    #---------------------------------------------------------------------------
    # Write all the needed files to disc
    #---------------------------------------------------------------------------
    
    # some file output name prep
    file.name <- laz.files.list[i]
    tile.numb <- strsplit(file.name, "_")[[1]][5:6]
    
    # laz array
    saveRDS(vox.array, file = paste0(Rdata.dir, "/", site, "_",
                                 tile.numb[1], "_", tile.numb[2], 
                                 "_laz_array.rds"))
    # metric raster list
    rasters.to.save <- list(
      lai_uncalibrated = lai.raster,
      chm = chm.raster)
    
    #---------------------------
    # Write rasters automatically
    #---------------------------
    for (metric in names(rasters.to.save)) {
      terra::writeRaster(
        rasters.to.save[[metric]],
        filename = file.path(sub.dirs[[metric]], 
                             paste0(site, "_", tile.numb[1], "_", tile.numb[2], 
                                    "_", metric,".tif")),
        filetype = "GTiff",
        overwrite = TRUE
      )
    }
    
    rm(vox.array, 
       lad.estimates, 
       lad.raster, 
       lad.raster.subset, 
       lai.raster, 
       chm.rasters
       )
    
  }  else{
    
    print("Skipping tile...no returns over 10m!")
    
    rm(vox.array, 
       lad.estimates, 
       lad.raster, 
       chm.rasters)
    
  }
  
  print(paste0(length(laz.files.list) - i, " tiles remaining!"))
  
}

#----------------------------
# MERGE ONLY THE LAI TILES
#---------------------------

lai.files <- list.files(file.path(out.indiv, "lai_uncalibrated"),
                        pattern = "\\.tif$",
                        full.names = TRUE)

print(lai.files)

if (length(lai.files) == 0) {
  stop("No LAI rasters found.")
}

lai.list <- vector("list", length(lai.files))

for (q in seq_along(lai.files)) {
  lai.list[[q]] <- rast(lai.files[q])
}

merged.lai <- do.call(terra::merge, lai.list)

terra::writeRaster(
  merged.lai,
  filename = file.path(out.site, paste0(site, "_", year, "_lai_uncalibrated.tif")),
  filetype = "GTiff",
  overwrite = TRUE
)

print("Merged LAI raster written successfully.")



