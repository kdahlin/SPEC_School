# Title: 02_clean_point_cloud.R
# Date: 03/03/2026
# Authors:  AGK, KMD, & TRG 
# This script is for creating a DTM from LAZ files, using the DTM to normalize the 
# elevation, and then removing outliers from the normalized data using a 
# voxel-wise IQR method. The clean LAZ files are exported to be used 
# in calculating canopy metrics.

# Load libraries
library(terra)
library(tidyverse)
library(lidR)
library(data.table)
#-----------

# read in required functions
source("./0_R_functions/canopyLazR/flag.outliers.las.R")

#-------------------------
# USER DEFINED VARIABLES - Directories and Site Specific
#-------------------------

# read in config file with site info
source("0_config_files/config_mlbs_kmd_20260608.R")

# set old working dates for things you are reading in (might be different from
# current working date set in config file)
tileCoords.wd <- "20260608"

# define voxel resolution
vox.xy <- 10
vox.z <- 1

#---------------------------
# set directories
#---------------------------

# set the input for the laz files 
laz.files.dir <- file.path(root, "shared_data", "NEON_AOP_data",
                       site, year, "L1", "DiscreteLidar", "ClassifiedPointCloud")

#list all the files in the path
laz.files.list <- list.files(laz.files.dir, pattern=c("\\.laz$|.las$"))

#get rid of any files that are less than 500kb
laz.files.list <- laz.files.list[sapply(paste0(laz.files.dir, "/", laz.files.list), 
                                        file.size) > 500000]
print(laz.files.list)

# create output directories for individual tiles and full site  
clean.pointclouds.dir <- file.path(root, "shared_data", "NEON_proc_data", site, 
                                   year, paste0("CleanLidarPointClouds_", wd))

dtm.dir <- file.path(root, "shared_data", "NEON_proc_data", site, year, 
                     paste0("LidarMetrics_", vox.xy,'x', vox.xy,'x', vox.z), 
                     paste0("IndividualFiles_", wd), "unmasked_rasters", 
                     "DTM")

# create metric directories if they don't exist
dirs <- c(clean.pointclouds.dir, dtm.dir)

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    message("Created directory: ", d)
  } else {
    message("Directory exists: ", d)
  }
}

# check strsplit
file.name <- laz.files.list[1]
# this should give you the utm coordinates from the tile -> if not, fix in the
# loop below!
tile.numb <- strsplit(file.name, "_")[[1]][5:6]
print(tile.numb)

#-----------------------
# OPTIONAL: load tile coordinates list and filter your input laz files 
#-----------------------

#### If you have a list of tiles you want to read in, call it here:
# if not, set this to NA (like if you want to process all the tiles in the folder)
tile.coords <- read.csv(file.path(root, "shared_data", "NEON_field_data", site, 
                                  year, "ERSAM", "GPS_Data", "Processed",
                                  paste0(site, substr(year, 3, 4),"_", 
                                         "TileCoordinates_2kmBuffer_", tileCoords.wd, 
                                         ".csv")))


# handle the case where tile.coords was set to NA (process all tiles)
if (length(tile.coords) == 1 && is.na(tile.coords)) {
  message("tile.coords is NA — processing all tiles in the folder")
} else {
  # ensure tile.coords has the expected columns
  if (!is.data.frame(tile.coords) || !all(c("easting","northing") %in% 
                                          names(tile.coords))) {
    stop("tile.coords must be NA or a data.frame with columns 'easting' and 'northing'")
  }
  # filter files if they contain the coordinates 
  files.to.keep <- paste0(tile.coords$easting, "_", 
                          as.integer(tile.coords$northing))
  
  pattern <- paste(files.to.keep, collapse = "|")
  
  laz.files.list <- laz.files.list[str_detect(laz.files.list, pattern)]
  
  if (length(laz.files.list) == 0) {
    warning("No .laz files matched the supplied tile.coords")
  }
}

#-------------------------------------------------------------------------------
# Point Cloud Diagnostics - Iterative Workflow
#-------------------------------------------------------------------------------
# This section processes each point cloud tile and may require adjustments 
# depending on the landscape characteristics and tile-specific anomalies.
#
# Steps:
# 1) Generate a DTM using TIN; optional focal smoothing can be applied 
#    to reduce artifacts at triangle edges.
# 2) Normalize the point cloud heights relative to the DTM so that ground 
#    elevation is set to zero.
# 3) Remove subterranean points (below a specified threshold) and reclassify 
#    points within a small height window as ground.
# 4) Identify outliers in the point cloud by computing voxel-wise IQR at a 
#    user-specified resolution. Adjust parameters as needed per tile.
# 5) Resample DTM to voxel resolution and export with clean laz file. 
#-------------------------------------------------------------------------------

#-------------------
# USER DEFINED VARIABLES - Parameters for Cleaning Loop
#-------------------

# Resolution (m) of the TIN-derived DTM used for height normalization
TIN_res <- 1

# Width of the focal smoothing window applied to the DTM to reduce
# small TIN artifacts and edge effects; larger values produce a smoother surface
dtm_smoothing <- 5

# estimate a reasonable max z for getting rid of extreme vertical outliers
# that may be erroneously classified as canopy. Damn birds!
max.ht.cut <- 50

# Minimum normalized height (m) to retain after normalization.
# Points below this threshold are removed as subterranean noise.
subground_threshold <- -0.05

# Absolute height tolerance (m) around zero used to reclassify points as ground
# after normalization
ground_class_threshold <- 0.05


# Horizontal voxel size (m) used for localized IQR-based outlier detection
# Larger values are appropriate for flatter landscapes.
# Smaller values are appropriate for heterogeneous terrain.
IQR_voxel_res <- 200

# IQR multiplier controlling how aggressively outliers are identified.
# Smaller values are more conservative; larger values retain more points.
outlier_k <- 3

#---- Initialize loop 

# initialize log file for loop 
log.file <- file.path(clean.pointclouds.dir, "clean_pointcloud_log.csv")

# initialize index for loop (required for while loops)
i <- 1

#-----------------------------------------------------
# MAIN LOOP
#----------------------------------------------------

while(i <= length(laz.files.list)) {
  
  # Ask only on first iteration
  if (i == 1) {
    user.input <- readline("Are you starting a new processing session or continuing? (start/continue) ")
    
    if (tolower(user.input) == "continue") {
      laz.processed <- list.files(clean.pointclouds.dir, pattern="\\.laz$|\\.las$")
      laz.files.list <- laz.files.list[!laz.files.list %in% laz.processed]
    }
  }
  
  print(paste("starting on", i, "of", length(laz.files.list), "which is file:",  
              laz.files.list[i]))
  
  
  laz <- readLAS(file.path(laz.files.dir, 
                           laz.files.list[i]), select = "xyzcin", 
                 filter = "-drop_withheld -drop_class 6 7 9")
  
  # set up empty df for logging files 
  log.row <- data.frame(
    file = basename(laz.files.list[i]),
    max_z = NA_real_)
  
  # Generate a DTM using the TIN algorithm.
  dtm <- rasterize_terrain(las = laz, res = TIN_res, algorithm = tin())
  
  # Add focal smoothing 
  dtm <- terra::focal(dtm, w = dtm_smoothing, fun = mean)
  
  # Visualize the DTM in 3D 
  plot_dtm3d(dtm)
  
  # normalize the height using the DTM we generated
  las_normalized <- normalize_height(laz, dtm)
  
  # filter the z values to remove points below ground (ground = 0)
  las_normalized <- filter_poi(las_normalized, Z >= subground_threshold)
  
  # classify points that fall within a threshold of 0 as ground
  las_normalized@data[abs(Z) <= ground_class_threshold, Classification := 2L]
  
  # remove canopy outliers 
  las.norm.clean <- flag.outliers.las(las_normalized, 
                                      voxel_res = IQR_voxel_res, 
                                      outlier.k = outlier_k,
                                      canopy_mask = TRUE,
                                      ground_mask = TRUE)
  
  # cut out any extreme outliers that might be missed
  las.norm.clean <- filter_poi(las.norm.clean, Z <= max.ht.cut)
  
  n <- sum(table(las.norm.clean$Classification))
  z <- sum(las.norm.clean@data[['Classification']] == 7L)
  
  print(paste("Number of noise points identified: ", z))
  print(paste("that's", round((z/n)*100,2), "% of the total returns"))
  
  # plot again
  plot(las.norm.clean, color = "Classification")
  
  # look at the point density of ground returns
  dtm.test <- rasterize_density(las.norm.clean
                                [las.norm.clean$Classification == 2L], 
                                res = vox.xy)
  plot(dtm.test != 0)
  
  user.input <- readline(
    paste("Proceed to write files (DTM raster, clean LAS) and go to next? (y/n) "))
  if(tolower(user.input) == "y") {
    
    # filter the laz by class to remove noise points 
    # las.norm.clean <- remove_noise(las.norm.clean)

    las.norm.clean <- filter_poi(las.norm.clean, Classification != 7L)
    
    # append z vals to log
    log.row$max_z <- max(las.norm.clean@data[['Z']], na.rm = TRUE)
    
    # project dtm raster to target epsg
    crs(dtm) <- sprintf("EPSG:%g", epsg)
    
    # resample dtm into voxel resolution 
    dtm.resample <- terra::aggregate(dtm, fact=(vox.xy/TIN_res), fun="mean")
    
    # export dtm
    file.name <- laz.files.list[i]
    tile.numb <- strsplit(file.name, "_")[[1]][5:6]
    writeRaster(dtm.resample, filename = file.path(dtm.dir, 
                                                   paste0(site, "_", 
                                                          tile.numb[1], "_", 
                                                          tile.numb[2], 
                                                          "_DTM.tif")))
    # export log
    write_csv(log.row, file = log.file, append = file.exists(log.file))
    
    # export LAS
    writeLAS(las.norm.clean, 
             file = file.path(clean.pointclouds.dir,laz.files.list[i]))
    
    # next iteration
    i <- i + 1
  }
  
  if(tolower(user.input) == "n") { 
    rm(laz, las_normalized, las.norm.clean, n, z, dtm)
    break
  }
}












