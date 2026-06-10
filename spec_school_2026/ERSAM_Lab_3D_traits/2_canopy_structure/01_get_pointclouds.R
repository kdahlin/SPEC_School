# Title: 01_get_pointclouds.R
# Date: 09/04/2025
# Author: TRG, KMD
# This script for downloading NEON LiDAR point cloud data

# load the neonUtilities package
library(neonUtilities)

# call the ERSAM functions file - this should be inside your Rproj
source("ERSAM_functions.R")

# -----------------------------------
# USER-DEFINED VARIABLES
# -----------------------------------

# read in config file with site info
source("0_config_files/config_base.R")

# set old working dates for things you are reading in (might be different from
# current working date set in config file)
tileCoords.wd <- "20260608"

# NEON Lidar info & storage info
dpID <- "DP1.30003.001"
level <- "L1"
product <- "DiscreteLidar"
subproduct <- "ClassifiedPointCloud"

# Specify your target directory
# NOTE: If you are moving large amounts of data, download directly to your disk
# drive and transfer using 'Globus' on the HPCC. 
# See here: https://docs.icer.msu.edu/Transferring_data_with_Globus/
save.dir <- file.path(root, "shared_data", "NEON_AOP_data", site, year, level, 
                      product, subproduct)

#---------
# Step 1 (OPTIONAL): load a vector of AOP tile coordinates 
#---------
# Define Directory Paths
# Input Directory (spatial & tabular)
dsn <- file.path(root, "shared_data", "NEON_field_data", site, year, "ERSAM", 
                 "GPS_Data", "Processed")

# get list of files in directory
files <- list.files(dsn)
print(files)

# Input file name
input.csv <- read.csv(file.path(dsn, paste0(site, substr(year, 3, 4),"_", 
                                            "TileCoordinates_2kmBuffer_",
                                            tileCoords.wd, ".csv")))

# Create vectors of tile easting and northing from the data frame
easting <- input.csv[,2]

northing <- input.csv[,3]

#--------
# step 2: Download data 
#--------

# use neon_download function to download data and transfer to your target 
# directory - importantly, this downloads to a temp directory on your desktop
# then moves files to destination file, and will make a new directory as needed
neon_download(
  dpID = dpID,
  site = site,
  easting = easting,
  northing = northing,
  year = year,
  out_path = save.dir,
  include.provisional = TRUE)



















