# Title: 01a_get_hyperspectral_tiles.R
# Date: 05/27/2026
# Author: TRG, AJP, & KMD 
# This script for downloading NEON hyperspectral data

# load the neonUtilities package
library(neonUtilities)

# call the ERSAM functions file - this should be inside your Rproj
source("ERSAM_functions.R")

# -----------------------------------
# USER-DEFINED VARIABLES
# -----------------------------------

# read in config file with site info
source("0_config_files/config_base.R")  

# date of tile coords file you want to use
tileCoords.date <- "20260522"

# Product code, default is the bidirectional mosaic "DP3.30006.002", 
# if you'd like to use the directional mosaic use "DP3.30006.001" 
# For the flightlines go to script "01b1_get_hyperspectral_flightlines"
dpID <- "DP3.30006.002"
level <- "L3"
product <- "Spectrometer"
subproduct <- "Reflectance"

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
                                            "TileCoordinates_1kmBuffer_",
                                            tileCoords.date, ".csv")))

# Create vectors of tile easting and northing from the data frame
easting <- input.csv[,2]

northing <- input.csv[,3]

#--------
# step 2: Download data 
#--------
# use neon_download function to download data and transfer to your target 
# directory - importantly, this downloads to a temp directory on your desktop
# then moves files to destination file, and will make a new directory as needed
# note that you should delete this temp file after downloading to avoid overloading
# your workstation
neon_download(
  dpID = dpID,
  site = site,
  easting = easting,
  northing = northing,
  year = year,
  out_path = save.dir,
  include.provisional = TRUE)
