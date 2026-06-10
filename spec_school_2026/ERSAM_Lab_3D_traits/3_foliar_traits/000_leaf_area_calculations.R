# Title: leaf_area_calculations.R
# Author: KMD & TRG 
# Date: 03/02/2026
# This script for calculating leaf area (cm2 and m2) from ERSAM lab leaf scans.

# load packages 
library(terra)
library(tidyverse)
library(tools)

#-----------------
# USER DEFINED VARIABLES 
#-----------------

# read in config file with site info
source("0_config_files/config_base.R")  

# define directories for scan data and output 
scan.dir <- file.path(root, "shared_data/NEON_field_data", site, year, "ERSAM",
                      "Lab_Data", "Leaf_Scans", "Corrected")
out.dir <- file.path(root, "shared_data/NEON_field_data", site, year, "ERSAM",
                     "Lab_Data", "Leaf_Scans")

# set the out file name
out.file.name <- paste0(site, substr(year,3,4), "_LeafScans_", wd, ".csv")

#--------------------------
# list files and create output data frame
#--------------------------

# list all the leaf scan png files in the directory
scan.files <- list.files(path = scan.dir,
                         pattern = c("\\.png$"),
                         full.names = TRUE)

# make a data frame to write names to
out.data <- as.data.frame(matrix(NA, nrow = length(leaf.files), ncol = 4))
names(out.data) <- c("ID", "pixels", "area_cm2", "area_m2")

# pull out the file name without the file extension to use as an ID 
out.data$ID <- file_path_sans_ext(basename(scan.files))

#------------------------------------
# Main loop: read in scans, rasterize, reclassify as binary and calculate number  
# of pixels, then calculate area and append to data frame.
#------------------------------------

# loop to read in each file and calculate area in cm2 and m2
for (i in seq_along(scan.files)) {
  in.pic <- rast(scan.files[i])
  
  # reclassify raster to make sure it's binary: 1 for values < 1, 0 for others 
  mask <- classify(in.pic[[1]], rcl = matrix(c(-Inf, 1, 1, 1, Inf, 0), ncol = 3, 
                                             byrow = TRUE))
  
  # Count how many pixels have value 1 (i.e., where original < 1)
  pix.count <- as.numeric(global(mask, fun = "sum", na.rm = TRUE))
  
  out.data$pixels[i] <- pix.count
  out.data$area_cm2[i] <- pix.count * ((2.53^2)/(150^2))
  out.data$area_m2[i] <- out.data$area_cm2[i] / (100^2)
}


# write csv
write_csv(out.data, file.path(out.dir, out.file.name),
          col_names = TRUE)
