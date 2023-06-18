#-------------------------------------------------------------#
#-------------------     MLBS TRAITS DATA   ------------------
#-------------------------------------------------------------#

# June 18, 2023
# Adriana Uscanga (uscanga1@msu.edu)
# SPEC School.

# This code is for reading trait data from Mountain Lake Biological Station flightlines
# created by Wang et al (2020) (link to paper: https://doi.org/10.1111/nph.16711).
# The first part of the code is a loop for reading and extracting data from the flightlines,
# which are ENVI files, and export data as tiff files.
# The second part of the code is for reading all tiff files and create a single raster (mosaic).
# Each trait in the ENVI files is stored as a band. There's a list of all traits (bands) below.

# Load packages

library(caTools)
library(terra)
library(tidyverse)

# Set working directory

setwd("/Users/adrianauscanga/Documents/Documents/spec_school/data/flightlines")

#------------------------------ ENVI files ----------------------------------

# Import ENVI files of trait data from MLBS flightlines,
# extract Nitrogen data and export it to tiff files.
# There are three bands with nitrogen data (mean, S.D., and CV)
# and one tiff file per flightline.

ls <- grep(list.files(), pattern= '*.hdr', invert=TRUE, value=TRUE)

for (i in ls){
  
  # Read envi file and get dimensions
  envi <- read.ENVI(i, headerfile = paste0(i,".hdr"))
  envi_dim <- dim(envi)
  lines <- as.numeric(envi_dim[1]) # lines = rows
  samples <- as.numeric(envi_dim[2]) # samples = columns
  
  # Read header and get map info
  header <- read.delim(paste0(i,".hdr"), header=FALSE)
  map_info <- header[7,1]
  map_info <- unlist(strsplit(map_info, ","))
  # upper left corner
  x <- as.numeric(map_info[4])
  y <- as.numeric(map_info[5])
  # projection
  projection <- header[3,1]
  projection <- unlist(strsplit(projection, "="))
  projection <- projection[2]
  projection <- as.character(str_sub(projection, 3, -2))
  
  # band names
  band_names <- header[10,1]
  band_names <- unlist(strsplit(band_names, "="))
  band_names <- band_names[[2]] 
  band_names <- as.character(str_sub(band_names, 3, -2))
  band_names <- unlist(strsplit(band_names, ", "))
  
  # Extract Nitrogen data (slices 52-54), change that range (and variable names) for other traits (see list of traits below)
  envi_nitrogen <- envi[1:lines, 1:samples, 52:54]
  
  # Convert to raster
  rast_nitrogen <- rast(envi_nitrogen)
  
  # Set projection
  crs(rast_nitrogen) <- projection
  
  # Set layer names
  names(rast_nitrogen)<- c("nitrogen_mean", "nitrogen_sd", "nitrogen_cv")
  
  # Set extent
  ext(rast_nitrogen) <- c(x, x+samples, y-lines, y)
  
  # Replace -9999 values with NAs
  rast_nitrogen <- classify(rast_nitrogen, cbind(-9999, NA))
  
  # Write raster
  writeRaster(rast_nitrogen, filename = paste0(substr(i, 1, 33),"_nitrogen.tif"))
  
  # Remove envi file from environment
  rm(envi)
}


# ------------------------     Tiff files mosaic    -----------------------------

# Read all nitrogen tiff files with terra and make a mosaic

ls <- list.files(pattern = "*.tif")
raster_files <- lapply(ls, rast)

# Make a SpatRasterCollection from a list
rsrc <- sprc(raster_files)

# Mosaic
mlbs_nitrogen <- mosaic(rsrc)

# See result in a plot
plot(mlbs_nitrogen)

# Save new raster 
writeRaster(mlbs_nitrogen, filename = "mlbs_nitrogen.tif")

# -----------------------------------------------------------------------------

# Band names

# List of traits (using flightline NEON_D07_MLBS_DP1_20170805_180446_reflectance_traits as example, all flightlines  have the same number of traits (bands))

mlbs180446 <- read.ENVI("NEON_D07_MLBS_DP1_20170805_180446_reflectance_traits", "NEON_D07_MLBS_DP1_20170805_180446_reflectance_traits.hdr")

# band names
header <- read.delim("NEON_D07_MLBS_DP1_20170805_180446_reflectance_traits.hdr")
band_names <- header[9,1]
band_names <- unlist(strsplit(band_names, "="))
band_names <- band_names[[2]] 
band_names <- as.character(str_sub(band_names, 3, -2))
band_names <- unlist(strsplit(band_names, ", "))





                  
