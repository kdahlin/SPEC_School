##  This script is for analyzing canopy structural attributes 
##  from NEON digital hemipsherical photos using hemispheR 
# Authors: Anthony Bowman, Dr. Kyla Dahlin, and 2024 SPEC School participants - Summer 2024  

# load packages to process hemispherical photos
library(terra)
library(hemispheR)
library(tidyverse)
library(neonUtilities)

library(devtools)
devtools::install_gitlab("fchianucci/bRaw")
library(bRaw)

# Set file name for downloading hemi photos 
#dsn <- "X:/shared_data/NEON_field_data/MLBS/hemiphoto_test/"
dsn <- "C:/Users/tonyb/OneDrive - Michigan State University/ERSAM LAB/Hemiphoto_Analysis/"


# load in a lab hemiphoto for comparison to NEON 
lab.img <- "X:/shared_data/NEON_field_data/MLBS/2018_kamoske/HemiPhotos/All_Photos/IMG_0464.jpg"


# Define some parameters

# hemiphoto product code
product_code <- "DP1.10017.001"
# NEON site code
site <- "MLBS"
# year(s) of hemiphoto
year <- 2020
# NEON camera distortion type (tentative)
cam_type <- "equidistant"


# Download the data and specify the month range
data <- loadByProduct(dpID = product_code,
                      site = site,
                      startdate = paste0(year, "-04"),
                      enddate = paste0(year, "-10"),
                      package = "basic",
                      check.size = TRUE)

# let's subset to the data we care about 
images <- data$dhp_perimagefile

# Subset to only upward facing images 
images <- images[images$imageType == "overstory",]

# Subset to a single hemiphoto 
dhp.file <- images[34,]

# Grab the filename for saving the image
file.name <- dhp.file$imageFileName

# download the hemiphoto(S) - IMPORTANT: Change the 'mode' here if not using Windows 
# to an appropriate alternative for your computer
photo <- download.file(url=dhp.file$imageFileUrl,
                       destfile=paste0(dsn, file.name), mode="wb")

# assign the downloaded hemiphoto file name to an object
file.export <- as.character(paste0(dsn, file.name))

# import raw image into R
img <- raw_blue(file.export, 
                gamma.adj = TRUE,
                circ.mask=NULL,
                display = TRUE,
                message = TRUE)

# get the file name without the suffix
file.pref <- substr(file.name, 1, 8)

# Import fisheye using hemispheR and the file name (FOR .jpg or .png, not raw .NEF NEON images)
img2 <- import_fisheye(filename=paste0(dsn, "bRaw/", file.pref, ".jpeg"),
                       channel=1,
                       circular = FALSE,
                       gamma = 1.0,
                       stretch = FALSE,
                       display=TRUE)

# binarize image to 0 (canopy) and 1 (gap/non-canopy) pixels
img.bw <- binarize_fisheye(img2, display=TRUE)

# Use zenith rings and azimuth sectors (segments) to calculate angular gap fraction
gap.frac <- gapfrac_fisheye(img.bw,startVZA=0,endVZA=35,nrings=3,nseg=5,display=TRUE)


# calculate LAI and other structural characteristics
canopy.data <- canopy_fisheye(gap.frac)

