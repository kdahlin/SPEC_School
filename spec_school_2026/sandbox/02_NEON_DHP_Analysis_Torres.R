# Title: 02_NEON_DHP_Analysis
# Authors: AWB, KMD, & TRG
# Date: 03/16/2026
# This script is for analyzing canopy structural attributes 
# from NEON digital hemispherical photos using hemispheR. This 
# script also downloads the associated GPS points from the NEON 
# repository. 

### !!!! IF FIRST TIME RUNNING SCRIPT ON YOUR MACHINE START HERE !!!! ###

# The bRaw uses the functionality of dcraw, a raw photo developing tool that  
# must be installed onto your machine before utilizing this script. 
# For Macintosh operating system, dcraw can be installed using the following 
# command from the Terminal: brew install dcraw
# For windows operating system, 'dcraw.exe must be downloaded
# (available here: https://www.fastpictureviewer.com/downloads/#links)
# and the file must be moved to he following location: C:\DCRaw\. The installer 
# must be named ‘dcraw.exe’.

### !!!! IF YOU HAVE USED THIS SCRIPT ON YOUR MACHINE START HERE !!!! ###

# load packages to process hemispherical photos
library(terra)
library(hemispheR)
library(tidyverse)
library(neonUtilities)
library(sf)
library(devtools)
devtools::install_gitlab("fchianucci/bRaw")
library(bRaw)
devtools::install_github('NEONScience/NEON-geolocation/geoNEON', 
                         dependencies=TRUE)
library(geoNEON)
#---------------

# read in ERSAM lab functions
source("0_R_functions/ERSAM_Lab/neon_photo_download.R")
source("0_R_functions/ERSAM_Lab/process_photos_nef.R")

#-------------------------
# USER-DEFINED VARIABLES 
#-------------------------

# read in config file with site info
source("0_config_files/config_base.R")

#---------------------------
# set directories
#---------------------------

# set the directory for downloading photos
download.dir <- file.path(root, "shared_data", "NEON_field_data", site, year, 
                       "NEON", "Hemiphotos", "Photos")

# set the out file name for tabular data 
tabular.dir <- file.path(root, "shared_data", "NEON_field_data", site, year, 
                       "NEON", "HemiPhotos", "Tabular")

# set the out file name for tabular data 
gps.dir <- file.path(root, "shared_data", "NEON_field_data", site, year, 
                       "NEON", "GPS_data", "Hemiphoto")

# create output directories if they don't exist
dirs <- c(download.dir, tabular.dir, gps.dir)

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    message("Created directory: ", d)
  } else {
    message("Directory exists: ", d)
  }
}


# hemiphoto product code for neonUtilities
product_code <- "DP1.10017.001"

# specify the month and year for downloading hemiphotos
startdate <- "2025-08"
enddate <- "2025-09"

#-----------------------------
# Step 1: load data tables & download photos
#-----------------------------

# Load the photo data tables into the R env and specify the month range
photo.data <- loadByProduct(dpID = product_code,
                      site = site,
                      startdate = startdate,
                      enddate = enddate,
                      package = "basic",
                      include.provisional = TRUE,
                      check.size = TRUE)

# let's subset to the data we care about
images <- data$dhp_perimagefile

# Subset to only upward facing images
images <- images[images$imageType == "overstory",]

# download the photos to your directory 
neon_photo_download(images, download.dir)

#------------------------------------------------------------------------------
# Step 2: Process photos 
#------------------------------------------------------------------------------

# Create a list of photo file names in your directory
photo.files.list <- list.files(download.dir, 
                               pattern = "\\.NEF$", full.names = TRUE)

#---------------------
# Calculate viewing angle
#---------------------
# your voxel x-y dimensions
vox.xy <- 10 

# the ~max z value of the site you're working at
max_z <- 45

# take the arc tangent 
view.angle <- round(atan((vox.xy / 2) / max_z) * 100)

#-------------
# Main Loop
#-------------
# Initialize photo processing loop - interactive prompts will be displayed in 
# the console! select 'cancel' at anytime to exit loop and progress will be auto 
# saved to your output directory.
process_photos_nef(photo.files.list, method = "Otsu", 
                    zonal = FALSE, gamma = FALSE,
                    startVZA = 0, endVZA = view.angle, yes_display = TRUE, 
                    lens = "equidistant", out_path = tabular.dir)

#-----------------------
# Step 3: create gpkg from NEON DHP data sampling locations
#-----------------------
# lets look at all the files in our directory
list.files(tabular.dir)

# the date you processed your hemiphotos 
hemi.date <- format(Sys.time(), "%Y%m%d")

# load the csv
hemi.data <- read_csv(file.path(tabular.dir, 
                                paste0("processed_hemiphotos_",
                                hemi.date,".csv")))

# filter the data frame loaded from NEON by the photos you actually processed 
images_filter <- images |> 
  filter(images$imageFileName %in% hemi.data$ImageName)

# generate point coordinates from pointID using geoNEON package 
DHP.points <- getLocTOS(images_filter, 'dhp_perimagefile')

# transform into an sf object 
DHP.sf <- st_as_sf(DHP.points, coords = c("adjEasting", "adjNorthing"), crs = epsg) 

# write GPS points and attributes to a geopackage 
st_write(DHP.sf, file.path(gps.dir, paste0(site, substr(year,3,4), 
                                           "_Hemiphoto_ProcessedData.gpkg")))


