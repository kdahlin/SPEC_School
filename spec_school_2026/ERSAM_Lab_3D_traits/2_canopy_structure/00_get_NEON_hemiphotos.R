# TiTle: 00_get_NEON_hemiphotos 
# Author: TRG & TB 
# Date: 09/19/2025
# This script for downloading NEF hemispherical photos from 
# the NEON data repository 

# load packages 
library(neonUtilities)

# call ERSAM lab functions script 
source("ERSAM_Functions.R")

# -----------------------------------
# USER-DEFINED VARIABLES
# -----------------------------------

# read in config file with site info
source("0_config_files/config_base.R")

# hemiphoto product code
product_code <- "DP1.10017.001"

# start month for photo query 
startmonth <- "08"

# end month for photo query 
startmonth <- "09"

# Dedfine output directory 
# Output Directory
save.dir <- file.path(root, "shared_data", "NEON_field_data", site, year, 
                      "NEON", "HemiPhotos", "Raw")

#--------
# Step 1: load data tables into R env
#--------

# Load the photo data tables into the R env and specify the month range
data <- loadByProduct(dpID = product_code,
                      site = site,
                      startdate = paste0(year, "-", startmonth ),
                      enddate = paste0(year, "-", endmonth),
                      package = "basic",
                      include.provisional = TRUE,
                      check.size = TRUE)

# let's subset to the data we care about
images <- data$dhp_perimagefile

# Subset to only upward facing images
images <- images[images$imageType == "overstory",]

#-------
# step 2: Use the bulk_data_download fn to download images to your directory
#-------

# Ensure save directory exists
if (!dir.exists(save.dir)) dir.create(save.dir, recursive = TRUE)

# download the photos to your directory 
bulk_photo_download(images, save.dir)




