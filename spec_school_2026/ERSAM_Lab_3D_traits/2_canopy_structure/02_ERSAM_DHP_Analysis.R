# Title: 01_ERSAM_DHP_Analysis.R
# Authors: AWB, KMD, TRG
# This script is for analyzing canopy structural attributes 
# from ERSAM Lab digital hemispherical photos using hemispheR 

# load packages to process lab hemispherical photos
library(hemispheR)

# call the ERSAM functions file - this should be inside your Rproj
source("./0_R_functions/ERSAM_Lab/process_photos_jpg.R")

#-------------------------
# USER DEFINED VARIABLES - Directories and Site Specific
#-------------------------

# read in config file with site info
source("0_config_files/config_base.R")

#------------------------
# Define Directory Paths
#------------------------

# Input Directory 
dsn <- file.path(root, "shared_data", "NEON_field_data", site, year, "ERSAM", 
                 "HemiPhotos")

# Output Directory
save.dir <- file.path(root, "shared_data", "NEON_field_data", site, year, 
                      "ERSAM", "HemiPhotos")

# Create a list of photo file names in your directory
photo.files.list <- list.files(dsn, pattern = "\\.JPG$", full.names = TRUE)

#---------------------
# Calculate viewing angle
#---------------------
# your voxel x-y dimensions
vox.xy <- 10 

# the ~max z value of the site you're working at
max_z <- 45

# take the arc tangent 
# Note: this is approximating the hemiphoto viewing angle (circular) for 
# a (square) lidar voxel footprint. This is an ERSAM Lab convention
# and is not intended to be an exact radians-to-degrees conversion. 
view.angle <- round(atan((vox.xy / 2) / max_z) * 100)

#-------------
# Main Loop
#-------------

# Initialize photo processing loop - interactive prompts will be displayed in 
# the console! select 'cancel' at anytime to exit loop and progress will be auto 
# saved to your output directory.
process_photos_jpg(photo.files.list, channel = 3, method = "Otsu", 
                   zonal = FALSE, stretch = FALSE, gamma = 1.0,
                   startVZA = 0, endVZA = view.angle, yes_display = FALSE, 
                   lens = "equidistant", out_path = save.dir)


