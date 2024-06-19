# SPEC School 2024
# 6/19/2024
# Rotating exercises - Tony Station with NEON Hemisphere Data for estimating LAI
# File path on mapped drive -> Z:\shared_data\NEON_field_data\MLBS\2020_NEON_hemispheric-photos-veg

# Set working directory
setwd("Z:/shared_data/NEON_field_data/MLBS/2020_NEON_hemispheric-photos-veg")

# Install R packages to process hemispherical photos
install.packages("hemispheR")
library(hemispheR)
library(tidyverse)

filename <- '/Users/benc/projects/spec/data/hemi_photos/D07_0474.NEF'

img <- import_fisheye(filename)

img.bw <- binarize_fisheye(img)

gap.frac <- gapfrac_fisheye(img.bw,startVZA=0,endVZA=35,nrings=5,nseg=3,display=TRUE)

canopy_fisheye(gap.frac) %>% t


# L is just leaves
# Le is leaves + stems
# To compare to lidar, use Le