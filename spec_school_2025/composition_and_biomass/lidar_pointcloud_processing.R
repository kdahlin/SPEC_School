### Processing LIDAR Point Cloud Data for Spec School 2025 ###
# This is a portion of the 'Composition and Biomass' project group
# Written by Niklas Blanadet
# Last updated 20250616

# Load libraries

library(tidyverse)
library(sf)
library(terra)
library(lidR)
library(neonUtilities)

# Set working directory
setwd("C:/Users/nblan/Desktop/Classes/SpecSchool/Repository/SPEC_School/spec_school_2025/composition_and_biomass")

# We are downloading the data to our own computers because we could not get the HPCC
# to connect to our laptops. Therefore, I am setting up a separate folder to hold
# the LIDAR point cloud data (so it won't upload to github)
data_path <- paste0("C:/Users/nblan/Desktop/Classes/SpecSchool/Project/LIDAR_Data")