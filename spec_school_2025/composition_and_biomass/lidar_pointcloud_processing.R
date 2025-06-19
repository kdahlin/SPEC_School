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
library(gstat)

# Set working directory
setwd("C:/Users/nblan/Desktop/Classes/SpecSchool/Repository/SPEC_School/spec_school_2025/composition_and_biomass")

# We are downloading the data to our own computers because we could not get the HPCC
# to connect to our laptops. Therefore, I am setting up a separate folder to hold
# the LIDAR point cloud data (so it won't upload to github)
data_path <- paste0("C:/Users/nblan/Desktop/Classes/SpecSchool/Project/LIDAR_Data/")

# Prep to download the single LIDAR point cloud
site <- "MLBS"
year <- "2023"
lat <- 37.370511
long <- -80.527942
orig.crs <- st_crs("EPSG:4326")

# Individual plot data
p2_e <- 542123.13
p2_n <- 4136253.43
p9_e <- 542632.31
p9_n <- 4136519.606
p61_e <- 542514.739
p61_n <- 4136762.794
p64_e <- 542482.304
p64_n <- 4136644.036

# Colors for our CHM's
col = height.colors(25)


# turn the points into a st object
points <- as.data.frame(cbind(long, lat))
points.sf <- st_as_sf(points, coords = c("long", "lat"), crs = orig.crs)
# first we find our UTM zone

utm_number <- (floor( (long + 180) / 6 ) %% 60) + 1 # from stackoverflow
print(utm_number)

# and after a lot of googling it seems like there's not a good way to automatically
# get an epsg code, so I googled "epsg code for utm zone 17 north nat 83" where
# you should change 10 to your utm_number
coord.ref <- st_crs("EPSG:26917")

utm.coords <- st_transform(points.sf,
                           crs = coord.ref)

corner.easting <- signif(unlist(utm.coords$geometry)[1], 3)
corner.northing <- signif(unlist(utm.coords$geometry)[2], 4)

# # now let's try to download some NEON data
# byTileAOP(dpID='DP1.30003.001', # lidar point cloud
#           site=site,
#           year=year,
#           easting=corner.easting,
#           northing=corner.northing,
#           check.size=TRUE, # set to TRUE or remove if you want to check the size before downloading
#           savepath = data_path)

# Read in the data
in.las <- readLAS(paste0(data_path, "DP1.30003.001/neon-aop-products/2023/FullSite/D07/2023_MLBS_6/L1/DiscreteLidar/ClassifiedPointCloud/NEON_D07_MLBS_DP1_542000_4136000_classified_point_cloud_colorized.laz"))

# take a look at the file structure
print(in.las)

# and what it contains
names(in.las)

# and look at a point cloud (this will open up a new window)
plot(in.las,
     axis = TRUE)

# note the outlier points!
# let's try to clean this up quickly, note that "sor" is a cleaning algorithm
# see ?classify_noise for more info.
noise.las <- classify_noise(in.las, sor(15, 10))

# now get rid of those points
clean.las <- filter_poi(noise.las, Classification != LASNOISE)

# now lets take a look
plot(clean.las,
     axis = TRUE, 
     bg = "white")


# Let's create smaller plot-scale point clouds based on the plot data

p2_las <- clip_rectangle(clean.las, p2_e-20, p2_n-20, p2_e+20, p2_n+20)
plot(p2_las, axis = TRUE, bg = "white")

p9_las <- clip_rectangle(clean.las, p9_e-20, p9_n-20, p9_e+20, p9_n+20)
plot(p9_las, axis = TRUE)

p61_las <- clip_rectangle(clean.las, p61_e-20, p61_n-20, p61_e+20, p61_n+20)
plot(p61_las, axis = TRUE)

p64_las <- clip_rectangle(clean.las, p64_e-20, p64_n-20, p64_e+20, p64_n+20)
plot(p64_las, axis = TRUE)

# Let's create canopy height models for each of the plots. I'm going to start
# by experimenting with a bunch of different methods. We also first need to create
# a DTM to normalize the data. 

p2_dtm <- rasterize_terrain(p2_las, algorithm = kriging())
plot(p2_dtm, bg = "white")

p2_nlas <- normalize_height(p2_las, algorithm = knnidw(), dtm = p2_dtm)
plot(p2_nlas, axis = TRUE, bg = "white")

p2_chm <- rasterize_canopy(p2_nlas, res = .5, algorithm = pitfree(subcircle = 0.1), pkg = "terra")
plot(p2_chm, col = col)

# Locate the tree tops
# This first section code takes the tree tops from the LIDAR point cloud data
# We're also defining a function taken from the lidR handbook to set the window
# size (aka the buffer between tall and small trees) which should help identify
# the trees

# Function for identifying window size
f <- function(x) {
  y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
  y[x < 2] <- 3
  y[x > 20] <- 5
  return(y)
}

p2_ttops <- locate_trees(p2_las, lmf(f))
plot(p2_chm, col = col)
plot(sf::st_geometry(p2_ttops), add = TRUE, pch = 3, col = "blue")

# The above looks pretty good, but let's also try to get the same thing from the 
# the CHM's

p2_ttops_chm <- locate_trees(p2_chm, lmf(f))
plot(p2_chm, col = col)
plot(sf::st_geometry(p2_ttops_chm), add = TRUE, pch = 1, col = "black")
plot(sf::st_geometry(p2_ttops), add = TRUE, pch = 3, col = "blue")

# use an algorithm to segment the trees based on the tree tops. we'll use the 
# chm tree tops. 

p2_algo <- dalponte2016(p2_chm, p2_ttops_chm)
p2_seg_las <- segment_trees(p2_nlas, p2_algo)
plot(p2_seg_las, bg = "white", size = 4, color = "treeID", axis = TRUE)

# Calculate the tree height metrics
p2_metrics <- crown_metrics(p2_seg_las, ~list(height = max(Z)))

# Let's create a for loop to process these for the other three plots

height_frame <- p2_metrics %>%
  select(-c(geometry))

writeRaster(p2_chm, "p2.tif")
