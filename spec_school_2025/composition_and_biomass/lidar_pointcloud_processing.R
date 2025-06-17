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
     axis = TRUE)


# Let's create smaller plot-scale point clouds based on the plot data

p2_las <- clip_rectangle(clean.las, p2_e, p2_n, p2_e+40, p2_n+40)
plot(p2_las, axis = TRUE)

p9_las <- clip_rectangle(clean.las, p9_e, p9_n, p9_e+40, p9_n+40)
plot(p9_las, axis = TRUE)

p61_las <- clip_rectangle(clean.las, p61_e, p61_n, p61_e+40, p61_n+40)
plot(p61_las, axis = TRUE)

p64_las <- clip_rectangle(clean.las, p64_e, p64_n, p64_e+40, p64_n+40)
plot(p64_las, axis = TRUE)

p65_las <- clip_rectangle(clean.las, p65_e, p65_n, p65_e+40, p65_n+40)
plot(p65_las, axis = TRUE)








# lets make a DTM
dtm <- rasterize_terrain(clean.las,
                         res = 1, 
                         algorithm = tin())

# and take a look
plot(dtm)
plot_dtm3d(dtm)

# normalize the point cloud so ground is flat
flat.ground <- clean.las - dtm

# and take a look
plot(flat.ground)

# and lets make a canopy height model
chm <- rasterize_canopy(flat.ground,
                        res = 1,
                        algorithm = p2r())
