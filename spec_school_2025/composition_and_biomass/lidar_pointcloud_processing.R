### Processing LIDAR Point Cloud Data for Spec School 2025 ###
# This is a portion of the 'Composition and Biomass' project group
# Written by Niklas Blanadet
# Last updated 20250616

###############################################################################
# The idea of this script is to estimate aboveground biomass using LIDAR point
# cloud data. We will first estimate canopy height using the LIDAR point clouds
# and CHM's we've generated from those point clouds, and then create allometric 
# equations using real ground data to estimate biomass from canopy height.
# Finally, we will calculate biomass from the canopy heights to get the biomass.

# We are using MLBS as our field site, but taking four different 40m x 40m 
# subplots from within a single 1km x 1km tile. This is mainly because we want 
# to be able to process this script on our personal laptop computers.

# Post-script, we want to relate the aboveground biomass within each plot to 
# several hyperspectral-derived traits such as leaf nitrogen. 
###############################################################################

###############################################################################
# GENERAL SCRIPT SETUP
###############################################################################

# Load libraries
library(tidyverse)      # General data wrangling package 
library(sf)             # Package to handle spatial vectors
library(terra)          # General package for managing raster data
library(lidR)           # General package for handling LIDAR point clouds
library(neonUtilities)  # Download NEON data
library(gstat)          # Geostatistical data modeling package (used to determine tree tops)

# Set working directory
# change this to your local working directory
setwd("C:/Users/nblan/Desktop/Classes/SpecSchool/Repository/SPEC_School/spec_school_2025/composition_and_biomass")

# We are downloading the data to our own computers because we could not get the HPCC
# to connect to our laptops. Therefore, I am setting up a separate folder to hold
# the LIDAR point cloud data (so it won't upload to github)
data_path <- paste0("C:/Users/nblan/Desktop/Classes/SpecSchool/Project/LIDAR_Data/")

# Colors for our CHM's (we will use this later)
col = height.colors(25)

###############################################################################
# DOWNLOAD LIDAR POINT CLOUD DATA
###############################################################################

# Prep to download the single LIDAR point cloud
# We are downloading the 2023 MLBS LIDAR point cloud data
# Note that this point cloud includes the main MLBS station!
site <- "MLBS"
year <- "2023"
lat <- 37.370511
long <- -80.527942
orig.crs <- st_crs("EPSG:4326")

# For this project, we're going to look at four subplots within our MLBS tile.
# The easting and northings of each plot were determined from shapefiles found 
# from NEON: https://www.neonscience.org/sites/default/files/All_NEON_TOS_Plots_V11.zip
# We inputted these shapefiles into Google Earth Pro to get the easting and northing. 
# These four subplots are going to be combined with the hyperspectral data and 
# ground diversity data. 
# The plots include MLBS_02 (baseplot), MLBS_09, MLBS_61, and MLBS_64
# Also note that these northings and eastings are the center of the plots. 
p2_e <- 542123.13
p2_n <- 4136253.43
p9_e <- 542632.31
p9_n <- 4136519.606
p61_e <- 542514.739
p61_n <- 4136762.794
p64_e <- 542482.304
p64_n <- 4136644.036

# The below code is from Kyla Dahlin's LIDAR point cloud demo. Here, we're 
# converting points to get the northing and easting, from which we will 
# download the NEON tile. 

# Turn the points into a st object
points <- as.data.frame(cbind(long, lat))
points.sf <- st_as_sf(points, coords = c("long", "lat"), crs = orig.crs)

# First we find our UTM zone
utm_number <- (floor( (long + 180) / 6 ) %% 60) + 1 # from stackoverflow
print(utm_number)

# And after a lot of googling it seems like there's not a good way to automatically
# get an epsg code, so I googled "epsg code for utm zone 17 north nat 83" where
# you should change 10 to your utm_number
coord.ref <- st_crs("EPSG:26917")
utm.coords <- st_transform(points.sf,
                           crs = coord.ref)
corner.easting <- signif(unlist(utm.coords$geometry)[1], 3)
corner.northing <- signif(unlist(utm.coords$geometry)[2], 4)

# Now we download the tile data from NEON. Note that the northing and easting
# are the lower left corner of the tile that we're downloading. 

# I commented out this section, but uncomment it to download the data. 

# byTileAOP(dpID='DP1.30003.001', # lidar point cloud
#           site=site,
#           year=year,
#           easting=corner.easting,
#           northing=corner.northing,
#           check.size=TRUE, # set to TRUE or remove if you want to check the size before downloading
#           savepath = data_path)

###############################################################################
# ESTIMATING CANOPY HEIGHT FROM LIDAR POINT CLOUDS
###############################################################################

# In this section, we're reading in the point clouds, cutting out the individual
# plots, determining where the top of each tree is, and calculating the canopy 
# height from there. We're relying heavily on the lidR package, and some of the
# algorithms are from the gstat package. 

# Read in the LIDAR point cloud data data
# This part takes a second - let the script run for a little.
# Once again, part of this section is adapted from Kyla Dahlin's LIDAR point 
# cloud demo. 
in_las <- readLAS(paste0(data_path, "DP1.30003.001/neon-aop-products/2023/FullSite/D07/2023_MLBS_6/L1/DiscreteLidar/ClassifiedPointCloud/NEON_D07_MLBS_DP1_542000_4136000_classified_point_cloud_colorized.laz"))

# take a look at the file structure
print(in_las)
# and what it contains
names(in_las)
# and look at a point cloud (this will open up a new window)
plot(in_las, axis = TRUE, bg = "white")

# Although there aren't a ton of outlier points, we will clean them up. 
# Note that "sor" is a cleaning algorithm
# see ?classify_noise for more info.
noise_las <- classify_noise(in_las, sor(15, 10))

# Now get rid of those points
clean_las <- filter_poi(noise_las, Classification != LASNOISE)

# now lets take a look
plot(clean_las,
     axis = TRUE, 
     bg = "white")


# Let's create smaller plot-scale point clouds based on the plot data
# note that the eastings and northings are the center of the plots, and the 
# plots are 40m x 40m, so the corners should be +20 and -20. 
p2_las <- clip_rectangle(clean_las, p2_e-20, p2_n-20, p2_e+20, p2_n+20)
plot(p2_las, axis = TRUE, bg = "white")

p9_las <- clip_rectangle(clean_las, p9_e-20, p9_n-20, p9_e+20, p9_n+20)
plot(p9_las, axis = TRUE, bg = "white")

p61_las <- clip_rectangle(clean_las, p61_e-20, p61_n-20, p61_e+20, p61_n+20)
plot(p61_las, axis = TRUE, bg = "white")

p64_las <- clip_rectangle(clean_las, p64_e-20, p64_n-20, p64_e+20, p64_n+20)
plot(p64_las, axis = TRUE, bg =)

# Let's create canopy height models for each of the plots. I'm going to start
# by experimenting with a bunch of different methods. We also first need to create
# a DTM to normalize the data. 

# We're going to start with just p2 to figure out what the best method is for 
# creating the CHM. We need to create a DTM (digital terrain model), normalize 
# the height of the points (basically make them all relative to the ground), and
# then create our canopy height model. 

p2_dtm <- rasterize_terrain(p2_las, algorithm = kriging())
plot(p2_dtm, bg = "white")

# Now we normalize the height
p2_nlas <- normalize_height(p2_las, algorithm = knnidw(), dtm = p2_dtm)
plot(p2_nlas, axis = TRUE, bg = "white")

# Now create the CHM. We are using the pitfree algorithm. We also use pkg = 
# "terra" to make the raster into a format that works with terra. 
p2_chm <- rasterize_canopy(p2_nlas, res = .5, algorithm = pitfree(subcircle = 0.1), pkg = "terra")
plot(p2_chm, col = col)

# Now let's do the other plots (same process, but just skipping the plotting)
p9_dtm <- rasterize_terrain(p9_las, algorithm = kriging())
p9_nlas <- normalize_height(p9_las, algorithm = knnidw(), dtm = p9_dtm)
p9_chm <- rasterize_canopy(p9_nlas, res = .5, algorithm = pitfree(subcircle = 0.1), pkg = "terra")
plot(p9_chm, col = col)

p61_dtm <- rasterize_terrain(p61_las, algorithm = kriging())
p61_nlas <- normalize_height(p61_las, algorithm = knnidw(), dtm = p61_dtm)
p61_chm <- rasterize_canopy(p61_nlas, res = .5, algorithm = pitfree(subcircle = 0.1), pkg = "terra")
plot(p61_chm, col = col)

p64_dtm <- rasterize_terrain(p64_las, algorithm = kriging())
p64_nlas <- normalize_height(p64_las, algorithm = knnidw(), dtm = p64_dtm)
p64_chm <- rasterize_canopy(p64_nlas, res = .5, algorithm = pitfree(subcircle = 0.1), pkg = "terra")
plot(p64_chm, col = col)

# Now we need to locate the individual trees. There are two methods of doing
# this - we can either use the LIDAR point cloud or we can use the CHM. 
# This first section code takes the tree tops from the LIDAR point cloud data
# We're also defining a function taken from the lidR handbook to set the window
# size (aka the buffer between tall and small trees) which should help identify
# the trees

# Function for identifying window size (from lidR handbook)
f <- function(x) {
  y <- 2.6 * (-(exp(-0.08*(x-2)) - 1)) + 3
  y[x < 2] <- 3
  y[x > 20] <- 5
  return(y)
}

# Let's try locating the trees in just plot 2
p2_ttops <- locate_trees(p2_las, lmf(f))
plot(p2_chm, col = col)
plot(sf::st_geometry(p2_ttops), add = TRUE, pch = 3, col = "blue")

# Same for the other plots
p9_ttops <- locate_trees(p9_las, lmf(f))
p61_ttops <- locate_trees(p61_las, lmf(f))
p64_ttops <- locate_trees(p64_las, lmf(f))

# The above looks pretty good, but let's also try to get the same thing from the 
# the CHM's. We're also going to compare the two methods. 
p2_ttops_chm <- locate_trees(p2_chm, lmf(f))
plot(p2_chm, col = col)
plot(sf::st_geometry(p2_ttops_chm), add = TRUE, pch = 1, col = "black")
plot(sf::st_geometry(p2_ttops), add = TRUE, pch = 3, col = "blue")

# Same for the other plots
p9_ttops_chm <- locate_trees(p9_chm, lmf(f))
plot(p9_chm, col = col)
plot(sf::st_geometry(p9_ttops_chm), add = TRUE, pch = 1, col = "black")
plot(sf::st_geometry(p9_ttops), add = TRUE, pch = 3, col = "blue")

p61_ttops_chm <- locate_trees(p61_chm, lmf(f))
plot(p61_chm, col = col)
plot(sf::st_geometry(p61_ttops_chm), add = TRUE, pch = 1, col = "black")
plot(sf::st_geometry(p61_ttops), add = TRUE, pch = 3, col = "blue")

p64_ttops_chm <- locate_trees(p64_chm, lmf(f))
plot(p64_chm, col = col)
plot(sf::st_geometry(p64_ttops_chm), add = TRUE, pch = 1, col = "black")
plot(sf::st_geometry(p64_ttops), add = TRUE, pch = 3, col = "blue")


# Now we will use an algorithm to segment the trees based on the tree tops. 
# we'll use the chm tree tops. 

p2_algo <- dalponte2016(p2_chm, p2_ttops_chm)
p2_seg_las <- segment_trees(p2_nlas, p2_algo)
plot(p2_seg_las, bg = "white", size = 4, color = "treeID", axis = TRUE)

# Same for the other plots
p9_algo <- dalponte2016(p9_chm, p9_ttops_chm)
p9_seg_las <- segment_trees(p9_nlas, p9_algo)
p61_algo <- dalponte2016(p61_chm, p61_ttops_chm)
p61_seg_las <- segment_trees(p61_nlas, p61_algo)
p64_algo <- dalponte2016(p64_chm, p64_ttops_chm)
p64_seg_las <- segment_trees(p64_nlas, p64_algo)

plot(p64_seg_las, bg = "white", size = 4, color = "treeID", axis = TRUE)


# Now that we have every tree segmented, we are going to calculate the heights
# for each of these trees. We will then combine all of the data files into 
# one data frame. Notice that we need to force the data frame into tibble format
# - This is to help remove columns later. 
p2_metrics <- tibble(crown_metrics(p2_seg_las, ~list(height = max(Z)))) %>%
  mutate(plot = 2) %>%
  select(plot, treeID, height)
p9_metrics <- tibble(crown_metrics(p9_seg_las, ~list(height = max(Z)))) %>%
  mutate(plot = 9) %>%
  select(plot, treeID, height)
p61_metrics <- tibble(crown_metrics(p61_seg_las, ~list(height = max(Z)))) %>%
  mutate(plot = 61) %>%
  select(plot, treeID, height)
p64_metrics <- tibble(crown_metrics(p64_seg_las, ~list(height = max(Z)))) %>%
  mutate(plot = 64) %>%
  select(plot, treeID, height)

# This isn't elegeant but I'm not sure if there's a better solution?
height_frame <- p2_metrics %>%
  full_join(p9_metrics) %>%
  full_join(p61_metrics) %>%
  full_join(p64_metrics)

###############################################################################
# CALCULATING BIOMASS
###############################################################################

# Let's now create a model that relates biomass to canopy height. We're going 
# to create a really simple linear model (no judging pls) from MLBS ground 
# data from 2023. 

# First, let's download the veg structure data from MLBS

# Comment out the block below to download and unzip the veg structure files

# zipsByProduct(dpID = "DP1.10098.001", # Veg structure, 
#               site = site, 
#               startdate = "2023-01", 
#               enddate = "2023-12", 
#               check.size = TRUE, 
#               savepath = data_path
#               )
# 
# stackByTable(filepath = paste0(data_path, "filesToStack10098"))

# There are two files we're interested in combining - "mappingandtagging" and 
# "apparentindividual"
# mappingandtagging has scientific names for each individual, while apparent
# individual has the data

mlbs_veg_data <- read.csv(paste0(data_path, "filesToStack10098/stackedFiles/vst_mappingandtagging.csv", sep = "")) %>%
  select(individualID, scientificName, genus)
mlbs_veg_structure <- tibble(read.csv(paste0(data_path, "filesToStack10098/stackedFiles/vst_apparentindividual.csv", sep = ""))) %>%
  inner_join(mlbs_veg_data, 
             by = "individualID")

# Now we calculate the biomass from the veg structure data. We're gonna assume a
# wood density of 0.5
# We're going to select only the columns that we actually want (to make it 
# easier to visualize), and then we will filter and keep only the trees that 
# are standing. Then we want data from plots that we are not measuring in this
# study. We're also filtering out small trees (the trees in our plot that the 
# LIDAR detects are mostly between 15 and 25 meters). 
# Finally, we use a general allometric equation to estimate the biomass. It is 
# generally used for tropical trees, but that's okay, Virginia is basically 
# the tropics 
mlbs_veg_structure <- mlbs_veg_structure %>%
  select(individualID, scientificName, genus, plotID, plantStatus, stemDiameter, height) %>%
  filter(plantStatus %in% c("Live", "Standing dead", "Live, other damage", "Live, broken bole", "Live, physically damaged", 
                            "Live, disease damaged", "Live, insect damaged"), 
         plotID %in% c("MLBS_067", "MLBS_63", "MLBS_069"), 
         !is.na(height),
         !is.na(stemDiameter), 
         height > 10 # We're only really looking at big trees in our data, so this eliminates the weird small ones
         ) %>%
  mutate(
    biomass = as.integer(0.0673*((0.5*((stemDiameter)^2)*height)^0.976))
  )

# Now let's create the linear regression model, using height to predict biomass
model = lm(biomass ~ height, data = mlbs_veg_structure) # biomass predicted by height
summary(model)

# Let's plot it, and see if the model results match geom_smooth regression line
mlbs_veg_structure %>%
  ggplot(
    aes(x = height, y = biomass)
  ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, alpha = .5) + # inbuilt ggplot linear regression
  geom_abline( # plot the line made by our model
    slope = 79.78, 
    intercept = -853.11, 
    color = "red", 
    size = 2, 
    alpha = 0.5
  ) + 
  labs(
    title = "model results",
    caption = "r^2 = not your problem"
  )

# Finally, let's create a biomass table for our plots using the model results
mlbs_biomass <- height_frame %>%
  mutate(
    biomass = 79.78*height-853.11
  ) %>%
  group_by(plot) %>%
  summarize(
    biomass = sum(biomass)
  )
write.csv(mlbs_biomass, file = "biomass.csv")
mlbs_biomass

# There we go! We have the biomass for all of the plots. 
