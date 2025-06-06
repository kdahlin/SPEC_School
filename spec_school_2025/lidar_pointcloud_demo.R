#### exploring NEON lidar data! ####
# written by Kyla Dahlin (MSU)
# updated 20250422

# Note that commented out parts of this script are for downloading NEON lidar
# data from NEON if you haven't already (but then make sure to organize files 
# since the way they arrive from NEON is VERY nested)

library(terra)
library(sf)
library(neonUtilities)
library(lidR)
library(ggplot2)

# set working directory
wd <- "X:/shared_data/NEON_AOP_data/"
setwd(wd)

# # create a new directory for today's data
# dir.create("lidar")
# save.path <- paste0(wd, "lidar/")

################### to get your own NEON lidar tile #######################
# go to
# https://neon-prod-earthengine.projects.earthengine.app/view/neon-aop-gee-data-viewer---desktop 
# pick a NEON site and note the lat/lon near the tower (need to use the wavelength
# viewer to get this) and year lidar data is available

site <- "MLBS"
year <- "2018"
domain <- "D07"
lat <- 37.378
lon <- -80.525
orig.crs <- st_crs("EPSG:4326")

# turn the points into a st object
points <- as.data.frame(cbind(lon, lat))
points.sf <- st_as_sf(points, coords = c("lon", "lat"), crs = orig.crs)

# now we need to convert our lat/lon to UTM coordinates
# first we find our UTM zone
utm_number <- (floor( (lon + 180) / 6 ) %% 60) + 1 # from stackoverflow
print(utm_number)

# and after a lot of googling it seems like there's not a good way to automatically
# get an epsg code, so I googled "epsg code for utm zone 17 north nat 83" where
# you should change 10 to your utm_number
coord.ref <- st_crs("EPSG:26917")

# and let's reproject our point location then round to the thousands
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
#           savepath = save.path)

##################### now we can look at some data! #######################
# note lidR is super powerful with lots of fun functions 
# https://r-lidar.github.io/lidRbook/ 
# hard coding the file paths though with NEON data they could be programmed
file.loc <- paste0(site, "/", year, "/lidar/ClassifiedPointCloud/")

# read in the point cloud data
in.las <- readLAS(paste0(file.loc, "NEON_", domain, "_", site, "_DP1_",
                         corner.easting, "_", corner.northing,
                         "_classified_point_cloud.laz"))

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

## clean up with thresholds if needed - NOTE THIS SHOULD BE ADJUSTED FOR A
# GIVEN TILE!!!
# clean.las <- subset(clean.las, clean.las$Z > 1000)
# clean.las <- subset(clean.las, clean.las$Z < 1300)

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

# now let's look at a cross section
# first define the start and end of the transect (in UTM coordinates)
p1 <- c(corner.easting + 100, corner.northing + 100)
p2 <- c(corner.easting + 300, corner.northing + 300)

# extract a transect
transect <- clip_transect(clean.las, p1, p2, width = 5, xz = TRUE)

# plot with ggplot (payload is a lidR function to transform las to coordinates)
ggplot(payload(transect), aes(X,Z, color = Z)) + 
  geom_point(size = 0.5) + 
  coord_equal() + 
  theme_minimal() +
  scale_color_gradientn(colours = height.colors(50))
