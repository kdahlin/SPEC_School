# Title: get_tile_coords
# Date: 07/23/2025
# Author: KMD & TRG
# Take whatever cleaned up geographic data you have and use it to figure out
# which NEON AOP tiles you want to download.

#load packages 
library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(cowplot)

# -----------------------------------
# USER-DEFINED VARIABLES
# -----------------------------------

# read in config file with site info
source("0_config_files/config_base.R")

# set old working dates for things you are reading in (might be different from
# current working date set in config file)
gpkg.wd <- "20260608"

# Define your data type
type <- "Hemiphoto" #Options: "Foliar", "Hemiphoto", "Spectra"

# Set buffer to determine how many adjoining tiles to add around the sampled
# points
# buffer = 0 fills gaps in the input data but doesn't add a buffer
buffer = 1 # in kilometers

# Input file name
in.gpkg.name <- paste0(site, substr(year, 3, 4), "_", type, "_",
                       "ProcessedData_", gpkg.wd, ".gpkg")

# Output file name
output.csv <- paste0(site, substr(year, 3, 4),"_", "TileCoordinates_",
                     buffer, "kmBuffer_", wd, ".csv")

# Define Directory Paths
# Input Directory (spatial & tabular)
dsn <- file.path(root, "shared_data", "NEON_field_data", site, year, "ERSAM", 
                 "GPS_Data", "Processed")

# Output Directory
save.dir <- file.path(root, "shared_data", "NEON_field_data", site, year, 
                      "ERSAM", "GPS_Data", "Processed")

# get list of files in directory
files <- list.files(dsn)
print(files)

# load the gpkg that contains combined spatial and tabular data 
in.gpkg <- st_read(file.path(dsn, in.gpkg.name))

#--------------------------------------------------------------
# SCRIPT SHOULD RUN FROM HERE 
#--------------------------------------------------------------

#-------
# step 1: wrangle the tabular data 
#-------

# create a data frame of unique tile eastings and northings 
easting <- in.gpkg$tile_easting

northing <- in.gpkg$tile_northing

tile_coords <- as.data.frame(unique(cbind(easting,northing)))

# Convert the data frame to an sf object
tile_points <- st_as_sf(tile_coords, 
                        coords = c("easting", "northing"), 
                        crs = epsg) 

#-------
# Step 2: plot your points!
#-------

# load basemap 
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE, crs = epsg))

# Reproject basemap into UTM
states_utm <- st_transform(states, epsg)

# specify the tile map bounding box coordinates 
bbox <- st_bbox(tile_points) + c(-10000, -10000, 10000, 10000)

# expand bbox limits for inset map
inset_bbox <- bbox + c(-500000, -500000, 500000, 500000)

# create polygon of the bbox for inset map
bbox.shp <- bbox %>% st_as_sfc()

# create inset map
inset <- ggplot(data = states_utm) +
  geom_sf(fill = "lightblue") +
  geom_sf(data = bbox.shp, fill = "red")+
  coord_sf(xlim = c(inset_bbox[["xmin"]], inset_bbox[["xmax"]]), 
           ylim = c(inset_bbox[["ymax"]], inset_bbox[["ymin"]]),
           crs = epsg) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# create map of tiles 
tiles_map <- ggplot(data = states_utm) +
  geom_sf() +
  geom_sf(data = tile_points, size = 3, shape = 24, fill = "lightgreen")+
  coord_sf(xlim = c(bbox[["xmin"]], bbox[["xmax"]]), 
           ylim = c(bbox[["ymax"]], bbox[["ymin"]]), crs = epsg, 
           datum = epsg) +
  theme_bw()

# combine inset and tile maps together and plot 
ggdraw(tiles_map) +
  draw_plot(inset, width = 0.3, height = 0.3, x = 0.15, y = 0.05)

#------
# Step 3 (Optional): Select adjoining tiles 
#------
# This step is for users who want to download a continuous bounding box 
# of tiles or add a buffer to the tiles. 
# If you are satisfied with the tiles plotted in step 2, proceed to step 4. 

tile_coords_new <- adjoin_neon_tiles(tile_coords, kmbuffer = buffer)

# convert tile coordinates to sf object 
tile_points2 <- st_as_sf(tile_coords_new, 
                        coords = c("easting", "northing"), 
                        crs = epsg) 

# create map of adjoining tiles for verification
tiles_map2 <- ggplot(data = states_utm) +
  geom_sf() +
  geom_sf(data = tile_points2, size = 3, shape = 24, fill = "blue")+
  geom_sf(data = tile_points, size = 3, shape = 24, fill = "lightgreen")+
  coord_sf(xlim = c(bbox[["xmin"]], bbox[["xmax"]]), 
           ylim = c(bbox[["ymax"]], bbox[["ymin"]]), crs = epsg, 
           datum = epsg) +
  theme_bw()

# combine inset and new tile map together and plot 
ggdraw(tiles_map2) +
  draw_plot(inset, width = 0.3, height = 0.3, x = 0.15, y = 0.05)

# if you are happy with your final coordinates, overwrite so you can save below
tile_coords <- tile_coords_new

#--------
# Step 4: Export csv of NEON tile coordinates 
#--------

# Ensure save directory exists
if (!dir.exists(save.dir)) dir.create(save.dir, recursive = TRUE)

# Export CSV 
write.csv(tile_coords, file.path(save.dir, output.csv))








