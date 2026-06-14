# Title: GPS_transformation
# Date: 06/24/2025
# Author: TRG
# This script for reading in field data GPS points, joining GPS data with 
# attribute data and locating tiles and flight lines based on GPS points 

#load packages 
library(sf)
library(tidyverse)
library(terra)

# -----------------------------------
# USER-DEFINED VARIABLES
# -----------------------------------

# read in config file with site info
source("0_config_files/config_base.R")

# Define your data type
type <- "Hemiphoto" #Options: "Field", "Hemiphoto", "Spectra"

# Input file names
in.csv.name <- paste0(site, substr(year, 3, 4), "_", type,"DataSheet.csv")

# Output file names
output.csv <- paste0(site, substr(year, 3, 4), "_", type, "_ProcessedData_",
                     wd, ".csv")
output.gpkg <- paste0(site, substr(year, 3, 4),"_", type, "_ProcessedData_",
                      wd, ".gpkg")

# Define Directory Paths
# Input Directory (spatial & tabular)
# note file structure still isn't 100% consistent
dsn.gps <- file.path(root, "shared_data", "NEON_field_data", site, year, 
                     "ERSAM", "GPS_Data", "Shapefiles", "HemiPhotos")
dsn.tab <- file.path(root, "shared_data", "NEON_field_data", site, year, 
                     "ERSAM", "HemiPhotos")

# Output Directory
save.dir <- file.path(root, "shared_data", "NEON_field_data", site, year, 
                      "ERSAM", "GPS_Data", "Processed")

# Load field data CSV 
in.csv <- read.csv(file.path(dsn.tab, in.csv.name))

# call the ERSAM functions file - this should be inside your Rproj
source("ERSAM_functions.R")

# list all the .shp files in the 'Raw' data directory that match the data type
shp.files.list <- list.files(dsn.gps, pattern= paste0(".*\\.shp$"), 
                             full.names = TRUE)

# load each .shp and combine into a single data frame
in.points <- do.call(rbind, lapply(shp.files.list, st_read))

# set crs if not already set
in.points <- st_set_crs(in.points, epsg)

# change your unique identifier column name for consistency 
colnames(in.points)[1] <- ("ID")

# ID's need capitalized 
in.points$ID<- toupper(in.points$ID)

#----------------------------------------------------------------------------
# Step 1: compare tabular data and spatial data to determine if there are any 
# inconsistencies between your joining column (ID)
#---------------------------------------------------------------------------

# Identify IDs in 'in.points' that do not have a match in 'in.csv'
bad.vals <- anti_join(in.points, in.csv, by = "ID")

# take a look at these to see if there are any
print(bad.vals)

# Create a table of mismatched ID's and their row numbers 
bad.rows <- in.points %>%
  mutate(row_num = row_number()) %>%
  filter(ID %in% bad.vals$ID) %>%
  select(ID, row_num)

# Check for duplicate IDs within 'in.points' (e.g., accidental double entry)
# Create a table of duplicated ID's and their row numbers
duplicate.ids <- in.points %>%
  mutate(row_num = row_number()) %>%
  group_by(ID) %>%
  filter(n() > 1) %>%
  summarise(rows = list(row_num), .groups = "drop")

# Output results for manual inspection/fixing. See commented sections below for
# help! 
print(bad.rows)
print(duplicate.ids)


###### EXAMPLES FOR FIXING STRING ERRORS ########

# -----------
# Example 1: Fix bad ID by writing  specifying row numbers and correct IDs
# -----------
# rows.list <- c(36)  # Example rows needing correction
# new.vals <- c("UMBS535")  # Corrected ID strings
# 
# # Apply corrections inline
# in.points$ID[rows.list] <- new.vals

# -----------
# Example 2: Insert missing characters into an ID string using regex.
# -----------
# Scenario: ID "OSBS001" should be "OSBS25001" in row #13.
# Explanation:
# - First group: capture first 4 characters (OSBS) → \\1
# - Insert '25' after the first group → \\125
# - Second group: capture the rest of the string (001) → \\2
# in.points$ID[13] <- sub("(.{4})(.*)$", "\\125\\2", in.points$ID[13])

# -----------
# Example 3: Directly replace an incorrect ID by overwriting the string.
# -----------
# Scenario: ID "OSBS24101" should be "OSBS25101" in row #13.
# in.points$ID[13] <- "OSBS25101"

# -----------
# Example 4: Remove rows that don't have matches - don't run if all is good!
# -----------
in.points <- in.points[-bad.rows$row_num,]

###### SCRIPT SHOULD RUN FROM HERE #######

#-------------
# STEP 2: generate UTM coordinates, ESPG code, and NEON tile coordinates
#-------------

# project data into target UTM Zone 
points.rpj <- st_transform(in.points, crs = epsg)

# plot to take a look (would be nice to make a better map here...)
plot(points.rpj)

# write point coordinates to a data frame 
raw_coords_df <- as.data.frame(st_coordinates(points.rpj))

# add unique a identifier column from your GPS data we can do an attribute join 
# later 
raw_coords_df$ID <- in.points$ID

# call the function to list the UTM coordinates and coordinates of all the tiles 
UTM_coords_df <- list_AOP_Tiles(raw_coords_df, input_crs = epsg) 
                                                             
# merge the point geometries and the tabular data that you just generated 
merged_df <- merge(UTM_coords_df, points.rpj, by.x = "ID", by.y = "ID")


#---------------
# Step 3: merge CSV file of tabular data with the point data
#---------------

all_merge <- merge(merged_df, in.csv, by.x = "ID", by.y = "ID") 

#--------------
# Step 4: export all your data 
#--------------

# Ensure save directory exists
if (!dir.exists(save.dir)) dir.create(save.dir, recursive = TRUE)

# Export CSV and GeoPackage
write.csv(all_merge, file.path(save.dir, output.csv))

st_write(all_merge, file.path(save.dir, output.gpkg))







