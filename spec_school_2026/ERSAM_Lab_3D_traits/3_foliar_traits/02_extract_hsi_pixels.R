# Title: 02_extract_hsi_pixels.R
# Date: 05/27/2026
# Author: TRG, AJP, & KMD 
# This script for extracting spectral signatures at each sample location from
# tiled NEON AOP data

# load necessary packages
library(rhdf5)
library(terra)
library(sf)

# set the number of digits displayed
options(digits = 20)

# -----------------------------------
# USER-DEFINED VARIABLES
# -----------------------------------

# read in config file with site info
source("0_config_files/config_base.R")  

# date of geopackage file for foliar samples that you want to use
gpkg.date <- "20260522"

# -----------------------------------
# set up input and output paths and data locations
# -----------------------------------

# set how you want the extraction to go (simple = just the pixel the point falls
# in, bilinear = mean of four nearest pixels)
extract.method <- "bilinear"

# set file path for hyperspectral images (assuming tiles for now)
dir_path <- file.path(root, "shared_data", "NEON_AOP_data", site, year, "L3", 
                      "Spectrometer", "Reflectance")

# file path for foliar sample locations
dsn <- file.path(root, "shared_data", "NEON_field_data", site, year, "ERSAM", 
                 "GPS_Data", "Processed")

# set out file path for saving output file 
out.path <- file.path(root, "shared_data", "NEON_proc_data", site, year,
                      paste0("3-02_ImageSpectra_", wd))

# make this directory
dir.create(out.path)

# set the out file name
out.file.name <- paste0(site, "_", year, "_ImageSpectra.csv")

# get foliar sampling geopackage (which has tile eastings and northings)
# Input file name
in.gpkg.name <- paste0(site, substr(year, 3, 4), "_Foliar_",
                       "ProcessedData_", gpkg.date, ".gpkg")

# load the gpkg that contains combined spatial and tabular data 
in.gpkg <- st_read(file.path(dsn, in.gpkg.name))

hsi.tiles <- paste0("NEON_", domain, "_", site, "_DP3_", in.gpkg$tile_easting,
                    "_", in.gpkg$tile_northing, "_bidirectional_reflectance.h5")
hsi.tiles.short <- unique(hsi.tiles)

# list all of the data types within the hdf5 (this should be the same for all files)
h5ls(file = paste0(dir_path, "/", hsi.tiles.short[1]))


# get a list of wavelengths (should be the same for all files in a given year
# and site but can change from year to year or sensor to sensor)
wavelengths <- h5read(file = paste0(dir_path, "/", hsi.tiles.short[1]),
                      name = paste0(site, 
                                    "/Reflectance/Metadata/Spectral_Data/Wavelength"))
wave.count <- 1:length(wavelengths)
wave.round <- round(wavelengths,0)
wave.names <- paste0("wave.", wave.round)

# make a quick table of the wavelength values for plotting later
out.waves <- as.data.frame(cbind(wave.count, wave.names, wavelengths))

write.csv(out.waves, 
          paste0(out.path, "/", "NEON_HSI_wavelengths_", today, ".csv"),
          row.names = FALSE)

# -----------------------------------
# start extracting data!
# -----------------------------------

# make an output table
out.data <- as.data.frame(matrix(data = NA, 
                                 nrow = 0, 
                                 ncol = (length(wave.names) + 4)), 
                          stringsAsFactors = FALSE)

names(out.data) <- c("ID", 
                     "easting", 
                     "northing", 
                     "tile", 
                     wave.names)

# this loop opens each tile and opens each hyperspectral band individually then
# extracts the point values for that tile x layer, so the whole hyperspectral
# cube never has to be read into memory

for (i in 1:length(hsi.tiles.short)) {
  
  # get the point locations that are in the first hsi tile
  locs.subset <- subset(in.gpkg, 
                        hsi.tiles == hsi.tiles.short[i])
  
  # make an output table for just the data in this tile
  out.subset <- as.data.frame(matrix(data = NA, 
                                     nrow = dim(locs.subset)[1], 
                                     ncol = (length(wave.names) + 4)), 
                              stringsAsFactors = FALSE)
  names(out.subset) <- c("ID", 
                         "easting", 
                         "northing", 
                         "tile", 
                         wave.names)
  
  # pull info from point data (individually just for clarity)
  out.subset$ID <- locs.subset$ID
  out.subset$easting <- locs.subset$easting
  out.subset$northing <- locs.subset$northing
  out.subset$tile <- hsi.tiles.short[i]
  
  # initiate a progress bar to track extraction time
  print(paste("Extracting points for", hsi.tiles.short[i], "starting at", 
              date()))
  pb <- txtProgressBar(min = 0, max = length(wavelengths), style = 3)
  
  # now open up each refl band and extract points
  for (j in 1:length(wavelengths)) {

    # need to get the coordinates from the file - note that file names are for
    # the lower left corner but R reads rasters in from upper left corner!
    # which are stored in the Map_Info of the hdf5 file
    coords <- h5read(file = paste0(dir_path, "/", hsi.tiles.short[i]),
                     name = paste0(site, "/Reflectance/Metadata/Coordinate_System"))
    
    map.info <- strsplit(coords$Map_Info, 
                         split = ",", 
                         fixed = T)
    easting.ul <- as.numeric(map.info[[1]][4])
    northing.ul <- as.numeric(map.info[[1]][5])
    
    # get subsetted reflectance
    refl.sub <- h5read(file = paste0(dir_path, "/", hsi.tiles.short[i]),
                       name = paste0(site, "/Reflectance/Reflectance_Data"),
                       index = list(j, NULL, NULL))
    
    refl.stack <- rast(as.matrix(refl.sub[1,,]))
    
    refl.stack[refl.stack == -9999] <- NA
    refl.stack.t <- t(refl.stack)
    
    line.dim <- dim(refl.stack.t)
    y.dim <- line.dim[1]
    x.dim <- line.dim[2]
    
    ext(refl.stack.t) <- c(easting.ul, 
                           easting.ul+x.dim, 
                           northing.ul-y.dim, 
                           northing.ul)
    
    # assign projection (note - this is hard coded because it's 'UTM' in the hdf5 
    #file and it's acutally case sensitive and should be 'utm' :[ sigh.)
    crs(refl.stack.t) <- paste0("EPSG:", epsg)
    
    out.vals <- extract(refl.stack.t, 
                        as.matrix(out.subset[,2:3]), 
                        method = extract.method)
    
    out.subset[,4+j] <- out.vals
    
    setTxtProgressBar(pb, j)
  }
  
  close(pb)
  out.data <- rbind(out.data, out.subset)
  print(paste("done with", hsi.tiles.short[i], "at", date()))
}

# plot spectra to see if there are any red flags (note that there will be noise
# especially in the two water absorption features in the SWIR)
x11()
matplot(wave.round, 
        t(out.data[5:ncol(out.data)]), 
        type = "l", 
        xlab = "Wavelength (nm)",
        ylab = "Reflectance",
        main = paste("Image Spectra for", site, year))

# write the final data set out!
write.csv(out.data, 
          paste0(out.path, "/", site, "_hsi_by_point_tile_", extract.method,
                 "_", wd, ".csv"),
          row.names = FALSE)


