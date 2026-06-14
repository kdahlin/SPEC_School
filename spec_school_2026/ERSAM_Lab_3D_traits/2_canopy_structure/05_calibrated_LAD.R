# Title: 05_calibrated_LAD.R
# Date: 09/16/2025
# Authors:  AGK, KMD, AWB, MS, AJP & TRG 
# This script for calculating calibrated LAD
#------------
# so far we have: 
# 1.) downloaded point clouds
# 2.) normalized the point clouds (ground points = 0) and removed outliers
# 3.) calculated uncalibrated LAD and LAI 
# 4.) calculated the Beer-Lambert coefficient (k) by creating linear 
# model of DHP LAI and LiDAR LAI

# where we are at: 
# we are now using the K we calculated to rerun the machorn LAD calculation and 
# calibrate our LAD and LAI before we move on to LAD metric calculations. 
#-----------

# Load libraries
library(tidyverse)
library(terra)
library(lidR)
library(data.table)
#-----------

# read in each of the functions 
source("./0_R_functions/canopyLazR/laz.to.array.R")
source("./0_R_functions/canopyLazR/array.to.chm.raster.R")
source("./0_R_functions/canopyLazR/machorn.lad.est.R")
source("./0_R_functions/canopyLazR/lad.array.to.raster.R")

#-------------------------
# USER DEFINED VARIABLES
#-------------------------

# read in config file with site info
source("0_config_files/config_base.R")

# date of CleanPointClouds folder you want to use
clean.date <- "20260330"

# date of "individual files" folder you want to use
indiv.date <- "20260522"

# date of processed hemiphoto file you want to use
hemi.date <- "20251209"

# define your k value 
k <- 0.74

# define the resolution of your voxels
vox.xy <- 10
vox.z <- 1

# define the number of understory layers (z dimension) you want to remove
min.height <-5

#---------------------------
# set directories
#---------------------------

# set the input for the laz arrays we saved as RDS files
laz.array.dir <- file.path(root, "shared_data", "NEON_proc_data", site, year, 
                           paste0("LidarMetrics_", vox.xy,'x', vox.xy,'x', vox.z),
                           paste0("IndividualFiles_", indiv.date), 
                           "Rdata")

# list all the files in the path
laz.array.list <- list.files(laz.array.dir, 
                             pattern="_laz_array\\.(RData|rds)$")
print(laz.array.list)

# define base dir for tile metrics 
base.dir <- file.path(root, "shared_data", "NEON_proc_data",site, year, 
                      paste0("LidarMetrics_", vox.xy,'x', vox.xy,'x', vox.z),
                      paste0("IndividualFiles_", indiv.date))

unmasked.dir <- file.path(base.dir, "unmasked_rasters")

# create metric directories if they don't exist
lad.dir <- file.path(unmasked.dir, "lad")
lai.dir <- file.path(unmasked.dir, "lai")

dirs <- c(lad.dir, lai.dir)

for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
    message("Created directory: ", d)
  } else {
    message("Directory exists: ", d)
  }
}

# create list of subdirectory names to call later 
sub.dirs <- c("lad", "lai")
sub.dirs <- setNames(as.list(file.path(unmasked.dir, sub.dirs)), sub.dirs)

# check strsplit
file.name <- laz.array.list[1]

# this should give you the utm coordinates from the tile -> if not, fix in the
# loop below!
tile.numb <- strsplit(file.name, "_")[[1]][2:3]
print(tile.numb)

# set the directory for the mosaicked files to go in
fullsite.dir <- file.path(root, "shared_data", "NEON_proc_data", site, year, 
                      paste0("LidarMetrics_", vox.xy,'x', vox.xy,'x', vox.z),
                      paste0("FullSite_", indiv.date))


#-------------------------------------------------------------------------------
# MAIN LOOP:
# Calculate calibrated LAD for each individual tile
#-------------------------------------------------------------------------------

for (i in seq_along(laz.array.list)) {
  
  print(i)
  print(paste0("Processing: ", laz.array.list[i]))
  
  # load the files
  vox.array <- readRDS(file.path(laz.array.dir, laz.array.list[i]))
  
  # Estimate LAD for each voxel in leveled array
  lad.estimates <- machorn.lad.est(lidar.array = vox.array,
                                   z.resolution = vox.z,
                                   beer.lambert.constant = k)
  
  # Convert the LAD array into a single raster stack
  lad.raster <- lad.array.to.raster(lad.array = lad.estimates,
                                    laz.array = vox.array,
                                    epsg.code = epsg)
  
  # Let's put a fail safe in case the lad raster doesn't contain any voxels over 
  # 10 meters
  if (nlyr(lad.raster) > 10){
    
    print("Calculating forest attributes!")
    
    # remove the bottom specified layers of LAD (min.height) to reduce influence
    # from the understory 
    lad.raster.subset <- subset(lad.raster, min.height:nlyr(lad.raster)) 
    
    # Create a single LAI raster from the LAD raster stack
    lai.raster <- terra::app(lad.raster.subset, fun = sum, na.rm = TRUE)
    
    #------------------------------------------
    # Lets write all the needed files to disc
    #------------------------------------------
    
    # some file output name prep
    file.name <- laz.array.list[i]
    tile.numb <- strsplit(file.name, "_")[[1]][2:3]
    
    # LAD estimates
    saveRDS(lad.estimates, 
            file = paste0(laz.array.dir, 
                          "/", site, "_", 
                          tile.numb[1], "_", 
                          tile.numb[2],
                          "_lad.rds"))
    
    rasters.to.save <- list(
      lad = lad.raster.subset,
      lai = lai.raster
    )
    
    #---------------------------
    # Write rasters automatically
    #---------------------------
    for (metric in names(rasters.to.save)) {
      terra::writeRaster(
        rasters.to.save[[metric]],
        filename = file.path(sub.dirs[[metric]], 
                             paste0(site, "_", tile.numb[1], "_", tile.numb[2], 
                                    "_", metric,".tif")),
        filetype = "GTiff",
        overwrite = TRUE
      )
    }
    
    #------------------------------------------------------
    # Lets clean up some memory so we can run another tile
    #------------------------------------------------------
    
    gc()
    rm(vox.array)
    rm(lad.estimates)
    rm(lad.raster)
    rm(lai.raster)
    gc()
  } else{
    
    print("Skipping tile...no returns over 10m!")
    
  }
  
  print(paste0(length(laz.array.list) - i, " tiles remaining!"))
  
}

#-------------------------------------------------------------------------------
# MOASIC RASTERS:
# Merge & export all the individual rasters so we can extract LAI values!
#-------------------------------------------------------------------------------
laz.folders <- c(file.path(lai.dir), file.path(lad.dir))

for (i in 1:length(laz.folders)) {
  
  #list all the files in the folder we want to process
  rasters.list <- list.files(laz.folders[i], pattern="\\.tif$", 
                             full.names = TRUE)
  
  #make an empty list
  rst.list <- list()
  
  #load all the rasters into the list
  for (q in 1:length(rasters.list)) {
    rst<- rast(rasters.list[q])
    
    rst.list[[q]] <- rst
  }
  
  # merge all the rasters together
  merged.raster <- do.call(terra::merge, rst.list)
  
  #lets make the file name
  file.name <- rasters.list[q]
  tile.type <- strsplit(file.name, "/")[[1]][9]
  
  # write the raster to the folder
  terra::writeRaster(merged.raster, 
                     filename = paste0(fullsite.dir, "/", site, "_", year, "_", tile.type, ".tif"), 
                     filetype = "GTiff", overwrite = TRUE)
  
  message("Merged raster successfully saved to: ", fullsite.dir)
}

#-------------------------------------------------------------------------------
# PLOTTING: 
# we are now plotting the calibrated LAD against the uncalibrated LAD to 
# investigate how it changed our output.
#------------------------------------------------------------------------------

#-------------------------------
# Assign file names/locations 
# for DHP-derived LAI estimates 
# and compare in a plot
#--------------------------------
# Input Directories for field data
field.data <- file.path(root, "shared_data", "NEON_field_data", site, year, "ERSAM/")

# Tabular hemiphoto data
hemi.file <- paste0("processed_hemiphotos_", hemi.date, ".csv")

# Geopackage of hemiphoto locations
sampling.locs <- paste0(site, substr(year, 3,4),
                        "_Hemiphoto_ProcessedData.gpkg")

# Path to LAI file (note: this should be the new, calibrated LAI file)
lai.path <- file.path(root, "shared_data", "NEON_proc_data", site, year,
                      paste0("LidarMetrics_", vox.xy,'x', vox.xy,'x', vox.z), 
                      paste0("FullSite_", indiv.date),
                      paste0(site, "_", year, "_lai.tif"))

# create directory for saving figures
fig.path <- file.path(root, "shared_data", "NEON_proc_data", site, year,
                      paste0("LidarMetrics_", vox.xy,'x', vox.xy,'x', vox.z),
                      paste0("MacarthurHorn_figs_", indiv.date))

if (!dir.exists(fig.path)) {
  dir.create(fig.path, recursive = TRUE)
  message("Created directory: ", fig.path)
} else {
  message("Directory exists: ", fig.path)
}


#------------------------------------
# Step 1: Read in vector data 
# & combine with DHP LAI output csv
#------------------------------------
# read in the vector data 
hemi.data <- st_read(file.path(field.data, paste0("uncalibrated_PAI_clean_", hemi.date,
                                                  ".gpkg")))

# lets double check that this all looks good
head(hemi.data)

#---------------------------------------
# Step 2: extract LAI values from LiDAR 
# raster and summarize into data frame
#---------------------------------------
# Load the calibrated LAI raster
lai.raster <- rast(lai.path)

# create a 20 m buffer around photo points (Note: the raster resolution is 10m)
photo.buffer <- st_buffer(hemi.data, 2)

# Extract the LiDAR LAI pixels for the photo points 
hemi.data$LAI.LAZ.cal<- terra::extract(lai.raster, photo.buffer, fun = 'mean', 
                                   method = 'simple', touches = TRUE)[,2]

# save vector data to a data frame, so it can be more easily manipulated 
lai.df <- as.data.frame(hemi.data)

#----------------------------------
# Step 3: Look at new LAI 
# relationships post-beer-lambert 
#----------------------------------
# # if you need to remove outliers, do so! (should match the 04 script)
# lai.df <- subset(lai.df, lai.df$Le < 6)
# lai.df <- subset(lai.df, lai.df$ID != "HARV507")


# Create a linear model 
lai.mod <- lm(LAI.LAZ.cal ~ Le-1, data = lai.df)

# lets find the R2 and p-value and coefficient for our model
summary(lai.mod)

mod.r2 <- round(summary(lai.mod)$r.squared,3)
f_statistic <- summary(lai.mod)$fstatistic
mod.p <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)
mod.p <- round(mod.p, 5)
slope <- round(summary(lai.mod)$coefficients[1],3)

# lets plot the xy plot
plot(lai.df$Le, lai.df$LAI.LAZ.cal,
     xlim = c(0, 8),
     ylim = c(0, 8),
     xlab = "LAI from HemiPhotos",
     ylab = "LAI from LiDAR",
     main = "LiDAR LAI vs Hemiphoto LAI",
     sub = paste0("w/o intercept (red): slope = ", slope, " r2 = ", mod.r2, 
                  " & p-value = ", mod.p))

# lets plot our model on top of our plot
abline(lm(LAI.LAZ.cal ~ Le-1, data = lai.df), col = "red", lwd = 1)

# and a 1:1 line for good measure
abline(0,1, lty = 2)

# and if it looks good let's save it to a file
png(filename = paste0(fig.path, "/", site, substr(year, 3,4), 
                      "_calibrated_scatter.png"), 
      width = 4, height = 4.5, units = "in", res = 300)
  par(mar = c(3.7,3.3,3,1), mgp = c(1.8, 0.7, 0), pty = "m")
  
  # lets plot the xy plot
  plot(lai.df$Le, lai.df$LAI.LAZ.cal,
       xlim = c(0, 6),
       ylim = c(0, 6),
       xlab = "LAI from HemiPhotos",
       ylab = "LAI from LiDAR",
       sub = paste0("w/o intercept (red): slope = ", slope, " r2 = ", mod.r2, 
                    " & p-value = ", mod.p),
       cex.sub = 0.8)
  
  title("LiDAR LAI vs Hemiphoto LAI", line = 1.5)
  mtext(paste0("CALIBRATED, k = ", k), side = 3, line = 0.2)
  
  # lets plot our model on top of our plot
  abline(lm(LAI.LAZ.cal ~ Le-1, data = lai.df), col = "red", lwd = 1)
  
  # and a 1:1 line
  abline(0,1, col = "black", lty = 2)
  
dev.off()





