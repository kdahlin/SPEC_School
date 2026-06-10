# Title: 04_Beer_Lambert_Calc
# Author: Aaron Kamoske (2021) and updated by TRG & KMD (2025)
# Date: 07/01/2025
# This script for creating linear model of DHP LAI and LiDAR LAI to 
# calculate Beer-Lambert coefficient (K)

# load packages 
library(sf)
library(tidyverse)
library(terra)

#--------------------
# USER DEFINED VARIABLES 
#--------------------

# read in config file with site info
source("0_config_files/config_base.R")

# date attached to processed & mosaicked lidar imagery
fullsite.date <- "20260330"

# date attached to hemiphoto processing file
hemi.date <- "20260402"

# define your voxel resolution 
vox.xy<- 10 
vox.z <- 1

#--------------------------
# Define Directory Paths & file names
#---------------------------
# Input Directories for field and processed data
field.data.dir <- file.path(root,  "shared_data", "NEON_field_data", site, year, 
                            "ERSAM/")

fullsite.dir <- file.path (root, "shared_data", "NEON_proc_data", site, year,
                        paste0("LidarMetrics_", vox.xy,'x', vox.xy,'x', vox.z),
                        paste0("FullSite_", fullsite.date))

# Path to LAI file
lai.path <- file.path(fullsite.dir, paste0(site, "_", year, 
                                           "_lai_uncalibrated.tif"))

# hemiphoto file names for GPS data and derived LAI data 
hemi.file <- paste0("processed_hemiphotos_",  hemi.date, ".csv")

sampling.locs <- paste0(site, substr(year,3,4), 
                        "_Hemiphoto_ProcessedData.gpkg")

# create directory for saving figures
fig.dir <- file.path(root, "shared_data", "NEON_proc_data", site, year,
                      paste0("LidarMetrics_", vox.xy,'x', vox.xy,'x', vox.z),
                      paste0("MacarthurHorn_figs_", fullsite.date))

if (!dir.exists(fig.dir)) {
  dir.create(fig.dir, recursive = TRUE)
  message("Created directory: ", fig.dir)
} else {
  message("Directory exists: ", fig.dir)
}


#-------------------------------------------------------------------------------
# Step 1: Read in vector data & combine with DHP LAI output csv
#-------------------------------------------------------------------------------
# read in the vector data ## need to edit!
hp.loc <- st_read(file.path(field.data.dir,"GPS_Data", "Processed", 
                            sampling.locs))

# CHECK THIS - depends on how photo.file is named in hp.loc
hp.loc$ImageName <- paste0("IMG_", str_sub(hp.loc$Photo.File.., -4, -1), ".JPG")

# read in the csv data of processed hemiphoto LAI 
pai.data <- read.csv(file.path(field.data.dir, "Hemiphotos", hemi.file))

names(pai.data)[1] <- "ImageName"

# only keep the columns that we need (ID & L (calculated LAI) & Le (effective LAI))
pai.data <- pai.data[,c(1,2,3)]

# lets merge the csv to the vector data
hemi.data <- merge(hp.loc, pai.data, by = "ImageName", all = FALSE)

# lets double check that this all looks good
head(hemi.data)

# lets write this file so we can use it later
st_write(hemi.data, file.path(field.data.dir, paste0("uncalibrated_PAI_clean_", 
                                                 hemi.date, ".gpkg")))

#-------------------------------------------------------------------------------
# Step 2: extract LAI values from LiDAR raster and summarize into data frame
#-------------------------------------------------------------------------------
lai.raster <- rast(lai.path)

# Buffer hemiphoto locations by a user-defined number of raster cells.
# For 10 m pixels and buffer.cells = 2, this creates a 20 m buffer.

buffer.cells <- 2
buffer.dist <- buffer.cells * vox.xy

photo.buffer <- st_buffer(hemi.data, buffer.dist)

# Extract the LiDAR LAI pixels for the photo points 
hemi.data$LAI.LAZ<- terra::extract(lai.raster, photo.buffer, fun = 'mean', 
                                   method = 'simple', touches = TRUE)[,2]

# save vector data to a data frame, so it can be more easily manipulated 
lai.df <- as.data.frame(hemi.data)

# let's look at the distributions of our data to see if we want to make any
# adjustments
hist(lai.df$Le, main = "HP Effective LAI")

hist(lai.df$LAI.LAZ, main = "lidar LAI")

#-------------------------------------------------------------------------------
# Step 3: create linear model of the DHP LAI and LiDAR LAI to calculate the 
# Beer-Lambert coefficient 
#-------------------------------------------------------------------------------
# Create a linear model without an intercept so that we can find the Beer-Lambert 
# coefficient that we can use to transform our LAD data. We do not use an 
# intercept because that is what the Beer-Lambert Law calls for

#-------------
# first let's plot to see how it looks
#------------
plot(c(0,6), c(0,6), type = "n", xlab = "HP Le", ylab = "lidar PAI")
text(lai.df$Le, lai.df$LAI.LAZ, labels = lai.df$ID)
abline(0,1, col = "red")

# leave this plot open so you can add slope line in a minute (unless you remove
# outliers below, then re-plot)

#-------------
# if you need to remove outliers, do so!
#------------
lai.df <- subset(lai.df, lai.df$Le < 6)

# lai.df <- subset(lai.df, lai.df$ID != "HARV507")

#----------
# make a model and do some more plotting
#---------
lai.mod <- lm(LAI.LAZ ~ Le-1, data = lai.df)

# lets find the R2 and p-value and coefficient for our model
summary(lai.mod)

# add the slope line to the plot
abline(0, lai.mod$coefficients, lty = 3)

# now let's store those values and make a nicer plot
mod.r2 <- round(summary(lai.mod)$r.squared,3)
f_statistic <- summary(lai.mod)$fstatistic
mod.p <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)
mod.p <- round(mod.p, 5)
slope <- round(summary(lai.mod)$coefficients[1],3)

# lets plot the xy plot
plot(lai.df$Le, lai.df$LAI.LAZ,
     xlim = c(0, 6),
     ylim = c(0, 6),
     xlab = "LAI from HemiPhotos",
     ylab = "LAI from LiDAR",
     main = "LiDAR LAI vs Hemiphoto LAI",
     sub = paste0("w/o intercept (red): slope = ", slope, " r2 = ", mod.r2, 
                  " & p-value = ", mod.p))

# lets plot our model on top of our plot
abline(lm(LAI.LAZ ~ Le-1, data = lai.df), col = "red", lwd = 1)

#-------------------------------------
# if it looks good let's save it to a file
#-------------------------------------
png(filename = paste0(fig.dir, "/", site, substr(year, 3,4), 
                      "_uncalibrated_scatter.png"), 
    width = 4, height = 4.5, units = "in", res = 300)
par(mar = c(3.7,3.3,3,1), mgp = c(1.8, 0.7, 0), pty = "m")

# lets plot the xy plot
plot(lai.df$Le, lai.df$LAI.LAZ,
     xlim = c(0, 6),
     ylim = c(0, 6),
     xlab = "LAI from HemiPhotos",
     ylab = "LAI from LiDAR",
     sub = paste0("w/o intercept (red): slope = ", slope, " r2 = ", mod.r2, 
                  " & p-value = ", mod.p),
     cex.sub = 0.8)

title("LiDAR LAI vs Hemiphoto LAI", line = 1.5)
mtext("UNCALIBRATED", side = 3, line = 0.2)

# lets plot our model on top of our plot
abline(lm(LAI.LAZ ~ Le-1, data = lai.df), col = "red", lwd = 1)

# and a 1:1 line
abline(0,1, col = "black", lty = 2)

dev.off()
