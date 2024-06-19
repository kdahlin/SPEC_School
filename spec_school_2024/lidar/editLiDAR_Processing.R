################################################################################
# This code will process NEON LiDAR data into a host of different RData and 
# Raster files

# Code written by Aaron Kamoske - kamoskea@msu.edu - Summer 2019

# Revised by Tony Bowman - bowman84@msu.edu - Fall 2023 
################################################################################

#let's load the necessary libraries
library(terra)
library(plyr)
library(sf)
library(plyr)
library(tidyverse)
#test
source("Z:/shared_code/SPEC_School/canopyLazR_V2.R")

#-------------------------------------------------------------------------------
# First some basics
#-------------------------------------------------------------------------------

#set the file path - CHANGE THIS TO MATCH YOUR MAPPED HPCC DRIVE
laz.files <- "Z:/shared_data/NEON_AOP_data/MLBS/2022/lidar_pointcloud_2022/"

#set a prefix for the rasters we'll generate and a home directory the save files 
#to EXAMPLE - Replace with your own home directory but keep the "/MLBS_" file 
#prefix ##
wd <- "K:/SPEC_School_2024/"

# make a directory for output
dir.create(paste0(wd, "lidar_output_MLBS_2022"))

#list all the files in the path
laz.files.list <- list.files(laz.files, pattern=c("\\.laz$|.las$"), 
                             full.names = TRUE)

#get rid of any files that are less than 500kb
laz.files.list <- laz.files.list[sapply(laz.files.list, file.size) > 500000]

# how many are left? (we only downloaded the four tiles surrounding the Bio
# Station for MLBS in 2022)
length(laz.files.list)

#-------------------------------------------------------------------------------
# Let's process each LiDAR tile individually 
#-------------------------------------------------------------------------------

# IMPORTANT: set i as the tile number from the list you want to process
i <- 1

# Convert .laz or .las file into a voxelized lidar array
laz.data <- laz.to.array.2(laz.file.path = laz.files.list[i], 
                           voxel.resolution = 5, 
                           z.resolution = 1,
                           use.classified.returns = TRUE)

# Level the voxelized array to mimic a canopy height model
level.canopy <- canopy.height.levelr.2(lidar.array = laz.data)
    
# Estimate LAD for each voxel in leveled array
lad.estimates <- machorn.lad.2(leveld.lidar.array = level.canopy,
                                 voxel.height = 1,
                                 beer.lambert.constant = 0.5328156) 
 
# Convert the LAD array into a single raster stack
lad.raster <- lad.array.to.raster.stack.2(lad.array = lad.estimates,
                                          laz.array = laz.data,
                                          epsg.code = 32617)
  
# We should remove the bottom 5 meters of LAD data to match everything else 
# before calculating LAI
lad.raster.5 <- subset(lad.raster, 5:nlyr(lad.raster)) 

# Create a single LAI raster from the LAD raster stack
lai.raster <- terra::app(lad.raster.5, fun = sum, na.rm = TRUE)

## take a look!
plot(lai.raster, col=map.pal("viridis"))
    
# Convert the list of LAZ arrays into a ground and canopy height raster
grd.can.rasters <- array.to.ground.and.canopy.rasters.2(laz.array = laz.data,
                                                          epsg.code = 32617)
    
# Calculate max LAD and height of max LAD
max.lad <- lad.ht.max.2(lad.array = lad.estimates,
                          laz.array = laz.data,
                          ht.cut = 5,
                          epsg.code = 32617)

# Calculate the ratio of filled and empty voxels in a given column of the canopy
empty.filled.ratio <- canopy.porosity.filled.ratio.2(lad.array = lad.estimates,
                                                       laz.array = laz.data,
                                                       ht.cut = 5,
                                                       epsg.code = 32617)

# Calculate the volume of filled and empty voxles in a given column of the canopy
empty.filled.volume <- canopy.porosity.filled.volume.2(lad.array = lad.estimates,
                                                         laz.array = laz.data,
                                                         ht.cut = 5,
                                                         xy.res = 5,
                                                         z.res = 1,
                                                         epsg.code = 32617)

# Calculate the rugosity in a given column of the canopy      
within.can.rugosity <- rugosity.within.canopy.1.5(lad.array = lad.estimates,
                                                  laz.array = laz.data,
                                                  ht.cut = 5,
                                                  epsg.code = 32617)


# Calculate the heights of various LAD quantiles
ht.quantiles <- lad.quantiles.2(lad.array = lad.estimates,
                                  laz.array = laz.data,
                                  ht.cut = 5,
                                  epsg.code = 32617)

# Calculate various canopy volume metrics from Lefsky
can.volume <- canopy.volume.2(lad.array = lad.estimates,
                                laz.array = laz.data,
                                ht.cut = 5,
                                xy.res = 5,
                                z.res = 1,
                                epsg.code = 32617)

# We can calculate the depth of the euphotic zone by dividing by the volume of 
# the voxel
euphotic.depth <- can.volume$euphotic.volume.column.raster / ( 5 * 5 * 1)
 
# #-----------------------------------------------------------------------------
# # Lets write all the needed files to disc
# #-----------------------------------------------------------------------------


#some file output name prep
file.name <- laz.files.list[i]
tile.numb <- strsplit(file.name, "_")[[1]][10:11]

#laz array
save(laz.data, 
     file = paste0(wd, tile.numb[1], "_", tile.numb[2], "_laz_array.RData"))
                                 

# lad raster stack
terra::writeRaster(lad.raster.5, 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2] , 
                                     "_lad.tif"),
                   filetype = "GTiff", overwrite = TRUE)
                           
               
#lai raster
terra::writeRaster(lai.raster, 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_lai.tif"),
                   filetype = "GTiff", overwrite = TRUE)
                                             

#ground raster
terra::writeRaster(grd.can.rasters[[1]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_dtm.tif"),
                   filetype = "GTiff", overwrite = TRUE)


#canopy raster
terra::writeRaster(grd.can.rasters[[2]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_dsm.tif"),
                  filetype = "GTiff", overwrite = TRUE)                 


#chm raster
terra::writeRaster(grd.can.rasters$chm.raster, 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_chm.tif"),
                   filetype = "GTiff", overwrite = TRUE)             
               

#max LAD raster
terra::writeRaster(max.lad[[1]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_maxlad.tif"),
                   filetype = "GTiff", overwrite = TRUE)       
            


#height of max LAD raster
terra::writeRaster(max.lad[[2]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_maxladheight.tif"),
                   filetype = "GTiff", overwrite = TRUE)                                       
              

#filled voxel ratio raster
terra::writeRaster(empty.filled.ratio[[1]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_filledvoxelratio.tif"),
                   filetype = "GTiff", overwrite = TRUE)                                      


#porosity voxel ratio raster
terra::writeRaster(empty.filled.ratio[[2]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_porosityratio.tif"),
                   filetype = "GTiff", overwrite = TRUE)

    

#standard deviation of LAD within a column 
terra::writeRaster(within.can.rugosity[[2]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_sdladcolumn.tif"),
                   filetype = "GTiff", overwrite = TRUE)
                                                                          


#height of lad at 10th quantile
terra::writeRaster(ht.quantiles[[1]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_quantile10.tif"),
                   filetype = "GTiff", overwrite = TRUE)
                                                                 

#height of lad at 25th quantile
terra::writeRaster(ht.quantiles[[2]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_quantile25.tif"),
                   filetype = "GTiff", overwrite = TRUE)
                                                                

#height of lad at 50th quantile
terra::writeRaster(ht.quantiles[[3]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_quantile50.tif"),
                   filetype = "GTiff", overwrite = TRUE)
                                                                  

#height of lad at 75th quantile
terra::writeRaster(ht.quantiles[[4]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_quantile75.tif"),
                   filetype = "GTiff", overwrite = TRUE)

                                                                   
#height of lad at 90th quantile
terra::writeRaster(ht.quantiles[[5]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_quantile90.tif"),
                   filetype = "GTiff", overwrite = TRUE)
                                                                  

#height of mean lad
terra::writeRaster(ht.quantiles[[6]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_meanladheight.tif"),
                   filetype = "GTiff", overwrite = TRUE)
                                  

#euphotic zone volume in a column
terra::writeRaster(can.volume[[1]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_euphoticvolume.tif"),
                   filetype = "GTiff", overwrite = TRUE)
                                                                            

#euphotic zone total leaf area in a column
terra::writeRaster(can.volume[[2]], 
                   filename = paste0(wd,tile.numb[1], "_", tile.numb[2], 
                                     "_euphotictla.tif"),
                   filetype = "GTiff", overwrite = TRUE)
                                                                         
#depth of euphotic zone in a column
terra::writeRaster(euphotic.depth, 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_euphoticdepth.tif"),
                   filetype = "GTiff", overwrite = TRUE)
                                                 

#volume of oligophotic zone in a column
terra::writeRaster(can.volume[[3]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_oligophoticvolume.tif"),
                   filetype = "GTiff", overwrite = TRUE)
                                                                               

#oligophotic zone total leaf area in a column
terra::writeRaster(can.volume[[4]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_oligophotictla.tif"),
                   filetype = "GTiff", overwrite = TRUE)
                                                                           

#volume of empty space in a column
terra::writeRaster(can.volume[[5]], 
                   filename = paste0(wd, tile.numb[1], "_", tile.numb[2], 
                                     "_emptyvolume.tif"),
                   filetype = "GTiff", overwrite = TRUE)
                                                                        


#let's do some visualization of our rasters using a palette of your choice
#See the 'Details' section of https://rdrr.io/cran/terra/man/mappal.html for 
#color palette options

## EXAMPLE ##
plot(within.canopy.rugosity, col=map.pal("viridis"))


