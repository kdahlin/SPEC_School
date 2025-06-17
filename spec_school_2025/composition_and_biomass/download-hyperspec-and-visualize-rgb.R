# first section from
# Introduction to Hyperspectral Remote Sensing Data in R tutorial
# from NEON

## ----install-load-library, results="hide"------------------------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(terra)
library(rhdf5)
library(neonUtilities)
library(tidyverse)
library(ggplot2)
library(sf)

## ----set-wd, results="hide"--------------------------------------------------------------------------------------------------------------------------------------------------

wd <- "C:\\Users\\mille\\Documents\\NAU 2024-2026\\spec-school-summer-2025\\" #This will depend on your local environment
setwd(wd)

# uncomment if need to download hyperspectral data
# #----download-refl, eval=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------
# byTileAOP(dpID='DP3.30006.001',
#           site='MLBS',
#           year='2018',
#           easting=542000,
#           northing=4137000,
#           check.size=TRUE, # set to FALSE if you don't want to enter y/n
#           savepath = wd)


## ----define-h5, results="hide"-----------------------------------------------------------------------------------------------------------------------------------------------
# Define the h5 file name to be opened
h5_file <- paste0(wd,"DP3.30006.001/neon-aop-products/2018/FullSite/D07/2018_MLBS_3/L3/Spectrometer/Reflectance/NEON_D07_MLBS_DP3_542000_4137000_reflectance.h5")


## ----view-file-strux, eval=FALSE, comment=NA---------------------------------------------------------------------------------------------------------------------------------
# look at the HDF5 file structure 
#View(h5ls(h5_file,all=T))


## ----read-band-wavelength-attributes-----------------------------------------------------------------------------------------------------------------------------------------

# get information about the wavelengths of this dataset
wavelengthInfo <- h5readAttributes(h5_file,"/MLBS/Reflectance/Metadata/Spectral_Data/Wavelength")
wavelengthInfo



## ----read-band-wavelengths---------------------------------------------------------------------------------------------------------------------------------------------------
# read in the wavelength information from the HDF5 file
wavelengths <- h5read(h5_file,"/MLBS/Reflectance/Metadata/Spectral_Data/Wavelength")
head(wavelengths)
tail(wavelengths)



## ----get-reflectance-shape---------------------------------------------------------------------------------------------------------------------------------------------------

# First, we need to extract the reflectance metadata:
reflInfo <- h5readAttributes(h5_file, "/MLBS/Reflectance/Reflectance_Data")
reflInfo

# Next, we read the different dimensions

nRows <- reflInfo$Dimensions[1]
nCols <- reflInfo$Dimensions[2]
nBands <- reflInfo$Dimensions[3]

nRows
nCols
nBands

#######
# this code from part 2 of NEON tutorial
## ----get-spatial-attributes-------------------------------------------------------------------------------------------------------------------------------------

# define coordinate reference system from the EPSG code provided in the HDF5 file
h5EPSG <- h5read(h5_file,"/MLBS/Reflectance/Metadata/Coordinate_System/EPSG Code" )
h5CRS <- crs(paste0("+init=epsg:",h5EPSG))

# get the Reflectance_Data attributes
reflInfo <- h5readAttributes(h5_file,"/MLBS/Reflectance/Reflectance_Data" )

# Grab the UTM coordinates of the spatial extent
xMin <- reflInfo$Spatial_Extent_meters[1]
xMax <- reflInfo$Spatial_Extent_meters[2]
yMin <- reflInfo$Spatial_Extent_meters[3]
yMax <- reflInfo$Spatial_Extent_meters[4]

# define the extent (left, right, top, bottom)
rastExt <- ext(xMin,xMax,yMin,yMax)

# view the extent to make sure that it looks right
rastExt

# Finally, define the no data value for later
h5NoDataValue <- as.integer(reflInfo$Data_Ignore_Value)
cat('No Data Value:',h5NoDataValue)

######

wavelengths <- h5read(h5_file, "/MLBS/Reflectance/Metadata/Spectral_Data/Wavelength")
length(wavelengths)  # should be 426
all_bands <- 1:length(wavelengths)

# function from neon tutorial
band2Raster <- function(file, band, noDataValue, extent, CRS){
  # first, read in the raster
  out <- h5read(file,"/MLBS/Reflectance/Reflectance_Data",index=list(band,NULL,NULL))
  # Convert from array to matrix
  out <- (out[1,,])
  # transpose data to fix flipped row and column order 
  # depending upon how your data are formatted you might not have to perform this
  # step.
  out <- t(out)
  # assign data ignore values to NA
  # note, you might chose to assign values of 15000 to NA
  out[out == noDataValue] <- NA
  
  # turn the out object into a raster
  outr <- rast(out,crs=CRS)
  
  # assign the extents to the raster
  ext(outr) <- extent
  
  # return the terra raster object
  return(outr)
}

# stack took several minutes
full_hs_stack <- lapply(all_bands, FUN = band2Raster,
                        file = h5_file,
                        noDataValue = h5NoDataValue,
                        extent = rastExt,
                        CRS = h5CRS)

# Combine into a raster stack
hsStack <- rast(full_hs_stack)

# Scale reflectance values
hsStack <- hsStack / as.integer(reflInfo$Scale_Factor)

hsStack
plot(hsStack[[c(1, 50, 100)]], main = c("Band 1", "Band 50", "Band 100"))

# Find band indices closest to typical RGB wavelengths
red_band   <- which.min(abs(wavelengths - 650))  # ~650 nm
green_band <- which.min(abs(wavelengths - 550))  # ~550 nm
blue_band  <- which.min(abs(wavelengths - 450))  # ~450 nm

# Create RGB image
rgb_img <- hsStack[[c(red_band, green_band, blue_band)]]

plotRGB(rgb_img, stretch = "lin")