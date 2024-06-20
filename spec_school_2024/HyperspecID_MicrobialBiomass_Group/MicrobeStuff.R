#Script for analyzing NEON Soil Microbe Data
rm(list=ls())

install.packages("neonUtilities")
library(neonUtilities)
library(geoNEON)
library(tidyverse)
library(neonOS)
library(reshape2)
library(rhdf5)
library(terra)


microb <- loadByProduct(dpID = "DP1.10104.001", 
                        site = "RMNP", 
                        release = "RELEASE-2024", 
                        check.size = F)





soil <- loadByProduct(dpID = "DP1.10086.001", 
                      site = "RMNP", 
                      release = "RELEASE-2024", 
                      check.size = F)

soil.loc <- getLocTOS(data=soil$sls_soilCoreCollection,
                      dataProd="sls_soilCoreCollection")

# Identify pre- (i.e., c18To0ScaledConcentration = NA) and post-November 2021 data

preNov2021 <- is.na(microb$sme_scaledMicrobialBiomass$c18To0ScaledConcentration)



# Add the pre-November 2021 total lipid concentration data to a new column (no correction needed)

microb$sme_scaledMicrobialBiomass$correctedTotLipidConc[preNov2021] <- 
  microb$sme_scaledMicrobialBiomass$totalLipidScaledConcentration[preNov2021]



# Add the corrected (totalLipidScaledConcentration - c18To0ScaledConcentration) total lipid concentration data to the new column

microb$sme_scaledMicrobialBiomass$correctedTotLipidConc[!preNov2021] <- 
  microb$sme_scaledMicrobialBiomass$totalLipidScaledConcentration[!preNov2021] - 
  microb$sme_scaledMicrobialBiomass$c18To0ScaledConcentration[!preNov2021]

# Identify the units of the totalLipidConcentration data

microb$variables_10104[grep("totalLipidConcentration", microb$variables_10104$fieldName), c("description", "units")]

microb$variables_10104[grep("coordinateUncertainty", microb$variables_10104$fieldName), c("description", "units")]




#Just the dataframe that has biomass
biomass <- microb$sme_scaledMicrobialBiomass


#relevant variables
soil.loc1 <- soil.loc %>%
  select(adjDecimalLatitude, adjDecimalLongitude, adjEasting, adjNorthing, sampleID, sampleTiming, horizon, sampleTopDepth, sampleBottomDepth)

#relevant variables
biomass1 <- biomass %>%
  select(siteID, sampleID, plotID, collectDate, biomassID, correctedTotLipidConc)


#join soil data and microbe data
soilmicrobe <- left_join(soil.loc1, biomass1)


#get rid of soil samples that ddidn't get microbed
soilmicrobe.1 <- soilmicrobe[!is.na(soilmicrobe$correctedTotLipidConc),]


#lets only look at peak green samples (June/July) to coincide w/ AOP data
soilmicrobe.2 <- subset(soilmicrobe.1, sampleTiming == "peakGreenness")



#Download hyperspectral data


field_spectra <- loadByProduct(dpID='DP1.30012.001',
                               site='RMNP',
                               package="expanded",
                               check.size=FALSE)

list2env(field_spectra, .GlobalEnv)

spectra_data_metadata <- joinTableNEON(fsp_spectralData,fsp_sampleMetadata)

spectra_data <- merge(spectra_data_metadata,per_sample,by="spectralSampleID")

spectra_data$wavelength <- as.numeric(spectra_data$wavelength)

spectra_data$reflectance <- as.numeric(spectra_data$reflectance)

foliar_trait_info$siteCodes[which(foliar_trait_info$siteCodes$siteCode == 'RMNP'),c("availableDataUrls")]

foliar_traits <- loadByProduct(dpID='DP1.10026.001',
                               site='RMNP',
                               startdate='2020-07',
                               package="expanded",
                               check.size=FALSE)

vst.loc <- getLocTOS(data=foliar_traits$vst_mappingandtagging,
                     dataProd="vst_mappingandtagging")

foliar_traits_loc <- merge(foliar_traits$cfc_fieldData,vst.loc,by="individualID")

spectra_traits <- merge(spectra_top_black,foliar_traits_loc,by="sampleID")

# display values of only first wavelength for each sample

spectra_top_black <- spectra_data %>% dplyr::filter(reflectanceCondition == "top of foliage (sunward) on black reference")

spectra_traits_sub <- merge(spectra_top_black[spectra_top_black$wavelength == 350,],foliar_traits_loc,by="sampleID")

spectra_traits_sub[c("spectralSampleID","taxonID","stemDistance","stemAzimuth","adjEasting","adjNorthing","crownPolygonID")]


wd <- "/Volumes/home-030/f0108963/SPEC_School"

setwd(wd)

#Now lets download AOP tiles for all the points where there are soil microbes
options(timeout = max(1000, getOption("timeout")))

byTileAOP(dpID='DP3.30006.001',
          
          site='RMNP',
          
          year=2020,
          
          easting=soilmicrobe.2$adjEasting,
          
          northing=soilmicrobe.2$adjNorthing,
          
          savepath=wd)


# Define the h5 file name to be opened

h5_file <- paste0("/Volumes/home-030/f0108963/SPEC_School/DP3.30006.001/neon-aop-products/2020/FullSite
                  /D10/2020_RMNP_3/L3/Spectrometer/Reflectance/NEON_D10_RMNP_DP3_453000_4458000_reflectance.h5")

geth5metadata <- function(h5_file){
  # get the site name
  site <- h5ls(h5_file)$group[2]
  
  # get the wavelengths
  wavelengths <- h5read(h5_file,paste0(site,"/Reflectance/Metadata/Spectral_Data/Wavelength"))
  
  # get the epsg code
  h5_epsg <- h5read(h5_file,paste0(site,"/Reflectance/Metadata/Coordinate_System/EPSG Code"))
  
  # get the Reflectance_Data attributes
  refl_attrs <- h5readAttributes(h5_file,paste0(site,"/Reflectance/Reflectance_Data"))
  
  # grab the UTM coordinates of the spatial extent
  xMin <- refl_attrs$Spatial_Extent_meters[1]
  xMax <- refl_attrs$Spatial_Extent_meters[2]
  yMin <- refl_attrs$Spatial_Extent_meters[3]
  yMax <- refl_attrs$Spatial_Extent_meters[4]
  
  ext <- ext(xMin,xMax,yMin,yMax) # define the extent (left, right, top, bottom)
  
  no_data <- as.integer(refl_attrs$Data_Ignore_Value)  # define the no data value
  meta_list <- list("wavelengths" = wavelengths, "crs" = crs(paste0("epsg:",h5_epsg)), "raster_ext" = ext, "no_data_value" = no_data)
  h5closeAll() # close all open h5 instances
  
  return(meta_list)
}

band2Raster <- function(h5_file, band, extent, crs, no_data_value){
  site <- h5ls(h5_file)$group[2] # extract the site info
  # read in the raster for the band specified, this will be an array
  refl_array <- h5read(h5_file,paste0(site,"/Reflectance/Reflectance_Data"),index=list(band,NULL,NULL))
  refl_matrix <- (refl_array[1,,]) # convert from array to matrix
  refl_matrix <- t(refl_matrix) # transpose data to fix flipped row and column order
  refl_matrix[refl_matrix == no_data_value] <- NA     # assign data ignore values to NA
  refl_out <- rast(refl_matrix,crs=crs) # write out as a raster
  ext(refl_out) <- extent # assign the extents to the raster
  h5closeAll() # close all open h5 instances
  return(refl_out) # return the terra raster object
}


# get the relevant metadata using the geth5metadata function

h5_meta <- geth5metadata(h5_file)


#not working, lets try this instead
# where to find stuff
hsi.data.dir <- paste0("Y:/shared_data/NEON_AOP_data/MLBS/2022/NEON_refl-surf-",
                       "bidir-ortho-mosaic/NEON.D07.MLBS.DP3.30006.002.2022-09.",
                       "basic.20240530T173638Z.PROVISIONAL/")

# get hsi filenames
hsi.files <- list.files(hsi.data.dir)

# list all of the data types within the hdf5 (should be the same for all files)
h5ls(file = paste0(hsi.data.dir, hsi.files[3]))

# get a list of wavelengths (should be the same for all files)
wavelengths <- h5read(file = paste0(hsi.data.dir, hsi.files[3]),
                      name = paste0(loc, 
                                    "/Reflectance/Metadata/Spectral_Data/Wavelength"))
wave.count <- 1:length(wavelengths)
wave.round <- round(wavelengths,0)
wave.names <- paste0("wave.", wave.round)

# get all bands - a consecutive list of integers from 1:426 (# of bands)

all_bands <- as.list(1:length(h5_meta$wavelengths))

# lapply applies the function `band2Raster` to each element in the list `all_bands`

refl_list <- lapply(all_bands,
                    FUN = band2Raster,
                    h5_file = h5_file,
                    extent = h5_meta$raster_ext,
                    crs = h5_meta$crs,
                    no_data_value = h5_meta$no_data_value)

refl_rast <- rast(refl_list)

rgb <- list(58,34,19)

# lapply applies the function to each element in the RGB list

rgb_list <- lapply(rgb,
                   FUN = band2Raster,
                   h5_file = h5_file,
                   extent = h5_meta$raster_ext,
                   crs = h5_meta$crs,
                   no_data_value = h5_meta$no_data_value)

rgb_rast <- rast(rgb_list)

plotRGB(rgb_rast,stretch='lin',axes=TRUE)

#convert the data frame into a shape file (vector)

tree_loc <- vect(cbind(soilmicrobe.2$adjEasting,
                       soilmicrobe.2$adjNorthing), crs=h5_meta$crs)

plot(tree_loc, col="red", add = T)



