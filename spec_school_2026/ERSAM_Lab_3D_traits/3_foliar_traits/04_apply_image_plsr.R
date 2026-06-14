# This script is for converting spectrometer measurements into a data frame 
# Author: MS, KMD
# Date: 06/14/2026

# load necessary packages 
library(prospectr)
library(terra)

# -----------------------------------
# USER-DEFINED VARIABLES
# -----------------------------------

# read in config file with site info
source("0_config_files/config_mlbs_kmd_20260608.R")  

# call the ERSAM functions file - this should be inside your Rproj
source("./0_R_functions/ERSAM_Lab/spectra_preprocessing.R")
source("./0_R_functions/ERSAM_Lab/matrix2rast.R")

# set file path for reading hyperspectral image files 
dir.path <- file.path(root, "shared_data", "NEON_AOP_data", site, year, 
                      "L3", "Spectrometer", "Reflectance")

# set out file path for saving output file 
out.path <- file.path(root, "shared_data", "NEON_proc_data", site, year,
                      paste0("3-040_ImagePLSR_", wd))

# make this directory if it doesn't already exist
if (!dir.exists(out.path)) {
  dir.create(out.path, recursive = TRUE)}
  

### ========================= STEP 1: LOAD THE COEFFICIENTS and REFERENCE 
### WAVELENGTHS FOR THE PLSR MODEL ================

Coefficient_folder <- file.path(root, "shared_data", "NEON_field_data", 
                                "Wang_etal_2020_coefficients")

LMA.coef.file <- "LMA.csv"
Chl.coef.file <- "Chlorophylls_mass.csv"
EWT.coef.file <- "EWT.csv"

LMA.coef <- read.csv(sprintf("%s/%s", Coefficient_folder, LMA.coef.file))
Chl.coef <- read.csv(sprintf("%s/%s", Coefficient_folder, Chl.coef.file))
EWT.coef <- read.csv(sprintf("%s/%s", Coefficient_folder, EWT.coef.file))

LMA.coef <- t(LMA.coef[,-1])
Chl.coef <- t(Chl.coef[,-1])
EWT.coef <- t(EWT.coef[,-1])

# reference wavelength for the PLSR model
wavelength_file <- paste0(root, "/meicheng/NEON_hsi_wl_update.csv")
wavelength_matrix <- read.csv(wavelength_file)

referenceID <- "TALL_2017"
reference_colID <- which(names(wavelength_matrix) == referenceID)
ref_wl <-  wavelength_matrix[,reference_colID]

#### NEED TO UPDATE to read hdf5 ####
# wavelength for the images we are processing
siteID <- "UMBS"
siteYear <- "2019"

site_colID <- which(names(wavelength_matrix) == sprintf("%s_%s", 
                                                        siteID, siteYear))
site_wl <- wavelength_matrix[,site_colID]

### ================ STEP 2: LOAD & PREPROCESS IMAGES ==========================

# load the mosaic images

footprint_folder <- tiff.dir
footprint_raster_filename <- sprintf("%s_%s_HSI_mosaic_raw.tif", siteID, 
                                     siteYear)
footprint_raster <- rast(sprintf("%s/%s", footprint_folder, 
                                 footprint_raster_filename))

img_size = dim(footprint_raster)
nrow <- img_size[1]
ncol <- img_size[2]
nband <- img_size[3]
npixel <- nrow * ncol

# from raster to matrix
ImgMatrix <- as.matrix(footprint_raster, wide = FALSE)

# wavelength re-sampling to the ref_wl
refl_resample <- prospectr::resample(ImgMatrix, site_wl, ref_wl, 
                                     interpol = "linear")

# plot re-sampled spectra
for (i in 1:dim(refl_resample)[1]){
  if (i == 1){
    plot(refl_resample[i,])
  }else{
    lines(refl_resample[i,], add = TRUE)
  }
}

# Export the resampled image reflectance
filename <- sprintf("Resample_Footprint_Img_%s.csv", siteID)
write.csv(refl_resample, file = sprintf("%s/%s", work.dir, filename))

# refl_resample <- read.csv(sprintf("%s/%s", work.dir, filename))

# pre-processing - remove noisy bands, shade (torefl) threshold, smoothing, 
# continuum removal, brightness normalization
keep.wl <- c((418.59:1335.04), (1460.23:1770.72), (1986.06:2396.71))
keep.bandID <- array(data = NA, dim = length(keep.wl))

# find the index of target wavelength
for (b in 1:length(keep.wl)){
  keep.bandID[b] <- which(abs(site_wl - keep.wl[b]) == min(abs(site_wl - keep.wl[b])))
}

keep_index <- unique(keep.bandID)

# keep_index <- c((8:191), (216:278), (321:403))

totref_threshold <- 40
nir_wl <- 865
red_wl <- 655
ndvi_threshold <- 0.6
brightness_wl <- 804
brightness_threshold <- 0.06

processed_spectra <- spectra_preprocessing(refl_resample, ref_wl, keep_index, 
                                           totref_threshold, smooth = "NA",
                                           nir_wl, red_wl, ndvi_threshold, 
                                           brightness_wl, brightness_threshold, 
                                           normalization = TRUE)
bn_spectra <- processed_spectra$bn_spectra
bn_spectra <- as.matrix(cbind(matrix(data = 1, nrow = dim(bn_spectra)[1], 
                                     ncol = 1), bn_spectra))

# save post-processed image spectrum (filtered and brightness normalized)
filename <- "BN_ImageRefl.csv"
write.csv(bn_spectra, file = sprintf("%s/%s", work.dir, filename))

### ==== STEP 3: APPLY THE COEFFICIENTS TO THE PROCESSED IMAGES ============

LMA.matrix <- as.matrix(bn_spectra) %*% LMA.coef
Chl.matrix <- as.matrix(bn_spectra) %*% Chl.coef
Car.matrix <- as.matrix(bn_spectra) %*% Car.coef
EWT.matrix <- as.matrix(bn_spectra) %*% EWT.coef

LMA <- matrix2rast(LMA.matrix, nrow, ncol)
Chl <- matrix2rast(Chl.matrix, nrow, ncol)
Car <- matrix2rast(Car.matrix, nrow, ncol)
EWT <- matrix2rast(EWT.matrix, nrow, ncol)

LMA_stat <- quantile(LMA$avg.vector, c(0.01, 0.25, 0.75, 0.99), na.rm = TRUE)
plot(LMA$avg.layer, zlim = c(LMA_lower, LMA_upper), 
     main = "LMA trait map (average across 200 PLSR models)")
# LMA_upper <- LMA_stat[4]
# LMA_lower <- max(0, LMA_stat[1])
# LMA$avg.clip <- LMA$avg.layer
# LMA$avg.clip[LMA$avg.clip < LMA_lower | LMA$avg.clip > LMA_upper] <- NA

Chl_stat <- quantile(Chl$avg.vector, c(0.01, 0.25, 0.75, 0.99), na.rm = TRUE)
plot(Chl$avg.layer, zlim = c(Chl_lower, Chl_upper), 
     main = "Chlorophyll trait map (average across 200 PLSR models)")
# Chl_upper <- Chl_stat[4]
# Chl_lower <- max(0, Chl_stat[1])
# Chl$avg.clip <- Chl$avg.layer
# Chl$avg.clip[Chl$avg.clip < Chl_lower | Chl$avg.clip > Chl_upper] <- NA

Car_stat <- quantile(Car$avg.vector, c(0.01, 0.25, 0.75, 0.99), na.rm = TRUE)
plot(Car$avg.layer, zlim = c(Car_lower, Car_upper), 
     main = "Carotenoid trait map (average across 200 PLSR models)")
# Car_upper <- Car_stat[4]
# Car_lower <- max(0, Car_stat[1])
# Car$avg.clip <- Car$avg.layer
# Car$avg.clip[Car$avg.clip < Car_lower | Car$avg.clip > Car_upper] <- NA

EWT_stat <- quantile(EWT$avg.vector, c(0.01, 0.25, 0.75, 0.99), na.rm = TRUE)
plot(EWT$avg.layer, zlim = c(EWT_lower, EWT_upper), 
     main = "Equivalent Water Thickness (EWT) trait map (average across 200 PLSR models)")
# EWT_upper <- EWT_stat[4]
# EWT_lower <- max(0, EWT_stat[1])
# EWT$avg.clip <- EWT$avg.layer
# EWT$avg.clip[EWT$avg.clip < EWT_lower | EWT$avg.clip > EWT_upper] <- NA

# ### ======================== STEP 4: Export Trait Maps as TIFF files ==================================

# # footprint size
radius <- 2000

LMA.avg.filename <- sprintf("%s/%s_LMA_avg_footprint_%d.tif", work.dir, siteID, 
                            radius)
Chl.avg.filename <- sprintf("%s/%s_Chl_avg_footprint_%d.tif", work.dir, siteID, 
                            radius)
Car.avg.filename <- sprintf("%s/%s_Car_avg_footprint_%d.tif", work.dir, siteID, 
                            radius)
EWT.avg.filename <- sprintf("%s/%s_EWT_avg_footprint_%d.tif", work.dir, siteID, 
                            radius)

writeRaster(LMA$avg.layer, LMA.avg.filename)
writeRaster(Chl$avg.layer, Chl.avg.filename)
writeRaster(Car$avg.layer, Car.avg.filename)
writeRaster(EWT$avg.layer, EWT.avg.filename)

LMA.std.filename <- sprintf("%s/%s_LMA_std_footprint_%d.tif", work.dir, siteID, 
                            radius)
Chl.std.filename <- sprintf("%s/%s_Chl_std_footprint_%d.tif", work.dir, siteID, 
                            radius)
Car.std.filename <- sprintf("%s/%s_Car_std_footprint_%d.tif", work.dir, siteID, 
                            radius)
EWT.std.filename <- sprintf("%s/%s_EWT_std_footprint_%d.tif", work.dir, siteID, 
                            radius)

writeRaster(LMA$std.layer, LMA.std.filename)
writeRaster(Chl$std.layer, Chl.std.filename)
writeRaster(Car$std.layer, Car.std.filename)
writeRaster(EWT$std.layer, EWT.std.filename)

##### ================ STEP5: calculate and export CV ======================== 

LMA.cv <- LMA$avg.layer/LMA$std.layer
Chl.cv <- Chl$avg.layer/Chl$std.layer
Car.cv <- Car$avg.layer/Car$std.layer
EWT.cv <- EWT$avg.layer/EWT$std.layer

LMA.cv.filename <- sprintf("%s/%S_LMA_cv_footprint_%d.tif", work.dir, siteID, 
                           radius)
Chl.cv.filename <- sprintf("%s/%S_Chl_cv_footprint_%d.tif", work.dir, siteID, 
                           radius)
Car.cv.filename <- sprintf("%s/%S_Car_cv_footprint_%d.tif", work.dir, siteID, 
                           radius)
EWT.cv.filename <- sprintf("%s/%S_EWT_cv_footprint_%d.tif", work.dir, siteID, 
                           radius)

writeRaster(LMA$cv, LMA.cv.filename)
writeRaster(Chl$cv, Chl.cv.filename)
writeRaster(Car$cv, Car.cv.filename)
writeRaster(EWT$cv, EWT.cv.filename)
