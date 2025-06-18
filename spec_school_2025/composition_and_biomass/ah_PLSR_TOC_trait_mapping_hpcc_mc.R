#####################  This script applies the PLSR coefficients to the whole images (footprint area) ##############  

library("prospectr")
library("terra")

### ========================= Function Definition =========================

# spectral matrix (n_pixel * n_waveband) pre-processing 
spectra_preprocessing <- function(raw_spectra, full_wl, keep_index, totref_threshold, smooth = "savitzy-golay",
                                  nir_wl, red_wl, ndvi_threshold, brightness_wl, brightness_threshold, normalization = TRUE){
  
  # set the column names of the spectra data for filtering
  colnames(raw_spectra) <- paste0("wave.", round(full_wl))
  
  # kept wavelength: band index -> actual wavelength
  keep.wl <- full_wl[keep_index]
  
  # noisy band removal
  keep.colname <- paste0("wave.", round(keep.wl))
  
  # set the noisy bands as 0 values
  raw_spectra[,which(! colnames(raw_spectra) %in% keep.colname)] <- 0
  
  # shade pixel removal
  torefl <- rowSums(raw_spectra, na.rm = TRUE)
  spectra <- cbind(torefl, raw_spectra)
  spectra_clean <- spectra
  spectra_clean[spectra[,1] < totref_threshold,] <- NaN
  
  # spectra smoothing
  if (smooth == "savitzy-golay"){
    
    # filter No.1: Savitzky-Golay filter - not using the first column (total-reflecance)
    
    TOC.spectra <- as.matrix(spectra_clean[, 2:ncol(spectra_clean)])
    filter_refl <- matrix(nrow = dim(TOC.spectra)[1], ncol = dim(TOC.spectra)[2])
    
    # p - fiter order, n - filter length (odd), m - return the m-th derivative of the filter coefficients, ts - time scaling factor
    filter_refl <- sgolay::sgolayfilt(TOC.spectra, p = 2, n = 7, m = 0, ts = 1, rowwise = TRUE)
    HSI.smoothSG <- data.frame(filter_refl)
    
  }else{
    
    # return reflectance without smoothing
    
    TOC.spectra <- as.matrix(spectra_clean[, 2:ncol(spectra_clean)])
    filter_refl <- TOC.spectra
    HSI.smoothSG <- data.frame(TOC.spectra)
  }
  
  # pixel filtering (shade/non-vegetation removal)
  brightness_index <- which(abs(full_wl - brightness_wl) == min(abs(full_wl - brightness_wl)))
  if (brightness_index %in% keep_index){
    rm.index.1 <- HSI.smoothSG[,brightness_index] < brightness_threshold
  }else{
    print("Current brightness wavelength not available")
    return(NA)
  }
  
  red_index <-  which(abs(full_wl - red_wl) == min(abs(full_wl - red_wl)))
  nir_index <-  which(abs(full_wl - nir_wl) == min(abs(full_wl - nir_wl)))
  
  if (red_index %in% keep_index & nir_index %in% keep_index){
    ndvi <- (HSI.smoothSG[, nir_index] - HSI.smoothSG[,red_index])/(HSI.smoothSG[, nir_index] + HSI.smoothSG[,red_index])
    rm.index.2 <- ndvi < ndvi_threshold
  }else{
    print("Current NDVI bands not available")
    return(NA)
  }
  
  rm.index <- rm.index.1 | rm.index.2
  rm.index[is.na(rm.index)] <- TRUE
  
  # continuum removal （not implemented)
  
  # brightness normalization (matrix)
  if (normalization == TRUE) {
    
    vector_norm <- sqrt(apply(filter_refl^2, 1, sum, na.rm = TRUE))
    bn_spectra <- filter_refl /vector_norm
    
    # confirm that the noisy bands are set as 0 values
    bn_spectra[,which(! colnames(raw_spectra) %in% keep.colname)] <- 0
    colnames(bn_spectra) <- colnames(raw_spectra)
    
  }else{
    bn_spectra <- NA
    vector_norm <- NA
  }
  
  return(list(bn_spectra = bn_spectra, vector_norm = vector_norm, 
              rm_index = rm.index, brightness_index = brightness_index,
              nir_index = nir_index, red_index = red_index, 
              spectra_smoothSG = HSI.smoothSG, ndvi = ndvi))
  
}


# from matrix (n_pixel * n_model) to average/std trait map
matrix2rast <- function(matrix, nrow, ncol){
  
  avg_vector <- rowMeans(matrix, na.rm = TRUE)
  std_vector <- apply(matrix, 1, sd, na.rm = TRUE)
  
  avg_2D <- matrix(avg_vector, nrow, ncol, byrow = TRUE)
  avg_2D_layer <- rast(avg_2D)
  
  std_2D <- matrix(std_vector, nrow, ncol, byrow = TRUE)
  std_2D_layer <- rast(std_2D)
  
  return(list(avg.layer = avg_2D_layer, std.layer = std_2D_layer, avg.vector = avg_vector, std.vector = std_vector))
}

### ========================= STEP 1: LOAD THE COEFFICIENTS and REFERENCE WAVELENGTHS FOR THE PLSR MODEL ================

# export folder
work.dir <- "/mnt/ufs18/rs-016/ersamlab/meicheng/"

hpcc.data.dir <- "/mnt/ufs18/home-116/shenmeic/DataArchive"

Coefficient_folder <- sprintf("%s/FieldData/FoliageSample/toc_sample_info/Coefficients/", hpcc.data.dir)

LMA.coef.file <- "LMA_Wang_2020.csv"
Chl.coef.file <- "Chlorophylls_area_Wang_2020.csv"
Car.coef.file <- "Carotenoids_area_Wang_2020.csv"
EWT.coef.file <- "EWT_Wang_2020.csv"

LMA.coef <- read.csv(sprintf("%s/%s", Coefficient_folder, LMA.coef.file))
Chl.coef <- read.csv(sprintf("%s/%s", Coefficient_folder, Chl.coef.file))
Car.coef <- read.csv(sprintf("%s/%s", Coefficient_folder, Car.coef.file))
EWT.coef <- read.csv(sprintf("%s/%s", Coefficient_folder, EWT.coef.file))

LMA.coef <- t(LMA.coef[,-1])
Chl.coef <- t(Chl.coef[,-1])
Car.coef <- t(Car.coef[,-1])
EWT.coef <- t(EWT.coef[,-1])

# reference wavelength for the PLSR model


wavelength_file <- "/mnt/ufs18/rs-016/ersamlab/shared_data/NEON_AOP_data/NEON_hsi_wavelength.csv"
wavelength_matrix <- read.csv(wavelength_file)

referenceID <- "TALL_2017"
reference_colID <- which(names(wavelength_matrix) == referenceID)
ref_wl <-  wavelength_matrix[,reference_colID]

# wavelength for the images we are processing

siteID <- "UMBS_2019"
site_colID <- which(names(wavelength_matrix) == siteID)
site_wl <- wavelength_matrix[,site_colID]

### ========================= STEP 2: LOAD & PREPROCESS IMAGES ===================================

# load the mosaic images

footprint_folder <- sprintf("%s/AirborneData/HSI/UMBS/footprint_raster", hpcc.data.dir)
footprint_raster_filename <- "UMBS_2019_raw_HSI_mosaic_footprint.tif"
footprint_raster <- rast(sprintf("%s/%s", footprint_folder, footprint_raster_filename))
img_size = dim(footprint_raster)
nrow <- img_size[1]
ncol <- img_size[2]
nband <- img_size[3]
npixel <- nrow * ncol

# from raster to matrix
ImgMatrix <- as.matrix(footprint_raster, wide = FALSE)

# wavelength re-sampling to the ref_wl
refl_resample <- prospectr::resample(ImgMatrix, site_wl, ref_wl, interpol = "linear")

# Export the resampled image reflectance
# filename <- sprintf("Resample_Footprint_Img_%s.csv", siteID)
# write.csv(refl_resample, file = sprintf("%s/%s", work.dir, filename))

# refl_resample <- read.csv(sprintf("%s/%s", work.dir, filename))

# pre-processing - remove noisy bands, shade (torefl) threshold, smoothing, continuum removal, brightness normalization
keep_index <- c((8:191), (216:278), (321:403))

totref_threshold <- 40
nir_wl <- 865
red_wl <- 655
ndvi_threshold <- 0.6
brightness_wl <- 804
brightness_threshold <- 0.15

processed_spectra <- spectra_preprocessing(refl_resample, ref_wl, keep_index, totref_threshold, smooth = "NA",
                                           nir_wl, red_wl, ndvi_threshold, brightness_wl, brightness_threshold, normalization = TRUE)
bn_spectra <- processed_spectra$bn_spectra
bn_spectra <- as.matrix(cbind(matrix(data = 1, nrow = dim(bn_spectra)[1], ncol = 1), bn_spectra))

# save post-processed image spectrum (smoothed and brightness normalized)
# filename <- "BN_ImageRefl.csv"
# write.csv(bn_spectra, file = sprintf("%s/%s", work.dir, filename))

### ======================== STEP 3: APPLY THE COEFFICIENTS TO THE PROCESSED IMAGES ===================

LMA.matrix <- as.matrix(bn_spectra) %*% LMA.coef
Chl.matrix <- as.matrix(bn_spectra) %*% Chl.coef
Car.matrix <- as.matrix(bn_spectra) %*% Car.coef
EWT.matrix <- as.matrix(bn_spectra) %*% EWT.coef

LMA <- matrix2rast(LMA.matrix, nrow, ncol)
Chl <- matrix2rast(Chl.matrix, nrow, ncol)
Car <- matrix2rast(Car.matrix, nrow, ncol)
EWT <- matrix2rast(EWT.matrix, nrow, ncol)

LMA_stat <- quantile(LMA$avg.vector, c(0.01, 0.25, 0.75, 0.99), na.rm = TRUE)
LMA_upper <- LMA_stat[4]
LMA_lower <- 0
LMA$avg.clip <- LMA$avg.layer
LMA$avg.clip[LMA$avg.clip <= LMA_lower | LMA$avg.clip > LMA_upper] <- NA
plot(LMA$avg.clip, zlim = c(LMA_lower, LMA_upper), main = "LMA trait map (average across 200 PLSR models)")

Chl_stat <- quantile(Chl$avg.vector, c(0.01, 0.25, 0.75, 0.99), na.rm = TRUE)
Chl_upper <- Chl_stat[4]
Chl_lower <- 0
Chl$avg.clip <- Chl$avg.layer
Chl$avg.clip[Chl$avg.clip <= Chl_lower | Chl$avg.clip > Chl_upper] <- NA
plot(Chl$avg.clip, zlim = c(Chl_lower, Chl_upper), main = "Chlorophyll trait map (average across 200 PLSR models)")

Car_stat <- quantile(Car$avg.vector, c(0.01, 0.25, 0.75, 0.99), na.rm = TRUE)
Car_upper <- Car_stat[4]
Car_lower <- 0
Car$avg.clip <- Car$avg.layer
Car$avg.clip[Car$avg.clip <= Car_lower | Car$avg.clip > Car_upper] <- NA
plot(Car$avg.clip, zlim = c(Car_lower, Car_upper), main = "Carotenoid trait map (average across 200 PLSR models)")

EWT_stat <- quantile(EWT$avg.vector, c(0.01, 0.25, 0.75, 0.99), na.rm = TRUE)
EWT_upper <- EWT_stat[4]
EWT_lower <- 0
EWT$avg.clip <- EWT$avg.layer
EWT$avg.clip[EWT$avg.clip <= EWT_lower | EWT$avg.clip > EWT_upper] <- NA
plot(EWT$avg.clip, zlim = c(EWT_lower, EWT_upper), main = "Equivalent Water Thickness (EWT) trait map (average across 200 PLSR models)")

# 
# ### ======================== STEP 4: Export Trait Maps as TIFF files (Footprint Radius: 1500 meter) ==================================
# 
# # footprint size
radius <- 1500

LMA.avg.filename <- sprintf("%s/%s_LMA_avg_footprint_%d.tif", work.dir, siteID, radius)
Chl.avg.filename <- sprintf("%s/%s_Chl_avg_footprint_%d.tif", work.dir, siteID, radius)
Car.avg.filename <- sprintf("%s/%s_Car_avg_footprint_%d.tif", work.dir, siteID, radius)
EWT.avg.filename <- sprintf("%s/%s_EWT_avg_footprint_%d.tif", work.dir, siteID, radius)

writeRaster(LMA$avg.layer, LMA.avg.filename)
writeRaster(Chl$avg.layer, Chl.avg.filename)
writeRaster(Car$avg.layer, Car.avg.filename)
writeRaster(EWT$avg.layer, EWT.avg.filename)

LMA.std.filename <- sprintf("%s/%s_LMA_std_footprint_%d.tif", work.dir, siteID, radius)
Chl.std.filename <- sprintf("%s/%s_Chl_std_footprint_%d.tif", work.dir, siteID, radius)
Car.std.filename <- sprintf("%s/%s_Car_std_footprint_%d.tif", work.dir, siteID, radius)
EWT.std.filename <- sprintf("%s/%s_EWT_std_footprint_%d.tif", work.dir, siteID, radius)

writeRaster(LMA$std.layer, LMA.std.filename)
writeRaster(Chl$std.layer, Chl.std.filename)
writeRaster(Car$std.layer, Car.std.filename)
writeRaster(EWT$std.layer, EWT.std.filename)

