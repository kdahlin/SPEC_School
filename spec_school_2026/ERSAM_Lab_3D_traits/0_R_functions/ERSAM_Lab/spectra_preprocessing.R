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
