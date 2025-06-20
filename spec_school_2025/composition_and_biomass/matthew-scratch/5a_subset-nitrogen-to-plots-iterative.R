# Define plot IDs of interest
target_plots <- c("MLBS_002", "MLBS_009", "MLBS_075", "MLBS_064", "MLBS_065")

# Initialize list to store cropped rasters
cropped_nitrogen_rasters <- list()

# Loop over each plot ID
for (pid in target_plots) {
  
  # Filter and reproject plot
  plot_sf <- plots_unique %>%
    filter(plotID == pid) %>%
    st_transform(crs = crs(nitrogen_raster))
  
  # Convert to SpatVector
  plot_vect <- vect(plot_sf)
  
  # Buffer to create square extent
  plot_buffer <- buffer(plot_vect, width = 20)
  plot_square <- as.polygons(ext(plot_buffer), crs = crs(nitrogen_raster))
  
  # Crop and mask nitrogen raster
  cropped <- try({
    cropped <- crop(nitrogen_raster_capped, plot_square)
    masked <- mask(cropped, plot_square)
    masked
  }, silent = TRUE)
  
  # Store only if successful
  if (inherits(cropped, "SpatRaster")) {
    cropped_nitrogen_rasters[[pid]] <- cropped
  } else {
    message(sprintf("Skipping %s: raster extraction failed.", pid))
  }
}

# Example plot
plot(cropped_nitrogen_rasters[["MLBS_075"]], main = "Nitrogen % in MLBS_075")
