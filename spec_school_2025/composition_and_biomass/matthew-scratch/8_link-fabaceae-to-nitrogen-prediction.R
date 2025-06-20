library(terra)

# plots mlbs 061 064 067

# Assume nitrogen_raster is your SpatRaster
# nitrogen_raster <- rast("path_to_your_raster.tif")

# Create vector object for plots
plot_vect <- vect(fabaceae_summary, geom = c("easting", "northing"), crs = "EPSG:32617")  # Adjust UTM zone if needed

# Extract canopy %N at plot locations
plot_N_values <- terra::extract(nitrogen_raster, plot_vect)
fabaceae_summary$canopy_percent_N <- plot_N_values[, 2]  # column 1 is ID

# Plot relationship - lol
ggplot(fabaceae_summary, aes(x = fabaceae_rel_abundance, y = canopy_percent_N)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Relative Abundance of Fabaceae",
       y = "Predicted Canopy % Nitrogen",
       title = "Nitrogen-Fixing Plant Presence vs. Canopy %N")
