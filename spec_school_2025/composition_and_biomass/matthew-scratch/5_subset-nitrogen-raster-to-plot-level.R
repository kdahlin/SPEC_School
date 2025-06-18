# draw 40 x 40 plot with buffer from centroid
# Filter for just MLBS_002
plot_002 <- plots_unique %>% 
  filter(plotID == "MLBS_002") %>% 
  st_transform(crs = crs(nitrogen_raster))  # match CRS if needed

# Convert to SpatVector
plot_002_vect <- vect(plot_002)

# Create a 20m buffer (circle), then convert to square extent
plot_002_buffer <- buffer(plot_002_vect, width = 20)

# Or create square directly from extent of buffered point
plot_002_square <- as.polygons(ext(plot_002_buffer), crs = crs(nitrogen_raster))

# Crop and mask the nitrogen raster to this square
nitrogen_002 <- crop(nitrogen_raster_capped, plot_002_square)
nitrogen_002 <- mask(nitrogen_002, plot_002_square)


# Plot it
plot(nitrogen_002, main = "Nitrogen % in MLBS_002")
