
# get plots to download only relevant hyperspectral tiles
plots <- st_read("lidar/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plots_V11/All_NEON_TOS_Plot_Centroids_V11.shp")

# Reproject to UTM Zone 17N
plots_utm <- st_transform(plots, crs = 26917)

# Filter and extract center coords
plot_coords <- plots_utm %>%
  filter(plotID %in% paste0("MLBS_", sprintf("%03d", c(6,20)))) %>%
  mutate(
    tile_easting = floor(st_coordinates(.)[,1] / 1000) * 1000,
    tile_northing = floor(st_coordinates(.)[,2] / 1000) * 1000
  ) %>%
  st_drop_geometry() %>%
  distinct(tile_easting, tile_northing)

print(plot_coords)

for (i in 1:nrow(plot_coords)) {
  byTileAOP(
    dpID = "DP3.30006.002",
    site = "MLBS",
    year = "2023",
    easting = plot_coords$tile_easting[i],
    northing = plot_coords$tile_northing[i],
    savepath = wd,
    check.size = TRUE,
    include.provisional=TRUE
  )
}

library(terra)

# List the reflectance .tif files in your directory
tile_files <- list.files(path = paste0(wd,"DP3.30006.002\\neon-aop-provisional-products\\2023\\FullSite\\D07\\2023_MLBS_6\\L3\\Spectrometer\\Reflectance\\"), full.names = TRUE, all.files = FALSE)

# Load each tile into a SpatRaster
tile_stack_list <- lapply(tile_files, rast)

# Mosaic all tiles into one raster
hs_mosaic <- do.call(mosaic, tile_stack_list)

# Check the result
print(hs_mosaic)
plot(hs_mosaic[[1]], main = "First Band of Hyperspectral Mosaic")

writeRaster(hs_mosaic, filename = "MLBS_2023_HS_Mosaic.tif", overwrite = TRUE)

  