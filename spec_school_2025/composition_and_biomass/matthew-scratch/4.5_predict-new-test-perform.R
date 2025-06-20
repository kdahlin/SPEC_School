nitrogen_coef <- read.csv("C:\\Users\\mille\\Documents\\SpecSchool2025\\gitdemo\\SPEC_School\\spec_school_2025\\composition_and_biomass\\Nitrogen.csv", header = TRUE)
nitrogen_coef <- as.matrix(nitrogen_coef[, -1])  # remove first column if it's wavelength or ID

# Define path to the second HDF5 tile
h5_file2 <- paste0(wd,"DP3.30006.002/neon-aop-provisional-products/2023/FullSite/D07/2023_MLBS_6/L3/Spectrometer/Reflectance/NEON_D07_MLBS_DP3_542000_4136000_bidirectional_reflectance.h5")

# EPSG, extent, nodata, etc.
h5EPSG2 <- h5read(h5_file2, "/MLBS/Reflectance/Metadata/Coordinate_System/EPSG Code")
h5CRS2 <- crs(paste0("+init=epsg:", h5EPSG2))
reflInfo2 <- h5readAttributes(h5_file2, "/MLBS/Reflectance/Reflectance_Data")

xMin2 <- reflInfo2$Spatial_Extent_meters[1]
xMax2 <- reflInfo2$Spatial_Extent_meters[2]
yMin2 <- reflInfo2$Spatial_Extent_meters[3]
yMax2 <- reflInfo2$Spatial_Extent_meters[4]
rastExt2 <- ext(xMin2, xMax2, yMin2, yMax2)
h5NoDataValue2 <- as.integer(reflInfo2$Data_Ignore_Value)

# Read bands and create SpatRaster stack
full_hs_stack2 <- lapply(all_bands, FUN = band2Raster,
                         file = h5_file,
                         noDataValue = h5NoDataValue2,
                         extent = rastExt2,
                         CRS = h5CRS2)

hsStack2 <- rast(full_hs_stack2)
hsStack2 <- hsStack2 / as.integer(reflInfo2$Scale_Factor)

# Match bands to model structure
hsStack2_subset <- hsStack2[[1:423]]
names(hsStack2_subset) <- colnames(plsr_data_filtered)[-1]  # predictor names

# Extract matrix and predict
hs_matrix2 <- terra::as.matrix(hsStack2_subset)
non_na_rows2 <- complete.cases(hs_matrix2)
hs_matrix_clean2 <- hs_matrix2[non_na_rows2, ]

predicted_nitrogen2 <- predict(plsr_model, newdata = hs_matrix_clean2, ncomp = 2)
predicted_nitrogen2 <- as.vector(predicted_nitrogen2)

# Rasterize output
nitrogen_raster2 <- rast(hsStack2_subset, nlyr = 1)
values(nitrogen_raster2) <- NA
values(nitrogen_raster2)[non_na_rows2] <- predicted_nitrogen2

# Cap values
nitrogen_raster2_capped <- clamp(nitrogen_raster2, lower = 0, upper = 5)

# Resample to 40m
nitrogen_agg40_2 <- aggregate(nitrogen_raster2_capped, fact = 40, fun = mean, na.rm = TRUE)

# Filter field plots that intersect tile 2
plots_tile2 <- plots_unique %>%
  st_transform(crs = h5CRS2) %>%
  filter(st_intersects(., st_as_sf(as.polygons(rastExt2, crs = h5CRS2)), sparse = FALSE)) %>%
  filter(plotID %in% fc_filtered$plotID)

# Join to nitrogen field values
fc_tile2 <- fc_filtered %>%
  filter(plotID %in% plots_tile2$plotID) %>%
  group_by(plotID) %>%
  slice(1) %>%
  ungroup()

# Turn to vector object for extract
plots_tile2_vect <- vect(plots_tile2)

# Extract and bind plot attributes
pred_vals2 <- terra::extract(nitrogen_agg40_2, plots_tile2_vect, bind = TRUE)

# Convert to data frame for joining
pred_vals2_df <- as.data.frame(pred_vals2)

# Join on plotID
evaluation_df2 <- left_join(fc_tile2, pred_vals2_df, by = c("plotID" = "plotID"))

# Simple scatter plot
ggplot(evaluation_df2, aes(x = lyr.1, y = nitrogenPercent)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Predicted %N (40m)", y = "Observed %N", title = "Model Evaluation: Tile 2")

# Calculate metrics
library(Metrics)
mae_val2 <- mae(evaluation_df2$nitrogenPercent, evaluation_df2$lyr.1)
rmse_val2 <- rmse(evaluation_df2$nitrogenPercent, evaluation_df2$lyr.1)
rsq_val2 <- cor(evaluation_df2$nitrogenPercent, evaluation_df2$lyr.1)^2

print(c(MAE = mae_val2, RMSE = rmse_val2, R2 = rsq_val2))

#########################
library(terra)
library(hdf5r)
library(dplyr)
library(ggplot2)
library(Metrics)

# ----- Setup and File Paths -----
wd <- "/your/working/directory/"  # CHANGE to your path if needed

h5_file2 <- paste0(wd, "DP3.30006.002/neon-aop-provisional-products/2023/FullSite/D07/2023_MLBS_6/L3/Spectrometer/Reflectance/NEON_D07_MLBS_DP3_541000_4136000_bidirectional_reflectance.h5")

# Read EPSG and CRS
h5EPSG2 <- h5read(h5_file2, "/MLBS/Reflectance/Metadata/Coordinate_System/EPSG Code")
h5CRS2 <- crs(paste0("+init=epsg:", h5EPSG2))

# Read extent and nodata value
reflInfo2 <- h5readAttributes(h5_file2, "/MLBS/Reflectance/Reflectance_Data")
xMin2 <- reflInfo2$Spatial_Extent_meters[1]
xMax2 <- reflInfo2$Spatial_Extent_meters[2]
yMin2 <- reflInfo2$Spatial_Extent_meters[3]
yMax2 <- reflInfo2$Spatial_Extent_meters[4]
rastExt2 <- ext(xMin2, xMax2, yMin2, yMax2)
h5NoDataValue2 <- as.integer(reflInfo2$Data_Ignore_Value)

# Define band range (adjust if needed)
all_bands <- 1:426  # typically 426 bands in NEON HSI

band2Raster <- function(file, band, noDataValue, extent, CRS) {
  # Read a single band as a 3D array with shape [y, x, 1]
  data_array <- h5read(file, "/MLBS/Reflectance/Reflectance_Data",
                       index = list(NULL, NULL, band))
  
  # Convert to 2D matrix if necessary
  if (length(dim(data_array)) == 3) {
    data_mat <- data_array[, , 1]
  } else if (length(dim(data_array)) == 2) {
    data_mat <- data_array
  } else {
    stop("Unexpected dimensions for HDF5 band data.")
  }
  
  # Transpose to match raster orientation (x by y)
  data_mat <- t(data_mat)
  
  # Replace noData values
  data_mat[data_mat == noDataValue] <- NA
  
  # Create raster
  r <- rast(data_mat, extent = extent, crs = CRS)
  return(r)
}



full_hs_stack2 <- lapply(all_bands, FUN = function(b) band2Raster(h5_file, b, h5NoDataValue2, rastExt2, h5CRS2))
hsStack2 <- rast(full_hs_stack2)
hsStack2 <- hsStack2 / as.integer(reflInfo2$Scale_Factor)

# ----- Match Band Names (optional if you need to rename layers) -----
# names(hsStack2) <- paste0("wl_", 1:nlyr(hsStack2))  # or from another dataset if needed

# ----- Convert to Matrix -----
hs_matrix2 <- terra::as.matrix(hsStack2)
non_na_rows2 <- complete.cases(hs_matrix2)
hs_matrix_clean2 <- hs_matrix2[non_na_rows2, ]

# ----- Load PLSR Coefficients for Nitrogen -----
nitrogen_coef <- read.csv("")
nitrogen_coef <- as.matrix(nitrogen_coef[, -1])  # drop first column (assumed to be wavelength or ID)

# ----- Predict Nitrogen Using 200 PLSR Models -----
# nitrogen_coef is 200 rows (models) × 426 columns (bands)
# hs_matrix_clean2 is [pixels × 426]

# Multiply hyperspectral matrix by each model (result: [pixels × 200])
nitrogen_pred_matrix <- hs_matrix_clean2 %*% t(nitrogen_coef)  # result: 1,000,000 × 200

# Average across models to get final nitrogen prediction per pixel
predicted_nitrogen2 <- rowMeans(nitrogen_pred_matrix, na.rm = TRUE)

# Optional: calculate per-pixel standard deviation for uncertainty map
predicted_nitrogen_sd2 <- apply(nitrogen_pred_matrix, 1, sd, na.rm = TRUE)


# ----- Rebuild Raster with Predictions -----
nitrogen_raster2 <- rast(hsStack2[[1]])  # use one layer to get template
values(nitrogen_raster2) <- NA
values(nitrogen_raster2)[non_na_rows2] <- predicted_nitrogen2

# ----- Cap Predicted Values -----
nitrogen_raster2_capped <- clamp(nitrogen_raster2, lower = 0, upper = 5)

# ----- Resample to 40m Resolution -----
nitrogen_agg40_2 <- aggregate(nitrogen_raster2_capped, fact = 40, fun = mean, na.rm = TRUE)

# 1. Plot the raw prediction raster before clamping
plot(nitrogen_raster2,
     main = "Predicted %N (Raw, Before Clamping)",
     col = hcl.colors(100, "Viridis"),
     zlim = range(predicted_nitrogen2, na.rm = TRUE))

# 2. Plot the raster after clamping but before resampling
plot(nitrogen_raster2_capped,
     main = "Predicted %N (Clamped to 0–5%, Pre-Resample)",
     col = hcl.colors(100, "Viridis"),
     zlim = c(0, 5))

# 3. Plot the final raster after 40m resampling
plot(nitrogen_agg40_2,
     main = "Predicted %N (40m Aggregated)",
     col = hcl.colors(100, "Viridis"),
     zlim = c(0, 5))

# ----- Filter Field Plots That Intersect the Tile -----
plots_tile2 <- plots_unique %>%
  st_transform(crs = h5CRS2) %>%
  filter(st_intersects(., st_as_sf(as.polygons(rastExt2, crs = h5CRS2)), sparse = FALSE)) %>%
  filter(plotID %in% fc_filtered$plotID)

# ----- Get Field Plot Data -----
fc_tile2 <- fc_filtered %>%
  filter(plotID %in% plots_tile2$plotID) %>%
  group_by(plotID) %>%
  slice(1) %>%
  ungroup()

# ----- Extract Raster Values at Plot Locations -----
plots_tile2_vect <- vect(plots_tile2)
pred_vals2 <- terra::extract(nitrogen_agg40_2, plots_tile2_vect, bind = TRUE)
pred_vals2_df <- as.data.frame(pred_vals2)

# ----- Join Predictions with Observed Field Data -----
evaluation_df2 <- left_join(fc_tile2, pred_vals2_df, by = c("plotID" = "plotID"))

# ----- Evaluate Model -----
ggplot(evaluation_df2, aes(x = lyr.1, y = nitrogenPercent)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Predicted %N (40m)", y = "Observed %N", title = "Model Evaluation: Tile 2")

# ----- Metrics -----
mae_val2 <- mae(evaluation_df2$nitrogenPercent, evaluation_df2$lyr.1)
rmse_val2 <- rmse(evaluation_df2$nitrogenPercent, evaluation_df2$lyr.1)
rsq_val2 <- cor(evaluation_df2$nitrogenPercent, evaluation_df2$lyr.1)^2

print(c(MAE = mae_val2, RMSE = rmse_val2, R2 = rsq_val2))
