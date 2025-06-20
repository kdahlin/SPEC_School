### a little help from my friend chatgpt
# match up stacked raster to model data
terra::nlyr(hsStack)
length(colnames(plsr_data_filtered)) - 1  # subtract 1 for nitrogenPercent

# Subset to 423 bands and assign names
hsStack_subset <- hsStack[[1:423]]
names(hsStack_subset) <- colnames(plsr_data_filtered)[-1]  # skip nitrogenPercent

# Define the prediction function
predict_nitrogen <- function(x) {
  as.vector(predict(plsr_model, newdata = as.data.frame(x), ncomp = 2))
}

# 1. Extract the reflectance matrix from the raster (each row = pixel, each column = band)
hs_matrix <- terra::as.matrix(hsStack_subset)

# 2. Remove rows with NA values (e.g., clouds, shadows)
non_na_rows <- complete.cases(hs_matrix)
hs_matrix_clean <- hs_matrix[non_na_rows, ]

# 3. Predict nitrogen values for valid pixels
predicted_nitrogen <- predict(plsr_model, newdata = hs_matrix_clean, ncomp = 2)
predicted_nitrogen <- as.vector(predicted_nitrogen)

# 4. Create an empty raster for the output
nitrogen_raster <- rast(hsStack_subset, nlyr = 1)
values(nitrogen_raster) <- NA  # initialize all to NA

# 5. Insert predicted values into non-NA rows
values(nitrogen_raster)[non_na_rows] <- predicted_nitrogen

# 6. Plot or save
plot(nitrogen_raster, main = "Predicted % Nitrogen Content")

# cap to 5 because unrealistic output
nitrogen_raster_capped <- clamp(nitrogen_raster, lower = 0, upper = 5)
plot(nitrogen_raster_capped, main = "Nitrogen Content (% dry mass)")
plot(fc_vect, add = TRUE, col = "red", pch = 16)
text(fc_vect, labels = fc_vect$plotID, pos = 3, cex = 0.8, col = "black")

# Resample to 40-meter resolution using aggregation
# 40x40 meter pixels assuming input is 1x1m resolution
agg_factor <- 40  # Adjust this if native pixel size is not exactly 1m

# Use mean aggregation across 40x40 pixel blocks
nitrogen_agg40 <- aggregate(nitrogen_raster_capped, fact = agg_factor, fun = mean, na.rm = TRUE)

# Optional: visualize
plot(nitrogen_agg40, main = "Predicted %N dry mass")
plot(fc_vect, add = TRUE, col = "red", pch = 16)
text(fc_vect, labels = fc_vect$plotID, pos = 3, cex = 0.8, col = "white")

