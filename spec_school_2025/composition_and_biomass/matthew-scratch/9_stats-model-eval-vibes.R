# Extract predicted nitrogen values from raster at field plot points
predicted_vals <- terra::extract(nitrogen_raster_capped, plot_vect)

# Combine with observed values
evaluation_df <- cbind(fabaceae_summary, predicted_nitrogen = predicted_vals[, 2])

evaluation_df <- evaluation_df %>%
  filter(!is.na(predicted_nitrogen), !is.na(canopy_percent_N))

# Load required package
library(Metrics)

# Mean Absolute Error
mae_val <- mae(evaluation_df$canopy_percent_N, evaluation_df$predicted_nitrogen)

# Root Mean Squared Error
rmse_val <- rmse(evaluation_df$canopy_percent_N, evaluation_df$predicted_nitrogen)

# R-squared
rsq_val <- cor(evaluation_df$canopy_percent_N, evaluation_df$predicted_nitrogen)^2

# Bias (mean predicted - mean observed)
bias_val <- mean(evaluation_df$predicted_nitrogen - evaluation_df$canopy_percent_N)

# Output results
metrics <- data.frame(
  MAE = mae_val,
  RMSE = rmse_val,
  R_squared = rsq_val,
  Bias = bias_val
)

print(metrics)
