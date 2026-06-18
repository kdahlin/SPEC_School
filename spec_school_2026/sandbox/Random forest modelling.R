install.packages(c("randomForest", "caret", "dplyr", "Metrics"))

library(randomForest)
library(caret)
library(dplyr)
library(Metrics)
data <- read.csv("MLBS_data.csv")

#lidar predictors
lidar_vars <- c("meanHt", "p95", "cover", "FHD")

# Hyperspectral predictors
hs_vars <- c("NDVI", "EVI", "NDWI", "PC1", "PC2", "PC3")

# Response
y_var <- "Biomass"

rf_loocv <- function(data, predictors, response) {
  
  n <- nrow(data)
  preds <- rep(NA, n)
  
  for (i in 1:n) {
    
    train <- data[-i, ]
    test  <- data[i, ]
    
    formula <- as.formula(
      paste(response, "~", paste(predictors, collapse = "+"))
    )
    
    model <- randomForest(
      formula,
      data = train,
      ntree = 500,
      importance = TRUE
    )
    
    preds[i] <- predict(model, newdata = test)
  }
  
  # Performance metrics
  actual <- data[[response]]
  
  r2 <- cor(actual, preds)^2
  rmse_val <- rmse(actual, preds)
  mae_val <- mae(actual, preds)
  
  return(list(
    predictions = preds,
    R2 = r2,
    RMSE = rmse_val,
    MAE = mae_val
  ))
}

model_lidar <- rf_loocv(data, lidar_vars, y_var)

model_lidar$R2
model_lidar$RMSE

model_hs <- rf_loocv(data, hs_vars, y_var)

model_hs$R2
model_hs$RMSE

all_vars <- c(lidar_vars, hs_vars)

model_combined <- rf_loocv(data, all_vars, y_var)

model_combined$R2
model_combined$RMSE

results <- data.frame(
  Model = c("LiDAR", "Hyperspectral", "Combined"),
  R2 = c(model_lidar$R2, model_hs$R2, model_combined$R2),
  RMSE = c(model_lidar$RMSE, model_hs$RMSE, model_combined$RMSE)
)

print(results)

plot(data[[y_var]], model_combined$predictions,
     xlab = "Observed Biomass",
     ylab = "Predicted Biomass",
     main = "Random Forest (LiDAR + Hyperspectral)")

abline(0, 1, col = "red", lwd = 2)

final_model <- randomForest(
  Biomass ~ .,
  data = data[, c(y_var, all_vars)],
  ntree = 500,
  importance = TRUE
)

importance(final_model)
varImpPlot(final_model)