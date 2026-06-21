############################################################
# Packages
############################################################

library(caret)
library(pls)
library(randomForest)
library(ggplot2)

############################################################
# Step 1: Read data
############################################################

field_df <- read.csv("X:/shared_data/NEON_field_data/MLBS/NEONForestAGBv2_Jenkins_MLBS_plot_2022.csv")
spc_df   <- read.csv("X:/shared_data/NEON_field_data/MLBS/MLBS_VIs_PCA_Hyperspectral.csv")
lidar_df <- read.csv("X:/shared_data/NEON_field_data/MLBS/MLBS_AOP_Lidar_Metrics.csv")

names(lidar_df)[1] <- "plotID"

############################################################
# Step 2: Define response and predictor variables
############################################################

y_var <- "AGB_Mg_ha"

hs_vars <- c("PC4", "PC3", "PRI", "ARI1", "ExG", "SIPI")
lidar_vars <- c("H_99", "H_30", "H_cv", "H_SD", "LII")

combined_vars <- c(hs_vars, lidar_vars)

############################################################
# Step 3: Build modeling dataset
############################################################

field_agb <- field_df[, c("plotID", y_var)]
hs_data_raw <- spc_df[, c("plotID", hs_vars)]
lidar_data_raw <- lidar_df[, c("plotID", lidar_vars)]

model_data <- merge(field_agb, hs_data_raw, by = "plotID")
model_data <- merge(model_data, lidar_data_raw, by = "plotID")

model_data <- model_data[complete.cases(model_data), ]

y <- model_data[[y_var]]

hs <- model_data[, hs_vars, drop = FALSE]
lidar <- model_data[, lidar_vars, drop = FALSE]
combined <- model_data[, combined_vars, drop = FALSE]

############################################################
# Cross-validation control
############################################################

ctrl <- trainControl(
  method = "LOOCV",
  savePredictions = "final"
)

############################################################
# Function to calculate PLS VIP scores
############################################################

calc_pls_vip <- function(pls_model, ncomp, predictor_names) {
  
  W <- pls_model$loading.weights[, 1:ncomp, drop = FALSE]
  T_scores <- pls_model$scores[, 1:ncomp, drop = FALSE]
  Q <- pls_model$Yloadings[1:ncomp, , drop = FALSE]
  
  p <- nrow(W)
  
  SSY <- rep(NA, ncomp)
  
  for (a in 1:ncomp) {
    SSY[a] <- sum(T_scores[, a]^2) * sum(Q[a, ]^2)
  }
  
  W_norm <- colSums(W^2)
  
  VIP <- rep(NA, p)
  
  for (j in 1:p) {
    weight_sum <- 0
    
    for (a in 1:ncomp) {
      weight_sum <- weight_sum + SSY[a] * (W[j, a]^2 / W_norm[a])
    }
    
    VIP[j] <- sqrt(p * weight_sum / sum(SSY))
  }
  
  data.frame(
    Variable = predictor_names,
    VIP = VIP
  )
}

############################################################
# Model function
############################################################

run_model <- function(df, y, method, sensor_name) {
  
  data <- data.frame(AGB_Mg_ha = y, df)
  
  stopifnot(nrow(data) == length(y))
  
  ############################################################
  # Fit model
  ############################################################
  
  if (method == "LM") {
    
    fit <- train(
      AGB_Mg_ha ~ .,
      data = data,
      method = "lm",
      trControl = ctrl,
      metric = "RMSE"
    )
    
  } else if (method == "PLS") {
    
    max_ncomp <- min(5, ncol(df), nrow(data) - 2)
    
    fit <- train(
      AGB_Mg_ha ~ .,
      data = data,
      method = "pls",
      preProcess = c("center", "scale"),
      tuneGrid = expand.grid(ncomp = 1:max_ncomp),
      trControl = ctrl,
      metric = "RMSE"
    )
    
  } else if (method == "RF") {
    
    set.seed(123)
    
    fit <- train(
      AGB_Mg_ha ~ .,
      data = data,
      method = "rf",
      ntree = 500,
      importance = TRUE,
      tuneGrid = expand.grid(mtry = 1:ncol(df)),
      trControl = ctrl,
      metric = "RMSE"
    )
    
  } else {
    
    stop("Method must be 'LM', 'PLS', or 'RF'")
    
  }
  
  ############################################################
  # Extract LOOCV predictions
  ############################################################
  
  pred_df <- fit$pred[order(fit$pred$rowIndex), ]
  
  pred <- pred_df$pred
  obs  <- pred_df$obs
  
  ############################################################
  # Predictive metrics
  ############################################################
  
  RMSE_val <- sqrt(mean((obs - pred)^2))
  
  MAE_val <- mean(abs(obs - pred))
  
  R2_pred <- 1 - sum((obs - pred)^2) /
    sum((obs - mean(obs))^2)
  
  ############################################################
  # Extract tuning parameters
  ############################################################
  
  best_ncomp <- NA
  best_mtry  <- NA
  
  if (method == "PLS") {
    best_ncomp <- fit$bestTune$ncomp
  }
  
  if (method == "RF") {
    best_mtry <- fit$bestTune$mtry
  }
  
  ############################################################
  # Model metrics table
  ############################################################
  
  metrics_df <- data.frame(
    Sensor = sensor_name,
    Model = method,
    R2_LOOCV = R2_pred,
    RMSE = RMSE_val,
    MAE = MAE_val,
    Best_ncomp = best_ncomp,
    Best_mtry = best_mtry
  )
  
  ############################################################
  # Variable importance table
  ############################################################
  
  importance_df <- NULL
  
  if (method == "PLS") {
    
    vip_df <- calc_pls_vip(
      pls_model = fit$finalModel,
      ncomp = best_ncomp,
      predictor_names = names(df)
    )
    
    importance_df <- data.frame(
      Sensor = sensor_name,
      Model = "PLS",
      Variable = vip_df$Variable,
      Importance_type = "VIP",
      Importance = vip_df$VIP
    )
    
  }
  
  if (method == "RF") {
    
    rf_imp <- varImp(fit, scale = FALSE)$importance
    
    rf_imp$Variable <- rownames(rf_imp)
    
    if ("Overall" %in% names(rf_imp)) {
      imp_values <- rf_imp$Overall
    } else {
      imp_values <- rf_imp[, 1]
    }
    
    importance_df <- data.frame(
      Sensor = sensor_name,
      Model = "RF",
      Variable = rf_imp$Variable,
      Importance_type = "RF_variable_importance",
      Importance = imp_values
    )
  }
  
  return(
    list(
      metrics = metrics_df,
      importance = importance_df,
      fit = fit
    )
  )
}

############################################################
# Run models
############################################################

lidar_lm <- run_model(lidar, y, "LM", "LiDAR")
lidar_pls <- run_model(lidar, y, "PLS", "LiDAR")
lidar_rf <- run_model(lidar, y, "RF", "LiDAR")

hs_lm <- run_model(hs, y, "LM", "Hyperspectral")
hs_pls <- run_model(hs, y, "PLS", "Hyperspectral")
hs_rf <- run_model(hs, y, "RF", "Hyperspectral")

combined_lm <- run_model(combined, y, "LM", "Combined")
combined_pls <- run_model(combined, y, "PLS", "Combined")
combined_rf <- run_model(combined, y, "RF", "Combined")

############################################################
# Combine model performance results
############################################################

final_results <- rbind(
  lidar_lm$metrics,
  lidar_pls$metrics,
  lidar_rf$metrics,
  hs_lm$metrics,
  hs_pls$metrics,
  hs_rf$metrics,
  combined_lm$metrics,
  combined_pls$metrics,
  combined_rf$metrics
)

final_results <- final_results[order(final_results$RMSE), ]

print(final_results)

############################################################
# Combine variable importance results
############################################################

importance_results <- rbind(
  lidar_pls$importance,
  lidar_rf$importance,
  hs_pls$importance,
  hs_rf$importance,
  combined_pls$importance,
  combined_rf$importance
)

importance_results <- importance_results[order(
  importance_results$Sensor,
  importance_results$Model,
  -importance_results$Importance
), ]

print(importance_results)

############################################################
# Separate VIP scores and RF importance if desired
############################################################

vip_scores <- importance_results[importance_results$Importance_type == "VIP", ]

rf_importance <- importance_results[
  importance_results$Importance_type == "RF_variable_importance", 
]

print(vip_scores)
print(rf_importance)

############################################################
# Plot PLS VIP scores
############################################################

ggplot(vip_scores, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ Sensor, scales = "free_y") +
  theme_classic() +
  xlab("Predictor") +
  ylab("VIP score") +
  ggtitle("PLS VIP Scores")

############################################################
# Plot Random Forest variable importance
############################################################

ggplot(rf_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ Sensor, scales = "free_y") +
  theme_classic() +
  xlab("Predictor") +
  ylab("Random Forest importance") +
  ggtitle("Random Forest Variable Importance")

############################################################
# Correlation plot
############################################################

df_cor <- data.frame(combined, AGB_Mg_ha = y)

cors <- cor(df_cor, use = "complete.obs")[, "AGB_Mg_ha"]

cors <- cors[names(cors) != "AGB_Mg_ha"]

corr_df <- data.frame(
  Variable = names(cors),
  Correlation = cors
)

corr_df <- corr_df[order(abs(corr_df$Correlation), decreasing = TRUE), ]

ggplot(corr_df, aes(x = reorder(Variable, abs(Correlation)), y = Correlation)) +
  geom_col() +
  coord_flip() +
  theme_classic() +
  xlab("Predictor") +
  ylab("Correlation with Biomass") +
  ggtitle("Predictor-Biomass Relationships, Combined Dataset")