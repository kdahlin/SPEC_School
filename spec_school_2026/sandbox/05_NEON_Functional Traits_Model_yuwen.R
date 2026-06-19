############################################################
# Packages
############################################################

# Install only once if needed:
# install.packages(c("randomForest", "dplyr", "ggplot2", "corrplot", "mgcv"))

library(randomForest)
library(dplyr)
library(ggplot2)
library(corrplot)
library(mgcv)

############################################################
# Step 1: Read data
############################################################

field_df <- read.csv("X:/shared_data/NEON_field_data/MLBS/NEONForestAGBv2_Jenkins_MLBS_plot_2022.csv")
spc_df   <- read.csv("X:/shared_data/NEON_field_data/MLBS/MLBS_VIs_PCA_Hyperspectral.csv")
lidar_df <- read.csv("X:/shared_data/NEON_field_data/MLBS/MLBS_AOP_Lidar_Metrics.csv")

# Use capital V
View(field_df)
View(spc_df)
View(lidar_df)

# Make sure plotID has the same name
names(lidar_df)[1] <- "plotID"

############################################################
# Step 2: Define response and predictor variables
############################################################

y_var <- "AGB_Mg_ha"

# hs_vars <- c("PC4", "PC2", "PRI", "ARI1", "TCARI", "ExG", "SIPI")
# lidar_vars <- c("H_99", "H_95", "H_90", "H_cv", "H_mean", "H_SD", "LII")

hs_vars <- c("PC4", "PC3", "PRI", "ARI1", "ExG", "SIPI")
lidar_vars <- c("H_99", "H_30", "H_cv", "H_SD", "LII")

combined_vars <- c(hs_vars, lidar_vars)

############################################################
# Step 3: Keep only needed columns and build datasets
############################################################

field_agb <- field_df %>%
  select(plotID, all_of(y_var))

hs_data <- spc_df %>%
  select(plotID, all_of(hs_vars)) %>%
  inner_join(field_agb, by = "plotID") %>%
  select(plotID, all_of(y_var), all_of(hs_vars)) %>%
  filter(complete.cases(.))

lidar_data <- lidar_df %>%
  select(plotID, all_of(lidar_vars)) %>%
  inner_join(field_agb, by = "plotID") %>%
  select(plotID, all_of(y_var), all_of(lidar_vars)) %>%
  filter(complete.cases(.))

combined_data <- spc_df %>%
  select(plotID, all_of(hs_vars)) %>%
  inner_join(lidar_df %>% select(plotID, all_of(lidar_vars)), by = "plotID") %>%
  inner_join(field_agb, by = "plotID") %>%
  select(plotID, all_of(y_var), all_of(combined_vars)) %>%
  filter(complete.cases(.))

dim(hs_data)
dim(lidar_data)
dim(combined_data)



############################################################
# Correlation with AGB
############################################################

plot_correlations <- function(data, predictors, response, title) {
  
  cor_vals <- sapply(data[, predictors], function(x) {
    cor(x, data[[response]], use = "complete.obs")
  })
  
  cor_df <- data.frame(
    variable = names(cor_vals),
    cor = as.numeric(cor_vals)
  )
  
  ggplot(cor_df, aes(x = reorder(variable, cor), y = cor)) +
    geom_col() +
    coord_flip() +
    theme_minimal() +
    labs(
      x = "Predictor",
      y = paste("Correlation with", response),
      title = title
    )
}

plot_correlations(hs_data, hs_vars, y_var, "Hyperspectral predictors vs AGB")
plot_correlations(lidar_data, lidar_vars, y_var, "LiDAR predictors vs AGB")
plot_correlations(combined_data, combined_vars, y_var, "Combined predictors vs AGB")


############################################################
# Correlation matrices
############################################################

cor_hs <- cor(hs_data[, hs_vars], use = "complete.obs")
cor_lidar <- cor(lidar_data[, lidar_vars], use = "complete.obs")
cor_combined <- cor(combined_data[, combined_vars], use = "complete.obs")

corrplot(cor_hs,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.6,
         tl.col = "black",
         tl.cex = 0.7,
         diag = FALSE)

corrplot(cor_lidar,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.6,
         tl.col = "black",
         tl.cex = 0.7,
         diag = FALSE)

corrplot(cor_combined,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.6,
         tl.col = "black",
         tl.cex = 0.7,
         diag = FALSE)



############################################################
# Helper functions for model evaluation
############################################################

calc_metrics <- function(observed, predicted) {
  
  rmse_val <- sqrt(mean((observed - predicted)^2, na.rm = TRUE))
  mae_val  <- mean(abs(observed - predicted), na.rm = TRUE)
  
  # Predictive R2: can be negative if model is worse than predicting the mean
  r2_val <- 1 - sum((observed - predicted)^2, na.rm = TRUE) /
    sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
  
  data.frame(
    R2 = r2_val,
    RMSE = rmse_val,
    MAE = mae_val
  )
}


select_top_cor_vars <- function(train, predictors, response, max_vars = 4) {
  
  cor_vals <- sapply(predictors, function(v) {
    cor(train[[v]], train[[response]], use = "complete.obs")
  })
  
  cor_vals <- abs(cor_vals)
  cor_vals <- sort(cor_vals, decreasing = TRUE)
  
  names(cor_vals)[1:min(max_vars, length(cor_vals))]
}


loocv_rf <- function(data, predictors, response) {
  
  n <- nrow(data)
  preds <- rep(NA, n)
  
  for (i in 1:n) {
    
    train <- data[-i, ]
    test  <- data[i, ]
    
    model <- randomForest(
      x = train[, predictors],
      y = train[[response]],
      ntree = 1000,
      importance = TRUE
    )
    
    preds[i] <- predict(model, newdata = test[, predictors])
  }
  
  observed <- data[[response]]
  
  list(
    predictions = data.frame(
      plotID = data$plotID,
      observed = observed,
      predicted = preds
    ),
    metrics = calc_metrics(observed, preds)
  )
}


loocv_lm <- function(data, predictors, response, max_vars = 4) {
  
  n <- nrow(data)
  preds <- rep(NA, n)
  
  for (i in 1:n) {
    
    train <- data[-i, ]
    test  <- data[i, ]
    
    # For small datasets, avoid too many predictors in LM
    selected_vars <- select_top_cor_vars(train, predictors, response, max_vars = max_vars)
    
    form <- as.formula(
      paste(response, "~", paste(selected_vars, collapse = " + "))
    )
    
    model <- lm(form, data = train)
    
    preds[i] <- predict(model, newdata = test)
  }
  
  observed <- data[[response]]
  
  list(
    predictions = data.frame(
      plotID = data$plotID,
      observed = observed,
      predicted = preds
    ),
    metrics = calc_metrics(observed, preds)
  )
}


loocv_gam <- function(data, predictors, response, max_vars = 3) {
  
  n <- nrow(data)
  preds <- rep(NA, n)
  
  for (i in 1:n) {
    
    train <- data[-i, ]
    test  <- data[i, ]
    
    # GAMs need fewer predictors because smooth terms are flexible
    selected_vars <- select_top_cor_vars(train, predictors, response, max_vars = max_vars)
    
    smooth_terms <- paste0("s(", selected_vars, ", k = 3)")
    
    form <- as.formula(
      paste(response, "~", paste(smooth_terms, collapse = " + "))
    )
    
    model <- gam(
      form,
      data = train,
      method = "REML",
      select = TRUE
    )
    
    preds[i] <- predict(model, newdata = test)
  }
  
  observed <- data[[response]]
  
  list(
    predictions = data.frame(
      plotID = data$plotID,
      observed = observed,
      predicted = preds
    ),
    metrics = calc_metrics(observed, preds)
  )
}


############################################################
# Run models
############################################################

set.seed(123)

# Hyperspectral only
rf_hs  <- loocv_rf(hs_data, hs_vars, y_var)
lm_hs  <- loocv_lm(hs_data, hs_vars, y_var, max_vars = 4)
gam_hs <- loocv_gam(hs_data, hs_vars, y_var, max_vars = 3)

# LiDAR only
rf_lidar  <- loocv_rf(lidar_data, lidar_vars, y_var)
lm_lidar  <- loocv_lm(lidar_data, lidar_vars, y_var, max_vars = 4)
gam_lidar <- loocv_gam(lidar_data, lidar_vars, y_var, max_vars = 3)

# Hyperspectral + LiDAR
rf_combined  <- loocv_rf(combined_data, combined_vars, y_var)
lm_combined  <- loocv_lm(combined_data, combined_vars, y_var, max_vars = 4)
gam_combined <- loocv_gam(combined_data, combined_vars, y_var, max_vars = 3)


############################################################
# Compare models
############################################################

results <- bind_rows(
  cbind(Data = "Hyperspectral", Model = "Random Forest", rf_hs$metrics),
  cbind(Data = "Hyperspectral", Model = "Linear model", lm_hs$metrics),
  cbind(Data = "Hyperspectral", Model = "GAM", gam_hs$metrics),
  
  cbind(Data = "LiDAR", Model = "Random Forest", rf_lidar$metrics),
  cbind(Data = "LiDAR", Model = "Linear model", lm_lidar$metrics),
  cbind(Data = "LiDAR", Model = "GAM", gam_lidar$metrics),
  
  cbind(Data = "Hyperspectral + LiDAR", Model = "Random Forest", rf_combined$metrics),
  cbind(Data = "Hyperspectral + LiDAR", Model = "Linear model", lm_combined$metrics),
  cbind(Data = "Hyperspectral + LiDAR", Model = "GAM", gam_combined$metrics)
)

results <- results %>%
  arrange(RMSE)

# # Best model = lowest RMSE and MAE, highest predictive R2
print(results)


############################################################
# Plot function
############################################################

plot_predictions <- function(pred_df, title) {
  
  ggplot(pred_df, aes(x = observed, y = predicted)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, linewidth = 1) +
    coord_equal() +
    theme_minimal() +
    labs(
      x = "Observed AGB (Mg/ha)",
      y = "Predicted AGB (Mg/ha)",
      title = title
    )
}

plot_predictions(rf_hs$predictions, "RF: Hyperspectral only")
plot_predictions(rf_lidar$predictions, "RF: LiDAR only")
plot_predictions(rf_combined$predictions, "RF: Hyperspectral + LiDAR")

plot_predictions(gam_hs$predictions, "GAM: Hyperspectral only")
plot_predictions(gam_lidar$predictions, "GAM: LiDAR only")
plot_predictions(gam_combined$predictions, "GAM: Hyperspectral + LiDAR")

plot_predictions(lm_hs$predictions, "LM: Hyperspectral only")
plot_predictions(lm_lidar$predictions, "LM: LiDAR only")
plot_predictions(lm_combined$predictions, "LM: Hyperspectral + LiDAR")


#######Synthetic analysis#######
# Synthetic-data sensitivity analysis with sample size 500
make_synthetic_data <- function(train_data,
                                predictors,
                                response,
                                n_synth = 500,
                                noise_frac = 0.05) {
  
  vars_needed <- c(response, predictors)
  
  train_small <- train_data[, vars_needed]
  
  sampled <- train_small[sample(1:nrow(train_small),
                                size = n_synth,
                                replace = TRUE), ]
  
  for (v in vars_needed) {
    
    if (is.numeric(sampled[[v]])) {
      
      sd_v <- sd(train_small[[v]], na.rm = TRUE)
      
      if (!is.na(sd_v) && sd_v > 0) {
        sampled[[v]] <- sampled[[v]] +
          rnorm(n_synth, mean = 0, sd = noise_frac * sd_v)
      }
    }
  }
  
  sampled[[response]][sampled[[response]] < 0] <- 0
  
  return(sampled)
}

synthetic_loocv_rf <- function(data,
                               predictors,
                               response,
                               n_synth = 500,
                               noise_frac = 0.05) {
  
  n <- nrow(data)
  preds <- rep(NA, n)
  
  for (i in 1:n) {
    
    train_real <- data[-i, ]
    test_real  <- data[i, ]
    
    synth_train <- make_synthetic_data(
      train_data = train_real,
      predictors = predictors,
      response = response,
      n_synth = n_synth,
      noise_frac = noise_frac
    )
    
    train_augmented <- bind_rows(
      train_real[, c(response, predictors)],
      synth_train
    )
    
    model <- randomForest(
      x = train_augmented[, predictors],
      y = train_augmented[[response]],
      ntree = 1000,
      importance = TRUE
    )
    
    preds[i] <- predict(model, newdata = test_real[, predictors])
  }
  
  list(
    predictions = data.frame(
      plotID = data$plotID,
      observed = data[[response]],
      predicted = preds
    ),
    metrics = calc_metrics(data[[response]], preds)
  )
}

synthetic_loocv_lm <- function(data,
                               predictors,
                               response,
                               n_synth = 500,
                               noise_frac = 0.05,
                               max_vars = 3) {
  
  n <- nrow(data)
  preds <- rep(NA, n)
  
  for (i in 1:n) {
    
    train_real <- data[-i, ]
    test_real  <- data[i, ]
    
    selected_vars <- select_top_cor_vars(train_real, predictors, response, max_vars)
    
    synth_train <- make_synthetic_data(
      train_data = train_real,
      predictors = selected_vars,
      response = response,
      n_synth = n_synth,
      noise_frac = noise_frac
    )
    
    train_augmented <- bind_rows(
      train_real[, c(response, selected_vars)],
      synth_train
    )
    
    form <- as.formula(
      paste(response, "~", paste(selected_vars, collapse = " + "))
    )
    
    model <- lm(form, data = train_augmented)
    
    preds[i] <- predict(model, newdata = test_real)
  }
  
  list(
    predictions = data.frame(
      plotID = data$plotID,
      observed = data[[response]],
      predicted = preds
    ),
    metrics = calc_metrics(data[[response]], preds)
  )
}

synthetic_loocv_gam <- function(data,
                                predictors,
                                response,
                                n_synth = 500,
                                noise_frac = 0.05,
                                max_vars = 2) {
  
  n <- nrow(data)
  preds <- rep(NA, n)
  
  for (i in 1:n) {
    
    train_real <- data[-i, ]
    test_real  <- data[i, ]
    
    selected_vars <- select_top_cor_vars(train_real, predictors, response, max_vars)
    
    synth_train <- make_synthetic_data(
      train_data = train_real,
      predictors = selected_vars,
      response = response,
      n_synth = n_synth,
      noise_frac = noise_frac
    )
    
    train_augmented <- bind_rows(
      train_real[, c(response, selected_vars)],
      synth_train
    )
    
    smooth_terms <- paste0("s(", selected_vars, ", k = 3)")
    
    form <- as.formula(
      paste(response, "~", paste(smooth_terms, collapse = " + "))
    )
    
    model <- gam(
      form,
      data = train_augmented,
      method = "REML",
      select = TRUE
    )
    
    preds[i] <- predict(model, newdata = test_real)
  }
  
  list(
    predictions = data.frame(
      plotID = data$plotID,
      observed = data[[response]],
      predicted = preds
    ),
    metrics = calc_metrics(data[[response]], preds)
  )
}


set.seed(123)

synthetic_n <- 500
noise_frac <- 0.05

syn_rf_hs  <- synthetic_loocv_rf(hs_data, hs_vars, y_var, synthetic_n, noise_frac)
syn_lm_hs  <- synthetic_loocv_lm(hs_data, hs_vars, y_var, synthetic_n, noise_frac, max_vars = 3)
syn_gam_hs <- synthetic_loocv_gam(hs_data, hs_vars, y_var, synthetic_n, noise_frac, max_vars = 2)

syn_rf_lidar  <- synthetic_loocv_rf(lidar_data, lidar_vars, y_var, synthetic_n, noise_frac)
syn_lm_lidar  <- synthetic_loocv_lm(lidar_data, lidar_vars, y_var, synthetic_n, noise_frac, max_vars = 3)
syn_gam_lidar <- synthetic_loocv_gam(lidar_data, lidar_vars, y_var, synthetic_n, noise_frac, max_vars = 2)

syn_rf_combined  <- synthetic_loocv_rf(combined_data, combined_vars, y_var, synthetic_n, noise_frac)
syn_lm_combined  <- synthetic_loocv_lm(combined_data, combined_vars, y_var, synthetic_n, noise_frac, max_vars = 3)
syn_gam_combined <- synthetic_loocv_gam(combined_data, combined_vars, y_var, synthetic_n, noise_frac, max_vars = 2)

synthetic_results <- bind_rows(
  cbind(Data = "Hyperspectral", Model = "Random Forest", syn_rf_hs$metrics),
  cbind(Data = "Hyperspectral", Model = "Linear model", syn_lm_hs$metrics),
  cbind(Data = "Hyperspectral", Model = "GAM", syn_gam_hs$metrics),
  
  cbind(Data = "LiDAR", Model = "Random Forest", syn_rf_lidar$metrics),
  cbind(Data = "LiDAR", Model = "Linear model", syn_lm_lidar$metrics),
  cbind(Data = "LiDAR", Model = "GAM", syn_gam_lidar$metrics),
  
  cbind(Data = "Hyperspectral + LiDAR", Model = "Random Forest", syn_rf_combined$metrics),
  cbind(Data = "Hyperspectral + LiDAR", Model = "Linear model", syn_lm_combined$metrics),
  cbind(Data = "Hyperspectral + LiDAR", Model = "GAM", syn_gam_combined$metrics)
) %>%
  mutate(Analysis = "Synthetic training data, n = 500") %>%
  arrange(RMSE)

print(synthetic_results)
