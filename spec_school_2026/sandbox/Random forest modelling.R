install.packages(c("randomForest", "caret", "dplyr", "Metrics", "VSURF"))

install.packages('corrplot')

library(randomForest)
library(caret)
library(dplyr)
library(Metrics)
library(ggplot2)

# variables selection using random forest
library(VSURF)

field_df <- read.csv('X:/shared_data/NEON_field_data/MLBS/NEONForestAGBv2_Jenkins_MLBS_plot_2022.csv')
view(field_df)
spc_df <- read.csv('X:/shared_data/NEON_field_data/MLBS/MLBS_VIs_PCA_Hyperspectral.csv')
names(spc_df)[-(1:3)]

lidar_df <- read.csv('X:/shared_data/NEON_field_data/MLBS/MLBS_AOP_Lidar_Metrics.csv')
dim(lidar_df) #25*17
names(lidar_df)[1] <- 'plotID'

# create data for randomforst
data_hpyer <- spc_df %>%
  left_join(field_df, by = "plotID")
dim(data_hpyer) #36*42
names(data_hpyer)

data_lidar <- lidar_df %>%
  left_join(field_df, by = "plotID")
dim(data_lidar) #25*22
names(data_lidar)

# hyperspetral + lidar
data_com <- data_hpyer %>%
  left_join(data_lidar, by = "plotID")
dim(data_com)

# for hyperspectral only
df_clean1 <- data_hpyer %>%
  filter(!is.na(AGB_Mg_ha), !is.na(plotID))
names(df_clean1)

# for lidar only
df_clean2 <- data_lidar %>%
  filter(!is.na(AGB_Mg_ha), !is.na(plotID))
names(df_clean2)

# for combined data
df_clean3 <- df_clean1[-38] %>%
  left_join(df_clean2[-c(2:3,18:22)], by = "plotID")

dim(df_clean3) #16*55-3/51
names(df_clean3)
View(df_clean3)

# df_clean <- df_clean[-c(1:3)]


# Correlation
cor_df <- data.frame(
  cor = sapply(df_clean3, function(b) {
    cor(df_clean3[-c(1:3,38:40)],
        df_clean3$AGB_Mg_ha,
        use = "complete.obs")
  })
)
# names(cor_df)
ggplot(cor_df, aes(x = variable, y = cor)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Variables",
       y = "Correlation with AGB")


vars <- df_clean3[-c(1:3, 38:40)]

cor_vals <- sapply(vars, function(x) {
  cor(x, df_clean3$AGB_Mg_ha, use = "complete.obs")
})

cor_df <- data.frame(
  variable = names(cor_vals),
  cor = as.numeric(cor_vals)
)

ggplot(cor_df, aes(x = reorder(variable, cor), y = cor)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(x = "Variables",
       y = "Correlation with AGB_Mg_ha")


vars <- df_clean3[-c(1:3, 38:40)]
vars1 <- df_clean1[-c(1:3, 38:41)]
vars2 <- df_clean2[-c(1:3, 18:21)]
cor_mat <- cor(vars, use = "complete.obs")

cor_mat1 <- cor(vars1, use = "complete.obs")
cor_mat2 <- cor(vars2, use = "complete.obs")

names(df_clean1)
names(df_clean2)

library(corrplot)
corrplot(cor_mat,
         method = "color",
         type = "upper",
         tl.cex = 0.7,
         tl.col = "black")

corrplot(cor_mat1,
         method = "color",
         type = "upper",
         tl.cex = 0.7,
         tl.col = "black")

corrplot(cor_mat2,
         method = "color",
         type = "upper",
         tl.cex = 0.7,
         tl.col = "black")

cor_mat1_clean <- cor_mat1
cor_mat1_clean[abs(cor_mat1_clean) > 0.6] <- NA


corrplot(
  cor_mat1,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.6,
  tl.col = "black",
  tl.cex = 0.7,
  diag = FALSE,
  col = colorRampPalette(c("blue", "white", "red"))(200)
)

corrplot(
  cor_mat1_clean,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.6,
  tl.col = "black",
  tl.cex = 0.7,
  diag = FALSE,
  na.label = " ",   # Blank out NA cells
  col = colorRampPalette(c("blue", "white", "red"))(200)
)


corrplot(
  # cor_mat1,
  cor_mat1_clean,
         method = "color",
         type = "upper",
         # order = "hclust",
         addCoef.col = "black",   # Add correlation coefficients
         number.cex = 0.6,        # Size of numbers
         tl.col = "black",
         tl.cex = 0.7,
         diag = FALSE,
         col = colorRampPalette(c("blue", "white", "red"))(200))

corrplot(cor_mat2,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",   # Add correlation coefficients
         number.cex = 0.6,        # Size of numbers
         tl.col = "black",
         tl.cex = 0.7,
         diag = FALSE,
         col = colorRampPalette(c("blue", "white", "red"))(200))



#lidar predictors
# lidar_vars <- c("meanHt", "p95", "cover", "FHD")

# Hyperspectral predictors
hs_vars <- names(spc_df)[-(1:3)]


# Response
y_var <- "AGB_Mg_ha"

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

model_hs <- rf_loocv(df_clean, hs_vars, y_var)

model_hs$R2
model_hs$RMSE

model_hs2 <- randomForest(
  AGB_Mg_ha ~ .,
  data = df_clean[, c(y_var, hs_vars)],
  ntree = 500,
  importance = TRUE
)

preds <- predict(model_hs2, df_clean)
head(preds)


plot(df_clean[[y_var]], preds,
     xlab = "Observed Biomass",
     ylab = "Predicted Biomass",
     main = "Random Forest (Hyperspectral)")
# 1:1 line
abline(a = 0, b = 1, col = "red", lwd = 2)
# 

plot_df <- data.frame(
  observed = df_clean[[y_var]],
  predicted = preds
)

r2 <- cor(plot_df$observed, plot_df$predicted)^2
rmse <- sqrt(mean((plot_df$observed - plot_df$predicted)^2))

ggplot(plot_df, aes(x = observed, y = predicted)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1.2) +
  theme_minimal() +
  coord_equal() +
  labs(
    x = "Observed Biomass (Mg/ha)",
    y = "Predicted Biomass (Mg/ha)",
    title = "Random Forest (Hyperspectral)"
  )

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

plot(data[[y_var]], model_hs2$predictions,
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


# GAM model

library(mgcv)
head(df_clean)

hs_vars <- names(df_clean)[-(1:2)]
var1 <- hs_vars[-c(32:34)]

form <- as.formula(
  paste("AGB_Mg_ha ~", paste("s(", hs_vars, ")", collapse = " + "))
)

form1 <- as.formula(
  paste("AGB_Mg_ha ~", paste("s(", var1, ")", collapse = " + "))
)

gam_model <- gam(form,
                 data = df_clean,
                 method = "REML")

summary(gam_model)

# Only PCA1
gam_model1 <- gam(form1,
                 data = df_clean,
                 method = "REML")


ggplot(df_clean, aes(x = AGB_Mg_ha, y = gam_pred)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  coord_equal() +
  theme_minimal() +
  labs(x = "Observed AGB", y = "Predicted AGB")