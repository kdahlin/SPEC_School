max_ncomp_lidar <- min(5, ncol(lidar), length(y) - 2)

pls_lidar_fit <- train(
  AGB_Mg_ha ~ .,
  data = data.frame(AGB_Mg_ha = y, lidar),
  method = "pls",
  preProcess = c("center", "scale"),
  tuneGrid = expand.grid(ncomp = 1:max_ncomp_lidar),
  trControl = ctrl,
  metric = "RMSE"
)

best_ncomp_lidar <- pls_lidar_fit$bestTune$ncomp

pls_obj <- pls_lidar_fit$finalModel

W <- as.matrix(pls_obj$loading.weights)[, 1:best_ncomp_lidar, drop = FALSE]
T_scores <- as.matrix(pls_obj$scores)[, 1:best_ncomp_lidar, drop = FALSE]
Q <- as.matrix(pls_obj$Yloadings)[1:best_ncomp_lidar, , drop = FALSE]

p <- nrow(W)

SSY <- rep(NA, best_ncomp_lidar)

for (a in 1:best_ncomp_lidar) {
  SSY[a] <- sum(T_scores[, a]^2) * sum(Q[a, ]^2)
}

W_norm <- colSums(W^2)

VIP <- rep(NA, p)

for (j in 1:p) {
  
  weight_sum <- 0
  
  for (a in 1:best_ncomp_lidar) {
    weight_sum <- weight_sum + SSY[a] * (W[j, a]^2 / W_norm[a])
  }
  
  VIP[j] <- sqrt(p * weight_sum / sum(SSY))
}

vip_lidar <- data.frame(
  Sensor = "LiDAR",
  Variable = names(lidar),
  VIP = VIP,
  Best_ncomp = best_ncomp_lidar
)

vip_lidar <- vip_lidar[order(vip_lidar$VIP, decreasing = TRUE), ]


# -------------------------
# Hyperspectral PLS VIP
# -------------------------

max_ncomp_hs <- min(5, ncol(hs), length(y) - 2)

pls_hs_fit <- train(
  AGB_Mg_ha ~ .,
  data = data.frame(AGB_Mg_ha = y, hs),
  method = "pls",
  preProcess = c("center", "scale"),
  tuneGrid = expand.grid(ncomp = 1:max_ncomp_hs),
  trControl = ctrl,
  metric = "RMSE"
)

best_ncomp_hs <- pls_hs_fit$bestTune$ncomp

pls_obj <- pls_hs_fit$finalModel

W <- as.matrix(pls_obj$loading.weights)[, 1:best_ncomp_hs, drop = FALSE]
T_scores <- as.matrix(pls_obj$scores)[, 1:best_ncomp_hs, drop = FALSE]
Q <- as.matrix(pls_obj$Yloadings)[1:best_ncomp_hs, , drop = FALSE]

p <- nrow(W)

SSY <- rep(NA, best_ncomp_hs)

for (a in 1:best_ncomp_hs) {
  SSY[a] <- sum(T_scores[, a]^2) * sum(Q[a, ]^2)
}

W_norm <- colSums(W^2)

VIP <- rep(NA, p)

for (j in 1:p) {
  
  weight_sum <- 0
  
  for (a in 1:best_ncomp_hs) {
    weight_sum <- weight_sum + SSY[a] * (W[j, a]^2 / W_norm[a])
  }
  
  VIP[j] <- sqrt(p * weight_sum / sum(SSY))
}

vip_hs <- data.frame(
  Sensor = "Hyperspectral",
  Variable = names(hs),
  VIP = VIP,
  Best_ncomp = best_ncomp_hs
)

vip_hs <- vip_hs[order(vip_hs$VIP, decreasing = TRUE), ]


# -------------------------
# Combined PLS VIP
# -------------------------

max_ncomp_combined <- min(5, ncol(combined), length(y) - 2)

pls_combined_fit <- train(
  AGB_Mg_ha ~ .,
  data = data.frame(AGB_Mg_ha = y, combined),
  method = "pls",
  preProcess = c("center", "scale"),
  tuneGrid = expand.grid(ncomp = 1:max_ncomp_combined),
  trControl = ctrl,
  metric = "RMSE"
)

best_ncomp_combined <- pls_combined_fit$bestTune$ncomp

pls_obj <- pls_combined_fit$finalModel

W <- as.matrix(pls_obj$loading.weights)[, 1:best_ncomp_hs, drop = FALSE]
T_scores <- as.matrix(pls_obj$scores)[, 1:best_ncomp_hs, drop = FALSE]

# Yloadings is usually stored as: response x components
# Since we have one response variable, extract the component loadings from row 1
Q <- as.numeric(as.matrix(pls_obj$Yloadings)[1, 1:best_ncomp_hs])

p <- nrow(W)

SSY <- rep(NA, best_ncomp_hs)

for (a in 1:best_ncomp_hs) {
  SSY[a] <- sum(T_scores[, a]^2) * Q[a]^2
}


W_norm <- colSums(W^2)

VIP <- rep(NA, p)

for (j in 1:p) {
  
  weight_sum <- 0
  
  for (a in 1:best_ncomp_combined) {
    weight_sum <- weight_sum + SSY[a] * (W[j, a]^2 / W_norm[a])
  }
  
  VIP[j] <- sqrt(p * weight_sum / sum(SSY))
}

vip_combined <- data.frame(
  Sensor = "Combined",
  Variable = names(combined),
  VIP = VIP,
  Best_ncomp = best_ncomp_combined
)

vip_combined <- vip_combined[order(vip_combined$VIP, decreasing = TRUE), ]


############################################################
# 2. Combine and print VIP scores
############################################################

vip_scores <- rbind(
  vip_lidar,
  vip_hs,
  vip_combined
)

vip_scores <- vip_scores[order(vip_scores$Sensor, -vip_scores$VIP), ]

print(vip_scores)


############################################################
# 3. Refit Random Forest models to extract variable importance
############################################################

# -------------------------
# LiDAR RF importance
# -------------------------

set.seed(123)

rf_lidar_fit <- train(
  AGB_Mg_ha ~ .,
  data = data.frame(AGB_Mg_ha = y, lidar),
  method = "rf",
  ntree = 500,
  importance = TRUE,
  tuneGrid = expand.grid(mtry = 1:ncol(lidar)),
  trControl = ctrl,
  metric = "RMSE"
)

rf_lidar_imp <- varImp(rf_lidar_fit, scale = FALSE)$importance

rf_lidar_importance <- data.frame(
  Sensor = "LiDAR",
  Variable = rownames(rf_lidar_imp),
  Importance = rf_lidar_imp$Overall,
  Best_mtry = rf_lidar_fit$bestTune$mtry
)

rf_lidar_importance <- rf_lidar_importance[
  order(rf_lidar_importance$Importance, decreasing = TRUE), 
]


# -------------------------
# Hyperspectral RF importance
# -------------------------

set.seed(123)

rf_hs_fit <- train(
  AGB_Mg_ha ~ .,
  data = data.frame(AGB_Mg_ha = y, hs),
  method = "rf",
  ntree = 500,
  importance = TRUE,
  tuneGrid = expand.grid(mtry = 1:ncol(hs)),
  trControl = ctrl,
  metric = "RMSE"
)

rf_hs_imp <- varImp(rf_hs_fit, scale = FALSE)$importance

rf_hs_importance <- data.frame(
  Sensor = "Hyperspectral",
  Variable = rownames(rf_hs_imp),
  Importance = rf_hs_imp$Overall,
  Best_mtry = rf_hs_fit$bestTune$mtry
)

rf_hs_importance <- rf_hs_importance[
  order(rf_hs_importance$Importance, decreasing = TRUE), 
]


# -------------------------
# Combined RF importance
# -------------------------

set.seed(123)

rf_combined_fit <- train(
  AGB_Mg_ha ~ .,
  data = data.frame(AGB_Mg_ha = y, combined),
  method = "rf",
  ntree = 500,
  importance = TRUE,
  tuneGrid = expand.grid(mtry = 1:ncol(combined)),
  trControl = ctrl,
  metric = "RMSE"
)

rf_combined_imp <- varImp(rf_combined_fit, scale = FALSE)$importance

rf_combined_importance <- data.frame(
  Sensor = "Combined",
  Variable = rownames(rf_combined_imp),
  Importance = rf_combined_imp$Overall,
  Best_mtry = rf_combined_fit$bestTune$mtry
)

rf_combined_importance <- rf_combined_importance[
  order(rf_combined_importance$Importance, decreasing = TRUE), 
]


############################################################
# 4. Combine and print RF variable importance
############################################################

rf_importance <- rbind(
  rf_lidar_importance,
  rf_hs_importance,
  rf_combined_importance
)

rf_importance <- rf_importance[order(rf_importance$Sensor, -rf_importance$Importance), ]

print(rf_importance)


############################################################
# 5. Plot PLS VIP scores
############################################################

ggplot(vip_scores, aes(x = reorder(Variable, VIP), y = VIP)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ Sensor, scales = "free_y") +
  theme_classic() +
  xlab("Predictor") +
  ylab("VIP score") +
  ggtitle("PLS VIP Scores")


############################################################
# 6. Plot RF variable importance
############################################################

ggplot(rf_importance, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ Sensor, scales = "free_y") +
  theme_classic() +
  xlab("Predictor") +
  ylab("Random Forest importance") +
  ggtitle("Random Forest Variable Importance")