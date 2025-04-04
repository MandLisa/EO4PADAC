


# check model diagnostics for spatial model
AIC(fit.ols)
AIC(fit.gam)
print(gwr_GW_temporal$GW.diagnostic$AIC)


# Extract R² and Adjusted R²
r2_ols <- summary(fit.ols)$r.squared
adj_r2_ols <- summary(fit.ols)$adj.r.squared


# Print summary of GAM model
summary(fit.gam)

# Extract R² and Adjusted R²
r2_gam <- summary(fit.gam)$r.sq  # Equivalent to R²
adj_r2_gam <- summary(fit.gam)$dev.expl  # Proportion of deviance explained (~adj. R²)

# Print values
cat("GAM R²:", r2_gam, "\n")
cat("GAM Adjusted R² (Deviance Explained):", adj_r2_gam, "\n")


library(spdep)

### spatial model
# Nearest neighbor weights für räumliche Autokorrelation
nb <- poly2nb(hexagons_recov10)  
lw <- nb2listw(nb)

# Moran's I für Residuen
moran.test(hexagons_recov10$resid_ols, lw)
moran.test(hexagons_recov10$resid_gam, lw)
moran.test(hexagons_recov10$resid_GWR, lw)


### temporal model
# Nearest neighbor weights für räumliche Autokorrelation
nb <- poly2nb(hexagons_recov10_temporal)  
lw <- nb2listw(nb)

# Extract coefficients for each predictor
gwr_results <- as.data.frame(gwr_GW_temporal$SDF)
hexagons_recov10_temporal$resid_GWR <- gwr_results$residual


# Moran's I für Residuen
moran.test(hexagons_recov10_temporal$resid_ols, lw)
moran.test(hexagons_recov10_temporal$resid_gam, lw)
moran.test(hexagons_recov10_temporal$resid_GWR, lw)




library(Metrics)

### ross validation for OLS
# K-fold Cross-Validation
set.seed(123)
k <- 10
folds <- sample(1:k, nrow(hexagons_recov10), replace = TRUE)

rmse_vals <- sapply(1:k, function(i) {
  test_idx <- which(folds == i)
  train_idx <- setdiff(1:nrow(hexagons_recov10), test_idx)
  
  # OLS Model
  fit_ols_cv <- lm(mean_percent_recovered ~ mean_elevation +
                      mean_severity + mean_VPD_yod1 + 
                      mean_prec_total + mean_temp_total + 
                      mean_pre_dist_tree_cover + mean_bare, 
                    data = hexagons_recov10[train_idx, ])
  
  preds_ols <- predict(fit_ols_cv, hexagons_recov10[test_idx, ])
  rmse(hexagons_recov10$mean_percent_recovered[test_idx], preds_ols)
})

mean(rmse_vals)



### cross validation for GAM

# Set parameters
set.seed(123)  # For reproducibility
k <- 10  # Number of folds
folds <- sample(1:k, nrow(hexagons_recov10), replace = TRUE)  # Assign random folds
rmse_vals_gam <- numeric(k)  # Store RMSE for each fold

for (i in 1:k) {
  train_idx <- which(folds != i)  # Training data indices
  test_idx <- which(folds == i)   # Testing data indices
  
  train_data <- hexagons_recov10[train_idx, ]  # Training set
  test_data <- hexagons_recov10[test_idx, ]    # Test set
  
  # Fit GAM model
  fit_gam_cv <- gam(mean_percent_recovered ~ 
                      s(mean_elevation) + s(mean_severity) + 
                      s(mean_VPD_yod1) + s(mean_prec_total) + 
                      s(mean_temp_total) + s(mean_pre_dist_tree_cover) + 
                      s(mean_bare), 
                    data = train_data)  # Train GAM on 9 folds
  
  # Predict on test set
  preds_gam <- predict(fit_gam_cv, newdata = test_data, type = "response")
  
  # Compute RMSE for this fold
  obs_gam <- test_data$mean_percent_recovered  # Actual values
  rmse_vals_gam[i] <- sqrt(mean((obs_gam - preds_gam)^2, na.rm = TRUE))  # RMSE calculation
}

# Compute final RMSE across all folds
mean_rmse_gam <- mean(rmse_vals_gam, na.rm = TRUE)

# Print result
print(paste("Mean RMSE for GAM (10-fold CV):", mean_rmse_gam))




# cross validation for GWR
set.seed(123)
k <- 10
folds <- sample(1:k, nrow(hexagons_sp), replace = TRUE)
rmse_vals_gwr <- numeric(k)

for (i in 1:k) {
  train_idx <- which(folds != i)  # Training set indices
  test_idx <- which(folds == i)   # Test set indices
  
  train_data <- hexagons_sp[train_idx, ]
  test_data <- hexagons_sp[test_idx, ]
  
  # Fit GWR model
  gwr_model <- gwr.basic(
    mean_percent_recovered ~ mean_elevation + 
      mean_severity + 
      mean_VPD_yod1 + 
      mean_prec_total +
      mean_temp_total +
      mean_pre_dist_tree_cover + 
      mean_bare,
    data = train_data,  
    bw = bw_GW_spatial,  
    kernel = "gaussian"
  )
  
  # Extract predictions
  pred_values <- gwr_model$SDF@data$yhat[test_idx]
  
  # Compute RMSE for this fold
  obs_values <- hexagons_sp$mean_percent_recovered[test_idx]
  rmse_vals_gwr[i] <- sqrt(mean((obs_values - pred_values)^2, na.rm = TRUE))
}

# Compute mean RMSE for GWR
mean_rmse_gwr <- mean(rmse_vals_gwr, na.rm = TRUE)
print(paste("Mean RMSE for GWR (10-fold CV):", mean_rmse_gwr))




#-------------------------------------------------------------------------------
### temporal model
#-------------------------------------------------------------------------------

library(Metrics)

### ross validation for OLS
# K-fold Cross-Validation
set.seed(123)
k <- 10
folds <- sample(1:k, nrow(hexagons_recov10_temporal), replace = TRUE)

rmse_vals <- sapply(1:k, function(i) {
  test_idx <- which(folds == i)
  train_idx <- setdiff(1:nrow(hexagons_recov10_temporal), test_idx)
  
  # OLS Model
  fit_ols_cv <- glm(percent_recovered_difference ~ mean_elevation_difference +
                      mean_severity_difference + 
                      mean_VPD_difference + 
                      mean_TC_difference + 
                      mean_prec_difference +
                      mean_temp_difference +
                      mean_bare_difference,
                    data = hexagons_recov10_temporal[train_idx, ])
  
  preds_ols <- predict(fit_ols_cv, hexagons_recov10_temporal[test_idx, ])
  rmse(hexagons_recov10_temporal$percent_recovered_difference[test_idx], preds_ols)
})

mean(rmse_vals)



### cross validation for GAM

# Set parameters
set.seed(123)  # For reproducibility
k <- 10  # Number of folds
folds <- sample(1:k, nrow(hexagons_recov10_temporal), replace = TRUE)  # Assign random folds
rmse_vals_gam <- numeric(k)  # Store RMSE for each fold

for (i in 1:k) {
  train_idx <- which(folds != i)  # Training data indices
  test_idx <- which(folds == i)   # Testing data indices
  
  train_data <- hexagons_recov10_temporal[train_idx, ]  # Training set
  test_data <- hexagons_recov10_temporal[test_idx, ]    # Test set
  
  # Fit GAM model
  fit_gam_cv <- gam(percent_recovered_difference ~ s(mean_elevation_difference) +
                      s(mean_severity_difference) + 
                      s(mean_VPD_difference) + 
                      s(mean_prec_difference) +
                      s(mean_temp_difference) +
                      s(mean_TC_difference) +
                      s(mean_bare_difference),
                    data = train_data) 
  
  # Predict on test set
  preds_gam <- predict(fit_gam_cv, newdata = test_data, type = "response")
  
  # Compute RMSE for this fold
  obs_gam <- test_data$percent_recovered_difference  # Actual values
  rmse_vals_gam[i] <- sqrt(mean((obs_gam - preds_gam)^2, na.rm = TRUE))  # RMSE calculation
}

# Compute final RMSE across all folds
mean_rmse_gam <- mean(rmse_vals_gam, na.rm = TRUE)

# Print result
print(paste("Mean RMSE for GAM (10-fold CV):", mean_rmse_gam))




# cross validation for GWR
set.seed(123)
k <- 10
folds <- sample(1:k, nrow(hexagons_recov10_temporal_sp), replace = TRUE)
rmse_vals_gwr <- numeric(k)

for (i in 1:k) {
  train_idx <- which(folds != i)  # Training set indices
  test_idx <- which(folds == i)   # Test set indices
  
  train_data <- hexagons_recov10_temporal_sp[train_idx, ]
  test_data <- hexagons_recov10_temporal_sp[test_idx, ]
  
  # Fit GWR model
  gwr_model <- gwr.basic(
    percent_recovered_difference ~ mean_elevation_difference + 
      mean_severity_difference + 
      mean_VPD_difference + 
      mean_TC_difference + 
      mean_bare_difference +
      mean_temp_difference +
      mean_prec_difference,
    data = train_data,  
    bw = bw_GW_temporal,  
    kernel = "gaussian"
  )
  
  # Extract predictions
  pred_values <- gwr_model$SDF@data$yhat[test_idx]
  
  # Compute RMSE for this fold
  obs_values <- hexagons_recov10_temporal_sp$percent_recovered_difference[test_idx]
  rmse_vals_gwr[i] <- sqrt(mean((obs_values - pred_values)^2, na.rm = TRUE))
}

# Compute mean RMSE for GWR
mean_rmse_gwr <- mean(rmse_vals_gwr, na.rm = TRUE)
print(paste("Mean RMSE for GWR (10-fold CV):", mean_rmse_gwr))





