# Load required libraries
library(raster)
library(tidyr)
library(dplyr)
library(sp)
library(sf)
library(ggplot2)
library(terra)
library(spatial)
library(readr)
library(spatstat)
library(pryr)
library(mgcv)
library(purrr)
library(readr)
library(mgcv)
library(stringr)
library(randomForest)
library(broom)

recovery_climate_filt <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate_topo_2.0_filt.csv")
recovery_climate_unique <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate_topo_2.0_unique.csv")


# Apply mean imputation only to numeric columns
recovery_imputed <- recovery_climate_unique %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Compute correlations
correlations <- recovery_imputed %>%
  select(recovery_rate, mean_VPD_pre, mean_VPD_post, mean_VPD_post_1_year, 
         mean_VPD_post_2_year, mean_VPD_post_3_year, VPD_consecutive_1y, 
         VPD_consecutive_2y, VPD_consecutive_3y) %>%
  cor(method = "pearson")  # Or use "spearman" if data is not normally distributed

print(correlations)



# Scatter plot
ggplot(recovery_imputed, aes(x = recovery_rate, y = mean_VPD_post_1_year)) +
  geom_point() +
  labs(title = "Scatter Plot of Recovery Time vs Mean VPD Post",
       x = "Recovery Time",
       y = "Mean VPD Post") +
  theme_minimal()



# Fit the model
model <- lm(recovery_rate ~ severity_relative + slope + height +
              aspect +
              mean_VPD_pre +
              VPD_consecutive_1y +
              VPD_consecutive_2y,
            data = recovery_imputed)
summary(model)


# Standardize the predictors
recovery_standardized <- recovery_imputed %>%
  mutate(across(c(severity_relative, slope, height, aspect, mean_VPD_pre, mean_VPD_post_1_year,
                  mean_VPD_post_2_year, mean_VPD_post_3_year,
                  VPD_consecutive_1y, VPD_consecutive_2y, VPD_consecutive_3y), scale))

# Fit the standardized model
model <- lm(recovery_rate ~ severity_relative + slope + height +
              aspect +
              mean_VPD_pre +
              VPD_consecutive_1y +
              VPD_consecutive_2y +
              VPD_consecutive_3y +
              mean_VPD_post_1_year +
              mean_VPD_post_2_year +
              mean_VPD_post_3_year,
            data = recovery_standardized)

summary(model)


# Extract the coefficients, standard errors, and confidence intervals
model_summary <- broom::tidy(model)

# Exclude the intercept
model_summary <- model_summary[model_summary$term != "(Intercept)", ]

# Reorder the predictors for better plotting (if needed)
model_summary$term <- factor(model_summary$term, levels = model_summary$term)

# Plot using ggplot2 with predictors on the y-axis and coefficients on the x-axis
ggplot(model_summary, aes(y = term, x = estimate)) +
  geom_point(size = 3) +  # Points for the estimates
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2) +  # Horizontal error bars for standard errors
  theme_minimal() +
  labs(x = "Estimated Coefficient", y = "Predictor",
       title = "Effect of Predictors on Recovery Rate") +
  theme(
    axis.text.y = element_text(hjust = 1, size = 14),        # Y-axis text size
    axis.text.x = element_text(size = 14),                    # X-axis text size
    axis.title.y = element_text(size = 16),                   # Y-axis title size
    axis.title.x = element_text(size = 16),                   # X-axis title size
    plot.title = element_text(size = 18, face = "bold")       # Plot title size
  )


### stratified by severity_class
# Fit separate models for each severity class
models <- recovery_standardized %>%
  group_by(severity_class) %>%
  do(model = lm(recovery_rate ~ 
                  severity_relative + 
                  slope + 
                  height +
                  aspect +
                  mean_VPD_pre +
                  VPD_consecutive_1y +
                  VPD_consecutive_2y +
                  VPD_consecutive_3y +
                  mean_VPD_post_1_year +
                  mean_VPD_post_2_year +
                  mean_VPD_post_3_year,
                  data = .))

# Extract coefficients and standard errors
model_summary_by_class <- models %>%
  rowwise() %>%
  do({
    summary_model <- summary(.$model)
    coef_df <- as.data.frame(summary_model$coefficients)
    coef_df$term <- rownames(coef_df)
    coef_df$severity_class <- .$severity_class
    coef_df
  }) %>%
  select(term, estimate = Estimate, std.error = `Std. Error`, severity_class) %>%
  filter(term != "(Intercept)")  # Remove intercept term if needed

# Example of filtering out NA values
model_summary_by_class <- model_summary_by_class %>%
  filter(!is.na(estimate) & !is.na(std.error) & !is.na(term) & !is.na(severity_class))

# Define custom colors
custom_colors <- c("non stand-replacing" = "blue",   # Example color for "Low"
                   "stand-replacing" = "red") # Example color for "Medium"


# Plotting
ggplot(model_summary_by_class, aes(y = term, x = estimate, color = severity_class)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2) +
  #scale_x_log10() +  # Apply log transformation to the x-axis
  theme_minimal() +
  scale_color_manual(values = custom_colors) +
  labs(x = "Estimated Coefficient", y = "Predictor",
       title = "") +
  theme(
    axis.text.y = element_text(hjust = 1, size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  )


# Custom function for symmetric log transformation
symmetric_log10 <- function(x, base = 10) {
  sign(x) * log10(abs(x) + 1) / log10(base)
}

# Define your custom order for predictors
custom_order <- c("height", "slope", "aspect", "severity_relative", 
                  "mean_VPD_pre", "mean_VPD_post_1_year", "mean_VPD_post_2_year",
                  "mean_VPD_post_3_year", "VPD_consecutive_1y", "VPD_consecutive_2y",
                  "VPD_consecutive_3y")

# Reorder the factor levels in your dataset
model_summary_by_class$term <- factor(model_summary_by_class$term, levels = custom_order)




# Plot with symmetric log transformation
ggplot(model_summary_by_class, aes(y = term, x = estimate, color = severity_class)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2) +
  scale_x_continuous(trans = scales::trans_new("symmetric_log10", 
                                               transform = symmetric_log10,
                                               inverse = function(x) 10^x - 1)) +
  theme_minimal() +
  scale_color_manual(values = custom_colors) +
  labs(x = "Estimated Coefficient (symmetric log scale)", y = "Predictor",
       title = "") +
  theme(
    axis.text.y = element_text(hjust = 1, size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  )


# Fit a Random Forest model
# Mean impute missing values for numeric columns only
recovery_imputed <- recovery_climate_unique %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Fit the Random Forest model on the cleaned data
set.seed(123)  # For reproducibility
rf_model <- randomForest(recovery_rate ~ severity_relative + slope + height +
                           aspect + mean_VPD_pre + VPD_consecutive_1y +
                           VPD_consecutive_2y + VPD_consecutive_3y,
                         data = recovery_imputed, 
                         importance = TRUE)


# Extract feature importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)

# Rename columns for clarity
colnames(importance_df) <- c("MeanDecreaseGini", "MeanDecreaseAccuracy", "Feature")

# Select the metric you want to plot, e.g., MeanDecreaseGini
importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini)) %>%  # Sort by importance
  mutate(Feature = factor(Feature, levels = Feature))  # Reorder factor levels for plotting

# Plot feature importance using ggplot2
ggplot(importance_df, aes(y = Feature, x = MeanDecreaseGini)) +
  geom_col(fill = "skyblue") +  # Bar plot with sky blue color
  theme_minimal() +
  labs(x = "Mean Decrease in Gini Index", y = "Feature",
       title = "Feature Importance from Random Forest Model") +
  theme(axis.text.y = element_text(size = 12))  # Adjust text size if needed






# Extract feature importance
importance_scores <- importance(rf_model)
importance_df <- as.data.frame(importance_scores)
importance_df$Feature <- rownames(importance_df)


# Plot feature importance
ggplot(importance_df, aes(x = reorder(Feature, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance from Random Forest",
       x = "Feature",
       y = "Importance (IncMSE)") +
  theme_minimal()



