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
library(ranger)
library(doParallel)
library(reshape2)
library(corrplot)
library(caret)

recovery_imputed_unique <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_imputed_unique.csv")


# Standardize the predictors
recovery_standardized <- recovery_imputed_unique %>%
  mutate(across(c(VPD_consecutive_1y, VPD_consecutive_yod, avg_tree_share_before,
                  mean_VPD_pre, mean_VPD_yod, mean_pre_dist_bare_land_share,
                  mean_pre_dist_grassland_share, mean_pre_dist_shrubland_share, 
                  mean_pre_dist_coniferous_woodland_share, mean_pre_dist_broadleaved_woodland_share,
                  VPD_autumn_yod, `VPD_autumn_yod+1`, VPD_spring_yod,`VPD_spring_yod+1`, VPD_summer_yod,
                  `VPD_summer_yod+1`, height, slope, severity_relative), scale))

### stratified by severity_class
# Fit separate models for each severity class
models <- recovery_standardized %>%
  group_by(severity_class) %>%
  do(model = lm(recovery_rate ~ VPD_consecutive_1y +
                  VPD_consecutive_yod +
                  avg_tree_share_before +
                  mean_VPD_yod +
                  mean_VPD_post_1_year +
                  mean_VPD_pre +
                  mean_pre_dist_bare_land_share +
                  mean_pre_dist_grassland_share +
                  mean_pre_dist_shrubland_share + 
                  mean_pre_dist_coniferous_woodland_share +
                  mean_pre_dist_broadleaved_woodland_share +
                  VPD_autumn_yod + 
                  `VPD_autumn_yod+1` +
                  VPD_spring_yod +
                  `VPD_spring_yod+1` +
                  VPD_summer_yod +
                  `VPD_summer_yod+1` +
                  height +
                  slope +
                  severity_relative,
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



# Custom function for symmetric log transformation
symmetric_log10 <- function(x, base = 10) {
  sign(x) * log10(abs(x) + 1) / log10(base)
}

# Define your custom order for predictors
#custom_order <- c("height", "slope", "aspect", "severity_relative", 
#"mean_VPD_pre","mean_VPD_yod", "mean_VPD_post_1_year", "VPD_consecutive_1y", "VPD_consecutive_2y",
#"VPD_consecutive_3y")

# Define your custom order for predictors
custom_order <- c("mean_VPD_pre", "mean_VPD_yod", "mean_VPD_post_1_year",
                  "VPD_consecutive_yod", "VPD_consecutive_1y",
                  "VPD_spring_yod", "`VPD_spring_yod+1`",
                  "VPD_summer_yod", "`VPD_summer_yod+1`",
                  "VPD_autumn_yod", "`VPD_autumn_yod+1`",
                  "avg_tree_share_before",
                  "mean_pre_dist_bare_land_share",
                  "mean_pre_dist_grassland_share",
                  "mean_pre_dist_shrubland_share",
                  "mean_pre_dist_coniferous_woodland_share",
                  "mean_pre_dist_broadleaved_woodland_share",
                  "height", "slope", "severity_relative")


# Reorder the factor levels in your dataset
model_summary_by_class$term <- factor(model_summary_by_class$term, levels = custom_order)




# Plot with symmetric log transformation
p <- ggplot(model_summary_by_class, aes(y = term, x = estimate, color = severity_class)) +
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

plot(p)

ggsave("~/eo_nas/EO4Alps/figs/effect_size.png", plot = p, width = 12, height = 6, dpi = 300)


#-------------------------------------------------------------------------------
### stratify model by disturbance agent

### stratified by severity_class
# Fit separate models for each severity class
models <- recovery_standardized %>%
  group_by(agent_name) %>%
  do(model = lm(recovery_rate ~ VPD_consecutive_1y +
                  VPD_consecutive_yod +
                  avg_tree_share_before +
                  mean_VPD_yod +
                  mean_VPD_post_1_year +
                  mean_VPD_pre +
                  mean_pre_dist_bare_land_share +
                  mean_pre_dist_grassland_share +
                  mean_pre_dist_shrubland_share + 
                  mean_pre_dist_coniferous_woodland_share +
                  mean_pre_dist_broadleaved_woodland_share +
                  VPD_autumn_yod + 
                  `VPD_autumn_yod+1` +
                  VPD_spring_yod +
                  `VPD_spring_yod+1` +
                  VPD_summer_yod +
                  `VPD_summer_yod+1` +
                  height +
                  slope +
                  severity_relative,
                data = .))

# Extract coefficients and standard errors
model_summary_by_class <- models %>%
  rowwise() %>%
  do({
    summary_model <- summary(.$model)
    coef_df <- as.data.frame(summary_model$coefficients)
    coef_df$term <- rownames(coef_df)
    coef_df$agent_name <- .$agent_name
    coef_df
  }) %>%
  select(term, estimate = Estimate, std.error = `Std. Error`, agent_name) %>%
  filter(term != "(Intercept)")  # Remove intercept term if needed

# Example of filtering out NA values
model_summary_by_class <- model_summary_by_class %>%
  filter(!is.na(estimate) & !is.na(std.error) & !is.na(term) & !is.na(agent_name))

# Define custom colors
custom_colors <- c("other" = "blue",
                  "Bark Beetle/Wind" ="#f4d03f",        # Example color for "Low"
                   "Fire" = "#78281f") # Example color for "Medium"



# Custom function for symmetric log transformation
symmetric_log10 <- function(x, base = 10) {
  sign(x) * log10(abs(x) + 1) / log10(base)
}

# Define your custom order for predictors
#custom_order <- c("height", "slope", "aspect", "severity_relative", 
#"mean_VPD_pre","mean_VPD_yod", "mean_VPD_post_1_year", "VPD_consecutive_1y", "VPD_consecutive_2y",
#"VPD_consecutive_3y")

# Define your custom order for predictors
custom_order <- c("mean_VPD_pre", "mean_VPD_yod", "mean_VPD_post_1_year",
                  "VPD_consecutive_yod", "VPD_consecutive_1y",
                  "VPD_spring_yod", "`VPD_spring_yod+1`",
                  "VPD_summer_yod", "`VPD_summer_yod+1`",
                  "VPD_autumn_yod", "`VPD_autumn_yod+1`",
                  "avg_tree_share_before",
                  "mean_pre_dist_bare_land_share",
                  "mean_pre_dist_grassland_share",
                  "mean_pre_dist_shrubland_share",
                  "mean_pre_dist_coniferous_woodland_share",
                  "mean_pre_dist_broadleaved_woodland_share",
                  "height", "slope", "severity_relative")


# Reorder the factor levels in your dataset
model_summary_by_class$term <- factor(model_summary_by_class$term, levels = custom_order)




# Plot with symmetric log transformation
p <- ggplot(model_summary_by_class, aes(y = term, x = estimate, color = agent_name)) +
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

plot(p)


ggsave("~/eo_nas/EO4Alps/figs/effect_size_agent.png", plot = p, width = 12, height = 6, dpi = 300)





#-------------------------------------------------------------------------------
### stratify model by geographical region

### stratified by severity_class
# Fit separate models for each severity class
models <- recovery_standardized %>%
  group_by(geoloc_name) %>%
  do(model = lm(recovery_rate ~ VPD_consecutive_1y +
                  VPD_consecutive_yod +
                  avg_tree_share_before +
                  mean_VPD_yod +
                  mean_VPD_post_1_year +
                  mean_VPD_pre +
                  mean_pre_dist_bare_land_share +
                  mean_pre_dist_grassland_share +
                  mean_pre_dist_shrubland_share + 
                  mean_pre_dist_coniferous_woodland_share +
                  mean_pre_dist_broadleaved_woodland_share +
                  VPD_autumn_yod + 
                  `VPD_autumn_yod+1` +
                  VPD_spring_yod +
                  `VPD_spring_yod+1` +
                  VPD_summer_yod +
                  `VPD_summer_yod+1` +
                  height +
                  slope +
                  severity_relative,
                data = .))

# Extract coefficients and standard errors
model_summary_by_class <- models %>%
  rowwise() %>%
  do({
    summary_model <- summary(.$model)
    coef_df <- as.data.frame(summary_model$coefficients)
    coef_df$term <- rownames(coef_df)
    coef_df$geoloc_name <- .$geoloc_name
    coef_df
  }) %>%
  select(term, estimate = Estimate, std.error = `Std. Error`, geoloc_name) %>%
  filter(term != "(Intercept)")  # Remove intercept term if needed

# Example of filtering out NA values
model_summary_by_class <- model_summary_by_class %>%
  filter(!is.na(estimate) & !is.na(std.error) & !is.na(term) & !is.na(geoloc_name))

# Define custom colors
custom_colors <- c("Northern West Alps" = "#2471a3",
                   "Southern West Alps" ="#dc7633",        # Example color for "Low"
                   "Northern East Alps" = "#27ae60",
                   "Central Alps" = "#34495e",
                   "Southern East Alps" = "#f1c40f") # Example color for "Medium"



# Custom function for symmetric log transformation
symmetric_log10 <- function(x, base = 10) {
  sign(x) * log10(abs(x) + 1) / log10(base)
}

# Define your custom order for predictors
#custom_order <- c("height", "slope", "aspect", "severity_relative", 
#"mean_VPD_pre","mean_VPD_yod", "mean_VPD_post_1_year", "VPD_consecutive_1y", "VPD_consecutive_2y",
#"VPD_consecutive_3y")

# Define your custom order for predictors
custom_order <- c("mean_VPD_pre", "mean_VPD_yod", "mean_VPD_post_1_year",
                  "VPD_consecutive_yod", "VPD_consecutive_1y",
                  "VPD_spring_yod", "`VPD_spring_yod+1`",
                  "VPD_summer_yod", "`VPD_summer_yod+1`",
                  "VPD_autumn_yod", "`VPD_autumn_yod+1`",
                  "avg_tree_share_before",
                  "mean_pre_dist_bare_land_share",
                  "mean_pre_dist_grassland_share",
                  "mean_pre_dist_shrubland_share",
                  "mean_pre_dist_coniferous_woodland_share",
                  "mean_pre_dist_broadleaved_woodland_share",
                  "height", "slope", "severity_relative")


# Reorder the factor levels in your dataset
model_summary_by_class$term <- factor(model_summary_by_class$term, levels = custom_order)




# Plot with symmetric log transformation
p <- ggplot(model_summary_by_class, aes(y = term, x = estimate, color = geoloc_name)) +
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

plot(p)


ggsave("~/eo_nas/EO4Alps/figs/effect_size_geoloc.png", plot = p, width = 12, height = 6, dpi = 300)


