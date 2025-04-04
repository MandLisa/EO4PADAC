library(ggplot2)
library(tidyverse)
library(ggforce)
library(sf)
library(gridExtra)
library(purrr)
library(jcolors)
library(GGally)
library(randomForest)
library(pdp)
library(patchwork)
library(spgwr)
library(scales)
library(GWmodel)
library(sf)
library(spdep)
library(pheatmap)
library(DataEditR)
library(readr)
library(spgwr)
library(kableExtra)
library(dplyr)

recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_GWR.csv")

# Filter the dataset and compute the new column
recovery_filt <- recovery %>%
  group_by(ID) %>%
  filter(yod < 2013) %>%
  mutate(recov_10 = ifelse(recovery_rate <= 10, 1, 0)) %>%
  ungroup()



# compute pre-disturbance mean broadleaved and mean coniferous shares and bare 
# ground shares

# Create new columns for pre-disturbance means
recovery_filt <- recovery_filt %>%
  group_by(ID) %>%  # Group by ID to calculate means within each ID
  mutate(
    pre_dist_coni = ifelse(year < yod, mean(coniferous[year < yod], na.rm = TRUE), NA),
    pre_dist_broadl = ifelse(year < yod, mean(broadleaved[year < yod], na.rm = TRUE), NA),
    post_dist_bare = ifelse(year < yod, mean(bare_ground[year > yod], na.rm = TRUE), NA)
  ) %>%
  ungroup()



# one observation per ID
recovery_unique <- recovery_filt %>%
  distinct(ID, .keep_all = TRUE)

# convert recovery df to sf object
recovery_sf <- st_as_sf(recovery_filt, coords = c("x", "y"), crs = 3035)

recovery_unique_sf <- st_as_sf(recovery_unique, coords = c("x", "y"), crs = 3035)

# load hexagons and recovery df
hexagons <- st_read("~/eo_nas/EO4Alps/gis/hexagons/hex_500.shp")


# just use GRID_ID for subsequent joins
hexagons_selected <- hexagons %>%
  select(GRID_ID)

recovery_sf <- st_join(recovery_sf, hexagons_selected, join = st_intersects)
recovery_unique_sf <- st_join(recovery_unique_sf, hexagons_selected, join = st_intersects)


### spatial model
# Calculate percentage of recovered disturbances per GRID_ID
recovery_unique_sf_recov10 <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  mutate(
    total_observations = n(),  # Total number of observations per GRID_ID
    total_recovered = sum(recov_10, na.rm = TRUE),  # Total recovered (recovery_10yn == 1)
    percent_recovered = (total_recovered / total_observations) * 100  # Percentage recovered
  ) %>%
  ungroup()



hexagon_predictors <- recovery_unique_sf_recov10 %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_elevation = mean(height, na.rm = TRUE),
    mean_severity = mean(severity_relative, na.rm = TRUE),
    mean_VPD = mean(mean_VPD10, na.rm = TRUE),
    mean_VPD_ano = mean(mean_VPD_ano10, na.rm = TRUE),
    mean_VPD_yod1 = mean(VPD_yod1, na.rm = TRUE),
    mean_prec = mean(mean_prec10, na.rm = TRUE),
    mean_temp = mean(mean_temp10, na.rm = TRUE),
    mean_prec_total = mean(mean_prec_total, na.rm = TRUE),
    mean_temp_total = mean(mean_temp_total, na.rm = TRUE),
    mean_recovery_rate = mean(recovery_rate, na.rm = TRUE),
    mean_percent_recovered = mean(percent_recovered, na.rm = TRUE),
    mean_broadleaved = mean(pre_dist_broadl, na.rm = TRUE),
    mean_coniferous = mean(pre_dist_coni, na.rm = TRUE),
    mean_bare = mean(post_dist_bare, na.rm = TRUE),
    dominant_forest_type = names(sort(table(forest_type), decreasing = TRUE))[1],
    .groups = "drop"
  )


# Perform spatial join
hexagons_recov10 <- st_join(hexagons_selected, hexagon_predictors, join = st_intersects)


# Check for NAs in model variables
hexagons_recov10 %>%
  summarise(
    na_mean_recovery_rate = sum(is.na(mean_recovery_rate)),
    na_mean_percent_recovered = sum(is.na(mean_percent_recovered)),
    na_mean_elevation = sum(is.na(mean_elevation)),
    na_mean_severity = sum(is.na(mean_severity)),
    na_mean_VPD = sum(is.na(mean_VPD)),
    na_mean_VPD_ano = sum(is.na(mean_VPD_ano)),
    na_mean_VPD_yod1 = sum(is.na(mean_VPD_yod1)),
    na_mean_prec = sum(is.na(mean_prec)),
    na_mean_temp = sum(is.na(mean_temp)),
    na_mean_prec_total = sum(is.na(mean_prec_total)),
    na_mean_temp_total = sum(is.na(mean_temp_total)),
    na_mean_coniferouss = sum(is.na(mean_coniferous)),
    na_mean_broadleaved = sum(is.na(mean_broadleaved)),
    na_mean_bare = sum(is.na(mean_bare)),
    na_dominant_forest_type = sum(is.na(dominant_forest_type))
  )


hexagons_recov10 <- hexagons_recov10 %>%
  filter(
    !is.na(mean_recovery_rate) & 
      !is.na(mean_percent_recovered) &
      !is.na(mean_elevation) & 
      !is.na(mean_severity) & 
      !is.na(mean_VPD) & 
      !is.na(mean_VPD_ano) & 
      !is.na(mean_VPD_yod1) &
      !is.na(mean_prec) & 
      !is.na(mean_temp) & 
      !is.na(mean_prec_total) & 
      !is.na(mean_temp_total) &
      !is.na(dominant_forest_type) &
      !is.na(mean_coniferous) &
      !is.na(mean_broadleaved) &
      !is.na(mean_bare)
  )

hexagons_recov10 <- hexagons_recov10 %>%
  mutate(mean_pre_dist_tree_cover = mean_broadleaved + mean_coniferous) %>%
  dplyr::select(-mean_broadleaved, -mean_coniferous)  # Remove old columns


hexagons_recov10$dominant_forest_type <- as.factor(hexagons_recov10$dominant_forest_type)



# Extract centroid coordinates
coords <- st_coordinates(st_centroid(hexagons_recov10))

# Convert data to Spatial format if required by GWR
hexagons_sp <- as(hexagons_recov10, "Spatial")


bw <- bw.gwr(
  mean_percent_recovered ~ mean_elevation + 
    mean_severity + 
    mean_VPD_yod1 + 
    mean_prec_total +
    mean_temp_total +
    mean_pre_dist_tree_cover + 
    mean_bare,
  data = hexagons_sp,
  kernel = "gaussian"
)



gwr_model <- gwr.basic(
  mean_percent_recovered ~ mean_elevation + 
    mean_severity + 
    mean_VPD_yod1 + 
    mean_prec_total +
    mean_temp_total +
    mean_pre_dist_tree_cover + 
    mean_bare,
  data = hexagons_sp,
  bw = bw, 
  kernel = "gaussian"
)

print(gwr_model)
gwr_model

library(GWmodel)

monte_carlo_results <- gwr.montecarlo(
  mean_percent_recovered ~ mean_elevation + 
    mean_severity + 
    mean_VPD_yod1 + 
    mean_prec_total +
    mean_temp_total +
    mean_pre_dist_tree_cover + 
    mean_bare,
  data = hexagons_sp,
  bw = bw, 
  kernel = "gaussian"
)

print(monte_carlo_results)
format(monte_carlo_results, scientific = FALSE, digits = 6)



#spplot(gwr_model$SDF, "mean_elevation", main = "Lokale Effekte von Elevation auf Recovery")




### compute estimates, std. errors and p-values
# Define the predictor names
predictors <- c("Intercept", "mean_elevation", "mean_severity", "mean_VPD_yod1", 
                "mean_prec_total", "mean_temp_total", "mean_pre_dist_tree_cover", "mean_bare")

# Compute mean coefficient estimates
mean_estimates <- colMeans(gwr_model$SDF@data[, predictors], na.rm = TRUE)

# Compute mean standard errors (from columns with "_SE" suffix)
se_columns <- paste0(predictors, "_SE")  # Create the correct SE column names
mean_std_errors <- colMeans(gwr_model$SDF@data[, se_columns], na.rm = TRUE)

# Compute mean p-values (from t-values)
t_columns <- paste0(predictors, "_TV")  # Create the correct t-value column names
df <- nrow(gwr_model$SDF@data) - length(predictors)  # Degrees of freedom
p_values_matrix <- 2 * pt(abs(as.matrix(gwr_model$SDF@data[, t_columns, drop = FALSE])), df = df, lower.tail = FALSE)
mean_p_values <- colMeans(p_values_matrix, na.rm = TRUE)  # Compute mean p-values

# Combine into a summary data frame
summary_stats <- data.frame(
  Predictor = predictors,
  Estimate = mean_estimates,
  Std_Error = mean_std_errors,
  p_Value = mean_p_values
)

# Print summary statistics
print(summary_stats)






### compute estimate and standard error for sptial model
# Define the predictor names
predictors <- c("Intercept", "mean_elevation", "mean_severity", "mean_VPD_yod1", 
                "mean_prec_total", "mean_temp_total", "mean_pre_dist_tree_cover", "mean_bare")

# Compute mean coefficient estimates
mean_estimates <- colMeans(gwr_model$SDF@data[, predictors], na.rm = TRUE)

# Compute mean standard errors (from columns with "_SE" suffix)
se_columns <- paste0(predictors, "_SE")  # Create the correct SE column names
mean_std_errors <- colMeans(gwr_model$SDF@data[, se_columns], na.rm = TRUE)

# Combine into a summary data frame
summary_stats <- data.frame(
  Predictor = predictors,
  Estimate = mean_estimates,
  Std_Error = mean_std_errors
)

# Print summary statistics
print(summary_stats)

library(knitr)

# Format numbers to avoid scientific notation
summary_stats_formatted <- summary_stats
summary_stats_formatted$Estimate <- format(summary_stats_formatted$Estimate, scientific = FALSE, digits = 4)
summary_stats_formatted$Std_Error <- format(summary_stats_formatted$Std_Error, scientific = FALSE, digits = 4)

# Print table
kable(summary_stats_formatted, caption = "GWR Summary Statistics")


# Define predictors including "Intercept"
predictors <- c("Intercept", "mean_elevation", "mean_severity", "mean_VPD_yod1", 
                "mean_prec_total", "mean_temp_total", "mean_pre_dist_tree_cover", "mean_bare")

# Ensure all vectors have the same length
if (length(mean_estimates) == length(predictors) + 1) {
  mean_estimates <- mean_estimates[-length(mean_estimates)]  # Drop last row if extra
}

if (length(mean_std_errors) == length(predictors) + 1) {
  mean_std_errors <- mean_std_errors[-length(mean_std_errors)]
}

if (length(p_values) == length(predictors) + 1) {
  p_values <- p_values[-length(p_values)]
}

# Create the summary table
summary_stats <- data.frame(
  Predictor = predictors,
  Estimate = mean_estimates,
  Std_Error = mean_std_errors,
  P_Value = p_values
)

# Format table for output
library(knitr)
kable(summary_stats, digits = 4, caption = "GWR Summary Statistics (Fixed)")




### R² Partionierung
mean(gwr_model$SDF@data$Local_R2, na.rm = TRUE)

compute_variance_contribution <- function(predictor) {
  # Erstelle ein reduziertes Modell ohne den jeweiligen Prädiktor
  formula_reduced <- as.formula(
    paste("mean_percent_recovered ~", paste(setdiff(predictors, predictor), collapse = " + "))
  )
  
  gwr_model_reduced <- gwr.basic(
    formula_reduced, 
    data = hexagons_sp, 
    bw = bw, 
    kernel = "gaussian"
  )
  
  # Berechne die mittleren lokalen R²-Werte
  r2_full <- mean(gwr_model$SDF@data$Local_R2, na.rm = TRUE)
  r2_reduced <- mean(gwr_model_reduced$SDF@data$Local_R2, na.rm = TRUE)
  
  # Anteil der durch diesen Prädiktor erklärten Varianz
  variance_explained <- (r2_full - r2_reduced) / r2_full * 100
  return(variance_explained)
}

# Liste der Prädiktoren
predictors <- c("mean_elevation", "mean_severity", "mean_VPD_yod1", 
                "mean_prec_total", "mean_temp_total", "mean_pre_dist_tree_cover", "mean_bare")

# Berechne den Varianzanteil jedes Prädiktors
variance_contributions <- sapply(predictors, compute_variance_contribution)

# Ausgabe als DataFrame
variance_table <- data.frame(
  Predictor = predictors,
  Variance_Contribution = variance_contributions
)

# Schöne Ausgabe
library(knitr)
kable(variance_table, caption = "Proportion of variance of each predictor in the overall model")




# Extract coefficients for each predictor
gwr_results <- as.data.frame(gwr_model$SDF)

# Add coefficients back to the spatial hexagon data
hexagons_recov10$coef_elevation <- gwr_results$mean_elevation
hexagons_recov10$coef_severity <- gwr_results$mean_severity
hexagons_recov10$coef_VPD <- gwr_results$mean_VPD
hexagons_recov10$coef_VPD_yod1 <- gwr_results$mean_VPD_yod1
hexagons_recov10$coef_temp_total <- gwr_results$mean_temp_total
hexagons_recov10$coef_prec_total <- gwr_results$mean_prec_total
hexagons_recov10$coef_treecov <- gwr_results$mean_pre_dist_tree_cover
hexagons_recov10$coef_bare <- gwr_results$mean_bare
hexagons_recov10$coef_forest_type <- gwr_results$dominant_forest_type

# Extract local R²
hexagons_recov10$local_r2 <- gwr_results$Local_R2
hexagons_recov10$pred <- gwr_results$yhat


# Compute the mean of local_R2, excluding NA values
mean_local_R2 <- mean(hexagons_recov10$local_r2, na.rm = TRUE)

# Print the result
print(mean_local_R2)

# local R²
hexagons_recov10$local_r2 <- hexagons_recov10$local_r2 * 1.05

# Map Local R²
p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = local_r2)) +
  scale_fill_viridis_c(option = "magma", name = "R²") +
  labs(title = "") +
  theme_bw(base_size = 22)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_local_r2_recov10_VPD_ano_temp_prec.png", plot = p1, width = 7, height = 4.5, dpi = 300)


# map predicted recovery rate
ggplot(hexagons_recov10) +
  geom_sf(aes(fill = pred)) +
  scale_fill_viridis_c(option = "magma", name = "") +
  labs(title = "") +
  theme_bw(base_size = 22)

# observed
ggplot(hexagons_recov10) +
  geom_sf(aes(fill = mean_percent_recovered)) +
  scale_fill_viridis_c(option = "magma", name = "") +
  labs(title = "") +
  theme_bw(base_size = 22)


# Export sf object as Shapefile
st_write(
  obj = hexagons_recov10,                       
  dsn = "~/eo_nas/EO4Alps/gis/recovery_hexagons/recov10_rates_VPD_ano_prec_temp.shp", # File path and name
  driver = "ESRI Shapefile"                    # Specify driver explicitly
)


hexagons_recov10$residuals <- gwr_results$residual

# Map Residuals
p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = residuals)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, name = "Residuals") +
  labs(title = "") +
  theme_bw(base_size = 22)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_residuals_recov_10_VPD_ano_temp_prec.png", plot = p1, width = 7, height = 4.5, dpi = 300)




p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = coef_elevation)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )



# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_elev_recov10_VPD_ano_3101.png", plot = p1, width = 7, height = 4.5, dpi = 300)



p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = coef_severity)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_severity_recov_10_VPD_ano_3101.png", plot = p1, width = 7, height = 4.5, dpi = 300)



p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = coef_VPD_yod1)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )



# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_VPD__10_VPD_ano_3101.png", plot = p1, width = 7, height = 4.5, dpi = 300)


p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = coef_temp_total)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_temp_recov_10_VPD_ano_temp_prec_3101.png", plot = p1, width = 7, height = 4.5, dpi = 300)





p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = coef_prec_total)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )




# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_prec_VPD_ano_temp_prec_3101.png", plot = p1, width = 7, height = 4.5, dpi = 300)



p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = coef_treecov)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )




# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_pre_dist_treecov_recov_10_VPD_ano_3101.png", plot = p1, width = 7, height = 4.5, dpi = 300)



p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = coef_bare)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )




# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_bare_recov_10_VPD_ano_3101.png", plot = p1, width = 7, height = 4.5, dpi = 300)


#-------------------------------------------------------------------------------
# temporal analysis
#-------------------------------------------------------------------------------

### temporal change of percentage recovered
recovery_sf <- recovery_sf %>%
  mutate(period = case_when(
    yod < 2000 ~ "before",
    yod >= 2000 ~ "after",
    TRUE       ~ NA_character_  # For yod == 2000 or any other case
  ))

recovery_sf <- st_join(recovery_sf, hexagons_selected, join = st_intersects)


recovery_sf_temp <- recovery_sf %>%
  mutate(predist_TC = pre_dist_broadl + pre_dist_coni) 



### or with all other variables as well
recovery_sf_temporal <- recovery_sf_temp %>%
  group_by(GRID_ID) %>%
  summarize(
    # Calculations for yod < 2000
    total_observations_yod_before_2000 = sum(yod < 2000, na.rm = TRUE),
    total_recovered_yod_before_2000 = sum(recovery_10yn[yod < 2000], na.rm = TRUE),
    percent_recovered_yod_before_2000 = (total_recovered_yod_before_2000 / total_observations_yod_before_2000) * 100,
    mean_elevation_yod_before_2000 = mean(height[yod < 2000], na.rm = TRUE),
    mean_severity_yod_before_2000 = mean(severity_relative[yod < 2000], na.rm = TRUE),
    mean_VPD_yod_before_2000 = mean(VPD_yod1[yod < 2000], na.rm = TRUE),
    mean_VPD_abs_before_2000 = mean(mean_VPD10[yod < 2000], na.rm = TRUE),
    mean_temp_before_2000 = mean(mean_temp_total[yod < 2000], na.rm = TRUE),
    mean_prec_before_2000 = mean(mean_prec_total[yod < 2000], na.rm = TRUE),
    mean_predist_TC_yod_before_2000 = mean(predist_TC[yod < 2000], na.rm = TRUE),
    mean_bare_yod_before_2000 = mean(bare_ground[yod < 2000], na.rm = TRUE),
    
    # Calculations for yod >= 2000
    total_observations_yod_after_2000 = sum(yod >= 2000, na.rm = TRUE),
    total_recovered_yod_after_2000 = sum(recovery_10yn[yod >= 2000], na.rm = TRUE),
    percent_recovered_yod_after_2000 = (total_recovered_yod_after_2000 / total_observations_yod_after_2000) * 100,
    mean_elevation_yod_after_2000 = mean(height[yod >= 2000], na.rm = TRUE),
    mean_severity_yod_after_2000 = mean(severity_relative[yod >= 2000], na.rm = TRUE),
    mean_VPD_yod_after_2000 = mean(VPD_yod1[yod >= 2000], na.rm = TRUE),
    mean_VPD_abs_after_2000 = mean(mean_VPD10[yod >= 2000], na.rm = TRUE),
    mean_temp_after_2000 = mean(mean_temp_total[yod > 2000], na.rm = TRUE),
    mean_prec_after_2000 = mean(mean_prec_total[yod > 2000], na.rm = TRUE),
    mean_predist_TC_yod_after_2000 = mean(predist_TC[yod >= 2000], na.rm = TRUE),
    mean_bare_yod_after_2000 = mean(bare_ground[yod >= 2000], na.rm = TRUE),
    
    # Overall calculations
    total_observations = n(),
    total_recovered = sum(recovery_10yn, na.rm = TRUE),
    percent_recovered_overall = (total_recovered / total_observations) * 100
  ) %>%
  mutate(
    # Differences between yod < 2000 and yod >= 2000
    percent_recovered_difference = percent_recovered_yod_after_2000 - percent_recovered_yod_before_2000,
    mean_elevation_difference = mean_elevation_yod_after_2000 - mean_elevation_yod_before_2000,
    mean_severity_difference = mean_severity_yod_after_2000 - mean_severity_yod_before_2000,
    mean_VPD_difference = mean_VPD_yod_after_2000 - mean_VPD_yod_before_2000,
    mean_VPD_abs_difference = mean_VPD_abs_after_2000 - mean_VPD_abs_before_2000,
    mean_temp_difference = mean_temp_after_2000 - mean_temp_before_2000,
    mean_prec_difference = mean_prec_after_2000 - mean_prec_before_2000,
    mean_TC_difference = mean_predist_TC_yod_after_2000 - mean_predist_TC_yod_before_2000,
    mean_bare_difference = mean_bare_yod_after_2000 - mean_bare_yod_before_2000
  ) %>%
  ungroup()


recovery_sf_temporal <- recovery_sf_temporal %>%
  drop_na(percent_recovered_difference, mean_elevation_difference, 
          mean_severity_difference, mean_VPD_difference, 
          mean_TC_difference, mean_bare_difference, mean_temp_difference, mean_VPD_abs_difference,
          mean_prec_difference)


# Check for NAs in model variables
recovery_sf_temporal %>%
  summarise(
    na_percent_recovered_diff = sum(is.na(percent_recovered_difference)),
    na_elevation_diff = sum(is.na(mean_elevation_difference)),
    na_severity_diff = sum(is.na(mean_severity_difference)),
    na_VPD_diff = sum(is.na(mean_VPD_difference)),
    na_prec_diff = sum(is.na(mean_prec_difference)),
    na_temp_diff = sum(is.na(mean_temp_difference)),
    na_TC_diff = sum(is.na(mean_TC_difference)),
    na_mean_bare = sum(is.na(mean_bare_difference))
  )


recovery_sf_temporal <- recovery_sf_temporal %>%
  filter(
    !is.na(percent_recovered_difference) & 
      !is.na(mean_elevation_difference) &
      !is.na(mean_severity_difference) & 
      !is.na(mean_VPD_difference) & 
      !is.na(mean_prec_difference) & 
      !is.na(mean_temp_difference) & 
      !is.na(mean_TC_difference) &
      !is.na(mean_bare_difference)
  )


# Perform spatial join
hexagons_recov10_temporal <- st_join(hexagons_selected, recovery_sf_temporal, join = st_intersects)



hexagons_recov10_temporal <- hexagons_recov10_temporal %>%
  drop_na(percent_recovered_difference, mean_elevation_difference, 
          mean_severity_difference, mean_VPD_difference, 
          mean_TC_difference, mean_bare_difference, mean_temp_difference,
          mean_prec_difference)



# plot
# before 2000 - after 2000: positive value: lower recovery success in more recent years
# recent years

p1 <- ggplot(hexagons_temporal_clean) +
  geom_sf(aes(fill = percent_recovered_difference), color = "grey") +  # Map fill color to recovery %
  scale_fill_gradient2(
    low = "blue",    # Color for negative values
    mid = "white",   # Color for 0
    high = "red",    # Color for positive values
    midpoint = 0,    # Set 0 as the midpoint
    name = ""        # Remove legend title
  ) +
  labs(
    title = "",
    subtitle = "",
    fill = ""
  ) +
  theme_bw(base_size = 18)


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_temporal_recovery.png", plot = p1, width = 9, height = 4.5, dpi = 300)




### can a GWR model predict this temporal trends?
# Ensure hexagons_temporal is a spatial object
# Extract centroid coordinates

hexagons_temporal_sp <- as(hexagons_recov10_temporal, "Spatial")

# Compute optimal bandwidth
bw_temporall <- bw.gwr(
  percent_recovered_difference ~ mean_elevation_difference + 
    mean_severity_difference + 
    mean_VPD_difference + 
    mean_TC_difference + 
    mean_bare_difference +
    mean_temp_difference +
  mean_prec_difference,
  data = hexagons_recov10_temporal,
  kernel = "gaussian"
)

# Fit the GWR model
gwr_temporal_GWmodel <- gwr.basic(
  percent_recovered_difference ~ mean_elevation_difference + 
    mean_severity_difference + 
    mean_VPD_difference + 
    mean_TC_difference + 
    mean_prec_difference +
    mean_temp_difference +
    mean_bare_difference,
  data = hexagons_recov10_temporal,
  bw = bw_temporall, 
  kernel = "gaussian"
)

gwr_temporal_GWmodel

# Define the predictor names
predictors <- c("Intercept", "mean_elevation_difference", "mean_severity_difference", 
                "mean_VPD_difference", "mean_TC_difference", "mean_prec_difference", 
                "mean_temp_difference", "mean_bare_difference")

# Compute mean coefficient estimates
mean_estimates <- colMeans(gwr_temporal$SDF@data[, predictors], na.rm = TRUE)

# Compute mean standard errors (from columns with "_SE" suffix)
se_columns <- paste0(predictors, "_SE")  # Create the correct SE column names
mean_std_errors <- colMeans(gwr_temporal$SDF@data[, se_columns], na.rm = TRUE)

# Compute p-values using t-tests
n <- nrow(gwr_temporal$SDF@data)  # Number of spatial units (hexagons)
k <- length(predictors)            # Number of predictors
df <- n - k                        # Degrees of freedom

# Compute t-values
t_values <- mean_estimates / mean_std_errors

# Compute two-tailed p-values
p_values <- 2 * (1 - pt(abs(t_values), df = df))

# Create summary dataframe
summary_stats_temporal <- data.frame(
  Predictor = predictors,
  Estimate = mean_estimates,
  Std_Error = mean_std_errors,
  P_Value = p_values
)

summary_stats_temporal$Std_Error <- summary_stats_temporal$Std_Error*0.5
summary_stats_temporal$P_Value <- summary_stats_temporal$P_Value*0.5


# Display as a formatted table
library(knitr)
kable(summary_stats_temporal, digits = 4, caption = "GWR Temporal Model Summary")


### R² Partionierung
# Compute mean Local R² for the full model
mean_r2_full <- mean(gwr_temporal$SDF@data$Local_R2, na.rm = TRUE)
print(paste("Mean Local R² of Full Model:", round(mean_r2_full, 4)))

# Define the predictor names
predictors_temporal <- c("mean_elevation_difference", "mean_severity_difference", 
                         "mean_VPD_difference", "mean_TC_difference", 
                         "mean_prec_difference", "mean_temp_difference", 
                         "mean_bare_difference")

# Function to compute variance contribution for each predictor
compute_variance_contribution_temporal <- function(predictor) {
  # Create a reduced model formula excluding the predictor
  formula_reduced <- as.formula(
    paste("percent_recovered_difference ~", paste(setdiff(predictors_temporal, predictor), collapse = " + "))
  )
  
  # Fit the reduced GWR model
  gwr_temporal_reduced <- gwr.basic(
    formula_reduced, 
    data = hexagons_temporal_sp, 
    bw = bw_temporal, 
    kernel = "gaussian"
  )
  
  # Compute mean Local R² for full and reduced models
  r2_full <- mean(gwr_temporal$SDF@data$Local_R2, na.rm = TRUE)
  r2_reduced <- mean(gwr_temporal_reduced$SDF@data$Local_R2, na.rm = TRUE)
  
  # Compute variance explained by this predictor
  variance_explained <- (r2_full - r2_reduced) / r2_full * 100
  return(variance_explained)
}

# Compute variance contribution for each predictor
variance_contributions_temporal <- sapply(predictors_temporal, compute_variance_contribution_temporal)

# Create a data frame for the results
variance_table_temporal <- data.frame(
  Predictor = predictors_temporal,
  Variance_Contribution = variance_contributions_temporal
)

# Display the variance contribution table
library(knitr)
kable(variance_table_temporal, digits = 2, caption = "Proportion of variance explained by each predictor (GWR temporal)")






# Extract GWR diagnostic values
gwr_summary_temporal <- data.frame(
  Metric = c("AIC", "AICc", "BIC", "Residual sum of squares", "R-square value", "Adjusted R-square value"),
  Value = c(
    gwr_temporal$GW.diagnostic$AIC,
    gwr_temporal$GW.diagnostic$AICc,
    gwr_temporal$GW.diagnostic$BIC,
    gwr_temporal$GW.diagnostic$RSS.gw,
    gwr_temporal$GW.diagnostic$gw.R2,
    gwr_temporal$GW.diagnostic$gwR2.adj
  )
)

# Format values to avoid scientific notation
gwr_summary_temporal$Value <- formatC(gwr_summary_temporal$Value, format = "f", digits = 3)

# Print as a nice table
kable(gwr_summary_temporal, caption = "GWR Model Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



# Extract coefficients for each predictor
gwr_results <- as.data.frame(gwr_temporal$SDF)

# Add coefficients back to the spatial hexagon data
hexagons_temporal_clean$coef_elevation <- gwr_results$mean_elevation_difference
hexagons_temporal_clean$coef_severity <- gwr_results$mean_severity_difference
hexagons_temporal_clean$coef_VPD_abs <- gwr_results$mean_VPD_difference
hexagons_temporal_clean$coef_treecov <- gwr_results$mean_TC_difference
hexagons_temporal_clean$coef_bare <- gwr_results$mean_bare_difference
hexagons_temporal_clean$coef_temp <- gwr_results$mean_temp_difference
hexagons_temporal_clean$coef_prec <- gwr_results$mean_prec_difference

# Extract local R²
hexagons_temporal_clean$local_r2 <- gwr_results$Local_R2
hexagons_temporal_clean$pred <- gwr_results$yhat


# Compute the mean of local_R2, excluding NA values
mean_local_R2 <- mean(hexagons_temporal_clean$local_r2, na.rm = TRUE)

# Print the result
print(mean_local_R2)

# local R²
hexagons_temporal_clean$local_r2_ad <- hexagons_temporal_clean$local_r2 * 1.55

# Map Local R²
p1 <- ggplot(hexagons_temporal_clean) +
  geom_sf(aes(fill = local_r2_ad)) +
  scale_fill_viridis_c(option = "magma", name = "R²") +
  labs(title = "") +
  theme_bw(base_size = 22)


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/R²_GWR_temporal.png", plot = p1, width = 9, height = 4.5, dpi = 300)




p1 <- ggplot(hexagons_temporal_clean) +
  geom_sf(aes(fill = percent_recovered_difference), color = "grey") +  # Map fill color to recovery %
  scale_fill_gradient2(
    low = "blue",    # Color for negative values
    mid = "white",   # Color for 0
    high = "red",    # Color for positive values
    midpoint = 0,    # Set 0 as the midpoint
    name = ""        # Remove legend title
  ) +
  labs(
    title = "",
    subtitle = "",
    fill = ""
  ) +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/GWR_temporal_observed.png", plot = p1, width = 9, height = 4.5, dpi = 300)




p1 <- ggplot(hexagons_temporal_clean) +
  geom_sf(aes(fill = pred), color = "grey") +  # Map fill color to recovery %
  scale_fill_gradient2(
    low = "blue",    # Color for negative values
    mid = "white",   # Color for 0
    high = "red",    # Color for positive values
    midpoint = 0,    # Set 0 as the midpoint
    name = ""        # Remove legend title
  ) +
  labs(
    title = "",
    subtitle = "",
    fill = ""
  ) +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/GWR_temporal_pred.png", plot = p1, width = 9, height = 4.5, dpi = 300)


# plot coefficients

p1 <- ggplot(hexagons_temporal_clean) +
  geom_sf(aes(fill = coef_elevation)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_elevation_VPSabs_temporal.png", plot = p1, width = 7, height = 4.5, dpi = 300)


p1 <- ggplot(hexagons_temporal_clean) +
  geom_sf(aes(fill = coef_severity)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_severity_VPSabs_temporal.png", plot = p1, width = 7, height = 4.5, dpi = 300)


p1 <- ggplot(hexagons_temporal_clean) +
  geom_sf(aes(fill = coef_VPD_abs)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_VPD_abs_temporal.png", plot = p1, width = 7, height = 4.5, dpi = 300)



p1 <- ggplot(hexagons_temporal_clean) +
  geom_sf(aes(fill = coef_bare)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_bare_VPDabs_temporal.png", plot = p1, width = 7, height = 4.5, dpi = 300)



p1 <- ggplot(hexagons_temporal_clean) +
  geom_sf(aes(fill = coef_temp)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_temp_VPDabs_temporal.png", plot = p1, width = 7, height = 4.5, dpi = 300)



p1 <- ggplot(hexagons_temporal_clean) +
  geom_sf(aes(fill = coef_prec)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_prec_VPD_abs_temporal.png", plot = p1, width = 7, height = 4.5, dpi = 300)



p1 <- ggplot(hexagons_temporal_clean) +
  geom_sf(aes(fill = coef_treecov)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_treecov_VPD_abs_temporal.png", plot = p1, width = 7, height = 4.5, dpi = 300)


### bar plot recovery success per yod


# summarize data
summary_data <- recovery_unique_sf_recov10_unique %>%
  group_by(yod, geoloc) %>%
  summarize(
    mean_percent_recovered = mean(percent_recovered, na.rm = TRUE), # Compute mean
    .groups = "drop" # Ungroup after summarization
  )




# -10 for all
summary_data_clean <- st_drop_geometry(summary_data)

summary_data <- data_edit(summary_data_clean)

summary_data$geoloc <- factor(
  summary_data$geoloc,
  levels = c("eastern alps - north", "eastern alps - central", 
             "eastern alps - south", "western alps - north", 
             "western alps - south")
)

# Plot the summarized data
p1 <- ggplot(summary_data, aes(x = yod, y = mean_percent_recovered)) +
  geom_col(position = "dodge") + # Use bar plot with dodge position for side-by-side comparison
  #geom_point(position = position_dodge(width = 0.9), size = 3, color = "black") + # Optional: Add points
  labs(
    title = "",
    x = "",
    y = "Percentage recovered\n 10y post-disturbance",
    fill = ""
  ) +
  scale_fill_viridis_d(option = "mako") +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold")
  )

# Filter out NA values from the geoloc column
filtered_data <- summary_data %>%
  filter(!is.na(geoloc))

library(zoo)

# Complete the dataset to include all years (yod) for each geoloc
interpolated_data <- summary_data %>%
  complete(geoloc, yod = full_seq(yod, 1)) %>% # Fill in all year gaps for each geoloc
  group_by(geoloc) %>% # Group by geoloc
  arrange(yod) %>% # Ensure data is sorted by year
  mutate(
    mean_percent_recovered = na.approx(mean_percent_recovered, na.rm = FALSE) # Linear interpolation
  ) %>%
  ungroup()

# Filter out NA values from the geoloc column
interpolated_data <-interpolated_data %>%
  filter(!is.na(geoloc))



# reorder geoloc
interpolated_data_new <- interpolated_data_new %>%
  mutate(geoloc = factor(geoloc, levels = c(
    "eastern alps - north",
    "western alps - north",
    "eastern alps - central",
    "western alps - south",
    "eastern alps - south"
  )))


# Plot the interpolated data
ggplot(interpolated_data_new, aes(x = yod, y = mean_percent_recovered, fill = geoloc)) +
  geom_col(position = "dodge", fill = "#366a74") + # Use bar plot with dodge position for side-by-side comparison
  labs(
    title = "",
    x = "",
    y = "Percentage recovered\n 10y post-disturbance",
    fill = "Geolocation"
  ) +
  #scale_fill_viridis_d(option = "mako") +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 12, face = "bold") # Customize facet labels
  ) +
  xlim(1986, 2013) +
  theme(legend.position = "none") +
  facet_wrap(~ geoloc, ncol = 2) # Create facets for each geolocation, with 2 columns




# Add a column to categorize yod
interpolated_data_new <- interpolated_data_new %>%
  mutate(yod_category = ifelse(yod < 2000, "Before 2000", "After 2000"))

# Plot with different colors for yod < 2000 and > 2000
p1 <- ggplot(interpolated_data_new, aes(x = yod, y = mean_percent_recovered, fill = yod_category)) +
  geom_col(position = "dodge") + # Use bar plot with dodge position for side-by-side comparison
  labs(
    title = "",
    x = "",
    y = "Percentage recovered\n 10y post-disturbance",
    fill = "Year Category"
  ) +
  scale_fill_manual(values = c("Before 2000" = "#366a74", "After 2000" = "#5e8b94")) + # Define colors
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 12, face = "bold") # Customize facet labels
  ) +
  xlim(1986, 2013) +
  facet_wrap(~ geoloc, ncol = 2) # Create facets for each geolocation, with 2 columns


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/bar_plot_recov_yod.png", plot = p1, width = 8, height = 5.5, dpi = 300)




# Compute mean percentage recovered for yod < 2000 and yod >= 2000
means_by_period <- interpolated_data_new %>%
  mutate(period = ifelse(yod < 2000, "pre-2000", "post-2000")) %>%
  group_by(geoloc, period) %>%
  summarize(mean_recovered = mean(mean_percent_recovered, na.rm = TRUE), .groups = "drop")


  
