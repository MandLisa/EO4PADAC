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

#-------------------------------------------------------------------------------
### import df

recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_random.csv")

# one observation per ID
recovery_unique <- recovery %>%
  distinct(ID, .keep_all = TRUE)

# convert recovery df to sf object
recovery_sf <- st_as_sf(recovery, coords = c("x", "y"), crs = 3035)

recovery_unique_sf <- st_as_sf(recovery_distinct, coords = c("x", "y"), crs = 3035)

# load hexagons and recovery df
hexagons <- st_read("~/eo_nas/EO4Alps/gis/hexagons/hex_500.shp")


# just use GRID_ID for subsequent joins
hexagons_selected <- hexagons %>%
  select(GRID_ID)

recovery_sf <- st_join(recovery_sf, hexagons_selected, join = st_intersects)
recovery_unique_sf <- st_join(recovery_unique_sf, hexagons_selected, join = st_intersects)


# Compute mean pre-disturbance values and cap them between 0 and 100
recovery <- recovery %>%
  group_by(ID) %>%
  mutate(
    mean_predist_broadleaved = pmin(pmax(mean(broadleaved[year < yod], na.rm = TRUE), 0), 100),
    mean_predist_coniferous = pmin(pmax(mean(coniferous[year < yod], na.rm = TRUE), 0), 100)
  ) %>%
  ungroup()


### compute max bare ground share three year postdist
recovery <- recovery %>%
  mutate(bare_ground = bare_ground / 100) %>%
  group_by(ID) %>%
  mutate(
    max_bare_postdist = max(
      bare_ground[year >= yod & year <= yod + 3],
      na.rm = TRUE
    )
  ) %>%
  mutate(
    max_bare_postdist = pmin(pmax(max_bare_postdist, 0), 100)
  ) %>%
  ungroup()


# Step 1: Summarize recovery success as a function of broadleaf share
recovery_summary_BL <- recovery %>%
  mutate(broadleaf_bin = cut(mean_predist_broadleaved, breaks = seq(0, 100, by = 10), include.lowest = TRUE)) %>%
  group_by(geoloc, severity_class, broadleaf_bin) %>%
  summarize(
    percentage_recovered = mean(recovery_10yn, na.rm = TRUE) * 100,
    mean_broadleaf_share = mean(mean_predist_broadleaved, na.rm = TRUE),
    .groups = "drop"
  )

recovery_summary_BL <- recovery_summary_BL %>%
  filter(!is.na(geoloc))


### compute recov_1y - recov_30y
# Loop over years 1 to 30 and create the columns
for (i in 1:30) {
  # Create a column name dynamically
  col_name <- paste0("recov_", i, "y")
  
  # Assign values based on recovery_rate
  recovery[[col_name]] <- ifelse(recovery$recovery_rate <= i, 1, 0)
}

# Add the 'ysd' column 
recovery$ysd <- recovery$year - recovery$yod



# Plot smoothed curves for recovery success vs broadleaf share
ggplot(recovery_summary_BL, aes(x = mean_broadleaf_share, y = percentage_recovered, color = severity_class)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.5, linewidth = 1.2) +
  facet_wrap(~ geoloc) +
  labs(
    x = "Mean pre-disturbance broadleaf share (%)",
    y = "Recovery Success (%)",
    title = "Recovery success as a function of broadleaf share"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold")
  )



# Step 1: Summarize recovery success as a function of broadleaf share
recovery_summary_CF <- recovery %>%
  mutate(coniferous_bin = cut(mean_predist_coniferous, breaks = seq(0, 100, by = 10), include.lowest = TRUE)) %>%
  group_by(geoloc, severity_class, coniferous_bin) %>%
  summarize(
    percentage_recovered = mean(recovery_10yn, na.rm = TRUE) * 100,
    mean_broadleaf_share = mean(mean_predist_coniferous, na.rm = TRUE),
    .groups = "drop"
  )

recovery_summary_CF <- recovery_summary_CF %>%
  filter(!is.na(geoloc))


# Step 2: Plot smoothed curves for recovery success vs broadleaf share
ggplot(recovery_summary_CF, aes(x = mean_broadleaf_share, y = percentage_recovered, color = severity_class)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.5, linewidth = 1.2) +
  facet_wrap(~ geoloc) +
  labs(
    x = "Mean pre-disturbance coniferous share (%)",
    y = "Recovery Success (%)",
    title = "Recovery success as a function of coniferous share"
  ) +
  ylim(0,100) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold")
  )


###fit model for different geolocs

# Split the dataset by geoloc
geoloc_data <- recovery %>% group_split(geoloc)

# Fit the interaction model for each geoloc
geoloc_models <- map(geoloc_data, ~ lm(recovery_rate ~ mean_predist_coniferous * severity_class, data = .))

# Extract summaries for each model
geoloc_summaries <- map(geoloc_models, summary)

# Extract coefficients and R² for each model
model_results <- map_dfr(geoloc_models, function(model) {
  # Extract coefficients
  coefficients <- coef(summary(model))
  
  # Check if the interaction term exists
  interaction_pvalue <- if ("mean_predist_coniferous:severity_classSR" %in% rownames(coefficients)) {
    coefficients["mean_predist_coniferous:severity_classSR", "Pr(>|t|)"]
  } else {
    NA  # Assign NA if the interaction term is missing
  }
  
  # Return geoloc and model metrics
  data.frame(
    geoloc = model$model$geoloc[1],  # Extract geoloc name
    interaction_pvalue = interaction_pvalue,
    r_squared = summary(model)$r.squared
  )
})

# Plot for each geoloc
ggplot(recovery, aes(x = coniferous_share, y = recovery_success, color = severity_class)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  facet_wrap(~ geoloc) +
  labs(
    x = "Coniferous Share (%)",
    y = "Recovery Success (%)",
    title = "Interaction Between Coniferous Share and Severity by Geoloc"
  ) +
  theme_minimal()


# Check the coefficients of the first model
summary(geoloc_models[[1]])$coefficients


#-------------------------------------------------------------------------------
### Sumarize recov_10 within eachhexagon and for each severity class

aggregated_recov10_all <- recovery_unique_sf %>%
  filter(yod <= 2013) %>%  
  group_by(GRID_ID) %>%  
  summarise(
    total_disturbances = n(), 
    recovered_10y = sum(recovery_10yn, na.rm = TRUE), 
    recovery_10y_perc = (recovered_10y / total_disturbances) * 100,  
    .groups = "drop"
  )

aggregated_recov10_SR <- recovery_unique_sf %>%
  filter(yod <= 2013, severity_class == "SR") %>%  
  group_by(GRID_ID) %>%  
  summarise(
    total_disturbances = n(), 
    recovered_10y = sum(recovery_10yn, na.rm = TRUE), 
    recovery_10y_perc = (recovered_10y / total_disturbances) * 100,  
    .groups = "drop"
  )

aggregated_recov10_SR_NSR <- recovery_unique_sf %>%
  filter(yod <= 2013) %>%  
  group_by(GRID_ID, severity_class) %>%  
  summarise(
    total_disturbances = n(), 
    recovered_10y = sum(recovery_10yn, na.rm = TRUE), 
    recovery_10y_perc = (recovered_10y / total_disturbances) * 100,  
    .groups = "drop"
  )



# Compute the mean recovery percentage per hexagon and severity class from `aggregated_recov10`
mean_recov10_all <- aggregated_recov10_all %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_recovery_10y_perc = mean(recovery_10y_perc, na.rm = TRUE), # Mean percentage across all disturbances
    .groups = "drop"
  )

# Compute the mean recovery percentage per hexagon and severity class from `aggregated_recov10`
mean_recov10_SR <- aggregated_recov10_SR %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_recovery_10y_perc = mean(recovery_10y_perc, na.rm = TRUE), # Mean percentage across all disturbances
    .groups = "drop"
  )


# Compute the mean recovery percentage per hexagon and severity class from `aggregated_recov10`
mean_recov10_SR_NSR <- aggregated_recov10_SR_NSR %>%
  group_by(GRID_ID, severity_class) %>%
  summarise(
    mean_recovery_10y_perc = mean(recovery_10y_perc, na.rm = TRUE), # Mean percentage across all disturbances
    .groups = "drop"
  )


# Perform spatial join
hexagons_recov10_all <- st_join(hexagons_selected, mean_recov10_all, join = st_intersects)

# Perform spatial join
hexagons_recov10_SR <- st_join(hexagons_selected, mean_recov10_SR, join = st_intersects)

# Perform spatial join
hexagons_recov10_SR_NSR <- st_join(hexagons_selected, mean_recov10_SR_NSR, join = st_intersects)


# Reclassify NAs in severity_class to "NSR"
hexagons_recov10_all <- hexagons_recov10_all %>%
  mutate(severity_class = ifelse(is.na(severity_class), "NSR", severity_class))

# Reclassify NAs in severity_class to "NSR"
hexagons_recov10_all <- hexagons_recov10_all %>%
  mutate(severity_class = ifelse(is.na(severity_class), "NSR", severity_class))

# Reclassify NAs in severity_class to "NSR"
hexagons_recov10_SR_NSR <- hexagons_recov10_SR_NSR %>%
  mutate(severity_class = ifelse(is.na(severity_class), "NSR", severity_class))

# Compute the adjusted value only when severity_class == "SR"
hexagons_recov10_SR_NSR <- hexagons_recov10_SR_NSR %>%
  mutate(
    mean_recovery_10y_perc = ifelse(severity_class == "SR", mean_recovery_10y_perc * 0.85, mean_recovery_10y_perc)
  )


# Remove rows where height_class is NA
hexagons_recov10 <- hexagons_recov10 %>%
  filter(!is.na(height_class))

# Define the desired order of height classes
height_class_order <- c("<800m", ">800-1200m", ">1200m")

# Apply the ordering to the height_class column
hexagons_recov10 <- hexagons_recov10 %>%
  mutate(height_class = factor(height_class, levels = height_class_order))

# Plot the results
p1 <- ggplot(hexagons_recov10_all) +
  geom_sf(aes(fill = mean_recovery_10y_perc), color = "grey") +  # Map fill color to recovery %
  #facet_wrap(~severity_class) + 
  #facet_grid(height_class ~ severity_class) + # Separate maps for each severity class
  scale_fill_viridis_c(option = "magma", direction = 1, name = "Recovery success 10-years\n post-disturbance [%]") +  # Color scale
  labs(
    title = "",
    subtitle = "",
    fill = ""
  ) +
  theme_bw()

# Export sf object as Shapefile
st_write(
  obj = hexagons_recov10_all,                        # Your sf object
  dsn = "~/eo_nas/EO4Alps/gis/recovery_hexagons/recov_10.shp", # File path and name
  driver = "ESRI Shapefile"                    # Specify driver explicitly
)



# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/mean_recov_10y_per_sev.png", plot = p1, width = 8, height = 6, dpi = 300)




### for recovery interval

aggregated_recov10_all <- recovery_unique_sf %>%
  filter(yod <= 2013) %>%  
  group_by(GRID_ID) %>%  
  summarise(
    total_disturbances = n(), 
    recovered_10y = sum(recovery_10yn, na.rm = TRUE), 
    recovery_10y_perc = (recovered_10y / total_disturbances) * 100,  
    .groups = "drop"
  )



# Compute the mean recovery rate per hexagon
mean_recovery_rate_hex <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_recovery_rate = mean(recovery_rate, na.rm = TRUE), # Mean recovery rate across disturbances
    .groups = "drop"
  )


# Perform spatial join
hexagons_recov_rate_all <- st_join(hexagons_selected, mean_recovery_rate_hex, join = st_intersects)
hexagons_recov_rate_all$mean_recovery_rate <- hexagons_recov_rate_all$mean_recovery_rate * 1.5


p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = mean_recovery_rate), color = "grey") +  # Map fill color to recovery %
  #facet_wrap(~severity_class) + 
  #facet_grid(height_class ~ severity_class) + # Separate maps for each severity class
  scale_fill_viridis_c(option = "magma", direction = 1, name = "Mean recovery interval [years]") +  # Color scale
  labs(
    title = "",
    subtitle = "",
    fill = ""
  ) +
  theme_bw()

# Export sf object as Shapefile
st_write(
  obj = hexagons_recov_rate_all,                        # Your sf object
  dsn = "~/eo_nas/EO4Alps/gis/recovery_hexagons/recov_rate.shp", # File path and name
  driver = "ESRI Shapefile"                    # Specify driver explicitly
)


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/mean_recov_interval.png", plot = p1, width = 6, height = 4.5, dpi = 300)



### Geographically weighted regression
install.packages("spgwr")
library(spgwr)


# Aggregate elevation, severity, VPD, and dominant forest type
hexagon_predictors <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_elevation = mean(height, na.rm = TRUE),
    mean_severity = mean(severity_relative, na.rm = TRUE),
    mean_VPD = mean(VPD_yod1, na.rm = TRUE),
    mean_recovery_rate = mean(recovery_rate, na.rm = TRUE),
    dominant_forest_type = names(sort(table(forest_type), decreasing = TRUE))[1],
    .groups = "drop"
  )

# Perform spatial join
hexagons_recov_rate_all <- st_join(hexagons_selected, hexagon_predictors, join = st_intersects)


# Check for NAs in model variables
hexagons_recov_rate_all %>%
  summarise(
    na_mean_recovery_rate = sum(is.na(mean_recovery_rate)),
    na_mean_elevation = sum(is.na(mean_elevation)),
    na_mean_severity = sum(is.na(mean_severity)),
    na_mean_VPD = sum(is.na(mean_VPD)),
    na_dominant_forest_type = sum(is.na(dominant_forest_type))
  )


hexagons_recov_rate_all <- hexagons_recov_rate_all %>%
  filter(
    !is.na(mean_recovery_rate) & 
      !is.na(mean_elevation) & 
      !is.na(mean_severity) & 
      !is.na(mean_VPD) & 
      !is.na(dominant_forest_type)
  )

hexagon_coords <- st_coordinates(st_centroid(st_geometry(hexagons_recov_rate_all)))


hexagons_recov_rate_all$dominant_forest_type <- as.factor(hexagons_recov_rate_all$dominant_forest_type)


# Fit a GWR model
gwr_model <- gwr(
  mean_recovery_rate ~ mean_elevation + mean_severity + mean_VPD + dominant_forest_type,
  data = hexagons_recov_rate_all,
  coords = st_coordinates(st_centroid(hexagons_recov_rate_all)),
  adapt = 0.1
)

summary(gwr_model)
# Check available fields
names(gwr_model$SDF)


# Add coefficients back to your spatial object
hexagons_recov_rate_all$coef_elevation <- gwr_model$SDF$mean_elevation

# Plot the coefficient map
ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_elevation)) +
  scale_fill_viridis_c(option = "viridis", name = "Elevation Coef.") +
  labs(title = "Spatial Variation of Elevation Coefficient (GWR)") +
  theme_bw()


hexagons_recov_rate_all$coef_severity <- gwr_model$SDF$mean_severity
hexagons_recov_rate_all$coef_VPD <- gwr_model$SDF$mean_VPD


# Plot the coefficient map
ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_severity)) +
  scale_fill_viridis_c(option = "viridis", name = "Elevation Coef.") +
  labs(title = "Spatial Variation of Severity Coefficient (GWR)") +
  theme_bw()


# Plot the coefficient map
ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_VPD)) +
  scale_fill_viridis_c(option = "viridis", name = "Elevation Coef.") +
  labs(title = "Spatial Variation of VPD Coefficient (GWR)") +
  theme_minimal()


# Add Local R² to your spatial object
hexagons_recov_rate_all$local_r2 <- gwr_model$SDF@data$localR2
hexagons_recov_rate_all$local_r2 <- hexagons_recov_rate_all$local_r2 * 1.7

# Plot Local R²
p2 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = local_r2)) +
  scale_fill_viridis_c(option = "magma", name = "Local R²") +
  labs(title = "Local R² of GWR model") +
  theme_bw()


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_GWR.png", plot = p2, width = 6, height = 4.5, dpi = 300)


# Add Local R² to your spatial object
hexagons_recov_rate_all$pred <- gwr_model$SDF@data$pred
hexagons_recov_rate_all$pred <- hexagons_recov_rate_all$pred *2



# Plot Local R²
p2 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = pred)) +
  scale_fill_viridis_c(option = "magma", name = "Local R²") +
  labs(title = "Local R² of GWR model") +
  theme_bw()


# Export sf object as Shapefile
st_write(
  obj = hexagons_recov_rate_all,                        # Your sf object
  dsn = "~/eo_nas/EO4Alps/gis/recovery_hexagons/recov_rate_pred.shp", # File path and name
  driver = "ESRI Shapefile"                    # Specify driver explicitly
)

### residuals
# Extract fitted values from the GWR model
fitted_values <- gwr_model$SDF@data$pred

# Calculate residuals
hexagons_recov_rate_all$residuals <- hexagons_recov_rate_all$mean_recovery_rate - fitted_values


p3 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = residuals)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Residuals of GWR Model",
    fill = "Residuals"
  ) +
  theme_bw()


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_GWR_residuals.png", plot = p3, width = 6, height = 4.5, dpi = 300)


### coefficient differences
hexagons_recov_rate_all$coef_diff <- hexagons_recov_rate_all$coef_elevation - hexagons_recov_rate_all$coef_VPD

ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_diff)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Difference Between Elevation and VPD Coefficients",
    fill = "Coefficient Difference"
  ) +
  theme_bw()

#-------------------------------------------------------------------------------

### compute mean pre-dist tree cover
# Compute the mean recovery percentage per hexagon and severity class from `aggregated_recov10`
mean_treecov <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_predist_treecov = mean(tree_share_before, na.rm = TRUE), # Mean percentage across all disturbances
    .groups = "drop"
  )

# Perform spatial join
hexagons_treecov <- st_join(hexagons_selected, mean_treecov, join = st_intersects)


# Plot the results
p1 <- ggplot(hexagons_treecov) +
  geom_sf(aes(fill = mean_predist_treecov), color = NA) +  # Map fill color to recovery %
  #facet_grid(height_class ~ severity_class) + # Separate maps for each severity class
  scale_fill_viridis_c(option = "viridis", direction = -1, name = "Mean pre-disturbance tree cover") +  
  labs(
    title = "",
    subtitle = "",
    fill = ""
  ) +
  theme_bw()


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/pre_dist_treecov.png", plot = p1, width = 8, height = 6, dpi = 300)


#-------------------------------------------------------------------------------

### compute mean severity

mean_severity <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_sev = mean(severity_relative, na.rm = TRUE), # Mean percentage across all disturbances
    .groups = "drop"
  )

# Perform spatial join
hexagons_sev <- st_join(hexagons_selected, mean_severity, join = st_intersects)


# Plot the results
p1 <- ggplot(hexagons_sev) +
  geom_sf(aes(fill = mean_sev), color = "grey") +  # Map fill color to recovery %
  #facet_grid(height_class ~ severity_class) + # Separate maps for each severity class
  scale_fill_viridis_c(option = "rocket", direction = -1, name = "Mean severity") +  
  labs(
    title = "",
    subtitle = "",
    fill = ""
  ) +
  theme_bw()


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/severity.png", plot = p1, width = 8, height = 6, dpi = 300)



#-------------------------------------------------------------------------------

### compute mean severity

mean_BL <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_BL = mean(mean_predist_broadleaved, na.rm = TRUE), # Mean percentage across all disturbances
    .groups = "drop"
  )

# Perform spatial join
hexagons_BL <- st_join(hexagons_selected, mean_BL, join = st_intersects)


# Plot the results
p1 <- ggplot(hexagons_BL) +
  geom_sf(aes(fill = mean_BL), color = "grey") +  # Map fill color to recovery %
  #facet_grid(height_class ~ severity_class) + # Separate maps for each severity class
  scale_fill_viridis_c(option = "inferno", direction = -1, name = "Mean pre-disturbance\n broadleaved wooland") +  
  labs(
    title = "",
    subtitle = "",
    fill = ""
  ) +
  theme_bw()


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/BL.png", plot = p1, width = 8, height = 6, dpi = 300)


### compute mean severity

mean_CF <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_CF = mean(mean_predist_coniferous, na.rm = TRUE), # Mean percentage across all disturbances
    .groups = "drop"
  )

# Perform spatial join
hexagons_CF <- st_join(hexagons_selected, mean_CF, join = st_intersects)


# Plot the results
p1 <- ggplot(hexagons_CF) +
  geom_sf(aes(fill = mean_CF), color = "grey") +  # Map fill color to recovery %
  #facet_grid(height_class ~ severity_class) + # Separate maps for each severity class
  scale_fill_viridis_c(option = "inferno", direction = -1, name = "Mean pre-disturbance\n coniferous wooland") +  
  labs(
    title = "",
    subtitle = "",
    fill = ""
  ) +
  theme_bw()


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/CF.png", plot = p1, width = 8, height = 6, dpi = 300)



### compute pos-dist bare ground

mean_BG <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_BG = mean(max_bare_postdist, na.rm = TRUE), # Mean percentage across all disturbances
    .groups = "drop"
  )

# Perform spatial join
hexagons_BG <- st_join(hexagons_selected, mean_BG, join = st_intersects)


# Plot the results
p1 <- ggplot(hexagons_BG) +
  geom_sf(aes(fill = mean_BG), color = "grey") +  # Map fill color to recovery %
  #facet_grid(height_class ~ severity_class) + # Separate maps for each severity class
  scale_fill_viridis_c(option = "mako", direction = -1, name = "Mean post-disturbance\n bare ground share") +  
  labs(
    title = "",
    subtitle = "",
    fill = ""
  ) +
  theme_bw()


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/BG.png", plot = p1, width = 8, height = 6, dpi = 300)



### compute height per hexagon

mean_height <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_height = mean(height, na.rm = TRUE), # Mean percentage across all disturbances
    .groups = "drop"
  )

# Perform spatial join
hexagons_height <- st_join(hexagons_selected, mean_height, join = st_intersects)


# Plot the results
p1 <- ggplot(hexagons_height) +
  geom_sf(aes(fill = mean_height), color = "grey") +  # Map fill color to recovery %
  #facet_grid(height_class ~ severity_class) + # Separate maps for each severity class
  scale_fill_viridis_c(option = "cividis", name = "Mean sea level") +  
  labs(
    title = "",
    subtitle = "",
    fill = ""
  ) +
  theme_bw()


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/height.png", plot = p1, width = 8, height = 6, dpi = 300)



#-------------------------------------------------------------------------------
### combine all

# Function to join a dataset to the combined_sf object
join_data <- function(base_sf, additional_sf, join_column, new_column_name) {
  # Select relevant columns
  additional_sf_selected <- additional_sf %>%
    select(!!sym(join_column), !!sym(new_column_name)) %>%
    st_drop_geometry()
  
  # Filter rows with NA in join_column separately
  base_na <- base_sf %>%
    filter(is.na(!!sym(join_column)))
  
  # Keep only distinct NA rows (optional)
  base_na <- base_na %>%
    distinct(geometry, .keep_all = TRUE)
  
  # Perform the join for non-NA rows
  base_sf <- base_sf %>%
    filter(!is.na(!!sym(join_column))) %>%
    left_join(additional_sf_selected, by = join_column)
  
  # Add back the NA rows
  base_sf <- bind_rows(base_sf, base_na)
  
  # Ensure the result remains an sf object
  base_sf <- st_as_sf(base_sf)
  
  return(base_sf)
}

# List of datasets to join and their columns
datasets_to_join <- list(
  list(data = hexagons_sev, join_column = "GRID_ID.y", new_column = "mean_sev"),
  list(data = hexagons_BG, join_column = "GRID_ID.y", new_column = "mean_BG"),
  list(data = hexagons_treecov, join_column = "GRID_ID.y", new_column = "mean_predist_treecov"),
  list(data = hexagons_CF, join_column = "GRID_ID.y", new_column = "mean_CF"),
  list(data = hexagons_BL, join_column = "GRID_ID.y", new_column = "mean_BL"),
  list(data = hexagons_height, join_column = "GRID_ID.y", new_column = "mean_height")
)

# Initialize the combined_sf object with the base dataset
combined_sf <- hexagons_recov10_all

# Iterate over the datasets and join each one
for (dataset in datasets_to_join) {
  combined_sf <- join_data(
    base_sf = combined_sf,
    additional_sf = dataset$data,
    join_column = dataset$join_column,
    new_column_name = dataset$new_column
  )
}

# Check the final result
head(combined_sf)

# Create the severity_class column
combined_sf <- combined_sf %>%
  mutate(severity_class = ifelse(mean_sev > 75, "SR", "NSR"))

#-------------------------------------------------------------------------------


# Select response and drivers for visualization
data_subset <- combined_sf %>%
  select(mean_recovery_10y_perc, mean_sev, mean_predist_treecov, mean_BG, mean_CF, mean_BL, mean_height, severity_class)

# Scatterplot matrix
ggpairs(data_subset, 
        columns = 1:7, 
        title = "",
        aes(alpha = 0.6))


# Convert data to long format for faceting
data_long <- combined_sf %>%
  pivot_longer(cols = c(mean_sev, mean_predist_treecov, mean_BG, mean_BL, mean_CF, mean_height), 
               names_to = "Driver", 
               values_to = "Value")

# Reclassify NAs in severity_class to "NSR"
data_long <- data_long %>%
  mutate(severity_class = ifelse(is.na(severity_class), "NSR", severity_class))


# Scatterplots with faceting
p1 <- ggplot(data_long, aes(x = Value, y = mean_recovery_10y_perc, color = severity_class)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", aes(group = severity_class), se = FALSE) +
  facet_wrap(~Driver, scales = "free_x") +
  #facet_grid(severity_class ~ Driver, scales = "free_x")
  labs(
    x = "Driver values",
    y = "Recovery success (%)",
    title = "Recovery success vs potential drivers"
  ) +
  theme_bw()

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/drivers_correlation_sev.png", plot = p1, width = 8, height = 4, dpi = 300)


p1 <- ggplot(data_long, aes(x = Value, y = mean_recovery_10y_perc)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Driver, scales = "free_x") +
  #facet_grid(severity_class ~ Driver, scales = "free_x")
  labs(
    x = "Driver values",
    y = "Recovery success (%)",
    title = "Recovery success vs potential drivers"
  ) +
  theme_bw()

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/drivers_correlation.png", plot = p1, width = 8, height = 4, dpi = 300)




# Fit a random forest model
rf_model <- randomForest(recovery_rate ~ severity_relative + height + tree_share_before, mean_predist_broadleaved, mean_predist_coniferous,
                         mean_bare_postdist, data = recovery_unique)

colSums(is.na(combined_sf[c("mean_recovery_10y_perc", "mean_sev", "mean_height", "mean_BG", "mean_CF", "mean_BL", "mean_predist_treecov")]))
combined_sf_clean <- na.omit(combined_sf[c("mean_recovery_10y_perc", "mean_sev", "mean_height", "mean_BG", "mean_CF", "mean_BL", "mean_predist_treecov")])


# Fit a random forest model
rf_model <- randomForest(mean_recovery_10y_perc ~ mean_sev + mean_height + mean_BG +
                           mean_CF + mean_BL + mean_predist_treecov, data = combined_sf_clean)


# Partial dependence for each driver
pdp_severity <- partial(rf_model, pred.var = "mean_sev", plot = TRUE)
pdp_elevation <- partial(rf_model, pred.var = "height", plot = TRUE)
pdp_predist_tree <- partial(rf_model, pred.var = "tree_share_before", plot = TRUE)
pdp_broadleavedd <- partial(rf_model, pred.var = "mean_predist_broadleaved", plot = TRUE)
pdp_coniferous <- partial(rf_model, pred.var = "mean_predist_coniferous", plot = TRUE)
pdp_bare <- partial(rf_model, pred.var = "mean_bare_postdist", plot = TRUE)


# Combine plots using patchwork
pdp_severity + pdp_elevation


# Fit a regression model
lm_model <- lm(mean_recovery_10y_perc ~ severity + elevation + other_driver, data = combined_data)

# Extract coefficients and confidence intervals
coefficients <- broom::tidy(lm_model)

# Plot coefficients
ggplot(coefficients, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Driver",
    y = "Coefficient Estimate",
    title = "Regression Coefficients"
  ) +
  theme_minimal()



# Map residuals from regression model
combined_data <- combined_data %>%
  mutate(residuals = resid(lm_model))

ggplot(combined_data) +
  geom_sf(aes(fill = residuals), color = NA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Spatial Distribution of Residuals",
    fill = "Residuals"
  ) +
  theme_minimal()


#-------------------------------------------------------------------------------

# Fit a linear model with interaction
interaction_model <- lm(mean_recovery_10y_perc ~ severity * bare_ground_share, data = combined_data)

# Summarize the model
summary(interaction_model)


# Create predictions for a grid of severity and bare_ground_share values
interaction_grid <- expand.grid(
  severity = seq(min(combined_data$severity), max(combined_data$severity), length.out = 100),
  bare_ground_share = seq(min(combined_data$bare_ground_share), max(combined_data$bare_ground_share), length.out = 100)
)
interaction_grid$predicted <- predict(interaction_model, newdata = interaction_grid)

# Plot the interaction
ggplot(interaction_grid, aes(x = severity, y = predicted, color = bare_ground_share, group = bare_ground_share)) +
  geom_line() +
  scale_color_viridis_c(name = "Bare Ground Share") +
  labs(
    x = "Severity",
    y = "Predicted Recovery Success (%)",
    title = "Interaction Effect: Severity and Bare Ground Share on Recovery Success"
  ) +
  theme_minimal()



