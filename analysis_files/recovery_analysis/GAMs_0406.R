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


# 1. VPD-Statistiken für die ersten 10 Jahre nach Störung
vpd_stats_10y <- recovery %>%
  filter(year <= yod + 10) %>%
  group_by(ID) %>%
  summarise(
    VPD_sd  = sd(VPD_anomaly, na.rm = TRUE),
    VPD_min = min(VPD_anomaly, na.rm = TRUE),
    VPD_max = max(VPD_anomaly, na.rm = TRUE),
    .groups = "drop"
  )

# 2. VPD im ersten Jahr nach Störung (yod + 1)
vpd_stats_yod3 <- recovery %>%
  filter(year <= yod + 3) %>%
  group_by(ID) %>%
  summarise(
    VPD_sd_yod3  = sd(VPD_anomaly, na.rm = TRUE),
    VPD_min_yod3 = min(VPD_anomaly, na.rm = TRUE),
    VPD_max_yod3 = max(VPD_anomaly, na.rm = TRUE),  # falls mehrere Werte pro ID vorhanden
    .groups = "drop"
  )

# 3. Zusammenführen beider Tabellen anhand der ID
vpd_stats_combined <- vpd_stats_10y %>%
  left_join(vpd_stats_yod3, by = "ID")

# 4. Anhängen der Metriken an den vollständigen `recovery`-Datensatz
recovery_stat <- recovery %>%
  left_join(vpd_stats_combined, by = "ID")

# Filter the dataset and compute the new column
recovery_filt <- recovery_stat %>%
  group_by(ID) %>%
  filter(yod < 2013) %>%
  mutate(recov_10 = ifelse(recovery_rate <= 10, 1, 0)) %>%
  ungroup()

# Create new columns for pre-disturbance means
recovery_filt <- recovery_filt %>%
  group_by(ID) %>%  # Group by ID to calculate means within each ID
  mutate(
    pre_dist_coni = ifelse(year < yod, mean(coniferous[year < yod], na.rm = TRUE), NA),
    pre_dist_broadl = ifelse(year < yod, mean(broadleaved[year < yod], na.rm = TRUE), NA),
    post_dist_bare = ifelse(year < yod, mean(bare_ground[year > yod], na.rm = TRUE), NA)
  ) %>%
  ungroup()

# one observation per ID, enough for the spatial model
recovery_unique <- recovery_filt %>%
  distinct(ID, .keep_all = TRUE)

# same for one ID per observation
recovery_unique_sf <- st_as_sf(recovery_unique, coords = c("x", "y"), crs = 3035)

# load hexagons and recovery df
hexagons <- st_read("~/eo_nas/EO4Alps/gis/hexagons/hex_500.shp")

# just use GRID_ID for subsequent joins
hexagons_selected <- hexagons %>%
  select(GRID_ID)

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
    max_VPD_yod1 = max(VPD_yod1, na.rm = TRUE),
    min_VPD_yod1 = min(VPD_yod1, na.rm = TRUE),
    sd_VPD_yod1 = sd(VPD_yod1, na.rm = TRUE),
    mean_prec = mean(mean_prec10, na.rm = TRUE),
    mean_temp = mean(mean_temp10, na.rm = TRUE),
    mean_prec_total = mean(mean_prec_total, na.rm = TRUE),
    mean_temp_total = mean(mean_temp_total, na.rm = TRUE),
    mean_recovery_rate = mean(recovery_rate, na.rm = TRUE),
    mean_percent_recovered = mean(percent_recovered, na.rm = TRUE),
    mean_broadleaved = mean(pre_dist_broadl, na.rm = TRUE),
    mean_coniferous = mean(pre_dist_coni, na.rm = TRUE),
    mean_bare = mean(post_dist_bare, na.rm = TRUE),
    mean_sd_VPD = mean(VPD_sd, na.rm = TRUE),
    max_sd_VPD = max(VPD_sd, na.rm = TRUE),
    min_sd_VPD = min(VPD_sd, na.rm = TRUE),
    mean_min_VPD = mean(VPD_min, na.rm = TRUE),
    min_min_VPD = min(VPD_min, na.rm = TRUE),
    max_min_VPD = max(VPD_min, na.rm = TRUE),
    mean_max_VPD = mean(VPD_max, na.rm = TRUE),
    max_max_VPD = max(VPD_max, na.rm = TRUE),
    min_max_VPD = min(VPD_max, na.rm = TRUE),
    mean_sd_VPD_yod3 = mean(VPD_sd_yod3, na.rm = TRUE), 
    max_sd_VPD_yod3 = max(VPD_sd_yod3, na.rm = TRUE), 
    min_sd_VPD_yod3 = min(VPD_sd_yod3, na.rm = TRUE), 
    mean_min_VPD_yod3 = mean(VPD_min_yod3, na.rm = TRUE),
    max_min_VPD_yod3 = max(VPD_min_yod3, na.rm = TRUE), 
    min_min_VPD_yod3 = min(VPD_min_yod3, na.rm = TRUE), 
    mean_max_VPD_yod3 = mean(VPD_max_yod3, na.rm = TRUE),
    max_max_VPD_yod3 = max(VPD_max_yod3, na.rm = TRUE), 
    min_max_VPD_yod3 = min(VPD_max_yod3, na.rm = TRUE), 
    dominant_forest_type = names(sort(table(forest_type), decreasing = TRUE))[1],  # Mode of forest type
    geolocation = names(sort(table(geoloc), decreasing = TRUE))[1],  # Mode of geolocation
    .groups = "drop"
  )

# Perform spatial join
hexagons_recov10 <- st_join(hexagons_selected, hexagon_predictors, join = st_intersects)


# List of variable names as defined in your summarise() block
vars <- c(
  "mean_elevation", "mean_severity", "mean_VPD", "mean_VPD_ano", 
  "mean_VPD_yod1", "max_VPD_yod1", "min_VPD_yod1", "sd_VPD_yod1",
  "mean_prec", "mean_temp", "mean_prec_total", "mean_temp_total",
  "mean_recovery_rate", "mean_percent_recovered",
  "mean_broadleaved", "mean_coniferous", "mean_bare",
  "mean_sd_VPD", "max_sd_VPD", "min_sd_VPD",
  "mean_min_VPD", "min_min_VPD", "max_min_VPD",
  "mean_max_VPD", "max_max_VPD", "min_max_VPD",
  "mean_sd_VPD_yod3", "max_sd_VPD_yod3", "min_sd_VPD_yod3",
  "mean_min_VPD_yod3", "max_min_VPD_yod3", "min_min_VPD_yod3",
  "mean_max_VPD_yod3", "max_max_VPD_yod3", "min_max_VPD_yod3",
  "dominant_forest_type", "geolocation"
)

# Generate the NA check lines
na_checks <- paste0("    na_", vars, " = sum(is.na(", vars, "))")


# Define the variable list as before
vars <- c(
  "mean_elevation", "mean_severity", "mean_VPD", "mean_VPD_ano", 
  "mean_VPD_yod1", "max_VPD_yod1", "min_VPD_yod1", "sd_VPD_yod1",
  "mean_prec", "mean_temp", "mean_prec_total", "mean_temp_total",
  "mean_recovery_rate", "mean_percent_recovered",
  "mean_broadleaved", "mean_coniferous", "mean_bare",
  "mean_sd_VPD", "max_sd_VPD", "min_sd_VPD",
  "mean_min_VPD", "min_min_VPD", "max_min_VPD",
  "mean_max_VPD", "max_max_VPD", "min_max_VPD",
  "mean_sd_VPD_yod3", "max_sd_VPD_yod3", "min_sd_VPD_yod3",
  "mean_min_VPD_yod3", "max_min_VPD_yod3", "min_min_VPD_yod3",
  "mean_max_VPD_yod3", "max_max_VPD_yod3", "min_max_VPD_yod3",
  "dominant_forest_type", "geolocation"
)

# Build filter conditions
filter_conditions <- paste0("    !is.na(", vars, ")")

# Print full filter block
cat("hexagons_recov10 <- hexagons_recov10 %>%\n  filter(\n",
    paste(filter_conditions, collapse = " &\n"),
    "\n  )")

# broadleaved plus coniferous forest share
hexagons_recov10 <- hexagons_recov10 %>%
  mutate(mean_pre_dist_tree_cover = mean_broadleaved + mean_coniferous) %>%
  dplyr::select(-mean_broadleaved, -mean_coniferous)  


# Compute centroids
hexagons_recov10_centros <- hexagons_recov10
hexagons_recov10_centros <- hexagons_recov10_centros %>%
  mutate(mean_severity = ifelse(mean_severity > 90, 70, mean_severity))
hexagons_recov10_centros$mean_bare <- hexagons_recov10_centros$mean_bare/100
hexagons_recov10_centros <- hexagons_recov10_centros %>%
  mutate(mean_bare = ifelse(mean_bare > 10, 1, mean_bare))
hexagons_recov10_centros <- hexagons_recov10_centros %>%
  mutate(mean_elevation = ifelse(mean_elevation > 1700, 600, mean_elevation))


hexagons_recov10_centros$centroid <- st_centroid(hexagons_recov10$geometry)

# Extract longitude and latitude
hexagons_recov10_centros$long <- st_coordinates(hexagons_recov10_centros$centroid)[,1]
hexagons_recov10_centros$lat <- st_coordinates(hexagons_recov10_centros$centroid)[,2]

library(mgcv)
hexagons_recov10_centros$geolocation <- as.factor(hexagons_recov10_centros$geolocation)

write.csv(hexagons_recov10_centros, "~/hexagons_recov_10_centros.csv", row.names = FALSE)
write.csv(hexagons_recov10, "~/hexagons_recov_10.csv", row.names = FALSE)



fit.gam <- gam(mean_percent_recovered ~ 
                 s(long, lat, bs = "tp") +  
                 s(mean_elevation) +
                 s(mean_severity) + 
                 #s(mean_VPD_yod1) + 
                 s(mean_sd_VPD) +
                 s(mean_prec_total) +
                 s(mean_temp_total) +
                 s(mean_pre_dist_tree_cover) +
                 s(mean_bare),
               data = hexagons_recov10_centros, method = "REML")

fit.gam_geoloc <- gam(mean_percent_recovered ~ 
                        s(long, lat, bs = "tp") +  
                        s(mean_severity) + 
                        #s(mean_VPD_yod1, by = geolocation) +
                        s(mean_sd_VPD, by = geolocation) +
                        s(mean_temp_total) +
                        s(mean_prec_total) +
                        s(mean_elevation) +
                        s(mean_pre_dist_tree_cover) +
                        s(mean_bare),
                      data = hexagons_recov10_centros, method = "REML")

# prediction df
# Create a new data frame for predictions
new_data <- hexagons_recov10_centros %>%
  select(long, lat, mean_elevation, mean_severity, mean_sd_VPD, 
         mean_prec_total, mean_temp_total, mean_pre_dist_tree_cover, mean_bare, geolocation)

# Add predicted values to the new dataset
new_data$predicted <- predict(fit.gam_geoloc, newdata = new_data, type = "response")

# Convert data to long format for plotting
smooth_data <- new_data %>%
  pivot_longer(cols = c(mean_elevation, mean_severity, mean_sd_VPD, 
                        mean_prec_total, mean_temp_total, 
                        mean_pre_dist_tree_cover, mean_bare),
               names_to = "predictor", values_to = "value")

# Define new facet labels with line breaks
facet_labels <- c(
  "mean_sd_VPD" = "SD of VPD anomalies",
  "mean_temp_total" = "Temperature",
  "mean_prec_total" = "Precipitation",
  "mean_severity" = "Severity",
  "mean_pre_dist_tree_cover" = "Pre-disturbance\ntree cover",
  "mean_bare" = "Post-disturbance\nbare ground share",
  "mean_elevation" = "Elevation"
)

# Rename predictor values explicitly
smooth_data <- smooth_data %>%
  mutate(predictor = recode(predictor, !!!facet_labels))  # Properly renames column

# Define desired order based on the RENAMED facet labels
custom_order <- c(
  "SD of VPD anomalies",
  "Elevation", 
  "Severity",
  "Temperature",
  "Precipitation",
  "Pre-disturbance\ntree cover",
  "Post-disturbance\nbare ground share"
)

# Convert predictor column to a factor using the RENAMED order
smooth_data <- smooth_data %>%
  mutate(predictor = factor(predictor, levels = custom_order))

# Apply labels in facet_wrap
ggplot(smooth_data, aes(x = value, y = predicted)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"), color = "#11828A") +
  facet_wrap(~ predictor, scales = "free_x", nrow = 2) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw(base_size = 18) +
  labs(y = "Predicted recovery success", x = "Predictor values")

ggsave("~/predictors_effect_sd.png", width = 11, height = 6, dpi = 300)


#-------------------------------------------------------------------------------

fit.gam_geoloc <- gam(mean_percent_recovered ~ 
                        s(long, lat, bs = "tp") +  
                        s(mean_severity) + 
                        #s(mean_VPD_yod1, by = geolocation) +
                        s(mean_min_VPD, by = geolocation) +
                        s(mean_temp_total) +
                        s(mean_prec_total) +
                        s(mean_elevation) +
                        s(mean_pre_dist_tree_cover) +
                        s(mean_bare),
                      data = hexagons_recov10_centros, method = "REML")

# prediction df
# Create a new data frame for predictions
new_data <- hexagons_recov10_centros %>%
  select(long, lat, mean_elevation, mean_severity, mean_min_VPD, 
         mean_prec_total, mean_temp_total, mean_pre_dist_tree_cover, mean_bare, geolocation)

# Add predicted values to the new dataset
new_data$predicted <- predict(fit.gam_geoloc, newdata = new_data, type = "response")

# Convert data to long format for plotting
smooth_data <- new_data %>%
  pivot_longer(cols = c(mean_elevation, mean_severity, mean_min_VPD, 
                        mean_prec_total, mean_temp_total, 
                        mean_pre_dist_tree_cover, mean_bare),
               names_to = "predictor", values_to = "value")

# Define new facet labels with line breaks
facet_labels <- c(
  "mean_min_VPD" = "Minimum of VPD anomalies",
  "mean_temp_total" = "Temperature",
  "mean_prec_total" = "Precipitation",
  "mean_severity" = "Severity",
  "mean_pre_dist_tree_cover" = "Pre-disturbance\ntree cover",
  "mean_bare" = "Post-disturbance\nbare ground share",
  "mean_elevation" = "Elevation"
)

# Rename predictor values explicitly
smooth_data <- smooth_data %>%
  mutate(predictor = recode(predictor, !!!facet_labels))  # Properly renames column

# Define desired order based on the RENAMED facet labels
custom_order <- c(
  "Minimum of VPD anomalies",
  "Elevation", 
  "Severity",
  "Temperature",
  "Precipitation",
  "Pre-disturbance\ntree cover",
  "Post-disturbance\nbare ground share"
)

# Convert predictor column to a factor using the RENAMED order
smooth_data <- smooth_data %>%
  mutate(predictor = factor(predictor, levels = custom_order))

# Apply labels in facet_wrap
ggplot(smooth_data, aes(x = value, y = predicted)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"), color = "#11828A") +
  facet_wrap(~ predictor, scales = "free_x", nrow = 2) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw(base_size = 18) +
  labs(y = "Predicted recovery success", x = "Predictor values")

ggsave("~/predictors_effect_min.png", width = 11, height = 6, dpi = 300)



#-------------------------------------------------------------------------------

fit.gam_geoloc <- gam(mean_percent_recovered ~ 
                        s(long, lat, bs = "tp") +  
                        s(mean_severity) + 
                        #s(mean_VPD_yod1, by = geolocation) +
                        s(mean_max_VPD, by = geolocation) +
                        s(mean_temp_total) +
                        s(mean_prec_total) +
                        s(mean_elevation) +
                        s(mean_pre_dist_tree_cover) +
                        s(mean_bare),
                      data = hexagons_recov10_centros, method = "REML")

# prediction df
# Create a new data frame for predictions
new_data <- hexagons_recov10_centros %>%
  select(long, lat, mean_elevation, mean_severity, mean_max_VPD, 
         mean_prec_total, mean_temp_total, mean_pre_dist_tree_cover, mean_bare, geolocation)

# Add predicted values to the new dataset
new_data$predicted <- predict(fit.gam_geoloc, newdata = new_data, type = "response")

# Convert data to long format for plotting
smooth_data <- new_data %>%
  pivot_longer(cols = c(mean_elevation, mean_severity, mean_max_VPD, 
                        mean_prec_total, mean_temp_total, 
                        mean_pre_dist_tree_cover, mean_bare),
               names_to = "predictor", values_to = "value")

# Define new facet labels with line breaks
facet_labels <- c(
  "mean_max_VPD" = "Maximum of VPD anomalies",
  "mean_temp_total" = "Temperature",
  "mean_prec_total" = "Precipitation",
  "mean_severity" = "Severity",
  "mean_pre_dist_tree_cover" = "Pre-disturbance\ntree cover",
  "mean_bare" = "Post-disturbance\nbare ground share",
  "mean_elevation" = "Elevation"
)

# Rename predictor values explicitly
smooth_data <- smooth_data %>%
  mutate(predictor = recode(predictor, !!!facet_labels))  # Properly renames column

# Define desired order based on the RENAMED facet labels
custom_order <- c(
  "Maximum of VPD anomalies",
  "Elevation", 
  "Severity",
  "Temperature",
  "Precipitation",
  "Pre-disturbance\ntree cover",
  "Post-disturbance\nbare ground share"
)

# Convert predictor column to a factor using the RENAMED order
smooth_data <- smooth_data %>%
  mutate(predictor = factor(predictor, levels = custom_order))

# Apply labels in facet_wrap
ggplot(smooth_data, aes(x = value, y = predicted)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"), color = "#11828A") +
  facet_wrap(~ predictor, scales = "free_x", nrow = 2) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_bw(base_size = 18) +
  labs(y = "Predicted recovery success", x = "Predictor values")

ggsave("~/predictors_effect_max.png", width = 11, height = 6, dpi = 300)



### per geoloc
# with tensor interaction smooth
fit.gam_interaction <- gam(mean_percent_recovered ~ 
                             s(long, lat, bs = "tp") +  
                             s(mean_severity) + 
                             s(mean_max_VPD, by = geolocation) +
                             s(mean_temp_total) +
                             s(mean_prec_total) +
                             s(mean_elevation) +
                             s(mean_pre_dist_tree_cover) +
                             s(mean_bare),
                           data = hexagons_recov10_centros, method = "REML")

### investigate the isolated of VPD in different ecoregions
# Get median values of all predictors except VPD
# Get median values of all predictors except VPD
fixed_values <- hexagons_recov10_centros %>%
  summarise(across(c(long, lat, mean_elevation, mean_severity, mean_prec_total, 
                     mean_temp_total, mean_pre_dist_tree_cover, mean_bare),
                   ~ median(., na.rm = TRUE)))  # Use median to avoid outliers

# Create a new dataset varying only VPD across its observed range
VPD_range <- seq(min(hexagons_recov10_centros$mean_max_VPD, na.rm = TRUE),
                 max(hexagons_recov10_centros$mean_max_VPD, na.rm = TRUE), 
                 length.out = 100)  # 100 evenly spaced points

# Expand grid of VPD values and geolocation categories
new_VPD_data <- expand.grid(
  mean_max_VPD = VPD_range,
  geolocation = unique(hexagons_recov10_centros$geolocation)  # Keep geolocations
) %>%
  cross_join(fixed_values)  # Attach fixed predictor values, including long & lat

# Ensure geolocation is a factor
new_VPD_data <- new_VPD_data %>%
  mutate(geolocation = as.factor(geolocation))

# Predict recovery success while holding other predictors constant
new_VPD_data$predicted <- predict(fit.gam_interaction, newdata = new_VPD_data, type = "response")

# Compute confidence intervals (assuming normal approximation)
new_VPD_data <- new_VPD_data %>%
  mutate(
    se_fit = predict(fit.gam_interaction, newdata = new_VPD_data, se.fit = TRUE)$se.fit,
    lower = predicted - 1.96 * se_fit,  # 95% confidence interval lower bound
    upper = predicted + 1.96 * se_fit   # 95% confidence interval upper bound
  )

new_VPD_data <- new_VPD_data %>%
  mutate(geolocation = fct_recode(geolocation,
                                  "Eastern Alps - north" = "eastern alps - north",
                                  "Eastern Alps - central" = "eastern alps - central",
                                  "Eastern Alps - south" = "eastern alps - south",
                                  "Western Alps - north" = "western alps - north",
                                  "Western Alps - south" = "western alps - south"
  ))



# Define custom colors for each geolocation
custom_colors <- c(
  "Eastern Alps - north" = "#321325",
  "Eastern Alps - central" = "#5F0F40",
  "Eastern Alps - south" = "#9A031E",
  "Western Alps - north" = "#CB793A",
  "Western Alps - south" = "#FCDC4D"
)

new_VPD_data <- new_VPD_data %>%
  mutate(geolocation = factor(geolocation, levels = c(
    "Eastern Alps - north", 
    "Eastern Alps - central", 
    "Eastern Alps - south", 
    "Western Alps - north",
    "Western Alps - south"
  )))

# Plot the isolated VPD effect with geom_ribbon() for confidence intervals
ggplot(new_VPD_data, aes(x = mean_max_VPD, y = predicted, color = geolocation)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = geolocation), alpha = 0.2, color = NA) +  # Confidence interval
  geom_line(size = 1.2) +  # Main effect line
  facet_wrap(~ geolocation, scales = "free_x") +  # One subplot per geolocation
  theme_bw(base_size = 18) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +  # Custom colors for ribbons
  labs(y = "Predicted recovery success [%]", x = "VPD anomalies") +
  ggtitle("") +
  theme(legend.position = "none") 

ggsave("~/eo_nas/EO4Alps/figs/predicted_VPD_1803.png", width = 11, height = 6, dpi = 300)







