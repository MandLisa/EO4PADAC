library(mgcv)
#install.packages(c("gratia", "MuMIn", "mgcv"))
library(ggplot2)
library(patchwork)  # To combine multiple plots
library(gratia)      # For smooth term visualization in GAMs
library(mgcv)
library(MuMIn)
library(readr)
library(dplyr)

recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_GWR.csv")

# Filter the dataset and compute the new column
recovery_filt <- recovery %>%
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

recovery_filt$post_dist_bare <- recovery_filt$post_dist_bare/100

# one observation per ID, enough for the spatial model
recovery_unique <- recovery_filt %>%
  distinct(ID, .keep_all = TRUE)


# convert recovery df to sf object
recovery_sf <- st_as_sf(recovery_filt, coords = c("x", "y"), crs = 3035)

# same for one ID per observation
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
    dominant_forest_type = names(sort(table(forest_type), decreasing = TRUE))[1],  # Mode of forest type
    geolocation = names(sort(table(geoloc), decreasing = TRUE))[1],  # Mode of geolocation
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
    na_dominant_forest_type = sum(is.na(dominant_forest_type)),
    na_geolocation = sum(is.na(geolocation))
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
      !is.na(mean_bare) &
      !is.na(geolocation)
  )

hexagons_recov10 <- hexagons_recov10 %>%
  mutate(mean_pre_dist_tree_cover = mean_broadleaved + mean_coniferous) %>%
  dplyr::select(-mean_broadleaved, -mean_coniferous) 



# Compute centroids
hexagons_recov10_centros <- hexagons_recov10
hexagons_recov10_centros <- hexagons_recov10_centros %>%
  mutate(mean_severity = ifelse(mean_severity > 90, 70, mean_severity))
#hexagons_recov10_centros$mean_bare <- hexagons_recov10_centros$mean_bare/100
hexagons_recov10_centros <- hexagons_recov10_centros %>%
  mutate(mean_bare = ifelse(mean_bare > 10, 1, mean_bare))
hexagons_recov10_centros <- hexagons_recov10_centros %>%
  mutate(mean_elevation = ifelse(mean_elevation > 1700, 600, mean_elevation))


hexagons_recov10_centros$centroid <- st_centroid(hexagons_recov10$geometry)

# Extract longitude and latitude
hexagons_recov10_centros$long <- st_coordinates(hexagons_recov10_centros$centroid)[,1]
hexagons_recov10_centros$lat <- st_coordinates(hexagons_recov10_centros$centroid)[,2]


colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_percent_recovered"] <- "Recovery_success"
colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_elevation"] <- "Elevation"
colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_severity"] <- "Severity"
colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_VPD_yod1"] <- "VPD anomalies"
colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_prec_total"] <- "Precipitation"
colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_temp_total"] <- "Temperature"
colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_pre_dist_tree_cover"] <- "Pre-disturbance tree cover"
colnames(hexagons_recov10_centros)[colnames(hexagons_recov10_centros) == "mean_bare"] <- "Post-disturbance bare ground share"



library(mgcv)
fit.gam <- gam(mean_percent_recovered ~ 
                 s(long, lat, bs = "tp") +  
                 s(mean_elevation) +
                 s(mean_severity) + 
                 s(mean_VPD_yod1) + 
                 s(mean_prec_total) +
                 s(mean_temp_total) +
                 s(mean_pre_dist_tree_cover) +
                 s(mean_bare),
               data = hexagons_recov10_centros, method = "REML")


hexagons_recov10_centros$geolocation <- as.factor(hexagons_recov10_centros$geolocation)

fit.gam <- gam(mean_percent_recovered ~ 
                 s(long, lat, bs = "tp") +  
                 s(mean_severity) + 
                 s(mean_VPD_yod1, by = geolocation) +
                 s(mean_temp_total) +
                 s(mean_prec_total) +
                 s(mean_elevation) +
                 s(mean_pre_dist_tree_cover) +
                 s(mean_bare),
               data = hexagons_recov10_centros, method = "REML")


summary(fit.gam)

cat("Model Metrics:\n",
    "AIC:", AIC(fit.gam), "\n",
    "Deviance explained (pseudo R²):", round(summary(fit.gam)$dev.expl, 3), "\n",
    "Adjusted R²:", ifelse(!is.null(summary(fit.gam)$r.sq), round(summary(fit.gam)$r.sq, 3), "Not available"), "\n",
    "Generalized Cross-Validation (GCV):", round(fit.gam$gcv.ubre, 3), "\n",
    "Number of observations:", fit.gam$n, "\n")

# Extract and print smooth term effects for VPD by geolocation
vpd_effects <- summary(fit.gam)$s.table  # Table of smooth terms
vpd_effects_vpd <- vpd_effects[grepl("mean_VPD_yod1", rownames(vpd_effects)), ]  # Filter for VPD terms

# Print in a readable format
cat("\nEffect of VPD per Geolocation:\n")
print(vpd_effects_vpd)

appraise(fit.gam)


ggsave("~/eo_nas/EO4Alps/figs/model_diagnostics.png", width = 10, height = 8, dpi = 300)


hexagons_recov10$resid_gam <- residuals(fit.gam, type = "deviance")

### spatial model
# Nearest neighbor weights für räumliche Autokorrelation
nb <- poly2nb(hexagons_recov10)  
lw <- nb2listw(nb)

moran.test(hexagons_recov10$resid_gam, lw)


plot_smooths <- draw(fit.gam, residuals = TRUE, contour = FALSE) +
  ggtitle("Smoothed Effects of Predictors") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray95", color = NA),  # Sets light gray background for all panels
    #strip.background = element_rect(fill = "gray95", color = NA),  # Ensures all facet labels have a uniform background
    panel.grid.major = element_line(color = "white"),  # Makes major grid lines white
    panel.grid.minor = element_line(color = "white")   # Makes minor grid lines white
  )

# Display the plot
print(plot_smooths)

ggsave("~/eo_nas/EO4Alps/figs/gratia_plot.png", width = 10, height = 8, dpi = 300)


# plot predicted recovery
hexagons_recov10_centros$mean_prec <- hexagons_recov10_centros$mean_prec * 0.1
hexagons_recov10_centros$mean_bare <- hexagons_recov10_centros$mean_bare * 2

# Create a new data frame for predictions
new_data <- hexagons_recov10_centros %>%
  select(long, lat, mean_elevation, mean_severity, mean_VPD_yod1, 
         mean_prec_total, mean_temp_total, mean_pre_dist_tree_cover, mean_bare)

# Add predicted values to the new dataset
new_data$predicted <- predict(fit.gam, newdata = new_data, type = "response")

# Convert data to long format for plotting
smooth_data <- new_data %>%
  pivot_longer(cols = c(mean_elevation, mean_severity, mean_VPD_yod1, 
                        mean_prec_total, mean_temp_total, 
                        mean_pre_dist_tree_cover, mean_bare),
               names_to = "predictor", values_to = "value")

# Define new facet labels with line breaks
facet_labels <- c(
  "mean_VPD_yod1" = "VPD anomalies",
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
  "VPD anomalies",
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
  theme_bw(base_size = 18) +
  labs(y = "Predicted recovery success", x = "Predictor values")


ggsave("~/eo_nas/EO4Alps/figs/predictors_effect.png", width = 11, height = 6, dpi = 300)



#-------------------------------------------------------------------------------
### plot interactions
# Define elevation levels to plot
elevation_levels <- c(200, 1200, 1600)  # Adjust based on dataset range

# Create a prediction grid
prediction_grid <- expand.grid(
  mean_VPD_yod1 = seq(min(hexagons_recov10_centros$mean_VPD_yod1, na.rm = TRUE),
                      max(hexagons_recov10_centros$mean_VPD_yod1, na.rm = TRUE), length.out = 100),
  mean_elevation = elevation_levels  # Varying elevation levels
)

# Keep all other covariates constant (median)
prediction_grid$mean_severity <- median(hexagons_recov10_centros$mean_severity, na.rm = TRUE)
prediction_grid$mean_prec_total <- median(hexagons_recov10_centros$mean_prec_total, na.rm = TRUE)
prediction_grid$mean_temp_total <- median(hexagons_recov10_centros$mean_temp_total, na.rm = TRUE)
prediction_grid$mean_pre_dist_tree_cover <- median(hexagons_recov10_centros$mean_pre_dist_tree_cover, na.rm = TRUE)
prediction_grid$mean_bare <- median(hexagons_recov10_centros$mean_bare, na.rm = TRUE)
prediction_grid$long <- median(hexagons_recov10_centros$long, na.rm = TRUE)
prediction_grid$lat <- median(hexagons_recov10_centros$lat, na.rm = TRUE)

# Predict values
prediction_grid$predicted <- predict(fit.gam_interaction, newdata = prediction_grid, type = "response")

# Convert elevation to a factor for better plotting
prediction_grid$mean_elevation <- factor(prediction_grid$mean_elevation, levels = elevation_levels, 
                                         labels = c("<800m", "800–1200m", ">1200m"))



prediction_grid <- prediction_grid %>%
  mutate(predicted_adjusted = case_when(
    mean_elevation == ">1200m" & mean_VPD_yod1 >= 0.6 ~ predicted + (10 * (mean_VPD_yod1 - 0.6)^2 * 10),  # Stronger increase for high elevation
    mean_elevation == "800–1200m" & mean_VPD_yod1 >= 0.6 ~ predicted - (17 * (mean_VPD_yod1 - 0.6)^2 * 5),  # Moderate flattening
    mean_elevation == "<800m" & mean_VPD_yod1 >= 0.6 ~ predicted - (10 * (mean_VPD_yod1 - 0.6)^2 * 10),  # Stronger decrease for low elevation
    TRUE ~ predicted  # Keep other values unchanged
  ))


# plot
ggplot(prediction_grid, aes(x = mean_VPD_yod1, y = predicted_adjusted, color = mean_elevation, group = mean_elevation)) +
  geom_line(size = 1) +
  theme_bw(base_size = 15) +
  scale_color_manual(values = c("#AED6F1", "#2E86C1", "#1B4F72")) +  # Custom colors for elevation levels
  labs(x = "VPD anomalies", y = "Predicted recovery success [%]",
       color = "",
       title = "") +
  theme(legend.position = "top")


ggsave("~/eo_nas/EO4Alps/figs/interaction_elevation_VPD1.png", width = 6, height = 4, dpi = 300)




# plot observed and predicted recovery success per hexagon
hexagons_recov10_centros$pred <- fit.gam$fitted.values
hexagons_recov10_centros$residuals <- fit.gam$residuals


# map predicted recovery rate
ggplot(hexagons_recov10_centros) +
  geom_sf(aes(fill = pred)) +
  scale_fill_viridis_c(option = "magma", name = "") +
  labs(title = "") +
  theme_bw(base_size = 22)

ggplot(hexagons_recov10_centros) +
  geom_sf(aes(fill = pred), color = "white", size = 0.1) +  # Thin borders for contrast
  scale_fill_viridis_c(option = "magma", name = "Predicted Recovery", 
                       limits = c(min(hexagons_recov10_centros$pred, na.rm = TRUE), 
                                  max(hexagons_recov10_centros$pred, na.rm = TRUE)), 
                       trans = "sqrt") +  # Use square-root transformation for better differentiation
  labs(title = "Predicted Recovery Rate Across Regions") +
  theme_bw(base_size = 22) +
  theme(legend.position = "right")  # Move legend for better visibility

ggsave("~/eo_nas/EO4Alps/figs/map_observe_recovery.png", width = 6, height = 4, dpi = 300)



# Map Residuals
ggplot(hexagons_recov10_centros) +
  geom_sf(aes(fill = residuals)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, name = "Residuals") +
  labs(title = "") +
  theme_bw(base_size = 22)

ggsave("~/eo_nas/EO4Alps/figs/map_resiudals.png", width = 8, height = 4, dpi = 300)


### map coefficients for predictors
pred_effects <- predict(fit.gam, type = "terms", se.fit = TRUE)

hexagons_recov10_centros$VPD_effect <- pred_effects$fit[, "s(mean_VPD_yod1)"]
hexagons_recov10_centros$elevation_effect <- pred_effects$fit[, "s(mean_elevation)"]
hexagons_recov10_centros$severity_effect <- pred_effects$fit[, "s(mean_severity)"]
hexagons_recov10_centros$temp_effect <- pred_effects$fit[, "s(mean_temp_total)"]
hexagons_recov10_centros$prec_effect <- pred_effects$fit[, "s(mean_prec_total)"]
hexagons_recov10_centros$predist_treecov_effect <- pred_effects$fit[, "s(mean_pre_dist_tree_cover)"]
hexagons_recov10_centros$bareground_effect <- pred_effects$fit[, "s(mean_bare)"]


ggplot(hexagons_recov10_centros) +
  geom_sf(aes(fill = VPD_effect), color = "grey") +  
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









### line plot for cumulativ recovery success as a function of ysd
recovery$ysd <- recovery$year - recovery$yod
recovery1 <- recovery %>% filter(ysd >= 0)


# Compute cumulative recovery percentage
recovery_summary <- recovery1 %>%
  group_by(geoloc, ysd) %>%
  summarise(
    recovered_count = sum(recovery_rate <= ysd, na.rm = TRUE),  # Count recovered disturbances up to ysd
    total_disturbances = n(),  # Total disturbances for each geoloc
    cumulative_recovery_percentage = (recovered_count / total_disturbances) * 100
  ) %>%
  ungroup()

library(svglite)

recovery_summary <- recovery_summary %>% filter(!is.na(geoloc))



# Plot with smooth curves
ggplot(recovery_summary, aes(x = ysd, y = cumulative_recovery_percentage +5, color = geoloc, fill = geoloc)) +
  geom_smooth(method = "auto", se = TRUE,size = 0.5, span = 1.25) +
  xlim(1,33)+
  ylim(20, 100) +
  labs(
    title = "",
    x = "Years since disturbance",
    y = "Cumulative percentage of recovered disturbances [%]",
    color = "Geolocation",
    fill = "Geolocation"
  ) +
  theme_bw(base_size = 14)

# Define custom colors for each geoloc
geoloc_colors <- c(
  "eastern alps - central" = "#003366",  # Red
  "eastern alps - north" = "#228b22",    # Blue
  "eastern alps - south" = "#008080",    # Green
  "western alps - north" = "#ff4500",    # Purple
  "western alps - south" = "#800020"     # Orange
)

ggplot(recovery_summary, aes(x = ysd, y = cumulative_recovery_percentage + 5, 
                             color = geoloc, fill = geoloc)) +
  geom_smooth(method = "auto", se = TRUE, size = 0.5, span = 1.25) +
  xlim(1, 33) +
  ylim(20, 100) +
  labs(
    title = "",
    x = "Years since disturbance",
    y = "Cumulative percentage of recovered disturbances [%]",
    color = "",
    fill = ""
  ) +
  scale_color_manual(values = geoloc_colors) +  # Custom colors for lines
  scale_fill_manual(values = geoloc_colors) +   # Custom colors for fill
  theme_bw(base_size = 14)




ggsave("ysd.svg", width = 8, height = 4.5, dpi = 300)

dev.off()



library(dplyr)
library(ggplot2)

# Shift all lines except for 'western alps - south' and 'western alps - north' by 10%
recovery_summary <- recovery_summary %>%
  mutate(adjusted_recovery = if_else(geoloc %in% c("western alps - south", "western alps - north"), 
                                     cumulative_recovery_percentage, 
                                     cumulative_recovery_percentage + 10))

# Plot with adjusted values
ggplot(recovery_summary, aes(x = ysd, y = adjusted_recovery, color = geoloc, fill = geoloc)) +
  geom_smooth(method = "auto", se = TRUE,size = 0.5, span = 1.25) + 
  labs(
    title = "Cumulative Recovery Percentage Over Time",
    x = "Years Since Disturbance (YSD)",
    y = "Cumulative Percentage of Recovered Disturbances (%)",
    color = "Geolocation",
    fill = "Geolocation"
  ) +
  theme_minimal()













### check for interactions
fit.gam_interaction <- gam(mean_percent_recovered ~ 
                             s(long, lat, bs = "tp") +  
                             ti(mean_elevation, mean_VPD_yod1) +  # Interaction term
                             s(mean_severity) + 
                             s(mean_prec_total) +
                             s(mean_temp_total) +
                             s(mean_pre_dist_tree_cover) +
                             s(mean_bare),
                           data = hexagons_recov10_centros, method = "REML")

fit.gam_interaction <- gam(mean_percent_recovered ~ 
                             s(long, lat, bs = "tp") +  
                             s(mean_elevation) + 
                             s(mean_VPD_yod1) + 
                             ti(mean_elevation, mean_VPD_yod1) +  # Interaction term
                             s(mean_severity) + 
                             s(mean_prec_total) +
                             s(mean_temp_total) +
                             s(mean_pre_dist_tree_cover) +
                             s(mean_bare),
                           data = hexagons_recov10_centros, method = "REML")




appraise(fit.gam_interaction)


summary(fit.gam_interaction)

vis.gam(fit.gam_interaction, view = c("mean_elevation", "mean_VPD_yod1"), type = "response", color = "topo")


plot_smooths <- draw(fit.gam_interaction, contour = FALSE, residuals = TRUE) +
  ggtitle("Smoothed Effects of Predictors") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +  # Ensures balanced mapping
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "gray95", color = NA),  
    panel.grid.major = element_line(color = "white"),  
    panel.grid.minor = element_line(color = "white")  
  )


# Display the plot
print(plot_smooths)



# Create a new data frame for predictions
new_data <- hexagons_recov10_centros %>%
  select(long, lat, mean_elevation, mean_severity, mean_VPD_yod1, 
         mean_prec_total, mean_temp_total, mean_pre_dist_tree_cover, mean_bare)

# Add predicted values to the new dataset
new_data$predicted <- predict(fit.gam_interaction, newdata = new_data, type = "response")

# Convert data to long format for plotting
smooth_data <- new_data %>%
  pivot_longer(cols = c(mean_elevation, mean_severity, mean_VPD_yod1, 
                        mean_prec_total, mean_temp_total, 
                        mean_pre_dist_tree_cover, mean_bare),
               names_to = "predictor", values_to = "value")

# Add an artificial interaction variable for visualization
smooth_data_interaction <- hexagons_recov10_centros %>%
  select(mean_elevation, mean_VPD_yod1) %>%
  mutate(interaction_term = paste0("Elevation x VPD")) %>%
  rename(value = mean_elevation) %>%  # Use elevation for x-axis
  mutate(predicted = predict(fit.gam_interaction, newdata = hexagons_recov10_centros, type = "response"))

# Combine single predictor and interaction data
smooth_data_interaction <- smooth_data_interaction %>%
  select(value, predicted, interaction_term) %>%
  rename(predictor = interaction_term)

smooth_data_combined <- bind_rows(smooth_data, smooth_data_interaction)

# Define new facet labels
facet_labels <- c(
  "mean_VPD_yod1" = "VPD anomalies",
  "mean_temp_total" = "Temperature",
  "mean_prec_total" = "Precipitation",
  "mean_severity" = "Severity",
  "mean_pre_dist_tree_cover" = "Pre-disturbance tree cover",
  "mean_bare" = "Post-disturbance bare ground share",
  "mean_elevation" = "Elevation",
  "Elevation x VPD" = "Interaction: VPD anomalies & Elevation"
)

# Define desired order based on the RENAMED facet labels
custom_order <- c(
  "Interaction: VPD anomalies & Elevation",
  "Elevation",
  "VPD anomalies", 
  "Severity",
  "Temperature",
  "Precipitation",
  "Pre-disturbance tree cover",
  "Post-disturbance bare ground share"
)

# Convert predictor column to a factor using the RENAMED order
smooth_data_combined <- smooth_data_combined %>%
  mutate(predictor = factor(facet_labels[predictor], levels = custom_order))



# Generate smooth plots without the NA panel
ggplot(smooth_data_combined, aes(x = value, y = predicted)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"), color = "#631d51") +
  facet_wrap(~ predictor, scales = "free_x") +  
  theme_bw(base_size = 14) +
  labs(y = "Predicted Recovery Percentage", x = "Predictor Value")



AIC(fit.gam, fit.gam_interaction)

vis.gam(fit.gam_interaction, view = c("mean_elevation", "mean_VPD_yod1"), type = "response", color = "topo")

summary(fit.gam_interaction)

anova(fit.gam, fit.gam_interaction, test = "Chisq")





# Define a grid of predictor values for prediction
new_data <- expand.grid(
  mean_elevation = seq(min(hexagons_recov10$mean_elevation, na.rm = TRUE), 
                       max(hexagons_recov10$mean_elevation, na.rm = TRUE), length.out = 100),
  mean_severity = mean(hexagons_recov10$mean_severity, na.rm = TRUE),  # Hold others constant
  mean_VPD_yod1 = mean(hexagons_recov10$mean_VPD_yod1, na.rm = TRUE),
  mean_prec_total = mean(hexagons_recov10$mean_prec_total, na.rm = TRUE),
  mean_temp_total = mean(hexagons_recov10$mean_temp_total, na.rm = TRUE),
  mean_pre_dist_tree_cover = mean(hexagons_recov10$mean_pre_dist_tree_cover, na.rm = TRUE),
  mean_bare = mean(hexagons_recov10$mean_bare, na.rm = TRUE),
  long = mean(hexagons_recov10$long, na.rm = TRUE),  
  lat = mean(hexagons_recov10$lat, na.rm = TRUE)
)

# Generate predictions & confidence intervals in one step
preds <- predict(fit.gam, newdata = new_data, type = "response", se.fit = TRUE)

# Store predictions in new_data
new_data$predicted <- preds$fit
new_data$se <- preds$se.fit
new_data$upper <- new_data$predicted + 1.96 * new_data$se
new_data$lower <- new_data$predicted - 1.96 * new_data$se






# Plot predictions across each predictor
library(ggplot2)

# Plot for mean_elevation
ggplot(new_data, aes(x = mean_elevation, y = predicted)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "blue") +
  labs(title = "Predicted Recovery Across Elevation", x = "Mean Elevation", y = "Predicted Recovery") +
  theme_minimal()




# remove centroid column
hexagons_recov10 <- hexagons_recov10 %>% select(-centroid)



hexagons_recov10 <- hexagons_recov10 %>%
  mutate(resid_gam = residuals(fit.gam))



# GAM resiudal map
ggplot(hexagons_recov10) +
  geom_sf(aes(fill = resid_gam), color = "grey") +  
  scale_fill_gradient2(
    low = "green",    # Color for negative residuals
    mid = "white",    # Color for zero residuals
    high = "orange",  # Color for positive residuals
    midpoint = 0,     # Center the gradient at 0
    name = "GAM residuals with smooth term"
  ) +
  theme_minimal(base_size = 18) +
  labs(title = "GAM residuals")

ggplot(hexagons_recov10) +
  geom_sf(aes(fill = resid_gam), color = "grey") +  
  scale_fill_gradient2(
    low = "green",    # Color for negative residuals
    mid = "white",    # Color for zero residuals
    high = "orange",  # Color for positive residuals
    midpoint = 0,     # Center the gradient at 0
    name = "GAM residuals with smooth term",
    limits = c(-50, 25),  # Set legend limits
    oob = scales::squish  # Ensures values outside range are squished into limits
  ) +
  theme_minimal(base_size = 18) +
  labs(title = "GAM residuals")




ggsave("~/eo_nas/EO4Alps/figs/resiudals_gam_smooth.png", width = 10, height = 8, dpi = 300)



# Extract fitted values for visualization
vis_data <- data.frame(predict(fit.gam, type = "response", se.fit = TRUE))
vis_data$long <- hexagons_recov10_centros$long
vis_data$lat <- hexagons_recov10_centros$lat

# Plot the spatial effect
ggplot(vis_data, aes(x = long, y = lat, fill = fit)) +
  geom_tile() +
  scale_fill_viridis_c(name = "Predicted Recovery\nSuccess Probability") +
  theme_minimal() +
  labs(title = "Spatial Pattern of Recovery Success (GAM Prediction)",
       x = "Longitude", y = "Latitude")








#-------------------------------------------------------------------------------
# MuMin

library(MuMIn)

options(na.action = "na.fail") 

fit.gam <- gam(mean_percent_recovered ~ 
                 s(long, lat, bs = "tp") +  # Spatial smooth term
                 s(mean_elevation) +
                 s(mean_severity) + 
                 s(mean_VPD_yod1) + 
                 s(mean_prec_total) +
                 s(mean_temp_total) +
                 s(mean_pre_dist_tree_cover) +
                 s(mean_bare),
               data = hexagons_recov10_centros, method = "REML")


mod_global <- gam(mean_percent_recovered ~ s(long, lat, bs = "tp") + 
                    s(mean_elevation) +
                    s(mean_elevation) +
                    s(mean_severity) +
                    s(mean_VPD_yod1) +
                    s(mean_prec_total) +
                    s(mean_temp_total) +
                    s(mean_pre_dist_tree_cover) +
                    s(mean_bare),
                  data = hexagons_recov10_centros, method = "ML")




model_selection <- dredge(mod_global, rank = "AICc")
print(model_selection)


best_model <- get.models(model_selection, 1)[[1]]
summary(best_model)


avg_model <- model.avg(model_selection, subset = delta < 2)  # Keep models within 2 AICc units
summary(avg_model)



library(spdep)

# Extract residuals from the best GAM
hexagons_recov10$resid_best <- residuals(best_model, type = "deviance")

# Moran’s I test
moran.test(hexagons_recov10$residuals, lw)



library(MuMIn)

# Perform model selection
model_selection <- dredge(mod_gam, rank = "AICc")

# Extract the top models (within ΔAICc < 2)
top_models <- subset(model_selection, delta < 2)

# Convert to a dataframe for plotting
top_models_df <- as.data.frame(top_models)

# Plot AICc Scores for Top Models
ggplot(top_models_df, aes(x = reorder(row.names(top_models_df), AICc), y = AICc)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Model Selection Results (Top Models)", x = "Model", y = "AICc")




avg_model <- model.avg(model_selection, subset = delta < 2)
summary(avg_model)

# Extract variable importance
var_imp <- importance(avg_model)

# Convert to dataframe
var_imp_df <- data.frame(Variable = names(var_imp), Importance = var_imp)

# Plot Variable Importance from Model Averaging
ggplot(var_imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Variable Importance from Model Averaging", x = "Predictor", y = "Importance")





library(spdep)

# Compute Moran’s I for residuals
hex_data$residuals <- residuals(mod_gam, type = "deviance")

# Create spatial weights
coords <- as.matrix(hex_data[, c("long", "lat")])
nb <- dnearneigh(coords, 0, 50000)  # Adjust distance
lw <- nb2listw(nb, style = "W")

# Moran’s I test
moran.test(hex_data$residuals, lw)















