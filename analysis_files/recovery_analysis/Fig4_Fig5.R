library(mgcv)
library(ggplot2)
library(patchwork)  
library(gratia)    
library(MuMIn)
library(readr)
library(sf)
library(dplyr)



recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_GWR.csv")

#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------

### fit GAM
hexagons_recov10_centros$geolocation <- as.factor(hexagons_recov10_centros$geolocation)
hexagons_recov10_centros$mean_prec_total <- hexagons_recov10_centros$mean_prec_total * 0.1
hexagons_recov10_centros$mean_bare <- hexagons_recov10_centros$mean_bare * 2

# with tensor interaction smooth
fit.gam_interaction <- gam(mean_percent_recovered ~ 
                             s(long, lat, bs = "tp") +  
                             s(mean_severity) + 
                             s(mean_VPD_yod1, by = geolocation) +
                             s(mean_temp_total) +
                             s(mean_prec_total) +
                             s(mean_elevation) +
                             s(mean_pre_dist_tree_cover) +
                             s(mean_bare),
                           data = hexagons_recov10_centros, method = "REML")

plot(fit.gam_interaction)
appraise(fit.gam)
summary(fit.gam)

### extract residuals
hexagons_recov10_centros$resid_gam <- residuals(fit.gam_interaction, type = "deviance")

### prediction grid
# Create a new data frame for predictions
new_data <- hexagons_recov10_centros %>%
  dplyr::select(long, lat, mean_elevation, mean_severity, mean_VPD_yod1, 
                mean_prec_total, mean_temp_total, mean_pre_dist_tree_cover, mean_bare, geolocation)

# Add predicted values to the new dataset
new_data$predicted <- predict(fit.gam_interaction, newdata = new_data, type = "response")

# Convert data to long format for plotting
data_long <- new_data %>%
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
data_long <- data_long %>%
  mutate(predictor = recode(predictor, !!!facet_labels))  

# Define order
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
data_long <- data_long %>%
  mutate(predictor = factor(predictor, levels = custom_order))

# Apply labels in facet_wrap
ggplot(data_long, aes(x = value, y = predicted)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"), color = "#11828A") +
  facet_wrap(~ predictor, scales = "free_x", nrow = 2) + 
  theme_bw(base_size = 18) +
  labs(y = "Predicted recovery success", x = "Predictor values")

# Apply labels in facet_wrap
ggplot(data_long, aes(x = value, y = predicted)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"), color = "#11828A") +
  facet_wrap(~ predictor, scales = "free_x", nrow = 2) + 
  theme_bw(base_size = 18) +
  labs(y = "Predicted recovery success", x = "Predictor values")



ggsave("~/eo_nas/EO4Alps/figs/predicted_recovery_1803.png", width = 11, height = 6, dpi = 300)

dev.off()


### investigate the isolated of VPD in different ecoregions
# Get median values of all predictors except VPD
# Get median values of all predictors except VPD
fixed_values <- hexagons_recov10_centros %>%
  summarise(across(c(long, lat, mean_elevation, mean_severity, mean_prec_total, 
                     mean_temp_total, mean_pre_dist_tree_cover, mean_bare),
                   ~ median(., na.rm = TRUE)))  # Use median to avoid outliers

# Create a new dataset varying only VPD across its observed range
VPD_range <- seq(min(hexagons_recov10_centros$mean_VPD_yod1, na.rm = TRUE),
                 max(hexagons_recov10_centros$mean_VPD_yod1, na.rm = TRUE), 
                 length.out = 100)  # 100 evenly spaced points

# Expand grid of VPD values and geolocation categories
new_VPD_data <- expand.grid(
  mean_VPD_yod1 = VPD_range,
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
ggplot(new_VPD_data, aes(x = mean_VPD_yod1, y = predicted, color = geolocation)) +
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



#-------------------------------------------------------------------------------
### with interactions
#-------------------------------------------------------------------------------

# add height classes
hexagons_recov10_centros<- hexagons_recov10_centros %>%
  mutate(height_class = cut(mean_elevation, c(0,800, 1200, 5000)))

hexagons_recov10_centros<- hexagons_recov10_centros %>%
  mutate(temp_class = cut(mean_temp_total, c(0,18, 30)))

hist(hexagons_recov10_centros$mean_temp_total)

hexagons_recov10_centros$geoloc <- as.factor(hexagons_recov10_centros$geoloc)



#-------------------------------------------------------------------------------

# add residuals
hexagons_recov10_centros$resid_gam <- residuals(fit.gam_interaction, type = "deviance")




ggplot(hexagons_recov10_centros) +
  geom_sf(aes(fill = VPD_effect), color = "grey") +  
  scale_fill_gradient2(
    low = "#126D0E",      
    mid = "white",     
    high = "#E69E03",     
    midpoint = 0,       
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








