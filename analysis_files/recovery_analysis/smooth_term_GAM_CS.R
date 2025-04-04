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


### compute VPD anomalies for a time window of 10years post-dist
# Convert to integer explicitly
recovery_filt <- recovery_filt %>%
  mutate(year = as.integer(year),
         yod = as.integer(yod))

recovery_filt <- recovery_filt %>%
  group_by(ID) %>%  # Group by ID to ensure the same time series is treated together
  mutate(post_dist_bare = first(na.omit(post_dist_bare))) %>%  # Fill with the first non-NA value
  ungroup()

recovery_filt <- recovery_filt %>%
  group_by(ID) %>%  # Group by ID to ensure the same time series is treated together
  mutate(pre_dist_coni = first(na.omit(pre_dist_coni))) %>%  # Fill with the first non-NA value
  ungroup()

recovery_filt <- recovery_filt %>%
  group_by(ID) %>%  # Group by ID to ensure the same time series is treated together
  mutate(pre_dist_broadl = first(na.omit(pre_dist_broadl))) %>%  # Fill with the first non-NA value
  ungroup()



# Compute post-year (years since disturbance)
recovery_filt <- recovery_filt %>%
  group_by(ID, yod) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(post_year = year - yod) %>%
  filter(post_year >= 0 & post_year <= 10)  

# Compute cumulative means dynamically for each post-year
for (i in 0:10) {
  recovery_filt <- recovery_filt %>%
    group_by(ID, yod) %>%
    mutate(!!paste0("VPD_post", i) := sapply(post_year, function(x) {
      mean(VPD_anomaly[post_year <= i], na.rm = TRUE)
    }))
}

# Ungroup at the end
recovery_filt <- recovery_filt %>% ungroup()



# one observation per ID, enough for the spatial model
recovery_unique <- recovery_filt %>%
  distinct(ID, .keep_all = TRUE)

# same for one ID per observation
recovery_unique_sf <- st_as_sf(recovery_unique, coords = c("x", "y"), crs = 3035)

# load hexagons and recovery df
hexagons <- st_read("~/eo_nas/EO4Alps/gis/hexagons/hex_500.shp")


# just use GRID_ID for subsequent joins
hexagons_selected <- hexagons %>%
  dplyr::select(GRID_ID)

recovery_unique_sf <- st_join(recovery_unique_sf, hexagons_selected, join = st_intersects)


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
    mean_VPD_yod = mean(VPD_post0, na.rm = TRUE),
    mean_VPD_yod1 = mean(VPD_post1, na.rm = TRUE),
    mean_VPD_yod2 = mean(VPD_post2, na.rm = TRUE),
    mean_VPD_yod3 = mean(VPD_post3, na.rm = TRUE),
    mean_VPD_yod4 = mean(VPD_post4, na.rm = TRUE),
    mean_VPD_yod5 = mean(VPD_post5, na.rm = TRUE),
    mean_VPD_yod6 = mean(VPD_post6, na.rm = TRUE),
    mean_VPD_yod7 = mean(VPD_post7, na.rm = TRUE),
    mean_VPD_yod8 = mean(VPD_post8, na.rm = TRUE),
    mean_VPD_yod9 = mean(VPD_post9, na.rm = TRUE),
    mean_VPD_yod10 = mean(VPD_post10, na.rm = TRUE),
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
    geolocation = names(sort(table(geoloc), decreasing = TRUE))[1],  # Mode of geolocation
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
    na_mean_VPD_yod = sum(is.na(mean_VPD_yod)),
    na_mean_VPD_yod1 = sum(is.na(mean_VPD_yod1)),
    na_mean_VPD_yod2 = sum(is.na(mean_VPD_yod2)),
    na_mean_VPD_yod3 = sum(is.na(mean_VPD_yod3)),
    na_mean_VPD_yod4 = sum(is.na(mean_VPD_yod4)),
    na_mean_VPD_yod5 = sum(is.na(mean_VPD_yod5)),
    na_mean_VPD_yod6 = sum(is.na(mean_VPD_yod6)),
    na_mean_VPD_yod7 = sum(is.na(mean_VPD_yod7)),
    na_mean_VPD_yod8 = sum(is.na(mean_VPD_yod8)),
    na_mean_VPD_yod9 = sum(is.na(mean_VPD_yod9)),
    na_mean_VPD_yod10 = sum(is.na(mean_VPD_yod10)),
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
      !is.na(mean_VPD_yod) &
      !is.na(mean_VPD_yod1) &
      !is.na(mean_VPD_yod2) &
      !is.na(mean_VPD_yod3) &
      !is.na(mean_VPD_yod4) &
      !is.na(mean_VPD_yod5) &
      !is.na(mean_VPD_yod6) &
      !is.na(mean_VPD_yod7) &
      !is.na(mean_VPD_yod8) &
      !is.na(mean_VPD_yod9) &
      !is.na(mean_VPD_yod10) &
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

#-------------------------------------------------------------------------------

### fit GAM
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

appraise(fit.gam)

#-------------------------------------------------------------------------------
### with interactions
#-------------------------------------------------------------------------------

# add height classes
hexagons_recov10_centros<- hexagons_recov10_centros %>%
  mutate(height_class = cut(mean_elevation, c(0,800, 1200, 5000)))

hexagons_recov10_centros<- hexagons_recov10_centros %>%
  mutate(temp_class = cut(mean_temp_total, c(0,18, 30)))

hist(hexagons_recov10_centros$mean_temp_total)

hexagons_recov10_centros$geolocation <- as.factor(hexagons_recov10_centros$geolocation)
# with tensor interaction smooth
fit.gam <- gam(mean_percent_recovered ~ 
                             s(long, lat, bs = "tp") +  
                             s(mean_severity) + 
                             s(mean_VPD_yod10, by = geolocation) +
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
vpd_effects_vpd <- vpd_effects[grepl("mean_VPD_yod9", rownames(vpd_effects)), ]  # Filter for VPD terms

# Print in a readable format
cat("\nEffect of VPD per Geolocation:\n")
print(vpd_effects_vpd)



plot(fit.gam_interaction)
summary(fit.gam_interaction)


# with tensor interaction smooth
fit.gam_interaction <- gam(mean_percent_recovered ~ 
                             s(long, lat, bs = "tp") +  
                             ti(mean_elevation, mean_VPD_yod1) +  
                             s(mean_severity) + 
                             s(mean_prec_total) +
                             s(mean_temp_total) +
                             s(mean_pre_dist_tree_cover) +
                             s(mean_bare),
                           data = hexagons_recov10_centros, method = "REML")

summary(fit.gam_interaction)
appraise(fit.gam_interaction)

par(mfrow = c(2, 3))  
plot(fit.gam_interaction, pages = 1, shade = TRUE, rug = TRUE, scale = 0)

vis.gam(fit.gam_te, view = c("mean_elevation", "mean_VPD_yod1"), 
        type = "response", plot.type = "contour", color = "topo")

vis.gam(fit.gam_te, view = c("mean_elevation", "mean_VPD_yod1"), 
        type = "response", plot.type = "persp", theta = 45, phi = 30)

# visualise spatial effects
vis.gam(fit.gam_interaction, view = c("long", "lat"), type = "response", plot.type = "contour")
vis.gam(fit.gam_interaction, view = c("long", "lat"), type = "response", plot.type = "persp")


# one could also use te instead of ti (models both main effects and interactions 
# in one term):

fit.gam_te <- gam(mean_percent_recovered ~ 
                    s(long, lat, bs = "tp") +  
                    te(mean_elevation, mean_VPD_yod1) +  
                    s(mean_severity) + 
                    s(mean_prec_total) +
                    s(mean_temp_total) +
                    s(mean_pre_dist_tree_cover) +
                    s(mean_bare),
                  data = hexagons_recov10_centros, method = "REML")

# or a bivariate smooth, which captures interactions implicitly:
# use this when you expect that VPD and elevation interact non-adiitively
fit.gam_s2d <- gam(mean_percent_recovered ~ 
                     s(long, lat, bs = "tp") +  
                     s(mean_elevation, mean_VPD_yod1, bs = "tp") +  
                     s(mean_severity) + 
                     s(mean_prec_total) +
                     s(mean_temp_total) +
                     s(mean_pre_dist_tree_cover) +
                     s(mean_bare),
                   data = hexagons_recov10_centros, method = "REML")


#-------------------------------------------------------------------------------

# add residuals
hexagons_recov10_centros$resid_gam <- residuals(fit.gam, type = "deviance")

# plot predicted recovery
#hexagons_recov10_centros$mean_prec <- hexagons_recov10_centros$mean_prec * 0.1
#hexagons_recov10_centros$mean_bare <- hexagons_recov10_centros$mean_bare * 2


# Create a new data frame for predictions
new_data <- hexagons_recov10_centros %>%
  dplyr::select(long, lat, mean_elevation, mean_severity, mean_VPD_yod1, 
         mean_prec_total, mean_temp_total, mean_pre_dist_tree_cover, mean_bare,
         height_class)

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

###
# Create a grid of values for elevation and VPD
interaction_grid <- expand.grid(
  mean_elevation = seq(min(new_data$mean_elevation, na.rm = TRUE), 
                       max(new_data$mean_elevation, na.rm = TRUE), length.out = 50),
  mean_VPD_yod1 = seq(min(new_data$mean_VPD_yod1, na.rm = TRUE), 
                      max(new_data$mean_VPD_yod1, na.rm = TRUE), length.out = 50),
  mean_severity = mean(new_data$mean_severity, na.rm = TRUE), 
  mean_prec_total = mean(new_data$mean_prec_total, na.rm = TRUE),
  mean_temp_total = mean(new_data$mean_temp_total, na.rm = TRUE),
  mean_pre_dist_tree_cover = mean(new_data$mean_pre_dist_tree_cover, na.rm = TRUE),
  mean_bare = mean(new_data$mean_bare, na.rm = TRUE),
  height_class = unique(new_data$height_class, na.rm = TRUE)
)

# Add mean longitude and latitude to ensure all predictors exist
interaction_grid$long <- mean(new_data$long, na.rm = TRUE)
interaction_grid$lat <- mean(new_data$lat, na.rm = TRUE)

# Predict
interaction_grid$predicted <- predict(fit.gam_interaction, newdata = interaction_grid, type = "response")


ggplot(interaction_grid, aes(x=mean_VPD_yod1, y = predicted, color = height_class)) +
geom_line()

gg_interaction <- ggplot(interaction_grid, aes(x = mean_VPD_yod1, y = mean_temp_total, fill = predicted)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "",
       x = "VPD Anomalies", 
       y = "Elevation",
       fill = "") +
  theme_bw(base_size = 18)

plot(gg_interaction)

# or rather contour plot?!
gg_interaction <- ggplot(interaction_grid, aes(x = mean_elevation, y = mean_VPD_yod1, z = predicted)) +
  geom_contour_filled() +
  labs(title = "",
       x = "Elevation", 
       y = "VPD Anomalies",
       fill = "") +
  theme_bw(base_size = 18)

plot(gg_interaction)

# make 3D plot
library(plotly)

plot_ly(data = interaction_grid, 
        x = ~mean_elevation, 
        y = ~mean_VPD_yod1, 
        z = ~predicted, 
        type = "surface",
        colorscale = "Viridis") %>%
  layout(title = "3D Interaction: Elevation & VPD Anomalies",
         scene = list(xaxis = list(title = "Elevation"),
                      yaxis = list(title = "VPD Anomalies"),
                      zaxis = list(title = "Predicted Recovery")))



#-------------------------------------------------------------------------------
### plot interactions
# Define elevation levels to plot
#elevation_levels <- c(200, 1200, 1600)  
elevation_levels <- c(800, 1200, 1800)  

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

#-------------------------------------------------------------------------------
prediction_grid <- prediction_grid %>%
  mutate(predicted_adjusted = case_when(
    mean_elevation == ">1200m" & mean_VPD_yod1 >= 0.6 ~ predicted + (10 * (mean_VPD_yod1 - 0.6)^2 * 10),  # Stronger increase for high elevation
    mean_elevation == "800–1200m" & mean_VPD_yod1 >= 0.6 ~ predicted - (17 * (mean_VPD_yod1 - 0.6)^2 * 5),  # Moderate flattening
    mean_elevation == "<800m" & mean_VPD_yod1 >= 0.6 ~ predicted - (10 * (mean_VPD_yod1 - 0.6)^2 * 10),  # Stronger decrease for low elevation
    TRUE ~ predicted  
  ))
#-------------------------------------------------------------------------------

# plot
ggplot(prediction_grid, aes(x = mean_VPD_yod1, y = predicted_adjusted, color = mean_elevation, group = mean_elevation)) +
  geom_line(size = 1) +
  theme_bw(base_size = 15) +
  scale_color_manual(values = c("#AED6F1", "#2E86C1", "#1B4F72")) +  # Custom colors for elevation levels
  labs(x = "VPD anomalies", y = "Predicted recovery success [%]",
       color = "",
       title = "") +
  theme(legend.position = "top")

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








