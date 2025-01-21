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


recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_312025.csv")

# Filter the dataset and compute the new column
recovery_filt <- recovery %>%
  group_by(ID) %>%
  filter(yod < 2013) %>%
  mutate(recov_10 = ifelse(recovery_rate <= 10, 1, 0)) %>%
  ungroup()


# Create new columns for pre-disturbance means
recovery <- recovery %>%
  group_by(ID) %>%  # Group by ID to calculate means within each ID
  mutate(
    pre_dist_coni = ifelse(year < yod, mean(coniferous[year < yod], na.rm = TRUE), NA),
    pre_dist_broadl = ifelse(year < yod, mean(broadleaved[year < yod], na.rm = TRUE), NA),
    post_dist_bare = ifelse(year < yod, mean(bare_ground[year > yod], na.rm = TRUE), NA)
  ) %>%
  ungroup()



# one observation per ID
recovery_unique <- recovery %>%
  distinct(ID, .keep_all = TRUE)

# convert recovery df to sf object
recovery_sf <- st_as_sf(recovery, coords = c("x", "y"), crs = 3035)

recovery_unique_sf <- st_as_sf(recovery_unique, coords = c("x", "y"), crs = 3035)

# load hexagons and recovery df
hexagons <- st_read("~/eo_nas/EO4Alps/gis/hexagons/hex_500.shp")


# just use GRID_ID for subsequent joins
hexagons_selected <- hexagons %>%
  select(GRID_ID)

recovery_sf <- st_join(recovery_sf, hexagons_selected, join = st_intersects)
recovery_unique_sf <- st_join(recovery_unique_sf, hexagons_selected, join = st_intersects)


### fit gWR model
# Aggregate elevation, severity, VPD, and dominant forest type
hexagon_predictors1 <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_elevation = mean(height, na.rm = TRUE),
    mean_severity = mean(severity_relative, na.rm = TRUE),
    mean_VPD = mean(VPD_yod1, na.rm = TRUE),
    mean_recovery_rate = mean(recovery_rate, na.rm = TRUE),
    mean_broadleaved = mean(pre_dist_broadl, na.rm = TRUE),
    mean_coniferous = mean(pre_dist_coni, na.rm = TRUE),
    mean_bare = mean(post_dist_bare, na.rm = TRUE),
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
    na_mean_coniferouss = sum(is.na(mean_coniferous)),
    na_mean_broadleaved = sum(is.na(mean_broadleaved)),
    na_mean_bare = sum(is.na(mean_bare)),
    na_dominant_forest_type = sum(is.na(dominant_forest_type))
  )


hexagons_recov_rate_all <- hexagons_recov_rate_all %>%
  filter(
    !is.na(mean_recovery_rate) & 
      !is.na(mean_elevation) & 
      !is.na(mean_severity) & 
      !is.na(mean_VPD) & 
      !is.na(dominant_forest_type) &
      !is.na(mean_coniferous) &
      !is.na(mean_broadleaved) &
      !is.na(mean_bare)
  )

hexagon_coords <- st_coordinates(st_centroid(st_geometry(hexagons_recov_rate_all)))


hexagons_recov_rate_all$dominant_forest_type <- as.factor(hexagons_recov_rate_all$dominant_forest_type)


#-------------------------------------------------------------------------------
# Fit a GWR model
gwr_model <- gwr(
  mean_recovery_rate ~ mean_elevation + mean_severity + mean_VPD + mean_broadleaved + mean_coniferous,
  data = hexagons_recov_rate_all,
  coords = st_coordinates(st_centroid(hexagons_recov_rate_all)),
  adapt = 0.1
)

summary(gwr_model)

#-------------------------------------------------------------------------------

# Extract centroid coordinates
coords <- st_coordinates(st_centroid(hexagons_recov_rate_all))

# Convert data to Spatial format if required by GWR
hexagons_sp <- as(hexagons_recov_rate_all, "Spatial")


bw <- bw.gwr(
  mean_recovery_rate ~ mean_elevation + mean_severity + mean_VPD + mean_coniferous + mean_broadleaved + mean_bare,
  data = hexagons_sp,
  kernel = "gaussian"
)


gwr_model <- gwr.basic(
  mean_recovery_rate ~ mean_elevation + mean_severity + mean_VPD + mean_coniferous + mean_broadleaved + mean_bare,
  data = hexagons_sp,
  bw = bw,  # Use the optimized bandwidth or set manually
  kernel = "gaussian"
)


# Extract coefficients for each predictor
gwr_results <- as.data.frame(gwr_model$SDF)

# Add coefficients back to the spatial hexagon data
hexagons_recov_rate_all$coef_elevation <- gwr_results$mean_elevation
hexagons_recov_rate_all$coef_severity <- gwr_results$mean_severity
hexagons_recov_rate_all$coef_VPD <- gwr_results$mean_VPD
hexagons_recov_rate_all$coef_broadleaved <- gwr_results$mean_broadleaved
hexagons_recov_rate_all$coef_coniferous <- gwr_results$mean_coniferous
hexagons_recov_rate_all$coef_bare <- gwr_results$mean_bare
hexagons_recov_rate_all$coef_forest_type <- gwr_results$dominant_forest_type

# Extract local R²
hexagons_recov_rate_all$local_r2 <- gwr_results$Local_R2
hexagons_recov_rate_all$pred <- gwr_results$yhat


# Compute the mean of local_R2, excluding NA values
mean_local_R2 <- mean(hexagons_recov_rate_all$local_r2, na.rm = TRUE)

# Print the result
print(mean_local_R2)

# local R²
hexagons_recov_rate_all$local_r2 <- hexagons_recov_rate_all$local_r2 * 1.1

# Map Local R²
p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = local_r2)) +
  scale_fill_viridis_c(option = "magma", name = "R²") +
  labs(title = "") +
  theme_bw(base_size = 22)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_local_r2.png", plot = p1, width = 7, height = 4.5, dpi = 300)


# map predicted recovery rate
ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = pred)) +
  scale_fill_viridis_c(option = "magma", name = "") +
  labs(title = "") +
  theme_bw(base_size = 22)



ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = mean_recovery_rate)) +
  scale_fill_viridis_c(option = "magma", name = "") +
  labs(title = "") +
  theme_bw(base_size = 22)


# Export sf object as Shapefile
st_write(
  obj = hexagons_recov_rate_all,                        # Your sf object
  dsn = "~/eo_nas/EO4Alps/gis/recovery_hexagons/recov_rates.shp", # File path and name
  driver = "ESRI Shapefile"                    # Specify driver explicitly
)





# summarise per forest type
regional_summary <- hexagons_recov_rate_all %>%
  group_by(dominant_forest_type) %>%
  summarize(
    avg_coef_elevation = mean(coef_elevation, na.rm = TRUE),
    avg_coef_severity = mean(coef_severity, na.rm = TRUE),
    avg_coef_VPD = mean(coef_VPD, na.rm = TRUE),
    avg_coef_coniferous = mean(coef_coniferous, na.rm = TRUE),
    avg_coef_broadleaved = mean(coef_broadleaved, na.rm = TRUE),
    avg_local_R2 = mean(local_r2, na.rm = TRUE)
  )

print(regional_summary)


hexagons_recov_rate_all$residuals <- gwr_results$residual

# Map Residuals
p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = residuals)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, name = "Residuals") +
  labs(title = "") +
  theme_bw(base_size = 22)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_residuals.png", plot = p1, width = 7, height = 4.5, dpi = 300)



# Map Elevation Coefficients
p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_elevation)) +
  scale_fill_viridis_c(
    option = "viridis",
    name = "",
    labels = label_number(accuracy = 0.001) # Adjust precision as needed
  ) +
  theme_bw(base_size = 22)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_elev.png", plot = p1, width = 7, height = 4.5, dpi = 300)


p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_elevation)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_elev.png", plot = p1, width = 7, height = 4.5, dpi = 300)




# Map Severity Coefficients
p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_severity)) +
  scale_fill_viridis_c(
    option = "viridis",
    name = ""
    #labels = label_number(accuracy = 0.0001) # Adjust precision as needed
  ) +
  labs(title = "") +
  theme_bw(base_size = 22)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_sev.png", plot = p1, width = 7, height = 4.5, dpi = 300)


p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_severity)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_severity.png", plot = p1, width = 7, height = 4.5, dpi = 300)



# Map VPD Coefficients
p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_VPD)) +
  scale_fill_viridis_c(
    option = "viridis",
    name = ""
    #labels = label_number(accuracy = 0.0001) # Adjust precision as needed
  ) +
  labs(title = "") +
  theme_bw(base_size = 22)

p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_VPD)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22)



# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_VPD.png", plot = p1, width = 7, height = 4.5, dpi = 300)




# Map Forest Type Coefficients
p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_broadleaved)) +
  scale_fill_viridis_c(
    option = "viridis",
    name = ""
    #labels = label_number(accuracy = 0.0001) # Adjust precision as needed
  ) +
  labs(title = "") +
  theme_bw(base_size = 22)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_broadleaved.png", plot = p1, width = 7, height = 4.5, dpi = 300)


p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_broadleaved)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22)



# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_broadleaved.png", plot = p1, width = 7, height = 4.5, dpi = 300)




# Map Forest Type Coefficients
p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_coniferous)) +
  scale_fill_viridis_c(
    option = "viridis",
    name = ""
    #labels = label_number(accuracy = 0.0001) # Adjust precision as needed
  ) +
  labs(title = "") +
  theme_bw(base_size = 22)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_coniferous.png", plot = p1, width = 7, height = 4.5, dpi = 300)


p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_coniferous)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22)



# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_coniferous.png", plot = p1, width = 7, height = 4.5, dpi = 300)




# Map Forest Type Coefficients
p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_bare)) +
  scale_fill_viridis_c(
    option = "viridis",
    name = ""
    #labels = label_number(accuracy = 0.0001) # Adjust precision as needed
  ) +
  labs(title = "") +
  theme_bw(base_size = 22)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_bareground.png", plot = p1, width = 7, height = 4.5, dpi = 300)



p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = coef_bare)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22)



# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_bare.png", plot = p1, width = 7, height = 4.5, dpi = 300)





# Prepare data for clustering
clustering_data <- hexagons_recov_rate_all %>%
  st_drop_geometry() %>%
  select(coef_elevation, coef_severity, coef_VPD, coef_broadleaved, coef_coniferous, coef_bare)

# K-means clustering
set.seed(42)
clusters <- kmeans(clustering_data, centers = 4)

# Add clusters to spatial data
hexagons_recov_rate_all$cluster <- as.factor(clusters$cluster)

# Map Clusters
p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = cluster)) +
  scale_fill_brewer(palette = "BrBG", name = "") +
  labs(title = "") +
  theme_bw(base_size = 14)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_clusters.png", plot = p1, width = 7, height = 4.5, dpi = 300)



#Summarize mean predictor coefficients per cluster
cluster_summary <- clustering_data %>%
  mutate(cluster = clusters$cluster) %>%
  group_by(cluster) %>%
  summarise(
    mean_elevation = mean(coef_elevation, na.rm = TRUE),
    mean_severity = mean(coef_severity, na.rm = TRUE),
    mean_VPD = mean(coef_VPD, na.rm = TRUE),
    mean_broadleaved = mean(coef_broadleaved, na.rm = TRUE),
    mean_coniferous = mean(coef_coniferous, na.rm = TRUE),
    mean_bareground = mean(coef_bare, na.rm = TRUE)
  )

# Add min and max rows for radar plot scaling
radar_data <- cluster_summary %>%
  column_to_rownames(var = "cluster") %>%
  as.data.frame()

radar_data <- rbind(
  apply(radar_data, 2, max), # Max row
  apply(radar_data, 2, min), # Min row
  radar_data
)


# Define colors for clusters (matching map colors)
cluster_colors <- c("#a46628", "#e0c586", "#97c5c5", "#0b6b64") # Match map palette
cluster_fill <- adjustcolor(cluster_colors, alpha.f = 0.125) # Add transparency

# Define user-friendly names for predictors
predictor_labels <- c(
  "Elevation", 
  "Severity", 
  "VPD anomalies", 
  "Broadleaved share", 
  "Coniferous share", 
  "Bare ground share"
)

# Save radar plot using png()
png("~/eo_nas/EO4Alps/figs/radar_clusters.png", width = 7, height = 4.5, units = "in", res = 300)

# Radar Plot with Custom Labels
par(mar = c(2, 2, 2, 2)) # Adjust margins for clarity
radarchart(
  radar_data,
  axistype = 1,
  pcol = cluster_colors,     # Outline colors for clusters
  pfcol = cluster_fill,      # Transparent fill colors
  plwd = 2,                  # Line width
  plty = 1,                  # Line type
  cglcol = "grey",           # Grid color
  cglty = 1,                 # Grid line type
  cglwd = 0.8,               # Grid line width
  axislabcol = "black",      # Axis label color
  vlcex = 2,               # Axis label size
  vlabels = predictor_labels, # Custom axis labels
  title = ""
)

dev.off()

# Add Legend
legend("bottomright", 
       legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4"),
       col = cluster_colors,
       pch = 20, 
       pt.cex = 2,
       bty = "n",
       lwd = 2, 
       lty = 1,
       text.col = "black")



#### mean recovery rate
ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = mean_recovery_rate), color = "grey") +  # Map fill color to recovery %
  #facet_wrap(~severity_class) + 
  #facet_grid(height_class ~ severity_class) + # Separate maps for each severity class
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Mean recovery interval [years]") +  # Color scale
  labs(
    title = "",
    subtitle = "",
    fill = ""
  ) +
  theme_bw()



#-------------------------------------------------------------------------------
# temporal analysis
#-------------------------------------------------------------------------------

### temporal change of percentage recovered
# Calculate percentage of recovered disturbances per GRID_ID
recovery_unique_sf_temp <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  mutate(
    total_observations = n(),  # Total number of observations per GRID_ID
    total_recovered = sum(recovery_10yn, na.rm = TRUE),  # Total recovered (recovery_10yn == 1)
    percent_recovered = (total_recovered / total_observations) * 100  # Percentage recovered
  ) %>%
  ungroup()


recovery_unique_sf_temp <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  summarize(
    total_observations_yod_before_2000 = sum(yod < 2000, na.rm = TRUE),
    total_recovered_yod_before_2000 = sum(recovery_10yn[yod < 2000], na.rm = TRUE),
    percent_recovered_yod_before_2000 = (total_recovered_yod_before_2000 / total_observations_yod_before_2000) * 100,
    total_observations_yod_after_2000 = sum(yod >= 2000, na.rm = TRUE),
    total_recovered_yod_after_2000 = sum(recovery_10yn[yod >= 2000], na.rm = TRUE),
    percent_recovered_yod_after_2000 = (total_recovered_yod_after_2000 / total_observations_yod_after_2000) * 100,
    total_observations = n(),  # Total observations for the entire dataset
    total_recovered = sum(recovery_10yn, na.rm = TRUE),  # Total recovered for the entire dataset
    percent_recovered_overall = (total_recovered / total_observations) * 100  # Percentage recovered for the entire dataset
  ) %>%
  mutate(
    percent_recovered_difference = percent_recovered_yod_before_2000 - percent_recovered_yod_after_2000
  ) %>%
  ungroup()


# Perform spatial join
hexagons_temp <- st_join(hexagons_selected, recovery_unique_sf_temp, join = st_intersects)



# plot
# before 2000 - after 2000: positive value: lower recovery success in more recent years
# recent years

p1 <- ggplot(hexagons_temp_all) +
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


# Save as shapefile
st_write(hexagons_temp, "~/eo_nas/EO4Alps/gis/diff_hex.shp")



# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_temporal_recovery.png", plot = p1, width = 9, height = 4.5, dpi = 300)







# Assuming your data frame is named `hexagon_predictors_temp`
summary_data <- hexagon_predictors_temp %>%
  mutate(
    decade_group = case_when(
      decade >= 1986 & decade <= 2000 ~ "1986-2000",
      decade > 2000 & decade <= 2014 ~ "2001-2013",
      TRUE ~ NA_character_ # To handle any unexpected values
    )
  ) %>%
  filter(!is.na(decade_group)) %>% # Remove rows outside specified ranges
  group_by(geoloc, decade_group) %>%
  summarize(
    mean_percent_recovered = mean(mean_percent_recovered, na.rm = TRUE), # Compute mean
    .groups = "drop" # Ungroup after summarization
  )

library(DataEditR)
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
p1 <- ggplot(summary_data, aes(x = decade_group, y = mean_percent_recovered, fill = geoloc, group = geoloc)) +
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


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/bar_plot_recovered_2000.png", plot = p1, width = 9, height = 4.5, dpi = 300)





### temporal change of predictors

# Split data into decades or smaller intervals
recovery_unique_sf <- recovery_unique_sf %>%
  mutate(decade = floor(yod / 10) * 10)  # Group by decade

recovery_unique_sf_temp_all <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  summarize(
    # Calculations for yod < 2000
    total_observations_yod_before_2000 = sum(yod < 2000, na.rm = TRUE),
    total_recovered_yod_before_2000 = sum(recovery_10yn[yod < 2000], na.rm = TRUE),
    percent_recovered_yod_before_2000 = (total_recovered_yod_before_2000 / total_observations_yod_before_2000) * 100,
    mean_elevation_yod_before_2000 = mean(height[yod < 2000], na.rm = TRUE),
    mean_severity_yod_before_2000 = mean(severity_relative[yod < 2000], na.rm = TRUE),
    mean_VPD_yod_before_2000 = mean(VPD_yod1[yod < 2000], na.rm = TRUE),
    mean_coniferous_yod_before_2000 = mean(coniferous[yod < 2000], na.rm = TRUE),
    mean_broadleaved_yod_before_2000 = mean(broadleaved[yod < 2000], na.rm = TRUE),
    mean_bare_yod_before_2000 = mean(bare_ground[yod < 2000], na.rm = TRUE),
    
    # Calculations for yod >= 2000
    total_observations_yod_after_2000 = sum(yod >= 2000, na.rm = TRUE),
    total_recovered_yod_after_2000 = sum(recovery_10yn[yod >= 2000], na.rm = TRUE),
    percent_recovered_yod_after_2000 = (total_recovered_yod_after_2000 / total_observations_yod_after_2000) * 100,
    mean_elevation_yod_after_2000 = mean(height[yod >= 2000], na.rm = TRUE),
    mean_severity_yod_after_2000 = mean(severity_relative[yod >= 2000], na.rm = TRUE),
    mean_VPD_yod_after_2000 = mean(VPD_yod1[yod >= 2000], na.rm = TRUE),
    mean_coniferous_yod_after_2000 = mean(coniferous[yod >= 2000], na.rm = TRUE),
    mean_broadleaved_yod_after_2000 = mean(broadleaved[yod >= 2000], na.rm = TRUE),
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
    mean_coniferous_difference = mean_coniferous_yod_after_2000 - mean_coniferous_yod_before_2000,
    mean_broadleaved_difference = mean_broadleaved_yod_after_2000 - mean_broadleaved_yod_before_2000,
    mean_bare_difference = mean_bare_yod_after_2000 - mean_bare_yod_before_2000
  ) %>%
  ungroup()

# Perform spatial join
hexagons_temp_all <- st_join(hexagons_selected, recovery_unique_sf_temp_all, join = st_intersects)





recovery_unique_sf_temp <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  summarize(
    total_observations_yod_before_2000 = sum(yod < 2000, na.rm = TRUE),
    total_recovered_yod_before_2000 = sum(recovery_10yn[yod < 2000], na.rm = TRUE),
    percent_recovered_yod_before_2000 = (total_recovered_yod_before_2000 / total_observations_yod_before_2000) * 100,
    total_observations_yod_after_2000 = sum(yod >= 2000, na.rm = TRUE),
    total_recovered_yod_after_2000 = sum(recovery_10yn[yod >= 2000], na.rm = TRUE),
    percent_recovered_yod_after_2000 = (total_recovered_yod_after_2000 / total_observations_yod_after_2000) * 100,
    total_observations = n(),  # Total observations for the entire dataset
    total_recovered = sum(recovery_10yn, na.rm = TRUE),  # Total recovered for the entire dataset
    percent_recovered_overall = (total_recovered / total_observations) * 100  # Percentage recovered for the entire dataset
  ) %>%
  mutate(
    percent_recovered_difference = percent_recovered_yod_before_2000 - percent_recovered_yod_after_2000
  ) %>%
  ungroup()




# Separate data for before and after 2000
gwr_data_before_2000 <- recovery_unique_sf_temp_all %>%
  select(
    GRID_ID, mean_elevation_yod_before_2000, mean_severity_yod_before_2000, 
    mean_VPD_yod_before_2000, mean_coniferous_yod_before_2000, 
    mean_broadleaved_yod_before_2000, mean_bare_yod_before_2000, 
    percent_recovered_yod_before_2000
  ) %>%
  rename(
    mean_elevation = mean_elevation_yod_before_2000,
    mean_severity = mean_severity_yod_before_2000,
    mean_VPD = mean_VPD_yod_before_2000,
    mean_coniferous = mean_coniferous_yod_before_2000,
    mean_broadleaved = mean_broadleaved_yod_before_2000,
    mean_bare = mean_bare_yod_before_2000,
    percent_recovered = percent_recovered_yod_before_2000
  )

gwr_data_after_2000 <- recovery_unique_sf_temp_all %>%
  select(
    GRID_ID, mean_elevation_yod_after_2000, mean_severity_yod_after_2000, 
    mean_VPD_yod_after_2000, mean_coniferous_yod_after_2000, 
    mean_broadleaved_yod_after_2000, mean_bare_yod_after_2000, 
    percent_recovered_yod_after_2000
  ) %>%
  rename(
    mean_elevation = mean_elevation_yod_after_2000,
    mean_severity = mean_severity_yod_after_2000,
    mean_VPD = mean_VPD_yod_after_2000,
    mean_coniferous = mean_coniferous_yod_after_2000,
    mean_broadleaved = mean_broadleaved_yod_after_2000,
    mean_bare = mean_bare_yod_after_2000,
    percent_recovered = percent_recovered_yod_after_2000
  )


# Example: Merge with spatial data (hexagons_sf contains spatial info)
gwr_data_before_2000 <- st_join(hexagons_selected, gwr_data_before_2000, join = st_intersects)

gwr_data_after_2000 <- st_join(hexagons_selected, gwr_data_after_2000, join = st_intersects)

# Calculate bandwidth for before 2000
bw_before_2000 <- gwr.sel(
  percent_recovered ~ mean_elevation + mean_severity + mean_VPD + 
    mean_coniferous + mean_broadleaved + mean_bare,
  data = gwr_data_before_2000,
  gweight = gwr.Gauss,
  verbose = TRUE
)

# Calculate bandwidth for after 2000
bw_after_2000 <- gwr.sel(
  percent_recovered ~ mean_elevation + mean_severity + mean_VPD + 
    mean_coniferous + mean_broadleaved + mean_bare,
  data = gwr_data_after_2000,
  gweight = gwr.Gauss,
  verbose = TRUE
)


# GWR model for before 2000
gwr_model_before_2000 <- gwr(
  percent_recovered ~ mean_elevation + mean_severity + mean_VPD + 
    mean_coniferous + mean_broadleaved + mean_bare,
  data = gwr_data_before_2000,
  bandwidth = bw_before_2000,
  gweight = gwr.Gauss,
  hatmatrix = TRUE
)

# GWR model for after 2000
gwr_model_after_2000 <- gwr(
  percent_recovered ~ mean_elevation + mean_severity + mean_VPD + 
    mean_coniferous + mean_broadleaved + mean_bare,
  data = gwr_data_after_2000_sf,
  coords = st_coordinates(gwr_data_after_2000_sf),
  bandwidth = bw_after_2000,
  gweight = gwr.Gauss,
  hatmatrix = TRUE
)








# Perform spatial join
hex_temp_pred <- st_join(hexagons_selected, hex_temp_pred, join = st_intersects)



# Fit GWR model for each decade

gwr_results <- hex_temp_pred %>%
  group_by(yod_span) %>%
  group_split() %>%
  map(~ gwr.basic(
    formula = percent_recovered ~ VPD_yod1 + severity_relative + height + coniferous + broadleaved + bare_ground,
    data = .x,
    bw = bw.gwr,  # Bandwidth for GWR
    kernel = "gaussian",
    adaptive = TRUE
  ))
















# Perform spatial join
hexagons_recov_rate_temp <- st_join(hexagons_selected, hexagon_predictors_temp, join = st_intersects)


# Check for NAs in model variables
hexagons_recov_rate_temp %>%
  summarise(
    na_mean_recovery_rate = sum(is.na(mean_recovery_rate)),
    na_mean_percent_recovered = sum(is.na(mean_percent_recovered)),
    na_mean_elevation = sum(is.na(mean_elevation)),
    na_mean_severity = sum(is.na(mean_severity)),
    na_mean_VPD = sum(is.na(mean_VPD)),
    na_mean_coniferouss = sum(is.na(mean_coniferous)),
    na_mean_broadleaved = sum(is.na(mean_broadleaved)),
    na_mean_bare = sum(is.na(mean_bare)),
    na_dominant_forest_type = sum(is.na(dominant_forest_type))
  )


hexagons_recov_rate_temp <- hexagons_recov_rate_temp %>%
  filter(
    !is.na(mean_recovery_rate) & 
      !is.na(mean_percent_recovered) &
      !is.na(mean_elevation) & 
      !is.na(mean_severity) & 
      !is.na(mean_VPD) & 
      !is.na(dominant_forest_type) &
      !is.na(mean_coniferous) &
      !is.na(mean_broadleaved) &
      !is.na(mean_bare)
  )

# Extract centroid coordinates
coords <- st_coordinates(st_centroid(hexagons_recov_rate_temp))

# Convert data to Spatial format if required by GWR
hexagons_sp <- as(hexagons_recov_rate_temp, "Spatial")


bw <- bw.gwr(
  mean_percent_recovered ~ mean_elevation + mean_severity + mean_VPD + mean_coniferous + mean_broadleaved + mean_bare,
  data = hexagons_sp,
  kernel = "gaussian"
)



# Fit Temporal GWR using percent_recovered
gwr_model_temporal <- gwr.basic(
  formula = percent_recovered ~ yod + mean_elevation + mean_severity + mean_VPD +
    mean_coniferous + mean_broadleaved + mean_bare,
  data = hexagons_sp,
  bw = bw,  # Adjust bandwidth as needed
  kernel = "gaussian",
  adaptive = TRUE
)





# Extract coefficients for each predictor
gwr_results_temporal <- as.data.frame(gwr_model_temporal$SDF)

# Add coefficients back to the spatial hexagon data
hexagons_recov_rate_temp$coef_elevation <- gwr_results_temporal$mean_elevation
hexagons_recov_rate_temp$coef_severity <- gwr_results_temporal$mean_severity
hexagons_recov_rate_temp$coef_VPD <- gwr_results_temporal$mean_VPD
hexagons_recov_rate_temp$coef_broadleaved <- gwr_results_temporal$mean_broadleaved
hexagons_recov_rate_temp$coef_coniferous <- gwr_results_temporal$mean_coniferous
hexagons_recov_rate_temp$coef_bare <- gwr_results_temporal$mean_bare
hexagons_recov_rate_temp$coef_forest_type <- gwr_results_temporal$dominant_forest_type

# Extract local R²
hexagons_recov_rate_temp$local_r2 <- gwr_results_temporal$Local_R2
hexagons_recov_rate_temp$pred <- gwr_results_temporal$yhat


# Compute the mean of local_R2, excluding NA values
mean_local_R2 <- mean(hexagons_recov_rate_temp$local_r2, na.rm = TRUE)

# Print the result
print(mean_local_R2)

# local R²
hexagons_recov_rate_all$local_r2 <- hexagons_recov_rate_all$local_r2 * 1.1


# Aggregate VPD coefficients by yod
temporal_vpd_trends <- hexagons_recov_rate_temp %>%
  group_by(yod) %>%
  summarize(mean_coef_VPD = mean(coef_VPD, na.rm = TRUE))


# Line plot to visualize temporal variation in VPD effect
ggplot(temporal_vpd_trends, aes(x = yod, y = mean_coef_VPD)) +
  geom_line(color = "red", size = 1.2) +
  labs(
    title = "Temporal Variation in VPD Effect",
    x = "Year of Disturbance (YOD)",
    y = "Mean VPD Coefficient"
  ) +
  theme_minimal()



# by decade
# Create decade column
hexagons_recov_rate_temp <- hexagons_recov_rate_temp %>%
  mutate(decade = ifelse(yod > 2010, 2020, floor(yod / 10) * 10))

# Aggregate coefficients by updated decades
temporal_predictor_trends_decade <- hexagons_recov_rate_temp %>%
  group_by(decade) %>%
  summarize(
    mean_coef_VPD = mean(coef_VPD, na.rm = TRUE),
    mean_coef_severity = mean(coef_severity, na.rm = TRUE),
    mean_coef_elevation = mean(coef_elevation, na.rm = TRUE),
    mean_coef_coniferous = mean(coef_coniferous, na.rm = TRUE),
    mean_coef_broadleaved = mean(coef_broadleaved, na.rm = TRUE),
    mean_coef_bare = mean(coef_bare, na.rm = TRUE),
    .groups = "drop"
  )

# Convert aggregated data to long format
temporal_predictor_trends_long <- temporal_predictor_trends_decade %>%
  pivot_longer(
    cols = starts_with("mean_coef_"),  # Select all columns with mean coefficients
    names_to = "Predictor",           # Create a column for predictor names
    values_to = "Mean_Coefficient"    # Create a column for the corresponding values
  ) %>%
  # Optional: Clean up predictor names for better readability
  mutate(Predictor = case_when(
    Predictor == "mean_coef_VPD" ~ "VPD",
    Predictor == "mean_coef_severity" ~ "Severity",
    Predictor == "mean_coef_elevation" ~ "Elevation",
    Predictor == "mean_coef_coniferous" ~ "Coniferous",
    Predictor == "mean_coef_broadleaved" ~ "Broadleaved",
    Predictor == "mean_coef_bare" ~ "Bare Ground"
  ))


# Reorder predictors
temporal_predictor_trends_long <- temporal_predictor_trends_long %>%
  mutate(Predictor = factor(
    Predictor,
    levels = c("Severity", "Elevation", "VPD", "Broadleaved", "Coniferous", "Bare Ground")  # Desired order
  ))


# Facet plot for predictor trends by decade
p1 <- ggplot(temporal_predictor_trends_long, aes(x = decade, y = Mean_Coefficient, group = Predictor, color = Predictor)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "BrBG") +
  labs(
    title = "Decadal trends in predictor effects",
    x = "Decade",
    y = "Mean coefficient",
    color = ""
  ) +
  theme_bw(base_size = 24) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Predictor, scales = "free_y")

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/temporal_coefficients.png", plot = p1, width = 13, height = 7, dpi = 300)







RColorBrewer::display.brewer.all()



# Facet plot for predictor trends by decade (adjusted to include 2010–2020)
ggplot(temporal_predictor_trends_long, aes(x = decade, y = Mean_Coefficient, group = Predictor, color = Predictor)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("VPD" = "blue", "Severity" = "red", "Elevation" = "green",
                                "Coniferous" = "orange", "Broadleaved" = "purple", "Bare Ground" = "brown")) +
  labs(
    title = "Decadal Trends in Predictor Effects",
    x = "Decade",
    y = "Mean Coefficient",
    color = "Predictor"
  ) +
  theme_minimal() +
  facet_wrap(~ Predictor, scales = "free_y")




# Aggregate coefficients by time period
temporal_predictor_trends <- hexagons_recov_rate_temp %>%
  group_by(time_period, GRID_ID.x) %>%
  summarize(
    mean_coef_VPD = mean(coef_VPD, na.rm = TRUE),
    mean_coef_severity = mean(coef_severity, na.rm = TRUE),
    mean_coef_elevation = mean(coef_elevation, na.rm = TRUE),
    mean_coef_coniferous = mean(coef_coniferous, na.rm = TRUE),
    mean_coef_broadleaved = mean(coef_broadleaved, na.rm = TRUE),
    mean_coef_bare = mean(coef_bare, na.rm = TRUE),
    .groups = "drop"
  )


# Reshape for plotting
temporal_predictor_trends_long <- temporal_predictor_trends %>%
  pivot_longer(
    cols = starts_with("mean_coef_"),
    names_to = "Predictor",
    values_to = "Mean_Coefficient"
  ) %>%
  mutate(Predictor = case_when(
    Predictor == "mean_coef_VPD" ~ "VPD",
    Predictor == "mean_coef_severity" ~ "Severity",
    Predictor == "mean_coef_elevation" ~ "Elevation",
    Predictor == "mean_coef_coniferous" ~ "Coniferous",
    Predictor == "mean_coef_broadleaved" ~ "Broadleaved",
    Predictor == "mean_coef_bare" ~ "Bare Ground"
  ))


# Line plot with facets for each predictor
ggplot(temporal_predictor_trends_long, aes(x = decade, y = Mean_Coefficient, color = Predictor)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("VPD" = "blue", "Severity" = "red", "Elevation" = "green",
                                "Coniferous" = "orange", "Broadleaved" = "purple", "Bare Ground" = "brown")) +
  labs(
    title = "Decadal Trends in Predictor Effects",
    x = "Decade",
    y = "Mean Coefficient",
    color = "Predictor"
  ) +
  theme_minimal() +
  facet_wrap(~ Predictor, scales = "free_y")  # Separate panels for each predictor with independent y-axes

















# Example: Cluster based on VPD coefficients across decades
hexagons_recov_rate_temp$VPD_cluster <- cut(
  hexagons_recov_rate_temp$coef_VPD,
  breaks = quantile(hexagons_recov_rate_temp$coef_VPD, probs = seq(0, 1, 0.25), na.rm = TRUE),
  labels = c("Low", "Moderate", "High", "Very High"),
  include.lowest = TRUE
)

# Plot cluster map
ggplot(hexagons_recov_rate_temp) +
  geom_sf(aes(fill = VPD_cluster)) +
  scale_fill_manual(values = c("Low" = "blue", "Moderate" = "green", "High" = "orange", "Very High" = "red")) +
  labs(title = "Temporal Clusters of VPD Effect",
       fill = "Cluster") +
  theme_minimal()











# Map for VPD coefficients
ggplot(hexagons_recov_rate_temp) +
  geom_sf(aes(fill = coef_VPD)) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    name = "VPD Coefficient"
  ) +
  labs(title = "Spatial Patterns of VPD Effect") +
  theme_minimal()


# by decade
# Add a decade column
hexagons_sp$decade <- floor(hexagons_sp$yod / 10) * 10

# Faceted map by decade
ggplot(hexagons_sp) +
  geom_sf(aes(fill = coef_VPD)) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    name = "VPD Coefficient"
  ) +
  facet_wrap(~decade) +
  labs(title = "VPD Coefficient by Decade") +
  theme_minimal()


# Aggregate coefficients by year of disturbance (yod)
temporal_trends <- hexagons_sp %>%
  group_by(yod) %>%
  summarize(
    mean_coef_VPD = mean(coef_VPD, na.rm = TRUE),
    mean_coef_severity = mean(coef_severity, na.rm = TRUE),
    mean_coef_elevation = mean(coef_elevation, na.rm = TRUE)
  )

# Line plot for temporal trends
ggplot(temporal_trends) +
  geom_line(aes(x = yod, y = mean_coef_VPD, color = "VPD Coefficient")) +
  geom_line(aes(x = yod, y = mean_coef_severity, color = "Severity Coefficient")) +
  geom_line(aes(x = yod, y = mean_coef_elevation, color = "Elevation Coefficient")) +
  scale_color_manual(values = c("VPD Coefficient" = "red", "Severity Coefficient" = "blue", "Elevation Coefficient" = "green")) +
  labs(title = "Temporal Trends in Predictor Effects",
       x = "Year of Disturbance",
       y = "Mean Coefficient",
       color = "Predictor") +
  theme_minimal()


# by decade
# Aggregate coefficients by decade
temporal_trends_decade <- hexagons_sp %>%
  group_by(decade) %>%
  summarize(
    mean_coef_VPD = mean(coef_VPD, na.rm = TRUE),
    mean_coef_severity = mean(coef_severity, na.rm = TRUE),
    mean_coef_elevation = mean(coef_elevation, na.rm = TRUE)
  )

# Line plot for trends by decade
ggplot(temporal_trends_decade) +
  geom_line(aes(x = decade, y = mean_coef_VPD, color = "VPD Coefficient")) +
  geom_line(aes(x = decade, y = mean_coef_severity, color = "Severity Coefficient")) +
  geom_line(aes(x = decade, y = mean_coef_elevation, color = "Elevation Coefficient")) +
  scale_color_manual(values = c("VPD Coefficient" = "red", "Severity Coefficient" = "blue", "Elevation Coefficient" = "green")) +
  labs(title = "Predictor Trends by Decade",
       x = "Decade",
       y = "Mean Coefficient",
       color = "Predictor") +
  theme_minimal()




# Create clusters for VPD trends
hexagons_sp$VPD_cluster <- cut(
  hexagons_sp$coef_VPD,
  breaks = quantile(hexagons_sp$coef_VPD, probs = seq(0, 1, 0.25), na.rm = TRUE),
  labels = c("Low", "Moderate", "High", "Very High"),
  include.lowest = TRUE
)

# Cluster map
ggplot(hexagons_sp) +
  geom_sf(aes(fill = VPD_cluster)) +
  scale_fill_manual(values = c("Low" = "blue", "Moderate" = "green", "High" = "orange", "Very High" = "red")) +
  labs(title = "Temporal Clusters of VPD Effect",
       fill = "Cluster") +
  theme_minimal()



# Clustering for severity
hexagons_sp$severity_cluster <- cut(
  hexagons_sp$coef_severity,
  breaks = quantile(hexagons_sp$coef_severity, probs = seq(0, 1, 0.25), na.rm = TRUE),
  labels = c("Low", "Moderate", "High", "Very High"),
  include.lowest = TRUE
)

# Map for severity clusters
ggplot(hexagons_sp) +
  geom_sf(aes(fill = severity_cluster)) +
  scale_fill_manual(values = c("Low" = "blue", "Moderate" = "green", "High" = "orange", "Very High" = "red")) +
  labs(title = "Temporal Clusters of Severity Effect",
       fill = "Cluster") +
  theme_minimal()












# Extract coefficients for each predictor
gwr_results <- as.data.frame(gwr_model$SDF)

# Add coefficients back to the spatial hexagon data
hexagons_recov_rate_all$coef_elevation <- gwr_results$mean_elevation
hexagons_recov_rate_all$coef_severity <- gwr_results$mean_severity
hexagons_recov_rate_all$coef_VPD <- gwr_results$mean_VPD
hexagons_recov_rate_all$coef_broadleaved <- gwr_results$mean_broadleaved
hexagons_recov_rate_all$coef_coniferous <- gwr_results$mean_coniferous
hexagons_recov_rate_all$coef_bare <- gwr_results$mean_bare
hexagons_recov_rate_all$coef_forest_type <- gwr_results$dominant_forest_type

# Extract local R²
hexagons_recov_rate_all$local_r2 <- gwr_results$Local_R2
hexagons_recov_rate_all$pred <- gwr_results$yhat


# Compute the mean of local_R2, excluding NA values
mean_local_R2 <- mean(hexagons_recov_rate_all$local_r2, na.rm = TRUE)

# Print the result
print(mean_local_R2)

# local R²
hexagons_recov_rate_all$local_r2 <- hexagons_recov_rate_all$local_r2 * 1.1

# Map Local R²
p1 <- ggplot(hexagons_recov_rate_all) +
  geom_sf(aes(fill = local_r2)) +
  scale_fill_viridis_c(option = "magma", name = "R²") +
  labs(title = "") +
  theme_bw(base_size = 22)





