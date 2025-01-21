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

recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_312025.csv")

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
recovery_sf <- st_as_sf(recovery, coords = c("x", "y"), crs = 3035)

recovery_unique_sf <- st_as_sf(recovery_unique, coords = c("x", "y"), crs = 3035)

# load hexagons and recovery df
hexagons <- st_read("~/eo_nas/EO4Alps/gis/hexagons/hex_500.shp")


# just use GRID_ID for subsequent joins
hexagons_selected <- hexagons %>%
  select(GRID_ID)

recovery_sf <- st_join(recovery_sf, hexagons_selected, join = st_intersects)
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


### or with all other variables as well
recovery_unique_sf_temp_all1 <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  summarize(
    # Calculations for yod < 2000
    total_observations_yod_before_2000 = sum(yod < 2000, na.rm = TRUE),
    total_recovered_yod_before_2000 = sum(recovery_10yn[yod < 2000], na.rm = TRUE),
    percent_recovered_yod_before_2000 = (total_recovered_yod_before_2000 / total_observations_yod_before_2000) * 100,
    mean_elevation_yod_before_2000 = mean(height[yod < 2000], na.rm = TRUE),
    mean_severity_yod_before_2000 = mean(severity_relative[yod < 2000], na.rm = TRUE),
    mean_VPD_yod_before_2000 = mean(VPD_yod1[yod < 2000], na.rm = TRUE),
    mean_temp_before_2000 = mean(temp_yod[yod < 2000], na.rm = TRUE),
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
    mean_temp_after_2000 = mean(temp[yod > 2000], na.rm = TRUE),
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
    mean_temp_difference = mean_temp_after_2000 - mean_temp_before_2000,
    mean_coniferous_difference = mean_coniferous_yod_after_2000 - mean_coniferous_yod_before_2000,
    mean_broadleaved_difference = mean_broadleaved_yod_after_2000 - mean_broadleaved_yod_before_2000,
    mean_bare_difference = mean_bare_yod_after_2000 - mean_bare_yod_before_2000
  ) %>%
  ungroup()

# Perform spatial join
hexagons_temp_all <- st_join(hexagons_selected, recovery_unique_sf_temp_all, join = st_intersects)




hexagon_predictors <- recovery_unique_sf_recov10 %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_elevation = mean(height, na.rm = TRUE),
    mean_severity = mean(severity_relative, na.rm = TRUE),
    mean_VPD = mean(VPD_yod1, na.rm = TRUE),
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
      !is.na(dominant_forest_type) &
      !is.na(mean_coniferous) &
      !is.na(mean_broadleaved) &
      !is.na(mean_bare)
  )


hexagons_recov10$dominant_forest_type <- as.factor(hexagons_recov10$dominant_forest_type)



# Extract centroid coordinates
coords <- st_coordinates(st_centroid(hexagons_recov10))

# Convert data to Spatial format if required by GWR
hexagons_sp <- as(hexagons_recov10, "Spatial")


bw <- bw.gwr(
  mean_percent_recovered ~ mean_elevation + 
    mean_severity + 
    mean_VPD + 
    mean_coniferous + 
    mean_broadleaved + 
    mean_bare,
  data = hexagons_sp,
  kernel = "gaussian"
)



gwr_model <- gwr.basic(
  mean_percent_recovered ~ mean_elevation + 
    mean_severity + 
    mean_VPD + 
    mean_coniferous + 
    mean_broadleaved +
    mean_bare,
  data = hexagons_sp,
  bw = bw,  # Use the optimized bandwidth or set manually
  kernel = "gaussian"
)


# Extract coefficients for each predictor
gwr_results <- as.data.frame(gwr_model$SDF)

# Add coefficients back to the spatial hexagon data
hexagons_recov10$coef_elevation <- gwr_results$mean_elevation
hexagons_recov10$coef_severity <- gwr_results$mean_severity
hexagons_recov10$coef_VPD <- gwr_results$mean_VPD
hexagons_recov10$coef_broadleaved <- gwr_results$mean_broadleaved
hexagons_recov10$coef_coniferous <- gwr_results$mean_coniferous
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
ggsave("~/eo_nas/EO4Alps/figs/map_local_r2_recov10.png", plot = p1, width = 7, height = 4.5, dpi = 300)


# map predicted recovery rate
ggplot(hexagons_recov10) +
  geom_sf(aes(fill = pred)) +
  scale_fill_viridis_c(option = "magma", name = "") +
  labs(title = "") +
  theme_bw(base_size = 22)



ggplot(hexagons_recov10) +
  geom_sf(aes(fill = mean_percent_recovered)) +
  scale_fill_viridis_c(option = "magma", name = "") +
  labs(title = "") +
  theme_bw(base_size = 22)


# Export sf object as Shapefile
st_write(
  obj = hexagons_recov10,                       
  dsn = "~/eo_nas/EO4Alps/gis/recovery_hexagons/recov10_rates.shp", # File path and name
  driver = "ESRI Shapefile"                    # Specify driver explicitly
)




# summarise per forest type
regional_summary <- hexagons_recov10 %>%
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


hexagons_recov10$residuals <- gwr_results$residual

# Map Residuals
p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = residuals)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, name = "Residuals") +
  labs(title = "") +
  theme_bw(base_size = 22)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_residuals_recov_10.png", plot = p1, width = 7, height = 4.5, dpi = 300)




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
  theme_bw(base_size = 22)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_elev_recov10.png", plot = p1, width = 7, height = 4.5, dpi = 300)



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
  theme_bw(base_size = 22)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_severity_recov_10.png", plot = p1, width = 7, height = 4.5, dpi = 300)



p1 <- ggplot(hexagons_recov10) +
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
ggsave("~/eo_nas/EO4Alps/figs/map_coef_VPD_recov_10.png", plot = p1, width = 7, height = 4.5, dpi = 300)



p1 <- ggplot(hexagons_recov10) +
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



p1 <- ggplot(hexagons_recov10) +
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
ggsave("~/eo_nas/EO4Alps/figs/map_pre_dist_treeshare_recov_10.png", plot = p1, width = 7, height = 4.5, dpi = 300)



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
  theme_bw(base_size = 22)



# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_bare_recov_10.png", plot = p1, width = 7, height = 4.5, dpi = 300)


#-------------------------------------------------------------------------------
# temporal analysis
#-------------------------------------------------------------------------------

### temporal change of percentage recovered
recovery_unique_sf_temp <- recovery_unique_sf_recov10 %>%
  group_by(GRID_ID) %>%
  summarize(
    total_observations_yod_before_2000 = sum(yod < 2000, na.rm = TRUE),
    total_recovered_yod_before_2000 = sum(recov_10[yod < 2000], na.rm = TRUE),
    percent_recovered_yod_before_2000 = (total_recovered_yod_before_2000 / total_observations_yod_before_2000) * 100,
    total_observations_yod_after_2000 = sum(yod >= 2000, na.rm = TRUE),
    total_recovered_yod_after_2000 = sum(recov_10[yod >= 2000], na.rm = TRUE),
    percent_recovered_yod_after_2000 = (total_recovered_yod_after_2000 / total_observations_yod_after_2000) * 100,
    total_observations = n(),  # Total observations for the entire dataset
    total_recovered = sum(recov_10, na.rm = TRUE),  # Total recovered for the entire dataset
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

p1 <- ggplot(hexagons_temp) +
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

p1 <- ggplot(recovery_before_after_2000_hex) +
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



### load VPD change shapefile
# Shapefile laden (Pfad anpassen)
VPD_hex <- st_read("~/eo_nas/EO4Alps/gis/recovery_hexagons/VPD_anomalies_n2.shp")


p1 <- ggplot(VPD_hex) +
  geom_sf(aes(fill = VPD_nml), color = "grey") +  # Map fill color to recovery %
  scale_fill_gradient2(
    low = "darkblue",    # Color for negative values
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
ggsave("~/eo_nas/EO4Alps/figs/map_diff_VPD.png", plot = p1, width = 9, height = 4.5, dpi = 300)



### bivariate maps
install.packages("biscale")
library(biscale)
library(ggplot2)
library(sf)



# Define custom blue-purple-red palette
bi_palette <- c(
  "1-1" = "#C3C3E5",  # Lightest blue
  "2-1" = "#9999C9",
  "3-1" = "#6161A3",  # Darker blue
  
  "1-2" = "#C8A9B9",  # Light purple
  "2-2" = "#A27A94",
  "3-2" = "#774F6E",  # Medium purple
  
  "1-3" = "#C58D8E",  # Light red
  "2-3" = "#99414F",
  "3-3" = "#5B1926"   # Darkest red
)



# Classify data for bivariate mapping
# Remove rows with NA values in relevant columns
hexagons_temp_bi <- hexagons_temp_all %>%
  drop_na(percent_recovered_difference, mean_VPD_difference) %>%
  bi_class(x = percent_recovered_difference, y = mean_VPD_difference, style = "quantile", dim = 3)


# Create a bivariate color scale with a predefined palette
bi_pal("Bluegill", dim = 3)

bi_legend(pal = "DkViolet", 
          dim = 3, 
          xlab = "Lower → Higher", 
          ylab = "Cool & Wet → Hot & Dry",
          size = 48)


ggplot(hexagons_temp_bi) +
  geom_sf(aes(fill = bi_class), color = "white", size = 0.1) +
  bi_scale_fill(pal = "DkViolet", dim = 3) +
  bi_theme()


# Count hexagons in each bivariate class
bi_class_counts <- hexagons_temp_bi %>%
  count(bi_class) %>%
  arrange(desc(n))  # Sort by frequency

print(bi_class_counts)

# Bar plot of bivariate class counts
ggplot(bi_class_counts, aes(x = reorder(bi_class, -n), y = n, fill = bi_class)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("#4B5E94", "#6E7FAD", "#8B6C98", "#A06C74",
                               "#B35A60", "#C84444", "#5A3D75", "#8A3759", "#D32F2F")) +  # Match colors to your map
  theme_minimal() +
  labs(title = "Number of Hexagons per Bivariate Class",
       x = "Bivariate Class",
       y = "Count of Hexagons") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability




# Filter for climate-vulnerable areas (hot/dry & slower recovery)
vulnerable_hexagons <- hexagons_temp_bi %>%
  filter(bi_class %in% c("1-3", "2-3", "3-3"))  # Classes where recovery is slower & hotter/drier

# Map these areas
ggplot(vulnerable_hexagons) +
  geom_sf(aes(fill = bi_class), color = "white", size = 0.1) +
  scale_fill_manual(values = c("#4B5E94", "#6E7FAD", "#8B6C98")) +  # Only color relevant categories
  theme_minimal() +
  labs(title = "Climate-Vulnerable Areas: Hotter/Drier & Slower Recovery")






### hexagon-specific correlation











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


  


































































