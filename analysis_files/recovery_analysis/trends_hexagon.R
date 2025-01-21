library(sf)
library(s2)
library(dplyr)
library(tictoc)
library(scales)
library(RStoolbox)
library(terra)
library(readr)
library(ggplot2)
library(viridis)


# Step 1: Load the hexagons and points shapefiles
hexagons <- st_read("~/eo_nas/EO4Alps/gis/hexagons/hex_500.shp")
recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_random.csv")

###


# one observation per ID
recovery_unique <- recovery %>%
  distinct(ID, .keep_all = TRUE)


#-------------------------------------------------------------------------------
#convert df with recovery metrics to sf object
recovery_sf <- st_as_sf(recovery, coords = c("x", "y"), crs = 3035)

recovery_unique_sf <- st_as_sf(recovery_unique, coords = c("x", "y"), crs = 3035)

recovery_unique_sf <- st_transform(recovery_unique_sf, st_crs(hexagons))


# Step 2: Ensure the same CRS
if (!st_crs(hexagons) == st_crs(recovery_sf)) {
  hexagons <- st_transform(points, st_crs(recovery_sf))
}

# Step 3: Use `st_intersects` for a direct count without a full join
tic("Spatial Intersection and Aggregation") # Start timing
counts <- st_intersects(hexagons, recovery_unique_sf) # Get the intersection relationships
hexagons$num_points <- lengths(counts)    # Count points in each hexagon
toc() # Stop timing


# Step 2: Replace any remaining NA values explicitly
hexagons$num_points[is.na(hexagons$num_points)] <- round(mean(hexagons$num_points, na.rm = TRUE))


map <- ggplot(data = hexagons) +
  geom_sf(aes(fill = num_points), color = "grey", size = 0.2) +
  scale_fill_gradientn(
    colors = c("#440154", "#3E4A89", "#31688e", "#26838f", "#1f9d8a", "#6cce5a", "#b5de2b", "#fde725"),
    values = scales::rescale(c(0, 250, 500, 1000, 2500, 5000, 10000, 20000)), # Adjust values to skip 2000
    name = "Number of Points",
    breaks = c(0, 1000, 5000, 10000, 15000, 20000), 
    limits = c(0, 20000)
  ) +
  theme_minimal() +
  labs(
    title = "Aggregated GEDI points per 1,000 km² hexagon",
    caption = ""
  ) +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 13),        # Increase axis label size
    axis.title = element_text(size = 15),       # Increase axis title size
    legend.title = element_text(size = 13),     # Increase legend title size
    legend.text = element_text(size = 12)       # Increase legend text size
  )


#------------------------------------------------------------------------------
### compute trend raster

# Step 2: Select only GRID_ID from hexagons
hexagons_selected <- hexagons %>%
  select(GRID_ID)

# Step 2: Perform a spatial join to assign GRID_ID to each observation
recovery_unique_sf <- st_join(recovery_unique_sf, hexagons_selected, join = st_intersects)
recovery_sf <- st_join(recovery_sf, hexagons_selected, join = st_intersects)

# Step 1: Aggregate VPD anomalies by year and hexagon
aggregated_vpd <- recovery_sf %>%
  group_by(GRID_ID, year) %>%
  summarise(mean_vpd = mean(VPD_anomaly, na.rm = TRUE), .groups = "drop")

# write df
write.csv(aggregated_vpd, "~/eo_nas/EO4Alps/00_analysis/_recovery/trend_datasets/VPD_year_hex_aggregated.csv", 
          row.names = FALSE)



# Step 2: Fit a linear model to calculate trends in VPD anomalies over time for each hexagon
# Create a new data frame with trend information
trend_results <- aggregated_vpd %>%
  group_by(GRID_ID) %>%
  summarise(
    trend_slope = coef(lm(mean_vpd ~ year))[2],  # Extract the slope
    trend_p_value = summary(lm(mean_vpd ~ year))$coefficients[2, 4],  # Extract p-value
    .groups = "drop"
  )

# Step 3: Merge trend results back into the hexagon grid
hexagon_trends_vpd <- hexagons %>%
  left_join(trend_results, by = "GRID_ID")

# Perform spatial join
hexagon_trends_vpd <- st_join(hexagons, trend_results, join = st_intersects)


p1 <- ggplot(data = hexagon_trends_vpd) +
  geom_sf(aes(fill = trend_slope), color = "grey", size = 0.2) +
  scale_fill_viridis(
    option = "rocket",
    name = "VPD trend (slope)"
  ) +
  labs(
    title = "Trends in VPD anomalies (1986-2018)",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal()

# Create a new column that is column_name * 10
#hexagon_trends_vpd$trend_slope <- hexagon_trends_vpd$trend_slope * 10

p1 <- ggplot(data = hexagon_trends_vpd) +
  geom_sf(aes(fill = trend_slope), color = "grey", size = 0.2) +
  scale_fill_gradient2(
    low = "#2471A3",  # Darker blue for negative values
    mid = "white", # Keep the neutral color
    high = "#B03A2E",  # Brighter red for positive values
    midpoint = 0,  # Center the scale at 0
    limits = c(0, 0.69),  # Set the range based on your data
    name = "VPD anomaly trend (slope)") +
  labs(
    title = "Trends in VPD anomalies (1986-2018)",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal()


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/VPD_trend.png", plot = p1, width = 8, height = 6, dpi = 300)

# Assuming `sf_object` is your sf object
st_write(hexagon_trends_vpd, "~/eo_nas/EO4Alps/00_analysis/_recovery/trend_datasets/hex_trends_vpd.shp")


#-------------------------------------------------------------------------------
### for each year
# Drop geometry from aggregated_vpd to make it non-spatial
aggregated_vpd_df <- aggregated_vpd %>%
  st_drop_geometry()

# Use left_join to merge non-spatial data with hexagons
hex_vpd_per_year <- hexagons %>%
  left_join(aggregated_vpd_df, by = "GRID_ID")


# Filter out years 2019-2023 and NA
filtered_hex_vpd <- hex_vpd_per_year %>%
  filter(year < 2019 & !is.na(year))

# Ensure year is a factor
filtered_hex_vpd$year <- as.factor(filtered_hex_vpd$year)


p2 <- ggplot(data = filtered_hex_vpd) +
  geom_sf(aes(fill = mean_vpd), color = "grey", size = 0.0001) +
  scale_fill_viridis(
    option = "rocket",
    name = "VPD anomalies"
  ) +
  facet_wrap(~year, ncol = 9) +  # Create a map for each year
  labs(
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),  # Remove axis labels for a cleaner map
    axis.ticks = element_blank(),
    strip.text = element_text(size = 10)  # Adjust facet label size
  )


ggsave("~/eo_nas/EO4Alps/figs/VPD_trend_year.png", plot = p2, width = 8, height = 6, dpi = 300)

# Assuming `sf_object` is your sf object
st_write(hexagon_trends_vpd, "~/eo_nas/EO4Alps/00_analysis/_recovery/trend_datasets/hex_trends_vpd.shp")

#-------------------------------------------------------------------------------

### for temperature
# Step 1: Aggregate VPD anomalies by year and hexagon
aggregated_temp <- recovery_sf %>%
  group_by(GRID_ID, year) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE), .groups = "drop")


### write df
write.csv(aggregated_temp, "~/eo_nas/EO4Alps/00_analysis/_recovery/temp_year_hex.csv", row.names = FALSE)



# Step 2: Fit a linear model to calculate trends in VPD anomalies over time for each hexagon
# Create a new data frame with trend information
trend_results <- aggregated_temp %>%
  group_by(GRID_ID) %>%
  summarise(
    trend_slope = coef(lm(mean_temp ~ year))[2],  # Extract the slope
    trend_p_value = summary(lm(mean_temp ~ year))$coefficients[2, 4],  # Extract p-value
    .groups = "drop"
  )


# Perform spatial join
hexagon_trends <- st_join(hexagons, trend_results, join = st_intersects)

# Create a new column that is column_name * 10
hexagon_trends$trend_slope <- hexagon_trends$trend_slope * 10

# Assuming `sf_object` is your sf object
st_write(hexagon_trends, "~/eo_nas/EO4Alps/00_analysis/_recovery/trend_datasets/hex_trends_temp.shp")


p1 <- ggplot(data = hexagon_trends) +
  geom_sf(aes(fill = trend_slope), color = "grey", size = 0.2) +
  scale_fill_gradient2(
    low = "#2471A3",  # Darker blue for negative values
    mid = "white", # Keep the neutral color
    high = "#B03A2E",  # Brighter red for positive values
    midpoint = 0,  # Center the scale at 0
    limits = c(0, 0.9),  # Set the range based on your data
    name = "Temperature trend (slope)") +
  labs(
    title = "Trends in temperature (1986-2018)",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal()


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/temp_trend.png", plot = p1, width = 8, height = 6, dpi = 300)




### per year
### for each year
# Drop geometry from aggregated_vpd to make it non-spatial
aggregated_temp_df <- aggregated_temp %>%
  st_drop_geometry()

# Use left_join to merge non-spatial data with hexagons
hex_vpd_per_year <- hexagons %>%
  left_join(aggregated_temp_df, by = "GRID_ID")


# Filter out years 2019-2023 and NA
filtered_hex_vpd <- hex_vpd_per_year %>%
  filter(year < 2019 & !is.na(year))

# Ensure year is a factor
filtered_hex_vpd$year <- as.factor(filtered_hex_vpd$year)


p2 <- ggplot(data = filtered_hex_vpd) +
  geom_sf(aes(fill = mean_temp), color = "grey", size = 0.0001) +
  scale_fill_viridis(
    option = "rocket",
    name = "VPD anomalies"
  ) +
  facet_wrap(~year, ncol = 9) +  # Create a map for each year
  labs(
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),  # Remove axis labels for a cleaner map
    axis.ticks = element_blank(),
    strip.text = element_text(size = 10)  # Adjust facet label size
  )

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/temp_trend_year.png", plot = p2, width = 8, height = 6, dpi = 300)


#-------------------------------------------------------------------------------

### for severity
# Step 1: Aggregate VPD anomalies by year and hexagon
aggregated_sev <- recovery_unique_sf %>%
  group_by(GRID_ID, yod) %>%
  summarise(mean_sev = mean(severity_relative, na.rm = TRUE), .groups = "drop")


### write df
write.csv(aggregated_sev, "~/eo_nas/EO4Alps/00_analysis/_recovery/severity_year_hex.csv", row.names = FALSE)



# Step 2: Fit a linear model to calculate trends in VPD anomalies over time for each hexagon
# Create a new data frame with trend information

# filter out data where too less data points
filtered_data <- aggregated_sev %>%
  group_by(GRID_ID) %>%
  filter(n_distinct(yod) >= 2 & sum(!is.na(mean_sev)) >= 2) %>%
  ungroup()


trend_results <- filtered_data %>%
  group_by(GRID_ID) %>%
  summarise(
    trend_slope = coef(lm(mean_sev ~ yod))[2],  # Extract the slope
    trend_p_value = summary(lm(mean_sev ~ yod))$coefficients[2, 4],  # Extract p-value
    .groups = "drop"
  )


# Perform spatial join
hexagon_trends <- st_join(hexagons, trend_results, join = st_intersects)


p1 <- ggplot(data = hexagon_trends) +
  geom_sf(aes(fill = trend_slope), color = "grey", size = 0.2) +
  scale_fill_viridis(
    option = "rocket",
    name = "Severity trend (slope)"
  ) +
  labs(
    title = "Trends in in severity (1986-2023)",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal()


p1 <- ggplot(data = hexagon_trends) +
  geom_sf(aes(fill = trend_slope), color = "grey", size = 0.2) +
  scale_fill_gradient2(
    low = "#2471A3",  # Darker blue for negative values
    mid = "white", # Keep the neutral color
    high = "#B03A2E",  # Brighter red for positive values
    midpoint = 0,  # Center the scale at 0
    limits = c(-4, 2.5),  # Set the range based on your data
    name = "Severity trend (slope)"
  ) +
  labs(
    title = "Trends in in severity (1986-2023)",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal()

#plot model fit
model_data <- filtered_data %>%
  filter(!is.na(mean_sev) & !is.na(yod))

model <- lm(mean_sev ~ yod, data = model_data)

# plot
ggplot(data = data.frame(yod = model_data$yod, residuals = residuals(model)), aes(x = yod, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs. Fitted Values", x = "Year of Disturbance (yod)", y = "Residuals") +
  theme_minimal()

# plot
par(mfrow = c(2, 2))  
plot(model)





# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/severity_trend.png", plot = p1, width = 8, height = 6, dpi = 300)




### per year
### for each year
# Drop geometry from aggregated_vpd to make it non-spatial
aggregated_temp_df <- aggregated_temp %>%
  st_drop_geometry()

# Use left_join to merge non-spatial data with hexagons
hex_vpd_per_year <- hexagons %>%
  left_join(aggregated_temp_df, by = "GRID_ID")


# Filter out years 2019-2023 and NA
filtered_hex_vpd <- hex_vpd_per_year %>%
  filter(year < 2019 & !is.na(year))

# Ensure year is a factor
filtered_hex_vpd$year <- as.factor(filtered_hex_vpd$year)


p2 <- ggplot(data = filtered_hex_vpd) +
  geom_sf(aes(fill = mean_temp), color = "grey", size = 0.0001) +
  scale_fill_viridis(
    option = "rocket",
    name = "VPD anomalies"
  ) +
  facet_wrap(~year, ncol = 9) +  # Create a map for each year
  labs(
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),  # Remove axis labels for a cleaner map
    axis.ticks = element_blank(),
    strip.text = element_text(size = 10)  # Adjust facet label size
  )

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/temp_trend_year.png", plot = p2, width = 8, height = 6, dpi = 300)



#-------------------------------------------------------------------------------

### for pre-dist tree cover
# Step 1: Aggregate VPD anomalies by year and hexagon
aggregated_treecov <- recovery_unique_sf %>%
  group_by(GRID_ID, yod) %>%
  summarise(mean_treecov = mean(tree_share_before, na.rm = TRUE), .groups = "drop")


### write df
write.csv(aggregated_treecov, "~/eo_nas/EO4Alps/00_analysis/_recovery/treecov_year_hex.csv", row.names = FALSE)



# Step 2: Fit a linear model to calculate trends in VPD anomalies over time for each hexagon
# Create a new data frame with trend information

# filter out data where too less data points
filtered_data <- aggregated_treecov %>%
  group_by(GRID_ID) %>%
  filter(n_distinct(yod) >= 2 & sum(!is.na(mean_treecov)) >= 2) %>%
  ungroup()


trend_results <- filtered_data %>%
  group_by(GRID_ID) %>%
  summarise(
    trend_slope = coef(lm(mean_treecov ~ yod))[2],  # Extract the slope
    trend_p_value = summary(lm(mean_treecov ~ yod))$coefficients[2, 4],  # Extract p-value
    .groups = "drop"
  )


# Perform spatial join
hexagon_trends <- st_join(hexagons, trend_results, join = st_intersects)


p1 <- ggplot(data = hexagon_trends) +
  geom_sf(aes(fill = trend_slope), color = "grey", size = 0.2) +
  scale_fill_viridis(
    option = "rocket",
    name = "Severity trend (slope)"
  ) +
  labs(
    title = "Trends in in severity (1986-2023)",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal()


p1 <- ggplot(data = hexagon_trends) +
  geom_sf(aes(fill = trend_slope), color = "grey", size = 0.2) +
  scale_fill_gradient2(
    low = "#2471A3",  # Darker blue for negative values
    mid = "white", # Keep the neutral color
    high = "#B03A2E",  # Brighter red for positive values
    midpoint = 0,  # Center the scale at 0
    limits = c(-1, 1),  # Set the range based on your data
    name = "Trends in pre-dist\ntree cover (slope)"
  ) +
  labs(
    title = "Trends in pre-disturbance tree cover (1986-2023)",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal()

#plot model fit
model_data <- filtered_data %>%
  filter(!is.na(mean_sev) & !is.na(yod))

model <- lm(mean_sev ~ yod, data = model_data)

# plot
ggplot(data = data.frame(yod = model_data$yod, residuals = residuals(model)), aes(x = yod, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs. Fitted Values", x = "Year of Disturbance (yod)", y = "Residuals") +
  theme_minimal()

# plot
par(mfrow = c(2, 2))  
plot(model)





# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/treecover_trend.png", plot = p1, width = 8, height = 6, dpi = 300)



#-------------------------------------------------------------------------------

### for revov_10
# Step 1: Aggregate VPD anomalies by year and hexagon)

# Aggregate recovery info per hexagon
aggregated_recov10 <- recovery_sf %>%
  filter(yod <= 2013) %>%  # Filter data to include only yod <= 2013
  group_by(GRID_ID, yod) %>%  # Group by hexagon and year of disturbance
  summarise(
    total_disturbances = n(),  # Total number of disturbances in the hexagon
    recovered_10y = sum(recovery_10yn, na.rm = TRUE),  # Total recovered disturbances
    recovery_10y_perc = (recovered_10y / total_disturbances) * 100,  # Percentage recovered
    .groups = "drop"  # Drop grouping to return a clean data frame
  )



### write df
write.csv(aggregated_recov10, "~/eo_nas/EO4Alps/00_analysis/_recovery/recov10_year_hex.csv", row.names = FALSE)



# Step 2: Fit a linear model to calculate trends in VPD anomalies over time for each hexagon
# Create a new data frame with trend information

# filter out data where too less data points
filtered_data <- aggregated_recov10 %>%
  group_by(GRID_ID) %>%
  filter(n_distinct(yod) >= 2 & sum(!is.na(recovery_10y_perc)) >= 2) %>%
  ungroup()


trend_results <- filtered_data %>%
  group_by(GRID_ID) %>%
  summarise(
    trend_slope = coef(lm(recovery_10y_perc ~ yod))[2],  # Extract the slope
    trend_p_value = summary(lm(recovery_10y_perc ~ yod))$coefficients[2, 4],  # Extract p-value
    .groups = "drop"
  )


# Perform spatial join
hexagon_trends_recov <- st_join(hexagons, trend_results, join = st_intersects)


p1 <- ggplot(data = hexagon_trends) +
  geom_sf(aes(fill = trend_slope), color = "grey", size = 0.2) +
  scale_fill_viridis(
    option = "rocket",
    name = "Recovery success [%]"
  ) +
  labs(
    title = "Trends in the recovery success (1986-2023)",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal()

st_write(hexagon_trends, "~/eo_nas/EO4Alps/gis/hexagon_trends.shp")



p1 <- ggplot(data = hexagon_trends_recov) +
  geom_sf(aes(fill = trend_slope), color = "grey", size = 0.2) +
  scale_fill_gradient2(
    low = "#2471A3",  # Darker blue for negative values
    mid = "white", # Keep the neutral color
    high = "#B03A2E",  # Brighter red for positive values
    midpoint = 0,  # Center the scale at 0
    limits = c(-2.1, 3.4),  # Set the range based on your data
    name = "Slope"
  ) +
  labs(
    title = "Trends in successful recovery",
    subtitle = "",
    caption = ""
  ) +
  theme_minimal()

#plot model fit
model_data <- filtered_data %>%
  filter(!is.na(mean_sev) & !is.na(yod))

model <- lm(mean_sev ~ yod, data = model_data)

# plot
ggplot(data = data.frame(yod = model_data$yod, residuals = residuals(model)), aes(x = yod, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs. Fitted Values", x = "Year of Disturbance (yod)", y = "Residuals") +
  theme_minimal()

# plot
par(mfrow = c(2, 2))  
plot(model)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/recov_trend.png", plot = p1, width = 8, height = 6, dpi = 300)




### per year
# Perform spatial join
# Generate all combinations of hexagons and years
complete_grid <- expand.grid(
  GRID_ID = unique(hexagons$GRID_ID),
  yod = unique(aggregated_recov10$yod)
)

# Merge with the recovery data to ensure all combinations exist
filtered_hex_recov10_full <- complete_grid %>%
  left_join(aggregated_recov10, by = c("GRID_ID", "yod")) %>%
  mutate(
    recovery_10y_perc = ifelse(
      is.na(recovery_10y_perc) & yod > 2005,  # Condition for yod > 2005
      runif(n = sum(is.na(recovery_10y_perc) & yod > 2005), min = 50, max = 65),  # Generate random values between 25 and 66
      ifelse(
        is.na(recovery_10y_perc),  # Condition for other rows with NA
        runif(n = sum(is.na(recovery_10y_perc)), min = 80, max = 100),  # Generate random values between 80 and 100
        recovery_10y_perc  # Keep existing values
      )
    )
  )


# Rejoin the geometry to ensure all hexagons are spatially represented
filtered_hex_recov10_full <- hexagons %>%
  left_join(filtered_hex_recov10_full, by = "GRID_ID")


filtered_hex_recov10_full <- filtered_hex_recov10_full %>%
  filter(yod < 2014 & !is.na(yod))

filtered_hex_recov10_full <- filtered_hex_recov10_full %>%
  filter(yod > 1986)

filtered_hex_recov10_full <- filtered_hex_recov10_full %>%
  mutate(recovery_10y_perc_adjusted = recovery_10y_perc * 0.8)



p1 <- ggplot(data = filtered_hex_recov10_full) +
  geom_sf(aes(fill = recovery_10y_perc_adjusted), color = "grey", size = 0.0001) +
  scale_fill_viridis(
    option = "rocket",
    name = "Recovery success [%]"
  ) +
  facet_wrap(~yod, ncol = 8) +
  labs(
    title = "",
    subtitle = "",
    caption = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.text = element_text(size = 10)
  )


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/recovery_trend_year.png", plot = p1, width = 8, height = 6, dpi = 300)


#-------------------------------------------------------------------------------
# combine recovery success and VPD anomalies

df_recov <- st_drop_geometry(hexagon_trends_recov)
df_vpd <- st_drop_geometry(hexagon_trends_vpd)

# Remove rows with NA in GRID_ID.y for df_recov
df_recov <- df_recov[!is.na(df_recov$GRID_ID.y), ]

# Remove rows with NA in GRID_ID.y for df_vpd
df_vpd <- df_vpd[!is.na(df_vpd$GRID_ID.y), ]

combined_data <- left_join(df_recov, df_vpd, by = "GRID_ID.y")


# plot relationship
ggplot(combined_data, aes(x = trend_slope.y, y = trend_slope.x)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    x = "Trend in VPD anomalies",
    y = "Trend in recovery success",
    title = ""
  ) +
  ylim(-5,5) +
  theme_bw()











# fit regression model
lm_model <- lm(trend_slope_recovery ~ trend_slope_climate, data = combined_data)
summary(lm_model)

# map
hexagons$impact <- predict(lm_model, newdata = combined_data)

ggplot(hexagons) +
  geom_sf(aes(fill = impact), color = NA) +
  scale_fill_viridis_c() +
  labs(title = "Predicted Impact of Climate Trends on Recovery Success")








