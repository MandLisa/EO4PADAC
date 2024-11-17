library(sf)
library(dplyr)
library(tictoc)
library(viridis)

# Load hexagon and point shapefiles
hexagons <- st_read("~/eo_nas/EO4Alps/GEDI/summarize_GEDI/hexagons.shp")
points <- st_read("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_2018_sf.shp")

# Convert hexagons to WGS84 (EPSG:4326)
hexagons <- st_transform(hexagons, crs = 4326)

# Ensure points are also in WGS84
if (st_crs(points) != st_crs(hexagons)) {
  points <- st_transform(points, crs = st_crs(hexagons))
}

# Start timing
tic("Summarizing points by hexagon")

# Step 1: Perform a spatial join to assign points to hexagons
points_with_hex <- st_join(points, hexagons)

# Step 2: Summarize by hexagon
hex_summary <- points_with_hex %>%
  st_drop_geometry() %>% # Drop geometry for faster processing
  group_by(GRID_ID) %>%       # Group by hexagon ID (corresponds to GRID_ID)
  summarise(
    total_points = n(),                                   # Total points in each hexagon
    recovered_points = sum(rcv_10_ == 1, na.rm = TRUE),   # Points where rcv_10_ == 1
    recovery_share = (recovered_points / total_points) * 1000  # Percentage of recovered points
  ) %>%
  mutate(recovery_share = pmin(recovery_share, 100))

# Step 3: Join the summary back to the hexagon shapefile
hexagons <- hexagons %>%
  left_join(hex_summary, by = "GRID_ID")



# Save the updated hexagon shapefile
st_write(hexagons, "~/eo_nas/EO4Alps/00_analysis/_recovery/hexagons.shp", delete_layer = TRUE)

# End timing
toc()





#Create map with recovery percentage as fill
map1 <- ggplot(data = hexagons) +
  geom_sf(aes(fill = recovery_share), color = "grey", size = 0.2) +
  scale_fill_gradientn(
    colors = viridis::viridis(6),    # Viridis palette
    values = scales::rescale(c(0, 20, 40, 60, 80, 100)), # Emphasize <60 values
    name = "Recovery Share (%)",
    breaks = c(0, 20, 40, 60, 80, 100),  # Customize legend breaks
    labels = c("0%", "20%", "40%", "60%", "80%", "100%")  # Legend labels
  ) +
  theme_minimal() +
  labs(
    title = "Percentage of Recovered Points per Hexagon",
    caption = "Recovery data aggregated at hexagon level"
  ) +
  theme(
    legend.position = "right"
  )

print(map1)




# Create a histogram of recovery_share
histogram <- ggplot(data = hexagons, aes(x = recovery_share)) +
  geom_histogram(binwidth = 5, color = "black", fill = "steelblue") + # Adjust binwidth as needed
  labs(
    title = "Distribution of Recovery Share",
    x = "Recovery Share (%)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold")
  )

# Print the histogram
print(histogram)



map <- ggplot(data = hexagons) +
  geom_sf(aes(fill = recovery_share), color = "grey", size = 0.2) +
  scale_fill_gradientn(
    colors = viridis::viridis(14),  # Use the Viridis palette
    values = scales::rescale(c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 75, 100)), # Focus on lower values
    name = "Recovery share (%)",
    breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 75, 100),  # Legend breaks
    labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30", "35", "40", "45", "50", "55", "75", "100") # Legend labels
  ) +
  theme_bw() +
  labs(
    title = "Percentage of recovered points per hexagon",
    caption = ""
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

print(map)

# Karte speichern
ggsave("~/eo_nas/EO4Alps/figs/map_recovered_within10y.png", plot = map, width = 8, height = 6, dpi = 300)



# map
library(ggplot2)

# Create a map showing VPD_mean and annotate with correlation
# Lineares Modell
hexagons %>% filter(is.na(VPD_mean) | is.na(recovery_share)) %>% nrow()

# Filter out rows with NA in VPD_mean or recovery_share
hexagons_clean <- hexagons %>%
  filter(!is.na(VPD_mean) & !is.na(recovery_share))

# Fit the linear model
lm_model <- lm(recovery_share ~ VPD_mean, data = hexagons_clean)

# Compute residuals
hexagons_clean <- hexagons_clean %>%
  mutate(residuals = residuals(lm_model))

# Remove geometry from hexagons_clean before joining
hexagons <- hexagons %>%
  left_join(
    hexagons_clean %>%
      st_drop_geometry() %>% # Drop geometry from the cleaned dataset
      select(GRID_ID, residuals),
    by = "GRID_ID"
  )


#where is predictive power of VPD especially high?
map1 <- ggplot(hexagons) +
  geom_sf(aes(fill = residuals), color = "grey", size = 0.2) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    name = "Residuals"
  ) +
  labs(
    title = "Residuals of recovery share vs VPD @yod",
    subtitle = ""
  ) +
  theme_bw()

# Karte speichern
ggsave("~/eo_nas/EO4Alps/figs/map_residuals.png", plot = map1, width = 8, height = 6, dpi = 300)




# bivariate map
# Normalize variables for visualization
hexagons <- hexagons %>%
  mutate(
    recovery_norm = (recovery_share - min(recovery_share, na.rm = TRUE)) / 
      (max(recovery_share, na.rm = TRUE) - min(recovery_share, na.rm = TRUE)),
    vpd_norm = (VPD_mean - min(VPD_mean, na.rm = TRUE)) / 
      (max(VPD_mean, na.rm = TRUE) - min(VPD_mean, na.rm = TRUE))
  )


# Bivariate Map
# Create a combined interaction metric (for gradient scaling)
hexagons <- hexagons %>%
  mutate(interaction_metric = recovery_norm + vpd_norm) # Example metric

# Bivariate map with gradient scale
ggplot(hexagons) +
  geom_sf(aes(fill = interaction_metric), color = "grey", size = 0.2) +
  scale_fill_viridis_c(name = "Interaction Metric") +
  labs(
    title = "Bivariate Map - Recovery share and VPD anomalies @yod",
    subtitle = ""
  ) +
  theme_bw()



# Assign the correct CRS to the points
st_crs(points) <- 3035  # Assign LAEA Europe (EPSG:3035)
# Reproject points to WGS84
points <- st_transform(points, crs = 4326)
st_bbox(points)

