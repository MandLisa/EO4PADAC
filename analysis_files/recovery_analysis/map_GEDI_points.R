library(sf)
library(dplyr)
library(tictoc)
library(scales)
library(RStoolbox)
library(terra)
        
# Step 1: Load the hexagons and points shapefiles
hexagons <- st_read("~/eo_nas/EO4Alps/GEDI/summarize_GEDI/hexagons.shp")
points <- st_read("~/eo_nas/EO4Alps/GEDI/summarize_GEDI/GEDI_points.shp")

# Step 2: Ensure the same CRS
if (!st_crs(hexagons) == st_crs(points)) {
  points <- st_transform(points, st_crs(hexagons))
}

# Step 3: Use `st_intersects` for a direct count without a full join
tic("Spatial Intersection and Aggregation") # Start timing
counts <- st_intersects(hexagons, points) # Get the intersection relationships
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
  # Points overlay
  geom_sf(data = points, color = "black", alpha = 0.1, size = 0.05) +
  theme_minimal() +
  labs(
    title = "Aggregated GEDI Points per 1,000 km² Hexagon",
    caption = ""
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

# Karte speichern
ggsave("~/eo_nas/EO4Alps/figs/map_GEDI_distribution_points.png", plot = map, width = 8, height = 6, dpi = 300)



##################
# Step 1: Load the mosaic raster
raster_file <- "~/eo_nas/EO4Alps/level3_predictions/l2/mosaic_2023_crop.tif" # Replace with the path to your raster
raster <- rast(raster_file)

# Step 1: Reclassify values > 10,000 to 10,000 and values < 0 to 0
raster[raster > 10000] <- 10000
raster[raster < 0] <- 0
gc()
# Step 2: Scale the values from 0 - 1000 to 0 - 100
# Since the values are in the range 0 - 10,000, dividing by 100 will bring them to 0 - 100
scaled_raster <- raster / 100
gc()

# Step 3: Select bands 6 and 7 for plotting
band6 <- scaled_raster[[6]]
band7 <- scaled_raster[[7]]

# Define custom color schemes for each band
colors_band6 <- hcl.colors(100, "mako", rev = TRUE)  # Customize color palette for band 6
colors_band7 <- hcl.colors(100, "rocket", rev = TRUE)  # Customize color palette for band 7

# Set up the file path and dimensions for saving the plot
png(filename = "~/eo_nas/EO4Alps/figs/coni_share.png", width = 8, height = 6, units = "in", res = 300)
# Plot band 6
plot(band6, col = colors_band6, main = "Coniferous share [%]", 
     plg = list(title = ""), mar = c(4, 4, 2, 5))
dev.off()

# Set up the file path and dimensions for saving the plot
png(filename = "~/eo_nas/EO4Alps/figs/broadleaved_share.png", width = 8, height = 6, units = "in", res = 300)
# Plot band 7
plot(band7, col = colors_band7, main = "Broadleaved share [%]", 
     plg = list(title = ""), mar = c(4, 4, 2, 5))
dev.off()


### visualise VPD anomalies
# Step 1: Load the mosaic raster
raster_file <- "~/eo_nas/EO4Alps/climate_data/VPD_anomalies_08_crop.tif" # Replace with the path to your raster
raster <- rast(raster_file)
plot(raster)

# Step 3: Select bands 6 and 7 for plotting
band18 <- raster[[18]]
plot(band18)

# define color ramp
colors_band18 <- hcl.colors(100, "Geyser", rev = FALSE) 
# Define a transparency mask for values to be excluded (e.g., values <= 0)
values(band18)[values(band18) <= 0] <- NA

# Set up the file path and dimensions for saving the plot
png(filename = "~/eo_nas/EO4Alps/figs/map_VPD_anomalies.png", width = 8, height = 6, units = "in", res = 300)
# Plot with axes for lat/long coordinates
plot(band18, col = colors_band18, main = "VPD anomalies",
     cex.main = 1.5,                # Main title size
     plg = list(title = "", cex = 1.5),  # Legend text size
     mar = c(4, 4, 2, 5), axes = TRUE)  # Suppress default axes

lat_lines <- seq(44, 48, by = 2)  # Adjust the sequence to match lat labels
lon_lines <- seq(4, 16, by = 2)   # Adjust the sequence to match lon labels

# Draw grid lines
abline(h = lat_lines, col = "gray", lty = "dotted", lwd = 0.5)  # Horizontal grid lines
abline(v = lon_lines, col = "gray", lty = "dotted", lwd = 0.5)  # Vertical grid lines

dev.off()


##############
# Step 2: Project to Lat/Long (EPSG:4326)
scaled_raster_latlon <- project(scaled_raster, "EPSG:4326")

# Step 3: Select band 6 and apply max limit in visualization
band6 <- scaled_raster_latlon[[6]]

# Apply a limit to the raster values
band6[band6 > 100] <- 100  # Cap values at 100

# Define a custom color palette
colors_band6 <- hcl.colors(100, "mako", rev = TRUE)


# Set up the file path and dimensions for saving the plot
png(filename = "~/eo_nas/EO4Alps/figs/coni_share.png", width = 8, height = 6, units = "in", res = 300)
# Plot with axes for lat/long coordinates
plot(band6, col = colors_band6, main = "Coniferous share [%]",
     cex.main = 1.5,                # Main title size
     plg = list(title = "", cex = 1.5),  # Legend text size
     mar = c(4, 4, 2, 5), axes = TRUE)  # Suppress default axes


# Define the grid lines based on the axis labels
# Check your map to set appropriate intervals for lat/lon
lat_lines <- seq(44, 48, by = 2)  # Adjust the sequence to match lat labels
lon_lines <- seq(4, 16, by = 2)   # Adjust the sequence to match lon labels

# Draw grid lines
abline(h = lat_lines, col = "gray", lty = "dotted", lwd = 0.5)  # Horizontal grid lines
abline(v = lon_lines, col = "gray", lty = "dotted", lwd = 0.5)  # Vertical grid lines

dev.off()


# Define the grid lines based on the axis labels
# Check your map to set appropriate intervals for lat/lon
lat_lines <- seq(44, 48, by = 2)  # Adjust the sequence to match lat labels
lon_lines <- seq(4, 16, by = 2)   # Adjust the sequence to match lon labels

# Draw grid lines
abline(h = lat_lines, col = "gray", lty = "dotted", lwd = 0.5)  # Horizontal grid lines
abline(v = lon_lines, col = "gray", lty = "dotted", lwd = 0.5)  # Vertical grid lines

dev.off()


#-------------------------------------------------------------------------------


# Step 3: Select band 6 and apply max limit in visualization
band7 <- scaled_raster_latlon[[7]]

# Apply a limit to the raster values
band7[band7 > 100] <- 100  # Cap values at 100

# Define a custom color palette
colors_band7 <- hcl.colors(100, "rocket", rev = TRUE)

# Set up the file path and dimensions for saving the plot
png(filename = "~/eo_nas/EO4Alps/figs/broadleaved_share.png", width = 8, height = 6, units = "in", res = 300)
# Plot with axes for lat/long coordinates

plot(band7, col = colors_band7, main = "Broadleaved share [%]",
     cex.main = 1.5,                # Main title size
     plg = list(title = "", cex = 1.5),  # Legend text size
     mar = c(4, 4, 2, 5), axes = TRUE)  # Suppress default axes


# Define the grid lines based on the axis labels
# Check your map to set appropriate intervals for lat/lon
lat_lines <- seq(44, 48, by = 2)  # Adjust the sequence to match lat labels
lon_lines <- seq(4, 16, by = 2)   # Adjust the sequence to match lon labels

# Draw grid lines
abline(h = lat_lines, col = "gray", lty = "dotted", lwd = 0.5)  # Horizontal grid lines
abline(v = lon_lines, col = "gray", lty = "dotted", lwd = 0.5)  # Vertical grid lines

dev.off()





#-------------------------------------------------------------------------------
