# Load necessary libraries
library(dplyr)
library(sp)
library(sf)
library(lme4)
library(ggplot2)
library(raster)


tiles_shapefile <- st_read("~/eo_nas/EO4Alps/level2/shp/grid.shp")
hexagons <- st_read("~/eo_nas/EO4Alps/level2/hexagons/hex.shp")

# Convert your dataframe to an sf object if needed
recovery_2608_sf <- st_as_sf(recovery_2608, coords = c("x", "y"), crs = st_crs(tiles_shapefile))

# Check and ensure CRS match
if (st_crs(recovery_2608_filt1_sf) != st_crs(tiles_shapefile)) {
  recovery_2608_filt1_sf <- st_transform(recovery_2608_filt1_sf, st_crs(tiles_shapefile))
}

# Perform the spatial join
recovery_with_tile_id <- st_join(recovery_2608_sf, tiles_shapefile["Tile_ID"])

# Perform the spatial join
recovery_with_hex_id <- st_join(recovery_2608_sf, hexagons["GRID_ID"])



# Step 4: Extract the x and y coordinates from the geometry column
recovery_with_hex_id <- recovery_with_hex_id %>%
  mutate(
    x = st_coordinates(.)[,1],  # Extract X coordinates
    y = st_coordinates(.)[,2]   # Extract Y coordinates
  )


# Step 5: Convert back to a regular dataframe without the geometry column
recovery_2608 <- recovery_with_hex_id %>%
  st_set_geometry(NULL) 

### write
write.csv(recovery_2608, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_2608_hex_ID.csv", row.names=FALSE)

# Use distinct to keep only one row per ID
recovery_2608_unique <- recovery_2608 %>%
  distinct(ID, .keep_all = TRUE)



# Assuming your dataframe is named 'df'
recovery_2608 <- recovery_2608 %>%
  # Create the recovery_status column based on recovery_rate
  mutate(recovery_status = ifelse(recovery_rate < 100, "recovered", "not recovered"))


recovery_summary_tile <- recovery_2608 %>%
  # Filter for disturbances that occurred before 2013
  filter(yod < 2013) %>%
  # Create a new column 'recov_10y' to indicate recovery within 10 years
  group_by(Tile_ID, ID) %>% # Group by pixel to check recovery status per pixel over time
  mutate(recov_10y = ifelse(any(recovery_status == "recovered" & year <= yod + 10), 1, 0)) %>%
  ungroup() %>%
  # Compute the proportion of pixels that recovered within 10 years per GRID_ID
  group_by(Tile_ID) %>%
  summarise(share_recovered_10yr = mean(recov_10y, na.rm = TRUE) * 100) %>%
  ungroup()

recovery_summary_hex <- recovery_2608 %>%
  # Filter for disturbances that occurred before 2013
  filter(yod < 2013) %>%
  # Create a new column 'recov_10y' to indicate recovery within 10 years
  group_by(GRID_ID, ID) %>% # Group by pixel to check recovery status per pixel over time
  mutate(recov_10y = ifelse(any(recovery_status == "recovered" & year <= yod + 10), 1, 0)) %>%
  ungroup() %>%
  # Compute the proportion of pixels that recovered within 10 years per GRID_ID
  group_by(GRID_ID) %>%
  summarise(share_recovered_10yr = mean(recov_10y, na.rm = TRUE) * 100) %>%
  ungroup()


# Join the summary data with the shapefile based on GRID_ID
hex_recov <- hexagons %>%
  left_join(recovery_summary_hex, by = "GRID_ID")

# Write the joined data to a new shapefile
st_write(hex_recov, "~/eo_nas/EO4Alps/level2/hexagons/hex_recov_10y.shp", delete_layer = TRUE)

# for tiles
tile_recov <- tiles_shapefile %>%
  left_join(recovery_summary_tile, by = "Tile_ID")

# Write the joined data to a new shapefile
st_write(tile_recov, "~/eo_nas/EO4Alps/level2/shp/tile_recov_10y.shp", delete_layer = TRUE)


# Assuming 'recovery_summary' is the dataframe with share of recovered pixels per tile_ID
ggplot(recovery_summary, aes(x = Tile_ID, y = share_recovered_10yr)) +
  geom_bar(stat = "identity") +
  labs(title = "Share of Recovered Observations per Tile",
       x = "Tile ID",
       y = "Share of Recovered Pixels (10 years post-disturbance)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### extract GEDI CHM values
CHM <- raster("~/eo_nas/EO4Alps/GEDI/output_vh.tif")
plot(CHM)

# Convert recovery_2608 to sf and reproject if needed
recovery_sf <- st_as_sf(recovery_2608, coords = c("x", "y"), crs = st_crs(CHM))
if (!st_crs(recovery_sf) == st_crs(CHM)) {
  recovery_sf <- st_transform(recovery_sf, st_crs(CHM))
}


CHM_values <- extract(CHM, recovery_sf)
CHM_values <- unlist(CHM_values)
recovery_sf$CHM <- CHM_values

coords <- st_coordinates(recovery_sf)

recovery_3110 <- st_drop_geometry(recovery_sf)  # Drop geometry to convert to regular dataframe
recovery_3110$x <- coords[, 1]  # Add x coordinates
recovery_3110$y <- coords[, 2]  # Add y coordinates but the CHM


# annualized recovery rate


# Calculate recovery metrics, setting annualized recovery rate to NA for years <= yod
recovery_3110 <- recovery_3110 %>%
  mutate(
    recovery_percent = (tree_share_GAM / avg_tree_share_before) * 100,
    annualized_recovery_rate = ifelse(year > yod, (recovery_percent - 100) / ysd, NA)  # NA for pre-disturbance years
  )



                                                                                                        



