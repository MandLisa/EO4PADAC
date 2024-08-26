# Load necessary libraries
library(dplyr)
library(sp)
library(lme4)
library(ggplot2)


tiles_shapefile <- st_read("~/eo_nas/EO4Alps/level2/shp/grid.shp")

# Convert your dataframe to an sf object if needed
recovery_2608_filt1_sf <- st_as_sf(recovery_2608_filt1, coords = c("x", "y"), crs = st_crs(tiles_shapefile))

# Check and ensure CRS match
if (st_crs(recovery_2608_filt1_sf) != st_crs(tiles_shapefile)) {
  recovery_2608_filt1_sf <- st_transform(recovery_2608_filt1_sf, st_crs(tiles_shapefile))
}

# Perform the spatial join
recovery_with_tile_id <- st_join(recovery_2608_filt1_sf, tiles_shapefile["Tile_ID"])

# Step 4: Extract the x and y coordinates from the geometry column
recovery_with_tile_id <- recovery_with_tile_id %>%
  mutate(
    x = st_coordinates(.)[,1],  # Extract X coordinates
    y = st_coordinates(.)[,2]   # Extract Y coordinates
  )


# Step 5: Convert back to a regular dataframe without the geometry column
recovery_2608_filt1 <- recovery_with_tile_id %>%
  st_set_geometry(NULL) 

### write
write.csv(recovery_2608_filt1, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_2608_filt_geoloc.csv", row.names=FALSE)


