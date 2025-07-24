library(terra)
library(dplyr)
library(tidyr)
library(tidyr)

# Load the raster
r <- rast("/mnt/eo/EO4Alps/level4_fcover/mosaics/mosaic_2011_crop.tif")

# Create 1000 random points within the raster extent, avoiding NA cells
pts <- spatSample(r, size = 1000, method = "random", na.rm = TRUE, as.points = TRUE)

points <- vect(points_df, geom = c("x", "y"), crs = "EPSG:4326") 


# Directory containing your mosaics
raster_dir <- "/mnt/eo/EO4Alps/level4_fcover/mosaics"

# List all mosaic files (e.g., mosaic_1986_crop.tif ... mosaic_2023_crop.tif)
raster_files <- list.files(raster_dir, pattern = "mosaic_\\d{4}_crop\\.tif$", full.names = TRUE)

# Example: If your points are in a data frame called points_df with columns x and y:
# Convert to SpatVector (update CRS if needed)
# points <- vect(points_df, geom = c("x", "y"), crs = "EPSG:XXXX")

# OR load from shapefile:
# points <- vect("path/to/your_points.shp")

# -----------------------------
# 2. Stack all mosaics
# -----------------------------
r_all <- rast(raster_files)

# -----------------------------
# 3. Extract raster values
# -----------------------------
# Extract values for all layers at the point locations
vals <- terra::extract(r_all, points)[, -1]  # remove ID column

# -----------------------------
# 4. Tidy the data
# -----------------------------
final_df <- vals %>%
  mutate(point_id = 1:nrow(.)) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("prefix", "Year", "band_number"),
    names_pattern = "(mosaic)_(\\d{4})_(\\d+)",
    values_to = "value"
  ) %>%
  mutate(
    Year = as.integer(Year),
    band_number = as.integer(band_number)
  ) %>%
  select(point_id, Year, band_number, value)

# -----------------------------
# 5. Inspect the final data
# -----------------------------
head(final_df)
