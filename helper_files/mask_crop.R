# Load libraries
library(terra)
library(sf)

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1988.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/alps_boundary_GMBA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1988_crop.tif"


# Load the raster mosaic
mosaic_raster <- rast(mosaic_raster_path)

# Load the shapefile
shapefile <- st_read(shapefile_path)

# Ensure both datasets have the same CRS
if (!st_crs(shapefile) == crs(mosaic_raster)) {
  shapefile <- st_transform(shapefile, crs(mosaic_raster))
}

# Crop the raster to the shapefile extent
cropped_raster <- crop(mosaic_raster, shapefile)

# Mask the raster to include only the shapefile area
masked_raster <- mask(cropped_raster, shapefile)

# Clip values: set all values > 10000 to 10000 and < 0 to 0
masked_raster <- clamp(masked_raster, lower = 0, upper = 10000, values = TRUE)

# Divide all bands by 100
masked_raster <- masked_raster / 100


# Save the cropped and masked raster
writeRaster(
  masked_raster,
  output_cropped_raster,
  overwrite = TRUE,
  filetype = "GTiff",
  gdal = c("COMPRESS=DEFLATE")
)

# Print success message
cat("Cropped and masked raster saved to:", output_cropped_raster, "\n")

#------------------
# Load libraries
library(terra)
library(sf)

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1996.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/alps_boundary_GMBA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1996_crop.tif"


# Load the raster mosaic
mosaic_raster <- rast(mosaic_raster_path)

# Load the shapefile
shapefile <- st_read(shapefile_path)

# Ensure both datasets have the same CRS
if (!st_crs(shapefile) == crs(mosaic_raster)) {
  shapefile <- st_transform(shapefile, crs(mosaic_raster))
}

# Crop the raster to the shapefile extent
cropped_raster <- crop(mosaic_raster, shapefile)

# Mask the raster to include only the shapefile area
masked_raster <- mask(cropped_raster, shapefile)

# Clip values: set all values > 10000 to 10000 and < 0 to 0
masked_raster <- clamp(masked_raster, lower = 0, upper = 10000, values = TRUE)

# Divide all bands by 100
masked_raster <- masked_raster / 100


# Save the cropped and masked raster
writeRaster(
  masked_raster,
  output_cropped_raster,
  overwrite = TRUE,
  filetype = "GTiff",
  gdal = c("COMPRESS=DEFLATE")
)

# Print success message
cat("Cropped and masked raster saved to:", output_cropped_raster, "\n")





