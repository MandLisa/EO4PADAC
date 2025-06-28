# Load libraries
library(terra)
library(sf)

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1995.tif"
mosaic_raster_path <- "~/eo_nas/EO4Alps/level3_STMs/mosaic/mosaic_STMs_l2.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1995_crop.tif"
output_cropped_raster <- "~/eo_nas/EO4Alps/level3_STMs/mosaic/mosaic_crop.tif"

mosaic_raster <- rast("/mnt/eo/EO4Alps/level4_fcover/mosaic_1995.tif")
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1995_crop.tif"

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


plot(masked_raster)
