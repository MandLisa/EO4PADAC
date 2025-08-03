# Load libraries
library(terra)
library(sf)

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1986.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1986_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1987.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1987_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1988.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1989.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1989_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1990.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1990_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1991.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1991_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1992.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1992_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1993.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1993_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1994.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1994_crop.tif"


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
rm(list = ls())
gc()


#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1995.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1996.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
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
rm(list = ls())
gc()


#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1997.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1997_crop.tif"


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
rm(list = ls())
gc()


#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1998.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1998_crop.tif"


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
rm(list = ls())
gc()






# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1999.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_1999_crop.tif"


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
rm(list = ls())
gc()

#------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2000.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2000_crop.tif"


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
rm(list = ls())
gc()


# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2001.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2001_crop.tif"


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
rm(list = ls())
gc()


# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2002.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2002_crop.tif"


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

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2003.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2003_crop.tif"


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
rm(list = ls())
gc()

#------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2004.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2004_crop.tif"


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

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2005.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2005_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2006.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2006_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2007.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2007_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------
# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2008.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2008_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2009.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2009_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2010.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2010_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2011.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2011_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2012.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2012_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2013.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2013_crop.tif"


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
rm(list = ls())
gc()



# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2014.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2014_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2015.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaics/mosaic_2015_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2016.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaics/mosaic_2016_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2017.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaics/mosaic_2017_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2018.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaics/mosaic_2018_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2019.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaics/mosaic_2019_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2020.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaics/mosaic_2020_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2021.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaics/mosaic_2021_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2023.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaics/mosaic_2023_crop.tif"


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
rm(list = ls())
gc()

#-------------------------------------------------------------------------------

# Define file paths
mosaic_raster_path <- "/mnt/eo/EO4Alps/level4_fcover/mosaic_2021.tif"
shapefile_path <- "/mnt/eo/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp"
output_cropped_raster <- "/mnt/eo/EO4Alps/level4_fcover/mosaics/mosaic_2021_crop.tif"


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
rm(list = ls())
gc()


