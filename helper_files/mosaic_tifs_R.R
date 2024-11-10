library(terra)

# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_2012.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "2012.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_2012.tif", overwrite=TRUE)


library(terra)

# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_2011.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "2011.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_2011.tif", overwrite=TRUE)




# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_2010.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "2010.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_2010.tif", overwrite=TRUE)


library(terra)

# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_2009.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "2009.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_2009.tif", overwrite=TRUE)




# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_2011.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "2011.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_2011.tif", overwrite=TRUE)




# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_2008.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "2008.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_2008.tif", overwrite=TRUE)


library(terra)

# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_2007.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "2007.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_2007.tif", overwrite=TRUE)




# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_2006.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "2006.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_2006.tif", overwrite=TRUE)



# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_2004.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "2004.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_2004.tif", overwrite=TRUE)

# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_2003.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "2003.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_2003.tif", overwrite=TRUE)



# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_2002.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "2002.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_2002.tif", overwrite=TRUE)



# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_2001.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "2001.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_2001.tif", overwrite=TRUE)



# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_2000.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "2000.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_2000.tif", overwrite=TRUE)



# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_1999.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "1999.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_1999.tif", overwrite=TRUE)



# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_1998.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "1998.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_1998.tif", overwrite=TRUE)



# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_1997.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "1997.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_1997.tif", overwrite=TRUE)



# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_1996.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "1996.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_1996.tif", overwrite=TRUE)




# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_1995.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "1995.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_1995.tif", overwrite=TRUE)



# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_1994.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "1994.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_1994.tif", overwrite=TRUE)


# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_1993.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "1993.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_1993.tif", overwrite=TRUE)



# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_1992.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "1992.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_1992.tif", overwrite=TRUE)



# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_1991.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "1991.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_1991.tif", overwrite=TRUE)



# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_1990.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "1990.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_1990.tif", overwrite=TRUE)




# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_1989.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "1989.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_1989.tif", overwrite=TRUE)



# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_1988.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "1988.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_1988.tif", overwrite=TRUE)




# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_1987.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "1987.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_1987.tif", overwrite=TRUE)



# Set the path to the parent directory
parent_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask"
output_mosaic <- file.path(parent_dir, "mosaic_1986.tif")

# Find all .tif files in the parent directory and its subdirectories
tif_files <- list.files(parent_dir, pattern = "1986.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_1986.tif", overwrite=TRUE)






# Read all rasters and merge them
rasters <- lapply(tif_files, rast)
mosaic <- do.call(merge, rasters)


# Write the SpatRaster to a file
writeRaster(mosaic, "~/eo_nas/EO4Alps/level3_predictions/l2_mask/mosaic_2004.tif", overwrite=TRUE)










# Load necessary libraries
# Load necessary libraries
library(terra)
library(sf)

# Load the raster
raster_data <- rast("path_to_your_raster_file.tif")

# Load the shapefile using sf
shapefile_data <- st_read("~/eo_nas/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp")

# Step 1: Reproject the shapefile to match the CRS of the raster
shapefile_reprojected <- st_transform(shapefile_data, crs(mosaic))

# Step 2: Crop the raster using the extent of the reprojected shapefile
raster_cropped <- crop(mosaic, shapefile_reprojected)

# Step 3: Mask the cropped raster using the reprojected shapefile
raster_masked <- mask(raster_cropped, shapefile_reprojected)

# Save or plot the masked raster
writeRaster(raster_masked, "~/eo_nas/EO4Alps/level3_predictions/l2/mosaic_2023_crop.tif", filetype="GTiff")
plot(raster_masked)


