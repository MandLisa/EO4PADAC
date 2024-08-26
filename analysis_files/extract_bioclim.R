# Load required libraries
library(raster)
library(tidyr)
library(dplyr)
library(sp)
library(sf)
library(ggplot2)
library(terra)
library(spatial)
library(readr)
library(spatstat)
library(pryr)
library(mgcv)
library(purrr)
library(readr)
library(mgcv)
library(stringr)
library(randomForest)
library(broom)

temp <- raster("~/eo_nas/EO4Alps/climate_data/bioclim/CHELSA_bio1_1981-2010_V.2.1.tif")
prec <- raster("~/eo_nas/EO4Alps/climate_data/bioclim/CHELSA_bio12_1981-2010_V.2.1.tif")


# Read the shapefile
alps <- st_read("~/eo_nas/EO4Alps/gis/alps_perimeter_buffer.shp")


recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_filtered_agent_geoloc.csv")

# just go with one observation per ID (for e.g. plotting)
recovery_unique <- recovery %>% 
  distinct(ID, .keep_all = TRUE)

prec <- raster("~/eo_nas/EO4Alps/climate_data/bioclim/prec2.tif")
temp <- raster("~/eo_nas/EO4Alps/climate_data/bioclim/temp2.tif")

plot(temp)


raster_crs <- crs(prec)

# Reproject the shapefile to match the raster's CRS
shp_reprojected <- st_transform(alps, crs = raster_crs)

prec_crop <- crop(prec, extent(shp_reprojected))

# Mask the raster with the reprojected shapefile
prec_masked <- mask(prec_crop, shp_reprojected)

# Plot the result
plot(prec_masked)


#------------

temp_crop <- crop(temp, extent(shp_reprojected))

# Mask the raster with the reprojected shapefile
temp_masked <- mask(temp_crop, shp_reprojected)

# Plot the result
plot(temp_masked)

# Save the masked raster
writeRaster(temp_masked, "~/eo_nas/EO4Alps/climate_data/bioclim/temperatur_clip.tif", format = "GTiff", overwrite = TRUE)

# Save the masked raster
writeRaster(prec_masked, "~/eo_nas/EO4Alps/climate_data/bioclim/precipitation_clip.tif", format = "GTiff", overwrite = TRUE)

#-----------------------

# Load the reference raster (assuming the reference raster file path is 'reference_raster.tif')
reference_raster <- raster("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0028_Y0028/PREDICTION_l2_1986_v3_HL_ML_MLP.tif")

# Extract the CRS from the reference raster
target_crs <- crs(reference_raster)

# Assume recovery_df has columns: 'longitude' and 'latitude'
recovery_sf <- st_as_sf(recovery, coords = c("x", "y"), crs = st_crs(reference_raster))

recovery_unique_sf <- st_as_sf(recovery_unique, coords = c("x", "y"), crs = st_crs(reference_raster))


prec <- projectRaster(prec, crs = crs(reference_raster))
temp <- projectRaster(temp, crs = crs(reference_raster))


# Extract values for each raster
prec_values <- extract(prec, recovery_sf)
temp_values <- extract(temp, recovery_sf)

# If dem_values is a list, convert it to a vector
prec_values <- unlist(prec_values)
temp_values <- unlist(temp_values)

# Add the DEM values as a new column to recovery_sf
recovery_all_fractions$prec <- prec_values
recovery_all_fractions$temp <- temp_values

### write
write.csv(recovery_unique, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_unique_2308.csv", row.names=FALSE)
write.csv(recovery_all_fractions, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_bioclim.csv", row.names=FALSE)


#-------------------------------------------------------------------------------
### extract absolute VPD values

# Define the start and end years
start_year <- 1986
end_year <- 2018

# Specify the parent directory where you want to create these folders
parent_directory <- "~/eo_nas/EO4Alps/climate_data/VPD_clip"  # Change this to your desired directory path

# Loop through each year and create a folder
for (year in start_year:end_year) {
  # Create the folder name
  folder_name <- file.path(parent_directory, as.character(year))
  
  # Create the directory
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
    cat("Created folder:", folder_name, "\n")
  } else {
    cat("Folder already exists:", folder_name, "\n")
  }
}


# Define the source directory where the files are located
source_directory <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/08"

# Define the parent directory where the new folders were created
parent_directory <- "~/eo_nas/EO4Alps/climate_data"  # Change this to the path where folders are created

# List all files in the source directory
files <- list.files(source_directory, pattern = "^m08_\\d{4}\\.tif$", full.names = TRUE)

# Loop through each file and copy to the corresponding year folder
for (file_path in files) {
  # Extract the year from the filename
  file_name <- basename(file_path)
  year <- sub("m08_(\\d{4})\\.tif", "\\1", file_name)
  
  # Define the destination folder based on the year
  destination_folder <- file.path(parent_directory, year)
  
  # Define the destination file path
  destination_path <- file.path(destination_folder, file_name)
  
  # Copy the file to the destination folder
  file.copy(file_path, destination_path, overwrite = TRUE)
  cat("Copied file:", file_name, "to", destination_folder, "\n")
}


### average

# Define the parent directory where the year folders are located
parent_directory <- "~/eo_nas/EO4Alps/climate_data"  # Change this to the path where your year folders are located

# Define the start and end years
start_year <- 1986
end_year <- 2018

# Loop through each year
for (year in start_year:end_year) {
  # Construct the directory path for the current year
  year_directory <- file.path(parent_directory, as.character(year))
  
  # List all raster files in the year directory
  raster_files <- list.files(year_directory, pattern = "\\.tif$", full.names = TRUE)
  
  # Load all the rasters for the current year
  rasters <- lapply(raster_files, raster)
  
  # Stack the rasters together
  raster_stack <- stack(rasters)
  
  # Compute the average raster
  average_raster <- mean(raster_stack, na.rm = TRUE)
  
  # Define the output file path for the average raster
  output_file <- file.path(parent_directory, paste0("average_", year, ".tif"))
  
  # Save the average raster
  writeRaster(average_raster, output_file, format = "GTiff", overwrite = TRUE)
  
  cat("Computed and saved average raster for year:", year, "\n")
}


# Define the parent directory where the average files are currently located
parent_directory <- "~/eo_nas/EO4Alps/climate_data"  # Change this to the path where your files are currently located

# Define the new folder where you want to move the files
new_folder <- file.path(parent_directory, "VPD_summer_averages")

# Create the new folder if it doesn't already exist
if (!dir.exists(new_folder)) {
  dir.create(new_folder)
  cat("Created new folder:", new_folder, "\n")
} else {
  cat("Folder already exists:", new_folder, "\n")
}

# List all files with "average" in their name
average_files <- list.files(parent_directory, pattern = "average.*\\.tif$", full.names = TRUE)

# Move each file to the new folder
for (file in average_files) {
  # Define the destination path
  destination <- file.path(new_folder, basename(file))
  
  # Move the file
  file.rename(file, destination)
  
  cat("Moved file:", basename(file), "to", new_folder, "\n")
}

# Define the directory where the files are located
directory <- "~/eo_nas/EO4Alps/climate_data/VPD_summer_averages"  # Change this to the path where your files are located

# List all files with "average" in their name
average_files <- list.files(directory, pattern = "^average_\\d{4}\\.tif$", full.names = TRUE)

# Loop through each file and rename it
for (file in average_files) {
  # Extract the year from the filename
  file_name <- basename(file)
  year <- sub("average_(\\d{4})\\.tif", "\\1", file_name)
  
  # Define the new filename
  new_file_name <- paste0("VPD_", year, ".tif")
  
  # Define the full path for the new file
  new_file_path <- file.path(directory, new_file_name)
  
  # Rename the file
  file.rename(file, new_file_path)
  
  cat("Renamed file:", file_name, "to", new_file_name, "\n")
}


### match absolute VPD values with recovery df
# Define the directory where the VPD rasters are stored
raster_folder <- "~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/"  # Change this to your path


extract_vpd_for_year <- function(year, x, y, raster_folder) {
  # Construct the raster file path based on the year
  raster_file <- paste0(raster_folder, "VPD_", year, ".tif")
  
  # Load the raster
  raster_data <- raster(raster_file)
  
  # Create SpatialPoints for the coordinates
  coordinates <- cbind(x, y)
  
  # Extract the raster values at the specified points
  vpd_values <- extract(raster_data, coordinates)
  
  return(vpd_values)
}


# Use mutate to create the new column
recovery_VPD <- recovery %>%
  rowwise() %>%
  mutate(VPD_absolute = extract_vpd_for_year(year, x, y, raster_folder)) %>%
  ungroup()



