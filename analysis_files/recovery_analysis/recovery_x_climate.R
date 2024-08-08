# Load required libraries
library(raster)
library(tidyr)
library(dplyr)
library(sp)
library(sf)
library(ggplot2)
library(terra)
library(spatial)
library(spatialEco)
library(readr)
library(spatstat)
library(pryr)
library(mgcv)
library(purrr)
library(readr)
library(mgcv)
library(stringr)

### load recory_climate.csv
recovery_climate <- read.csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate.csv")

# convert CRS of climate data
# Load the reference raster (assuming the reference raster file path is 'reference_raster.tif')
reference_raster <- raster("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0028_Y0028/PREDICTION_l2_1986_v3_HL_ML_MLP.tif")

# Extract the CRS from the reference raster
target_crs <- crs(reference_raster)

# List of raster files to be reprojected (update the paths as needed)
raster_files <- list.files(path = "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies", pattern = "\\.tif$", full.names = TRUE)

# Function to reproject a raster with multiple bands
reproject_raster <- function(raster_path, target_crs) {
  raster_layer <- brick(raster_path)  # Use brick to handle multi-band rasters
  reprojected_raster <- projectRaster(raster_layer, crs = target_crs)
  return(reprojected_raster)
}

# Reproject each raster file and save the results
output_dir <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies"
lapply(raster_files, function(raster_path) {
  reprojected_raster <- reproject_raster(raster_path, target_crs)
  
  # Create the new filename with _LAEA suffix
  base_name <- tools::file_path_sans_ext(basename(raster_path))
  new_filename <- file.path(output_dir, paste0(base_name, "_LAEA.tif"))
  
  # Save the reprojected raster
  writeRaster(reprojected_raster, filename = new_filename, format = "GTiff", overwrite = TRUE)
})

#-------------------------------------------------------------------------------
### extract VPD anomalie values

# Assume recovery_df has columns: 'longitude' and 'latitude'
recovery_sf <- st_as_sf(recovery, coords = c("x", "y"), crs = st_crs(reference_raster))


# Function to extract VPD anomaly values from each raster band
extract_vpd_anomalies <- function(raster_path, recovery_sf) {
  raster_layer <- brick(raster_path)  # Use brick to handle multi-band rasters
  
  # Extract values for each band
  vpd_values <- extract(raster_layer, as(recovery_sf, "Spatial"))
  
  return(vpd_values)
}

# List of reprojected raster files (update the paths as needed)
raster_files <- list.files(path = "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA", pattern = "_LAEA\\.tif$", full.names = TRUE)

# Extract VPD anomaly values for each reprojected raster
vpd_anomalies <- lapply(raster_files, extract_vpd_anomalies, recovery_sf = recovery_sf)

# Combine extracted values into a single dataframe
vpd_anomalies_df <- do.call(cbind, vpd_anomalies)
colnames(vpd_anomalies_df) <- paste0("VPD_anomaly_band", 1:ncol(vpd_anomalies_df))

# Add VPD anomaly values to recovery_df
recovery_climate <- cbind(recovery, vpd_anomalies_df)

# Count the number of NA values in the 'value' column
num_na <- sum(is.na(recovery_climate$VPD_anomaly_band3))

print(num_na)


# Define column ranges for each month
col_ranges <- list(
  Apr = 19:57,
  May = 58:96,
  Jun = 97:135,
  Jul = 136:174,
  Aug = 175:213,
  Sep = 214:252,
  Oct = 253:291
)

# Function to generate new column names
generate_column_names <- function(start_col, end_col, month, start_year, end_year) {
  years <- start_year:end_year
  num_years <- length(years)
  num_cols <- end_col - start_col + 1
  names <- character(num_cols)
  
  for (i in 1:num_cols) {
    year_index <- ((i - 1) %% num_years) + 1
    names[i] <- paste0("VPD_", month, "_", years[year_index])
  }
  
  return(names)
}

# Define year range
start_year <- 1980
end_year <- 2018

# Generate new column names for each month
new_column_names <- unlist(lapply(names(col_ranges), function(month) {
  start_col <- col_ranges[[month]][1]
  end_col <- col_ranges[[month]][length(col_ranges[[month]])]
  generate_column_names(start_col, end_col, month, start_year, end_year)
}))

# Number of columns in the dataframe
num_cols <- ncol(recovery_climate)

# Create new column names for columns 19 to 291
new_colnames <- paste0("new_name_", seq(1, num_cols - 18))

# Check the length of new column names
if (length(new_colnames) == (num_cols - 18)) {
  # Get the original column names
  original_colnames <- colnames(recovery_climate)
  
  # Rename columns from 19 to 291
  colnames(recovery_climate) <- c(original_colnames[1:18], new_column_names)
  
  # Verify the changes
  head(colnames(recovery_climate), 30)  # View the first few column names for verification
} else {
  stop("Number of new column names does not match the number of columns to rename.")
}


# Count the number of NA values in the 'value' column
num_na <- sum(is.na(recovery_climate$VPD_Oct_2018))

print(num_na)

### write
write.csv(recovery_climate, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate.csv", row.names=FALSE)


# Remove columns with years 1980 to 1985
cols_to_remove <- grep("VPD_(Apr|May|Jun|Jul|Aug|Sep|Oct)_(1980|1981|1982|1983|1984|1985)", names(recovery_climate))
recovery_climate <- recovery_climate[ , -cols_to_remove]

### write
write.csv(recovery_climate, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate.csv", row.names=FALSE)


#-------------------------------------------------------------------------------
# reorder data
# Assuming your data frame is named 'df'
recovery_climate_ <- df[, 19:256]





