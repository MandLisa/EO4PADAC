library(terra)
library(pbapply)

# Define the path to your directory
dir_path <- "~/eo_nas/EO4Alps/climate_data/PR"

# Define the shapefile to use for cropping and masking
shapefile_path <- "~/eo_nas/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_WGS.shp"

# List all tif files in subdirectories
tif_files <- list.files(dir_path, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

# Filter tif files based on the naming pattern (_pr_05_ to _pr_09_)
filtered_files <- grep("_pr_0[5-9]_", tif_files, value = TRUE)

# Read the shapefile
shp <- vect(shapefile_path)

# Function to process each file
process_file <- function(tif) {
  # Read the raster
  r <- rast(tif)
  
  # Crop and mask
  r_cropped <- crop(r, shp)
  r_masked <- mask(r_cropped, shp)
  
  # Define output file path
  output_file <- sub("\\.tif$", "_cropped_masked.tif", tif)
  
  # Write the output raster
  writeRaster(r_masked, output_file, overwrite = TRUE)
  
  return(output_file)  # Return output file name for logging
}

# Apply function with progress bar
processed_files <- pblapply(filtered_files, process_file)

cat("Processing complete! Processed files:\n", unlist(processed_files), "\n")




# Define the main directory
dir_path <- "~/eo_nas/EO4Alps/climate_data/PR"

# List all `_cropped_masked.tif` files
cropped_files <- list.files(dir_path, pattern = "_cropped_masked\\.tif$", recursive = TRUE, full.names = TRUE)

# Extract folder names from file paths
folder_names <- unique(dirname(cropped_files))

# Function to compute mean raster per folder
compute_mean_raster <- function(folder) {
  # Get all cropped/masked files in this folder
  raster_files <- list.files(folder, pattern = "_cropped_masked\\.tif$", full.names = TRUE)
  
  if (length(raster_files) == 0) return(NULL)  # Skip if no files
  
  # Load rasters into a SpatRaster collection
  rasters <- rast(raster_files)
  
  # Compute the mean raster
  mean_raster <- mean(rasters, na.rm = TRUE)
  
  # Define output file name based on folder name
  folder_name <- basename(folder)
  output_file <- file.path(folder, paste0("prec_mean_", folder_name, ".tif"))
  
  # Write the mean raster to disk
  writeRaster(mean_raster, output_file, overwrite = TRUE)
  
  return(output_file)  # Return output file for logging
}

# Apply the function to each folder with a progress bar
mean_rasters <- pblapply(folder_names, compute_mean_raster)

cat("Mean raster computation complete! Processed files:\n", unlist(mean_rasters), "\n")



# List all 'prec_mean_[foldername].tif' files
mean_rasters <- list.files(dir_path, pattern = "^prec_mean_.*\\.tif$", recursive = TRUE, full.names = TRUE)

# Extract folder names to use as band names
band_names <- basename(mean_rasters)
band_names <- gsub("^prec_mean_|\\.tif$", "", band_names)  # Clean names

# Load all mean rasters as a SpatRaster stack
raster_stack <- rast(mean_rasters)

# Assign band names
names(raster_stack) <- band_names

# Define output file path
output_stack_file <- file.path(dir_path, "prec_mean_stack.tif")

# Save the stacked raster
writeRaster(raster_stack, output_stack_file, overwrite = TRUE)

cat("Raster stack created and saved as:", output_stack_file, "\n")


