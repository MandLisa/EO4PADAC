library(sf)
library(raster)
library(dplyr)
library(fs)
library(rgdal)


# Load your raster file
r <- raster("/data/eo/EO4Alps/gis/disturbances_reclass.tif")


plot(r)
resolution <- res(r)
print(resolution)

### raster do not align 100%
# Check CRS
pred <- raster("/data/eo/EO4Alps/level3_predictions/l2/X0030_Y0028/PREDICTION_l2_2023_v3_HL_ML_MLP.tif")
crs(r)
crs(pred)

# Check extent
extent(r)
extent(pred)

# Resample raster2 to match raster1
dist_resampled <- resample(r, pred, method = "ngb")



# convert disturbance raser to same CRS as grid shapefile
shapefile_crs <- st_crs(shape)

# Reproject the raster
disturbances_LAEA <- projectRaster(r, crs = shapefile_crs$proj4string)

# Save the reprojected raster to a new file
writeRaster(disturbances_LAEA, "/data/eo/EO4Alps/gis/disturbances_reclass_LAEA1.tif", overwrite = TRUE)


# Set the file paths
shapefile <- "/data/eo/EO4Alps/level2/shp/grid.shp"
rasterfile <- "/data/eo/EO4Alps/gis/dist_aligned1.tif"


# Read in the shapefile and raster
shape <- st_read(shapefile)
raster <- raster(rasterfile)

# Check CRS of raster
crs(raster)

# Check CRS of shapefile
st_crs(shape)

# Reproject raster to match shapefile's CRS
raster <- projectRaster(raster, crs = st_crs(shape)$proj4string)

# Get the output directory path
out_dir <- dirname(rasterfile)

# Loop through each feature in the shapefile
for (i in 1:nrow(shape)) {
  
  # Get the name of the feature
  name <- as.character(shape %>% slice(i) %>% pull(Tile_ID))
  
  # Clip the raster based on the feature
  clip <- crop(raster, extent(shape[i,]))
  
  # Construct the output file path
  out_path <- file.path(out_dir, paste0(name, ".tif"))
  
  # Save the clipped raster to the output file
  writeRaster(clip, out_path, format="GTiff", overwrite=TRUE)
  
}




### RENAME
### create subfolders named after the tif files itself
# THIS IS IMPORTANT!

# Define the directory containing the .tif files
parent_dir <- "/data/eo/EO4Alps/gis/proc_mask"

# List all .tif files in the directory
tif_files <- dir_ls(parent_dir, glob = "*.tif")

# Loop through each .tif file
for (file in tif_files) {
  # Get the filename without the extension
  file_name <- path_ext_remove(path_file(file))
  
  # Create a new subfolder with the file name
  subfolder_path <- path(parent_dir, file_name)
  dir_create(subfolder_path)
  
  # Move the .tif file to the new subfolder
  file_move(file, path(subfolder_path, path_file(file)))
}

# Confirm the operation
cat("Files have been successfully moved to their respective subfolders.")


### raster do not align 100%
# Check CRS
pred <- raster("/data/eo/EO4Alps/level3_predictions/l2/X0030_Y0028/PREDICTION_l2_2023_v3_HL_ML_MLP.tif")
crs(r)
crs(pred)

# Check extent
extent(r)
extent(pred)

# Resample raster2 to match raster1
resampled_raster2 <- resample(r, pred, method = "ngb")

writeRaster(resampled_raster2, "/data/eo/EO4Alps/gis/dist_aligned.tif", overwrite = TRUE)



# rename
# Define the parent directory
parent_dir <- "/data/eo/EO4Alps/gis/proc_mask"

# List all subdirectories in the parent directory
subdirs <- list.dirs(parent_dir, full.names = TRUE, recursive = FALSE)

# Loop over each subdirectory
for (subdir in subdirs) {
  
  # List all tif files in the current subdirectory
  tif_files <- list.files(subdir, pattern = "\\.tif$", full.names = TRUE)
  
  # Check if there is exactly one tif file
  if (length(tif_files) == 1) {
    
    # Define the new file name
    new_file_name <- file.path(subdir, "proc_mask.tif")
    
    # Rename the file
    file.rename(tif_files, new_file_name)
    
    # Print a message to confirm renaming
    cat("Renamed:", tif_files, "to", new_file_name, "\n")
  } else {
    cat("Warning: No tif file or multiple tif files found in", subdir, "\n")
  }
}



