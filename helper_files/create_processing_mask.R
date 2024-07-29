library(sf)
library(raster)
library(dplyr)
library(fs)
# Load your raster file
r <- raster("/data/eo/EO4Alps/gis/dist_alps_LAEA.tif")

# Reclassify raster values
r[r >= 1990 & r <= 2020] <- 1
r[r < 1990 | r > 2020] <- 0
r[is.na(r)] <- 0

plot(r)

# Save the reclassified raster to a new file
#writeRaster(r, "/data/public/Projects/DataCube/projects/foreco/alps/gis/processing_mask_new.tif", format = "GTiff")


# Set the file paths
shapefile <- "/data/eo/EO4Alps/level2/shp/grid.shp"
rasterfile <- "/data/eo/EO4Alps/gis/dist_alps_LAEA.tif"

# Read in the shapefile and raster
shape <- st_read(shapefile)
raster <- raster(rasterfile)

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