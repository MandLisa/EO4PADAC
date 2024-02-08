# Specify the directory where the "aster_dem" folder should be created
target_directory <- "/data/eo/EO4Alps/dem"

# Create the "aster_dem" folder if it doesn't exist
dir.create(file.path(target_directory, "aster_dem"), showWarnings = FALSE)

# Get a list of files with "_dem" in their name
files_to_move <- list.files(path = target_directory, pattern = "_dem")

# Move files to the "aster_dem" folder
for (file in files_to_move) {
  source_path <- file.path(target_directory, file)
  dest_path <- file.path(target_directory, "aster_dem", file)
  file.rename(source_path, dest_path)
}



# Build a virtual mosaic using gdalbuildvrt
# Specify the desired CRS (replace 'EPSG:XXXX' with the desired EPSG code)
desired_crs <- "EPSG:3035"  # For example, WGS 84

# Build a virtual mosaic using gdalbuildvrt
system(sprintf("gdalbuildvrt %s/aster_dem_mosaic.vrt %s/aster_dem/*.tif", target_directory, target_directory))

# Change the CRS using gdalwarp
system(sprintf("gdalwarp -t_srs %s %s/aster_dem_mosaic.vrt %s/aster_dem_mosaic_LAEA.vrt", desired_crs, target_directory, target_directory))




head -n 14 /data/eo/EO4Alps/dem/aster_dem_mosaic_LAEA.vrt