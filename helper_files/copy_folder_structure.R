library(fs)
# Specify source and destination directories
source_directory <- "/data/eo/EO4Alps/level3/STMs"
new_directory <- "/data/eo/EO4Alps/level3/NDVI"

replicate_folder_structure <- function(source_dir, dest_dir) {
  # List all directories recursively in source directory
  dirs <- list.dirs(path = source_dir, recursive = TRUE, full.names = TRUE)
  
  # Create corresponding empty directories in the destination directory
  for (dir in dirs) {
    new_dir <- file.path(dest_dir, sub(source_dir, "", dir))
    dir.create(new_dir, recursive = TRUE, showWarnings = FALSE)
  }
}

# Specify source and destination directories
source_directory <- "/data/eo/EO4Alps/level3/STMs"
new_directory <- "/data/eo/EO4Alps/level3/NDVI"

# Create the new directory structure (only directories)
dir.create(new_directory, showWarnings = FALSE)

# Call function to replicate folder structure
replicate_folder_structure(source_directory, new_directory)
