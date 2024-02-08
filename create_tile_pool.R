# Set the path to your folder containing tar files
folder_path <- "/data/eo/EO4Alps/level1"

# Get a list of all tar files in the folder
tar_files <- list.files(folder_path, pattern = "\\.tar$", full.names = TRUE)

# Add "QUEUED" after each file path
queued_tar_files <- paste(tar_files, "QUEUED", sep = " ")

# Specify the output file path
output_file <- "/data/eo/EO4Alps/level1/tile_pool_new.txt"

# Write the queued tar files to the output file
writeLines(queued_tar_files, con = output_file)
