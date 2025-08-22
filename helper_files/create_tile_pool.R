# Set the path to your folder containing tar files
folder_path <- "/mnt/eo/EO4Alps/level1"

# Get a list of all tar files in the folder
tar_files <- list.files(folder_path, pattern = "\\.tar$", full.names = TRUE)

# Add "QUEUED" after each file path
queued_tar_files <- paste(tar_files, "QUEUED", sep = " ")

# Specify the output file path
output_file <- "/mnt/eo/EO4Alps/level1/tile_pool_1985_2024.txt"

# Write the queued tar files to the output file
writeLines(queued_tar_files, con = output_file)
