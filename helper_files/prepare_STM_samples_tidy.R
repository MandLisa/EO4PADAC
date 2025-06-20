# Set your path
path <- "/mnt/eo/EO4Alps/level3_samples/response"

# List files in chronological order
files <- list.files(path, pattern = "^response_\\d{4}\\.txt$", full.names = TRUE)
files <- sort(files)  # Ensures order from 1986 to 2023

# Read and bind all files into one character vector
all_lines <- unlist(lapply(files, readLines))

# Write to a new file
writeLines(all_lines, file.path(path, "response_combined.txt"))
