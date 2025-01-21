# Read the text files
file1 <- read.table("~/eo_nas/EO4Alps/level3/STM_samples_l1_nov/features/features_1986.txt", header = FALSE, stringsAsFactors = FALSE)
file2 <- read.table("~/eo_nas/EO4Alps/level3/STM_samples_l1_nov/response/response_1986.txt", header = FALSE, stringsAsFactors = FALSE)

# Combine column-wise
combined <- cbind(file1, file2)



# Define the range of years
years <- 1986:2023

# Base directories for features and response files
features_dir <- "~/eo_nas/EO4Alps/level3/STM_samples_l1_nov/features/"
response_dir <- "~/eo_nas/EO4Alps/level3/STM_samples_l1_nov/response/"
output_dir <- "~/eo_nas/EO4Alps/level3/STM_samples_l1_nov/combined/"  # Define an output directory

# Ensure the output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Loop through each year
for (year in years) {
  # Construct file paths
  features_file <- paste0(features_dir, "features_", year, ".txt")
  response_file <- paste0(response_dir, "response_", year, ".txt")
  output_file <- paste0(output_dir, "combined_", year, ".txt")
  
  # Read the files
  file1 <- read.table(features_file, header = FALSE, stringsAsFactors = FALSE)
  file2 <- read.table(response_file, header = FALSE, stringsAsFactors = FALSE)
  
  # Combine column-wise
  combined <- cbind(file1, file2)
  
  # Write the combined data to a new file
  write.table(combined, output_file, row.names = FALSE, col.names = FALSE, quote = FALSE)
}

# Directory where combined files are stored
combined_dir <- "~/eo_nas/EO4Alps/level3/STM_samples_l1_nov/combined/"

# Get a list of all combined_*.txt files
combined_files <- list.files(combined_dir, pattern = "combined_\\d+\\.txt", full.names = TRUE)

# Initialize an empty list to store data frames
all_data <- list()

# Read each combined file and store it in the list
for (file in combined_files) {
  data <- read.table(file, header = FALSE, stringsAsFactors = FALSE)
  all_data <- append(all_data, list(data))
}

# Row-bind all data frames together
final_combined <- do.call(rbind, all_data)

# Write the final combined data to a new file
output_file <- paste0(combined_dir, "final_combined.txt")
write.table(final_combined, output_file, row.names = FALSE, col.names = FALSE, quote = FALSE)

cat("Final combined file written to:", output_file, "\n")



# Directory where combined files are stored
combined_dir <- "~/eo_nas/EO4Alps/level3/STM_samples_l1_nov/combined/"

# Get a list of all combined_*.txt files
combined_files <- list.files(combined_dir, pattern = "combined_\\d+\\.txt", full.names = TRUE)

# Initialize an empty list to store data frames
all_data <- list()

# Read each combined file and store it in the list
for (file in combined_files) {
  data <- read.table(file, header = FALSE, stringsAsFactors = FALSE)
  
  # Remove rows containing -9999 in any column
  data <- data[!apply(data == -9999, 1, any), ]
  
  all_data <- append(all_data, list(data))
}

# Row-bind all data frames together
final_combined <- do.call(rbind, all_data)

# Write the final combined data to a new file
output_file <- paste0(combined_dir, "final_combined_filtered.txt")
write.table(final_combined, output_file, row.names = FALSE, col.names = FALSE, quote = FALSE)

cat("Final combined file without -9999 values written to:", output_file, "\n")



# Directory where the combined filtered file is stored
combined_dir <- "~/eo_nas/EO4Alps/level3/STM_samples_l1_nov/combined/"
input_file <- paste0(combined_dir, "final_combined_filtered.txt")

# Read the combined filtered data
data <- read.table(input_file, header = FALSE, stringsAsFactors = FALSE)

# Separate the last column (response) and the rest (features)
response_all <- data[, ncol(data)]  # Extract the last column
features_all <- data[, -ncol(data)]  # Extract all columns except the last

# Save response to a file
response_file <- paste0(combined_dir, "response_all_cleaned.txt")
write.table(response_all, response_file, row.names = FALSE, col.names = FALSE, quote = FALSE)

# Save features to a file
features_file <- paste0(combined_dir, "features_all_cleaned.txt")
write.table(features_all, features_file, row.names = FALSE, col.names = FALSE, quote = FALSE)

cat("Response saved to:", response_file, "\n")
cat("Features saved to:", features_file, "\n")





