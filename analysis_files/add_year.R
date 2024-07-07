# Load necessary library
library(dplyr)

# Set the directory containing the files
setwd("/data/eo/EO4Alps/level3/NDVI/features")

# List all text files in the directory
file_list <- list.files(pattern = "features_\\d{4}\\.txt")

# Function to process each file
process_file <- function(file_name) {
  # Extract the year from the file name
  year <- gsub("features_(\\d{4})\\.txt", "\\1", file_name)
  
  # Read the data from the file
  data <- read.table(file_name, header = FALSE, stringsAsFactors = FALSE)
  
  # Add the year column
  data <- data %>% mutate(Year = year)
  
  # Write the modified data back to the file
  write.table(data, file_name, row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
}

# Process each file in the list
lapply(file_list, process_file)


# rowbind all feature files
# List all text files in the directory
file_list <- list.files(pattern = "features_\\d{4}\\.txt")

# Initialize an empty list to store the data frames
data_list <- list()

# Function to read each file and add to the list
read_file <- function(file_name) {
  # Read the data from the file
  data <- read.table(file_name, header = FALSE, stringsAsFactors = FALSE, sep = ",")
  
  # Add the data frame to the list
  data_list <<- append(data_list, list(data))
}

# Read each file in the list
lapply(file_list, read_file)

# Combine all data frames into one
features_combined <- bind_rows(data_list)

# Write the combined data to a new file (optional)
write.table(features_combined, "combined_features.csv", row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)

# View the combined data frame (optional)
print(combined_data)


