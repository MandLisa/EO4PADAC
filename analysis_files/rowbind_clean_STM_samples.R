# Script for rowbind all feature and response files created by the sampling
library(ggplot2)
library(dplyr)

### for all the responses

# Define the path to the folder containing the files
folder_path <- "/data/eo/EO4Alps/level3/STMs_samples/response"

# Get the list of files in the folder
file_list <- list.files(path = folder_path, pattern = "response_\\d{4}\\.txt$", full.names = TRUE)

# Extract the years from the filenames
years <- as.numeric(sub(".*response_(\\d{4})\\.txt$", "\\1", basename(file_list)))

# Order the files by the extracted years
file_list_ordered <- file_list[order(years)]

# Read and row bind the files
response_all <- do.call(rbind, lapply(file_list_ordered, read.table, header = TRUE))

# View the combined data
print(combined_data)

# Define the output file path
output_file <- file.path(folder_path, "response_all.txt")

# Write the combined data to the new text file
write.table(response_all, file = output_file, row.names = FALSE, col.names = FALSE, sep = "\t")


### For all the features

# Define the path to the folder containing the files
folder_path <- "/data/eo/EO4Alps/level3/STMs_samples/features"

# Get the list of files in the folder
file_list <- list.files(path = folder_path, pattern = "features_\\d{4}\\.txt$", full.names = TRUE)

# Extract the years from the filenames
years <- as.numeric(sub(".*features_(\\d{4})\\.txt$", "\\1", basename(file_list)))

# Order the files by the extracted years
file_list_ordered <- file_list[order(years)]

# Read and row bind the files
features_all <- do.call(rbind, lapply(file_list_ordered, read.table, header = TRUE))

# View the combined data
print(features_all)

# Define the output file path
output_file <- file.path(folder_path, "features_all.txt")

# Write the combined data to the new text file
write.table(features_all, file = output_file, row.names = FALSE, col.names = FALSE, sep = "\t")


#-------------------------------------------------------------------------------
### load pivoted df and merge
all_1 <- read_csv("/data/eo/EO4Alps/level3/STMs_samples/all_1.csv")
all_2 <- read_csv("/data/eo/EO4Alps/level3/STMs_samples/all_2.csv")
all_3 <- read_csv("/data/eo/EO4Alps/level3/STMs_samples/all_3.csv")

# combine
STM_samples <- rbind(all_1, all_2, all_3)




#-------------------------------------------------------------------------------
### Remove noise (e.g. negative STM values) from features dataframe
#-------------------------------------------------------------------------------

# Load the data from a text file
features_all <- read.table("/data/eo/EO4Alps/level3/STMs_samples/features/features_all.txt", header = FALSE, sep = "\t")




