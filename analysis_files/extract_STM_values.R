# Load required packages
library(raster)
library(sp)
library(rgdal)
library(data.table)

# Set the working directory to the folder with the tif files and the shapefile
setwd("/data/eo/EO4Alps/level3/NDVI/")

### rename csv files according to the folder they are stored in
# Load necessary library
library(stringr)

# Function to rename and copy CSV files
rename_and_copy_csv_files <- function(source_directory, destination_directory) {
  # Create the destination directory if it does not exist
  if (!dir.exists(destination_directory)) {
    dir.create(destination_directory, recursive = TRUE)
  }
  
  # List all folders in the source directory
  folders <- list.dirs(source_directory, full.names = TRUE, recursive = FALSE)
  
  # Iterate over each folder
  for (folder in folders) {
    # Get the folder name
    folder_name <- basename(folder)
    
    # List all csv files in the folder
    csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
    
    # Iterate over each CSV file
    for (csv_file in csv_files) {
      # Get the original file name
      original_file_name <- basename(csv_file)
      
      # Construct the new file name
      new_file_name <- paste0(folder_name, "_", original_file_name)
      
      # Construct the full path for the new file name in the destination directory
      new_file_path <- file.path(destination_directory, new_file_name)
      
      # Copy the file to the new location with the new name
      file.copy(csv_file, new_file_path)
      
      # Print the copying action
      cat("Copied:", csv_file, "to", new_file_path, "\n")
    }
  }
}

# Specify the source directory containing the folders with CSV files
source_directory <- "/data/eo/EO4Alps/level3/NDVI"

# Specify the destination directory where renamed CSV files will be copied
destination_directory <- "/data/eo/EO4Alps/level3/NDVI/csv"

# Call the function to rename and copy CSV files
rename_and_copy_csv_files(source_directory, destination_directory)

#-------------------------------------------------------------------------------

# Read the shapefile
shapefile <- readOGR(dsn = ".", layer = "candidates")

# Get the coordinates of the points in the shapefile
coords <- coordinates(shapefile)

# Extract the X and Y coordinates from the coords object
X <- coords[, 1]
Y <- coords[, 2]

# Extract additional columns from the attribute table of the shapefile
class <- shapefile$class

# Loop through all tif files in the folder
tif_files <- list.files(pattern = "\\.tif$", recursive = TRUE)

for (tif_file in tif_files) {
  cat("Processing:", tif_file, "\n")
  
  # Read the tif file using stack to ensure all bands are read
  tif <- stack(tif_file)
  
  # Print detailed information about the raster
  print(tif)
  
  # Check the number of bands in the tif file
  nbands <- nlayers(tif)
  cat("Number of bands detected:", nbands, "\n")
  
  if (nbands >= 3) {
    # Extract values from band 3  
    values <- extract(tif[[3]], shapefile)
    
    # Convert the extracted values to a data table
    values <- data.table(value = values)
    
    # Combine the extracted values with the X, Y coordinates, and the additional columns from the shapefile
    result <- data.table(X = X, Y = Y, value = values$value, class = class)
    
    # Set the name of the output CSV file based on the input tif file
    output_file <- gsub(".tif", "_band3.csv", basename(tif_file))  # Using basename to get just the file name
    
    # Write the extracted values to the output CSV file
    fwrite(result, file = output_file, sep = ",", row.names = FALSE)
    
    cat("Successfully extracted values from band 3 to", output_file, "\n")
  } else {
    cat("Warning: Not enough bands in", tif_file, "to extract band 3\n")
  }
}





############################


# Function to add the year column to CSV files in place
add_year_to_csv_files <- function(directory) {
  # List all CSV files in the directory
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Iterate over each csv file
  for (csv_file in csv_files) {
    # Read the CSV file
    data <- read.csv(csv_file, stringsAsFactors = FALSE)
    
    # Check if data frame is empty or couldn't be read
    if (is.null(data) || nrow(data) == 0) {
      cat("Warning: File", csv_file, "is empty or could not be read.\n")
      next
    }
    
    # Extract the year from the file name
    file_name <- basename(csv_file)
    year <- str_extract(file_name, "(?<=_)[0-9]+(?=-)")  # Extract digits after the second _ and before the first -
    
    # Add the year column to the data frame
    data <- data %>%
      mutate(year = as.integer(year))
    
    # Overwrite the existing CSV file with the modified data
    write.csv(data, csv_file, row.names = FALSE)
    
    # Print the processing action
    cat("Processed:", csv_file, "\n")
  }
}

# Specify the directory containing the CSV files
directory <- "/data/eo/EO4Alps/level3/NDVI/csv"

# Call the function to add year to CSV files in place
add_year_to_csv_files(directory)














### Merge all the csv files into one file and write the tile in a new column

# Function to merge CSV files from nested directories
merge_csv_files <- function(directory, output_file) {
  # List all CSV files in the directory and subdirectories
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  
  # Initialize an empty data frame to store the merged data
  merged_data <- data.frame()
  
  # Iterate over each CSV file
  for (csv_file in csv_files) {
    # Read the CSV file
    data <- read.csv(csv_file)
    
    # Extract the tile name from the file name
    file_name <- basename(csv_file)
    tile_name <- strsplit(file_name, "_")[[1]][1:2] %>% paste(collapse = "_")
    
    # Add the tile column to the data
    data$tile <- tile_name
    
    # Append the data to the merged_data data frame
    merged_data <- bind_rows(merged_data, data)
  }
  
  # Remove rows with no V1 value
  merged_data <- merged_data %>% filter(!is.na(V1))
  
  # Select the desired columns and write to the output file
  merged_data <- merged_data %>% select(X, Y, V1, tile)
  write.csv(merged_data, output_file, row.names = FALSE)
  
  cat("Merged CSV file created:", output_file, "\n")
}

# Specify the main directory containing the subfolders with CSV files
directory <- "/data/eo/EO4Alps/level3/NDVI/csv"

# Specify the output file name
output_file <- "/data/eo/EO4Alps/level3/NDVI/csv/NDVI_all.csv"

# Call the function to merge CSV files
merge_csv_files(directory, output_file)


##########################



### merge all values from one metric into one csv file

# Set the working directory to where the CSV files are located
setwd("/data/eo/EO4Alps/level3/NDVI/csv")

# Get a list of all the CSV files in the directory
csv_files <- list.files(pattern = "*.csv")

# Define the keywords you're looking for in the file names
keywords <- c("keyword1", "keyword2", "keyword3")

# Create an empty data frame to store the merged data
merged_data <- data.frame()

# Loop through each CSV file
for (i in 1:length(csv_files)) {
  
  # Check if the file name contains any of the keywords
  if (any(grepl(keywords, csv_files[i], ignore.case = TRUE))) {
    
    # Read in the CSV file and add a column with the file name
    data <- read.csv(csv_files[i])
    data$file_name <- csv_files[i]
    
    # Add the data to the merged data frame
    merged_data <- rbind(merged_data, data)
    
  }
  
}

# Write the merged data to a new CSV file
write.csv(merged_data, "merged_data.csv", row.names = FALSE)




### create a bare soil df
# Load required packages
library(dplyr)

# Specify the folder path where CSV files are located
folder_path <- "/mnt/public/Projects/DataCube/projects/foreco/alps/level3/all/csv_bare_soil"

# Get a list of all CSV files containing "NDV_" in their names
csv_files <- list.files(path = folder_path, pattern = "NDV_", full.names = TRUE)

# Initialize an empty list to store individual dataframes
dfs <- list()

# Loop through the CSV files, read them and add to the list
for (file in csv_files) {
  df <- read.csv(file, header = TRUE) # Assuming CSV files have headers
  dfs <- append(dfs, list(df))
}

# Bind all dataframes in the list into one large dataframe
result_df <- do.call(rbind, dfs)

# Generate unique IDs based on combinations of columns X and Y
result_df <- result_df %>%
  group_by(X, Y) %>%
  mutate(ID = group_indices())


# Filter the dataframe to keep only rows where STM is "Q50"
result_df_filtered <- result_df %>% filter(STM == "Q50")


# Optionally, you can save the resulting dataframe to a CSV file
write.csv(result_df_filtered, file = "/mnt/public/Projects/DataCube/projects/foreco/alps/level3/all/csv_bare_soil/bare_soil_NDVI_Q50.csv", row.names = FALSE)


