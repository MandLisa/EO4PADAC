# Load required packages
library(raster)
library(sp)
library(rgdal)
library(data.table)

# Set the working directory to the folder with the tif files and the shapefile
setwd("/data/eo/EO4Alps/level3/NDVI/")

# Read the shapefile
shapefile <- readOGR(dsn = ".", layer = "candidates")

# Get the coordinates of the points in the shapefile
coords <- coordinates(shapefile)

# Extract the X and Y coordinates from the coords object
X <- coords[, 1]
Y <- coords[, 2]

# Extract additional columns from the attribute table of the shapefile
type <- shapefile$type

# Loop through all tif files in the folder
tif_files <- list.files(pattern = "\\.tif$", recursive = TRUE)

for (tif_file in tif_files) {
  # Read the tif file
  tif <- raster(tif_file)
  
  # Get the number of bands in the tif file
  nbands <- nlayers(tif)
  
  # Loop through all bands in the tif file
  for (band in 1:nbands) {
    # Extract the cell values at the locations in the shapefile
    values <- extract(tif[[band]], shapefile)
    
    # Convert the extracted values to a data table
    values <- data.table(values)
    
    # Combine the extracted values with the X and Y coordinates and the additional columns from the shapefile
    values <- cbind(values, X, Y, type)
    
    # Set the name of the output CSV file based on the input tif file and band
    output_file <- paste0(gsub(".tif", "", tif_file), "_band", band, ".csv")
    
    # Write the extracted values to the output CSV file
    fwrite(values, file = output_file, sep = ",", row.names = FALSE)
  }
}


###
# Combine information from one metric and each band and year in one csv file
###

### move all csv files in a separate folder
# Set the directory where the CSV files are located
source_dir <- "/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all"

# Set the directory where the CSV files should be moved to
destination_dir <- "/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/csv"

# Get a list of all CSV files in the source directory
csv_files <- list.files(source_dir, pattern = "\\.csv$")

# Loop through each CSV file and move it to the destination directory
for (file in csv_files) {
  file.copy(file.path(source_dir, file), file.path(destination_dir, file))
}


### merge all values from one metric into one csv file

# Set the working directory to where the CSV files are located
setwd("/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/csv")

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


