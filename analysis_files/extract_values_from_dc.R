library(raster)

# Set the working directory where the tif files and shapefile are stored
setwd("/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/tif")

# Read the shapefile
shp <- shapefile("candidates_bare_soil.shp")

# Loop over the tif files in the directory
for (filename in list.files(pattern = "\\.tif$")) {
  
  # Read the tif file as a multi-layer raster object
  raster <- stack(filename)
  
  # Loop over the bands in the raster
  for (i in 1:nlayers(raster)) {
    
    # Extract the values at the locations specified by the shapefile for the ith band
    values <- extract(raster, shp, sel = i)
    
    # Create a data frame with the X, Y, type, and extracted values
    df <- data.frame(X = shp@coords[,1],
                     Y = shp@coords[,2],
                     type = shp@data$type,
                     extracted_value = unlist(values))
    
    # Add a column for the band number
    df$band <- i
    
    # Write the data frame to a CSV file with the same name as the tif file
    csv_filename <- gsub("\\.tif$", "", filename)
    write.csv(df, file = paste0(csv_filename, "_band", i, ".csv"), row.names = FALSE)
  }
}

###
#-------------------------------------------------------------------------------
###

### remove all csv files besides those having "band1" in their file name
### the above code gives 13 csv files per metric and band. They are all redundant

# Set the path to the folder containing the CSV files
csv_folder <- "/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/tif/"

# Set the path to the folder where the CSV files not containing the keyword will be moved
destination_folder <- "/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/tif/csv_del/"

# Set the keyword to exclude
keyword <- "_band1."

# Get a list of all the CSV files in the folder
csv_files <- list.files(path = csv_folder, pattern = ".csv")

# Loop through the CSV files
for (file in csv_files) {
  # Check if the keyword is present in the file name
  keyword_present <- grepl(keyword, file, ignore.case = TRUE)
  
  # If the keyword is not present, move the file to the destination folder
  if (!keyword_present) {
    file.rename(paste0(csv_folder, file), paste0(destination_folder, file))
  }
}


# Set the path to the folder containing the CSV files
csv_folder <- "/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/tif/"

# Set the path to the folder where the CSV files with specific keywords will be moved
destination_folder <- "/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/tif/csv_del/"

# Set the list of keywords to include
keywords <- c("band13", "band12", "band11", "band10")

# Get a list of all the CSV files in the folder
csv_files <- list.files(path = csv_folder, pattern = ".csv")

# Loop through the CSV files
for (file in csv_files) {
  # Check if any of the keywords are present in the file name
  keyword_present <- any(sapply(keywords, function(keyword) grepl(keyword, file, ignore.case = TRUE)))
  
  # If any of the keywords are present, move the file to the destination folder
  if (keyword_present) {
    file.rename(paste0(csv_folder, file), paste0(destination_folder, file))
  }
}



###
#-------------------------------------------------------------------------------
###

### rename column names of csv files according to the STMs

# Define the vector of new column names
new_col_names <- c("MIN", "MAX", "AVG", "STD", "RNG", "IQR", "SKW", "KRT", "NUM", "Q25", "Q50", "Q75", "Q90", "bands")

# Set the working directory to the folder containing the csv files
setwd("/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/csv_bare_soil")

# Get a list of all csv files in the folder
csv_files <- list.files(pattern = "*.csv")

# Loop over each csv file
for (csv_file in csv_files) {
  
  # Read in the csv file
  data <- read.csv(csv_file)
  
  # Change the column names of columns 4 to 17
  names(data)[4:17] <- new_col_names
  
  # Write the updated data to a new csv file with "_updated" appended to the filename
  write.csv(data, paste0(sub(".csv", "_v1.csv", csv_file)))
  
}


### delete the column "bands" from all csv files


# Set the directory where your csv files are located
setwd("/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/csv_bare_soil/")

# Get a list of all csv files in the directory
csv_files <- list.files(pattern = "*.csv")

# Loop through each file
for (file in csv_files) {
  # Read in the csv file
  data <- read.csv(file)
  
  # Remove the "bands" column
  data$bands <- NULL
  
  # Write the modified data back to the csv file
  write.csv(data, file, row.names = FALSE)
}

### delete "_v1_v1" csv files
# Set the working directory to the folder containing the CSV files
setwd("/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/csv_bare_soil/")

# Get a list of all CSV files in the folder
csv_files <- list.files(pattern = "\\.csv$")

# Loop through each CSV file
for (file in csv_files) {
  # Check if the filename contains the string "_v1_v1"
  if (grepl("_v1_v1", file)) {
    # If it does, delete the file
    file.remove(file)
    cat("Deleted file:", file, "\n")
  }
}

### also delete the original csv files, which do not contain band names
# Set the working directory to the folder containing the CSV files
setwd("/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/csv_bare_soil/")

# Get a list of all CSV files in the folder
csv_files <- list.files(pattern = "\\.csv$")

# Loop through each CSV file
for (file in csv_files) {
  # Check if the filename contains the string "_v1"
  if (!grepl("_v1", file)) {
    # If it doesn't, delete the file
    file.remove(file)
    cat("Deleted file:", file, "\n")
  }
}


###
#-------------------------------------------------------------------------------
###

# Load necessary packages
library(tidyr)
library(dplyr)

# Set directory containing CSV files
csv_dir <- "/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/csv_bare_soil/"

# Get list of CSV files in directory
csv_files <- list.files(path = csv_dir, pattern = "*.csv", full.names = TRUE)

# Loop through CSV files
csv_list <- list()
for (csv_file in csv_files) {
  
  # Read in CSV file
  df <- read.csv(csv_file)
  
  # Convert columns 4-17 to character
  df[, 5:17] <- lapply(df[, 5:17], as.character)
  
  # Pivot columns 4-17 into tidy format
  df_tidy <- df %>% 
    pivot_longer(cols = 5:17, 
                 names_to = "STM", 
                 values_to = "value")
  
  # Extract filename without extension
  filename <- basename(csv_file)
  filename <- sub("\\.csv$", "", filename)
  
  # Save tidy dataframe as CSV
  write.csv(df_tidy, paste0(filename, "_tidy.csv"), row.names = FALSE)
  
  # Add tidy dataframe to list
  csv_list[[filename]] <- df_tidy
}

# Bind all tidy dataframes into one large dataframe
df_all <- bind_rows(csv_list)


### remove _band1, _v1 and _mosaic in the csv files naming
# Set the directory where your csv files are located
setwd("/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/csv_bare_soil/")

# Get a list of all csv files in the directory
csv_files <- list.files(pattern = "*.csv")

# Define the string to be removed
string_to_remove <- "-"

# Loop through each file
for (file in csv_files) {
  # Remove the string from the file name
  new_file_name <- gsub(string_to_remove, "", file)
  
  # Rename the file
  file.rename(file, new_file_name)
}


### replace two __ with _
# Set the working directory to the folder containing the CSV files
setwd("/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/csv_bare_soil/")

# Get a list of all CSV files in the folder
csv_files <- list.files(pattern = "\\.csv$")

# Loop through each CSV file
for (file in csv_files) {
  # Replace all instances of "__" with "_"
  new_file <- gsub("__", "_", file)
  
  # Rename the file with the updated name
  file.rename(file, new_file)
  cat("Renamed file:", file, "to", new_file, "\n")
}


###
#-------------------------------------------------------------------------------
###

### add relevant info from csv file name into the csv files as new columns

# here metric (indices and metrics)

library(readr)

# set your working directory to the folder where your CSV files are located
setwd("/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/csv_bare_soil")

# get a list of all CSV files in the directory
csv_files <- list.files(pattern = "*.csv")

# loop through each CSV file and add the new "metric" column
for (file in csv_files) {
  # read in the CSV file
  data <- read_csv(file)
  
  # add the new "metric" column
  metric <- substr(file, 1, 3)
  data$metric <- metric
  
  # write the updated data back to the CSV file
  write_csv(data, file)
}


# add year
# Set the directory where your CSV files are located
setwd("/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/csv_bare_soil")

# Get a list of all the CSV files in the directory
csv_files <- list.files(pattern = "*.csv")

# Loop through each CSV file and add a new column with the year value
for (csv_file in csv_files) {
  # Read in the CSV file
  df <- read.csv(csv_file)
  
  # Get the year from the filename
  year <- gsub("^.*_(\\d+)_.*$", "\\1", csv_file)
  
  # Add the year column to the data frame
  df$year <- year
  
  # Overwrite the original CSV file with the updated data frame
  write.csv(df, csv_file, row.names = FALSE)
}


###
#-------------------------------------------------------------------------------
###

### combine all csv files into one

# Set working directory to where CSV files are located
setwd("/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/csv_bare_soil")

# Get all CSV file names in the directory
file_names <- list.files(pattern = "\\.csv$")

# Read in each CSV file and combine into a single data frame
df <- do.call(rbind, lapply(file_names, read.csv))

# Write the combined data frame to a new CSV file
write.csv(df, "extracted_STMs_tidy_bare_soil.csv", row.names = FALSE)


### combine bare soil data with other endbmembers

# Read in the first CSV file
data1 <- read.csv("file1.csv", header = TRUE)

# Read in the second CSV file
data2 <- read.csv("file2.csv", header = TRUE)

# Merge the two data frames using rbind()
merged_data <- rbind(extracted_STMs_tidy, df)

# Write the merged data frame to a new CSV file
write.csv(merged_data, "all_STMs.csv", row.names = FALSE)

head(merged_data)

