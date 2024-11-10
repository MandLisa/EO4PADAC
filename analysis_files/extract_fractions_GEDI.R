# Load required libraries
library(raster)
library(tidyr)
library(dplyr)
library(sp)
library(sf)
library(ggplot2)
library(terra)
library(spatial)
install.packages("spatialEco")
library(spatialEco)
library(readr)
library(spatstat)
library(pryr)

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0028_Y0028")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)
gc()

# Convert raster_stack to data frame
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Remove NAs from select
raster_df <- na.omit(raster_df)

# remove raster_stack to save RAM
rm(raster_stack)
gc()

# Get the names of all columns starting from the 3rd column
columns_to_rename <- names(raster_df)[3:length(names(raster_df))]

# New column names
new_column_names <- c("artificial land", "bare land", "water", "grassland", "shrubland", "coniferous woodland", "broadleaved wodland")

# Rename the columns
names(raster_df)[3:length(names(raster_df))] <- new_column_names

### add year
# Calculate the number of years based on the number of columns and the assumption that each year occupies 7 columns (= 7 classes)
num_years <- (ncol(raster_df) - 2) %/% 7

# Generate the years based on the starting year (1986) and store them in a vector
starting_year <- 1986
years <- starting_year + seq_len(num_years) - 1

# Create a list to store the new column names with the corresponding years
new_column_names_with_year <- vector("list", num_years)

# Populate the list with the new column names including the years
for (i in seq_len(num_years)) {
  new_column_names_with_year[[i]] <- paste0(new_column_names, " ", years[i])
}

# Unlist the list to get the final set of column names with years
new_column_names_final <- unlist(new_column_names_with_year)


# Rename the columns in the raster_df
names(raster_df)[3:length(names(raster_df))] <- new_column_names_final

# Add a new column with unique IDs for each x value
raster_df <- raster_df %>%
  group_by(x, y) %>%
  mutate(ID = cur_group_id())


fcover_long <- pivot_longer(raster_df,
                            cols = 3:268,
                            names_to = "class",
                            values_to = "share")

rm(raster_df)
gc()

# Check the number of unique IDs
num_unique_ids <- length(unique(fcover_long$ID))

# Ensure there are at least 10,000 unique IDs
if (num_unique_ids < 10000) {
  stop("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
}

# Set seed for reproducibility
set.seed(123)

# Randomly sample 10,000 unique IDs
sampled_ids <- sample(unique(fcover_long$ID), 10000)

# Filter the data frame to include only the sampled IDs
fcover_subset <- fcover_long %>%
  filter(ID %in% sampled_ids)

rm(fcover_long)
gc()

# Extract the year information from the last part after the last empty space
fcover_subset$year <- sub(".*\\s(\\d+)$", "\\1", fcover_subset$class)

# Delete the year from the class column
fcover_subset$class <- sub("\\s+[^\\s]+$", "", fcover_subset$class)

# extract yod from disturbance map
fcover_subset$yod <- extract(disturbance_map, fcover_subset[, c("x", "y")])
gc()


# Function to clean 'share' values in a data frame
rescale <- function(table) {
  # Step 1: Replace negative 'share' values with 0
  table <- table %>%
    mutate(share = ifelse(share < 0, 0, share))
  
  # Step 2: Cap 'share' values at 10000
  table <- table %>%
    mutate(share = ifelse(share > 10000, 10000, share))
  
  return(table)
}

# Call the function and store the result in a new data frame
fcover_subset <- rescale(fcover_subset)

# just go with one observation per ID (for e.g. plotting)
fcover_subset_unique <- fcover_subset %>% distinct(ID, .keep_all = TRUE)

write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X28Y28.csv", row.names=FALSE)
write.csv(fcover_subset_unique, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X28Y28_unique.csv", row.names=FALSE)

rm(fcover_subset)
rm(fcover_subset_unique)
rm(new_column_names)
rm(columns_to_rename)
rm(new_column_names)
rm(new_column_names_final)
rm(new_column_names_with_year)
rm(raster_files)
rm(sampled_ids)
rm(starting_year)
rm(years)
gc()

#-------------------------------------------------------------------------------

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0028_Y0029")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)
gc()

# Convert raster_stack to data frame
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Remove NAs from select
raster_df <- na.omit(raster_df)

# remove raster_stack to save RAM
rm(raster_stack)
gc()

# Get the names of all columns starting from the 3rd column
columns_to_rename <- names(raster_df)[3:length(names(raster_df))]

# New column names
new_column_names <- c("artificial land", "bare land", "water", "grassland", "shrubland", "coniferous woodland", "broadleaved wodland")

# Rename the columns
names(raster_df)[3:length(names(raster_df))] <- new_column_names

### add year
# Calculate the number of years based on the number of columns and the assumption that each year occupies 7 columns (= 7 classes)
num_years <- (ncol(raster_df) - 2) %/% 7

# Generate the years based on the starting year (1986) and store them in a vector
starting_year <- 1986
years <- starting_year + seq_len(num_years) - 1

# Create a list to store the new column names with the corresponding years
new_column_names_with_year <- vector("list", num_years)

# Populate the list with the new column names including the years
for (i in seq_len(num_years)) {
  new_column_names_with_year[[i]] <- paste0(new_column_names, " ", years[i])
}

# Unlist the list to get the final set of column names with years
new_column_names_final <- unlist(new_column_names_with_year)


# Rename the columns in the raster_df
names(raster_df)[3:length(names(raster_df))] <- new_column_names_final

# Add a new column with unique IDs for each x value
raster_df <- raster_df %>%
  group_by(x, y) %>%
  mutate(ID = cur_group_id())


fcover_long <- pivot_longer(raster_df,
                            cols = 3:268,
                            names_to = "class",
                            values_to = "share")

rm(raster_df)
gc()

# Check the number of unique IDs
num_unique_ids <- length(unique(fcover_long$ID))

# Ensure there are at least 10,000 unique IDs
if (num_unique_ids < 10000) {
  stop("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
}

# Set seed for reproducibility
set.seed(123)

# Randomly sample 10,000 unique IDs
sampled_ids <- sample(unique(fcover_long$ID), 10000)

# Filter the data frame to include only the sampled IDs
fcover_subset <- fcover_long %>%
  filter(ID %in% sampled_ids)

rm(fcover_long)
gc()

# Extract the year information from the last part after the last empty space
fcover_subset$year <- sub(".*\\s(\\d+)$", "\\1", fcover_subset$class)

# Delete the year from the class column
fcover_subset$class <- sub("\\s+[^\\s]+$", "", fcover_subset$class)

# extract yod from disturbance map
fcover_subset$yod <- extract(disturbance_map, fcover_subset[, c("x", "y")])
gc()


# Function to clean 'share' values in a data frame
rescale <- function(table) {
  # Step 1: Replace negative 'share' values with 0
  table <- table %>%
    mutate(share = ifelse(share < 0, 0, share))
  
  # Step 2: Cap 'share' values at 10000
  table <- table %>%
    mutate(share = ifelse(share > 10000, 10000, share))
  
  return(table)
}

# Call the function and store the result in a new data frame
fcover_subset <- rescale(fcover_subset)

# just go with one observation per ID (for e.g. plotting)
fcover_subset_unique <- fcover_subset %>% distinct(ID, .keep_all = TRUE)

write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X28Y29.csv", row.names=FALSE)
write.csv(fcover_subset_unique, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X28Y29_unique.csv", row.names=FALSE)

rm(fcover_subset)
rm(fcover_subset_unique)
rm(new_column_names)
rm(columns_to_rename)
rm(new_column_names)
rm(new_column_names_final)
rm(new_column_names_with_year)
rm(raster_files)
rm(sampled_ids)
rm(starting_year)
rm(years)
gc()


#-------------------------------------------------------------------------------

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0029_Y0028")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)
gc()

# Convert raster_stack to data frame
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Remove NAs from select
raster_df <- na.omit(raster_df)

# remove raster_stack to save RAM
rm(raster_stack)
gc()

# Get the names of all columns starting from the 3rd column
columns_to_rename <- names(raster_df)[3:length(names(raster_df))]

# New column names
new_column_names <- c("artificial land", "bare land", "water", "grassland", "shrubland", "coniferous woodland", "broadleaved wodland")

# Rename the columns
names(raster_df)[3:length(names(raster_df))] <- new_column_names

### add year
# Calculate the number of years based on the number of columns and the assumption that each year occupies 7 columns (= 7 classes)
num_years <- (ncol(raster_df) - 2) %/% 7

# Generate the years based on the starting year (1986) and store them in a vector
starting_year <- 1986
years <- starting_year + seq_len(num_years) - 1

# Create a list to store the new column names with the corresponding years
new_column_names_with_year <- vector("list", num_years)

# Populate the list with the new column names including the years
for (i in seq_len(num_years)) {
  new_column_names_with_year[[i]] <- paste0(new_column_names, " ", years[i])
}

# Unlist the list to get the final set of column names with years
new_column_names_final <- unlist(new_column_names_with_year)


# Rename the columns in the raster_df
names(raster_df)[3:length(names(raster_df))] <- new_column_names_final

# Add a new column with unique IDs for each x value
raster_df <- raster_df %>%
  group_by(x, y) %>%
  mutate(ID = cur_group_id())


fcover_long <- pivot_longer(raster_df,
                            cols = 3:268,
                            names_to = "class",
                            values_to = "share")

rm(raster_df)
gc()

# Check the number of unique IDs
num_unique_ids <- length(unique(fcover_long$ID))

# Ensure there are at least 10,000 unique IDs
if (num_unique_ids < 10000) {
  stop("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
}

# Set seed for reproducibility
set.seed(123)

# Randomly sample 10,000 unique IDs
sampled_ids <- sample(unique(fcover_long$ID), 10000)

# Filter the data frame to include only the sampled IDs
fcover_subset <- fcover_long %>%
  filter(ID %in% sampled_ids)

rm(fcover_long)
gc()

# Extract the year information from the last part after the last empty space
fcover_subset$year <- sub(".*\\s(\\d+)$", "\\1", fcover_subset$class)

# Delete the year from the class column
fcover_subset$class <- sub("\\s+[^\\s]+$", "", fcover_subset$class)

# extract yod from disturbance map
fcover_subset$yod <- extract(disturbance_map, fcover_subset[, c("x", "y")])
gc()


# Function to clean 'share' values in a data frame
rescale <- function(table) {
  # Step 1: Replace negative 'share' values with 0
  table <- table %>%
    mutate(share = ifelse(share < 0, 0, share))
  
  # Step 2: Cap 'share' values at 10000
  table <- table %>%
    mutate(share = ifelse(share > 10000, 10000, share))
  
  return(table)
}

# Call the function and store the result in a new data frame
fcover_subset <- rescale(fcover_subset)

# just go with one observation per ID (for e.g. plotting)
fcover_subset_unique <- fcover_subset %>% distinct(ID, .keep_all = TRUE)

write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X29Y28.csv", row.names=FALSE)
write.csv(fcover_subset_unique, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X29Y28_unique.csv", row.names=FALSE)

rm(fcover_subset)
rm(fcover_subset_unique)
rm(new_column_names)
rm(columns_to_rename)
rm(new_column_names)
rm(new_column_names_final)
rm(new_column_names_with_year)
rm(raster_files)
rm(sampled_ids)
rm(starting_year)
rm(years)
gc()

#-------------------------------------------------------------------------------

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0029_Y0029")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)
gc()

# Convert raster_stack to data frame
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Remove NAs from select
raster_df <- na.omit(raster_df)

# remove raster_stack to save RAM
rm(raster_stack)
gc()

# Get the names of all columns starting from the 3rd column
columns_to_rename <- names(raster_df)[3:length(names(raster_df))]

# New column names
new_column_names <- c("artificial land", "bare land", "water", "grassland", "shrubland", "coniferous woodland", "broadleaved wodland")

# Rename the columns
names(raster_df)[3:length(names(raster_df))] <- new_column_names

### add year
# Calculate the number of years based on the number of columns and the assumption that each year occupies 7 columns (= 7 classes)
num_years <- (ncol(raster_df) - 2) %/% 7

# Generate the years based on the starting year (1986) and store them in a vector
starting_year <- 1986
years <- starting_year + seq_len(num_years) - 1

# Create a list to store the new column names with the corresponding years
new_column_names_with_year <- vector("list", num_years)

# Populate the list with the new column names including the years
for (i in seq_len(num_years)) {
  new_column_names_with_year[[i]] <- paste0(new_column_names, " ", years[i])
}

# Unlist the list to get the final set of column names with years
new_column_names_final <- unlist(new_column_names_with_year)


# Rename the columns in the raster_df
names(raster_df)[3:length(names(raster_df))] <- new_column_names_final

# Add a new column with unique IDs for each x value
raster_df <- raster_df %>%
  group_by(x, y) %>%
  mutate(ID = cur_group_id())


fcover_long <- pivot_longer(raster_df,
                            cols = 3:268,
                            names_to = "class",
                            values_to = "share")

rm(raster_df)
gc()

# Check the number of unique IDs
num_unique_ids <- length(unique(fcover_long$ID))

# Ensure there are at least 10,000 unique IDs
if (num_unique_ids < 10000) {
  stop("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
}

# Set seed for reproducibility
set.seed(123)

# Randomly sample 10,000 unique IDs
sampled_ids <- sample(unique(fcover_long$ID), 10000)

# Filter the data frame to include only the sampled IDs
fcover_subset <- fcover_long %>%
  filter(ID %in% sampled_ids)

rm(fcover_long)
gc()

# Extract the year information from the last part after the last empty space
fcover_subset$year <- sub(".*\\s(\\d+)$", "\\1", fcover_subset$class)

# Delete the year from the class column
fcover_subset$class <- sub("\\s+[^\\s]+$", "", fcover_subset$class)

# extract yod from disturbance map
fcover_subset$yod <- extract(disturbance_map, fcover_subset[, c("x", "y")])
gc()


# Function to clean 'share' values in a data frame
rescale <- function(table) {
  # Step 1: Replace negative 'share' values with 0
  table <- table %>%
    mutate(share = ifelse(share < 0, 0, share))
  
  # Step 2: Cap 'share' values at 10000
  table <- table %>%
    mutate(share = ifelse(share > 10000, 10000, share))
  
  return(table)
}

# Call the function and store the result in a new data frame
fcover_subset <- rescale(fcover_subset)

# just go with one observation per ID (for e.g. plotting)
fcover_subset_unique <- fcover_subset %>% distinct(ID, .keep_all = TRUE)

write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X29Y29.csv", row.names=FALSE)
write.csv(fcover_subset_unique, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X29Y29_unique.csv", row.names=FALSE)

rm(fcover_subset)
rm(fcover_subset_unique)
rm(new_column_names)
rm(columns_to_rename)
rm(new_column_names)
rm(new_column_names_final)
rm(new_column_names_with_year)
rm(raster_files)
rm(sampled_ids)
rm(starting_year)
rm(years)
gc()


#-------------------------------------------------------------------------------

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0029_Y0030")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)
gc()

# Convert raster_stack to data frame
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Remove NAs from select
raster_df <- na.omit(raster_df)

# remove raster_stack to save RAM
rm(raster_stack)
gc()

# Get the names of all columns starting from the 3rd column
columns_to_rename <- names(raster_df)[3:length(names(raster_df))]

# New column names
new_column_names <- c("artificial land", "bare land", "water", "grassland", "shrubland", "coniferous woodland", "broadleaved wodland")

# Rename the columns
names(raster_df)[3:length(names(raster_df))] <- new_column_names

### add year
# Calculate the number of years based on the number of columns and the assumption that each year occupies 7 columns (= 7 classes)
num_years <- (ncol(raster_df) - 2) %/% 7

# Generate the years based on the starting year (1986) and store them in a vector
starting_year <- 1986
years <- starting_year + seq_len(num_years) - 1

# Create a list to store the new column names with the corresponding years
new_column_names_with_year <- vector("list", num_years)

# Populate the list with the new column names including the years
for (i in seq_len(num_years)) {
  new_column_names_with_year[[i]] <- paste0(new_column_names, " ", years[i])
}

# Unlist the list to get the final set of column names with years
new_column_names_final <- unlist(new_column_names_with_year)


# Rename the columns in the raster_df
names(raster_df)[3:length(names(raster_df))] <- new_column_names_final

# Add a new column with unique IDs for each x value
raster_df <- raster_df %>%
  group_by(x, y) %>%
  mutate(ID = cur_group_id())


fcover_long <- pivot_longer(raster_df,
                            cols = 3:268,
                            names_to = "class",
                            values_to = "share")

rm(raster_df)
gc()

# Check the number of unique IDs
num_unique_ids <- length(unique(fcover_long$ID))

# Ensure there are at least 10,000 unique IDs
if (num_unique_ids < 10000) {
  stop("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
}

# Set seed for reproducibility
set.seed(123)

# Randomly sample 10,000 unique IDs
sampled_ids <- sample(unique(fcover_long$ID), 10000)

# Filter the data frame to include only the sampled IDs
fcover_subset <- fcover_long %>%
  filter(ID %in% sampled_ids)

rm(fcover_long)
gc()

# Extract the year information from the last part after the last empty space
fcover_subset$year <- sub(".*\\s(\\d+)$", "\\1", fcover_subset$class)

# Delete the year from the class column
fcover_subset$class <- sub("\\s+[^\\s]+$", "", fcover_subset$class)

# extract yod from disturbance map
fcover_subset$yod <- extract(disturbance_map, fcover_subset[, c("x", "y")])
gc()

# Function to clean 'share' values in a data frame
rescale <- function(table) {
  # Step 1: Replace negative 'share' values with 0
  table <- table %>%
    mutate(share = ifelse(share < 0, 0, share))
  
  # Step 2: Cap 'share' values at 10000
  table <- table %>%
    mutate(share = ifelse(share > 10000, 10000, share))
  
  return(table)
}

# Call the function and store the result in a new data frame
fcover_subset <- rescale(fcover_subset)

# just go with one observation per ID (for e.g. plotting)
fcover_subset_unique <- fcover_subset %>% distinct(ID, .keep_all = TRUE)

write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X29Y30.csv", row.names=FALSE)
write.csv(fcover_subset_unique, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X29Y30_unique.csv", row.names=FALSE)

rm(fcover_subset)
rm(fcover_subset_unique)
rm(new_column_names)
rm(columns_to_rename)
rm(new_column_names)
rm(new_column_names_final)
rm(new_column_names_with_year)
rm(raster_files)
rm(sampled_ids)
rm(starting_year)
rm(years)
gc()

#-------------------------------------------------------------------------------

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0030_Y0027")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)
gc()

# Convert raster_stack to data frame
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Remove NAs from select
raster_df <- na.omit(raster_df)

# remove raster_stack to save RAM
rm(raster_stack)
gc()

# Get the names of all columns starting from the 3rd column
columns_to_rename <- names(raster_df)[3:length(names(raster_df))]

# New column names
new_column_names <- c("artificial land", "bare land", "water", "grassland", "shrubland", "coniferous woodland", "broadleaved wodland")

# Rename the columns
names(raster_df)[3:length(names(raster_df))] <- new_column_names

### add year
# Calculate the number of years based on the number of columns and the assumption that each year occupies 7 columns (= 7 classes)
num_years <- (ncol(raster_df) - 2) %/% 7

# Generate the years based on the starting year (1986) and store them in a vector
starting_year <- 1986
years <- starting_year + seq_len(num_years) - 1

# Create a list to store the new column names with the corresponding years
new_column_names_with_year <- vector("list", num_years)

# Populate the list with the new column names including the years
for (i in seq_len(num_years)) {
  new_column_names_with_year[[i]] <- paste0(new_column_names, " ", years[i])
}

# Unlist the list to get the final set of column names with years
new_column_names_final <- unlist(new_column_names_with_year)


# Rename the columns in the raster_df
names(raster_df)[3:length(names(raster_df))] <- new_column_names_final

# Add a new column with unique IDs for each x value
raster_df <- raster_df %>%
  group_by(x, y) %>%
  mutate(ID = cur_group_id())


fcover_long <- pivot_longer(raster_df,
                            cols = 3:268,
                            names_to = "class",
                            values_to = "share")

rm(raster_df)
gc()

# Check the number of unique IDs
num_unique_ids <- length(unique(fcover_long$ID))

# Ensure there are at least 10,000 unique IDs
if (num_unique_ids < 10000) {
  stop("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
}

# Set seed for reproducibility
set.seed(123)

# Randomly sample 10,000 unique IDs
sampled_ids <- sample(unique(fcover_long$ID), 10000)

# Filter the data frame to include only the sampled IDs
fcover_subset <- fcover_long %>%
  filter(ID %in% sampled_ids)

rm(fcover_long)
gc()


# Extract the year information from the last part after the last empty space
fcover_subset$year <- sub(".*\\s(\\d+)$", "\\1", fcover_subset$class)

# Delete the year from the class column
fcover_subset$class <- sub("\\s+[^\\s]+$", "", fcover_subset$class)

# extract yod from disturbance map
fcover_subset$yod <- extract(disturbance_map, fcover_subset[, c("x", "y")])
gc()


# Function to clean 'share' values in a data frame
rescale <- function(table) {
  # Step 1: Replace negative 'share' values with 0
  table <- table %>%
    mutate(share = ifelse(share < 0, 0, share))
  
  # Step 2: Cap 'share' values at 10000
  table <- table %>%
    mutate(share = ifelse(share > 10000, 10000, share))
  
  return(table)
}

# Call the function and store the result in a new data frame
fcover_subset <- rescale(fcover_subset)

# just go with one observation per ID (for e.g. plotting)
fcover_subset_unique <- fcover_subset %>% distinct(ID, .keep_all = TRUE)

write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X30Y27.csv", row.names=FALSE)
write.csv(fcover_subset_unique, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X30Y27_unique.csv", row.names=FALSE)

rm(fcover_subset)
rm(fcover_subset_unique)
rm(new_column_names)
rm(columns_to_rename)
rm(new_column_names)
rm(new_column_names_final)
rm(new_column_names_with_year)
rm(raster_files)
rm(sampled_ids)
rm(starting_year)
rm(years)
gc()


#-------------------------------------------------------------------------------

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0030_Y0028")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)
gc()

# Convert raster_stack to data frame
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Remove NAs from select
raster_df <- na.omit(raster_df)

# remove raster_stack to save RAM
rm(raster_stack)
gc()

# Get the names of all columns starting from the 3rd column
columns_to_rename <- names(raster_df)[3:length(names(raster_df))]

# New column names
new_column_names <- c("artificial land", "bare land", "water", "grassland", "shrubland", "coniferous woodland", "broadleaved wodland")

# Rename the columns
names(raster_df)[3:length(names(raster_df))] <- new_column_names

### add year
# Calculate the number of years based on the number of columns and the assumption that each year occupies 7 columns (= 7 classes)
num_years <- (ncol(raster_df) - 2) %/% 7

# Generate the years based on the starting year (1986) and store them in a vector
starting_year <- 1986
years <- starting_year + seq_len(num_years) - 1

# Create a list to store the new column names with the corresponding years
new_column_names_with_year <- vector("list", num_years)

# Populate the list with the new column names including the years
for (i in seq_len(num_years)) {
  new_column_names_with_year[[i]] <- paste0(new_column_names, " ", years[i])
}

# Unlist the list to get the final set of column names with years
new_column_names_final <- unlist(new_column_names_with_year)


# Rename the columns in the raster_df
names(raster_df)[3:length(names(raster_df))] <- new_column_names_final

# Add a new column with unique IDs for each x value
raster_df <- raster_df %>%
  group_by(x, y) %>%
  mutate(ID = cur_group_id())


fcover_long <- pivot_longer(raster_df,
                            cols = 3:268,
                            names_to = "class",
                            values_to = "share")

rm(raster_df)
gc()

# Check the number of unique IDs
num_unique_ids <- length(unique(fcover_long$ID))

# Ensure there are at least 10,000 unique IDs
if (num_unique_ids < 10000) {
  stop("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
}

# Set seed for reproducibility
set.seed(123)

# Randomly sample 10,000 unique IDs
sampled_ids <- sample(unique(fcover_long$ID), 10000)

# Filter the data frame to include only the sampled IDs
fcover_subset <- fcover_long %>%
  filter(ID %in% sampled_ids)

rm(fcover_long)
gc()


# Extract the year information from the last part after the last empty space
fcover_subset$year <- sub(".*\\s(\\d+)$", "\\1", fcover_subset$class)

# Delete the year from the class column
fcover_subset$class <- sub("\\s+[^\\s]+$", "", fcover_subset$class)

# extract yod from disturbance map
fcover_subset$yod <- extract(disturbance_map, fcover_subset[, c("x", "y")])
gc()


# Function to clean 'share' values in a data frame
rescale <- function(table) {
  # Step 1: Replace negative 'share' values with 0
  table <- table %>%
    mutate(share = ifelse(share < 0, 0, share))
  
  # Step 2: Cap 'share' values at 10000
  table <- table %>%
    mutate(share = ifelse(share > 10000, 10000, share))
  
  return(table)
}

# Call the function and store the result in a new data frame
fcover_subset <- rescale(fcover_subset)

# just go with one observation per ID (for e.g. plotting)
fcover_subset_unique <- fcover_subset %>% distinct(ID, .keep_all = TRUE)

write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X30Y28.csv", row.names=FALSE)
write.csv(fcover_subset_unique, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X30Y28_unique.csv", row.names=FALSE)

rm(fcover_subset)
rm(fcover_subset_unique)
rm(new_column_names)
rm(columns_to_rename)
rm(new_column_names)
rm(new_column_names_final)
rm(new_column_names_with_year)
rm(raster_files)
rm(sampled_ids)
rm(starting_year)
rm(years)
gc()

#-------------------------------------------------------------------------------

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0030_Y0029")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)
gc()

# Convert raster_stack to data frame
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Remove NAs from select
raster_df <- na.omit(raster_df)

# remove raster_stack to save RAM
rm(raster_stack)
gc()

# Get the names of all columns starting from the 3rd column
columns_to_rename <- names(raster_df)[3:length(names(raster_df))]

# New column names
new_column_names <- c("artificial land", "bare land", "water", "grassland", "shrubland", "coniferous woodland", "broadleaved wodland")

# Rename the columns
names(raster_df)[3:length(names(raster_df))] <- new_column_names

### add year
# Calculate the number of years based on the number of columns and the assumption that each year occupies 7 columns (= 7 classes)
num_years <- (ncol(raster_df) - 2) %/% 7

# Generate the years based on the starting year (1986) and store them in a vector
starting_year <- 1986
years <- starting_year + seq_len(num_years) - 1

# Create a list to store the new column names with the corresponding years
new_column_names_with_year <- vector("list", num_years)

# Populate the list with the new column names including the years
for (i in seq_len(num_years)) {
  new_column_names_with_year[[i]] <- paste0(new_column_names, " ", years[i])
}

# Unlist the list to get the final set of column names with years
new_column_names_final <- unlist(new_column_names_with_year)


# Rename the columns in the raster_df
names(raster_df)[3:length(names(raster_df))] <- new_column_names_final

# Add a new column with unique IDs for each x value
raster_df <- raster_df %>%
  group_by(x, y) %>%
  mutate(ID = cur_group_id())


fcover_long <- pivot_longer(raster_df,
                            cols = 3:268,
                            names_to = "class",
                            values_to = "share")

rm(raster_df)
gc()

# Check the number of unique IDs
num_unique_ids <- length(unique(fcover_long$ID))

# Ensure there are at least 10,000 unique IDs
if (num_unique_ids < 10000) {
  stop("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
}

# Set seed for reproducibility
set.seed(123)

# Randomly sample 10,000 unique IDs
sampled_ids <- sample(unique(fcover_long$ID), 10000)

# Filter the data frame to include only the sampled IDs
fcover_subset <- fcover_long %>%
  filter(ID %in% sampled_ids)

rm(fcover_long)
gc()

# Extract the year information from the last part after the last empty space
fcover_subset$year <- sub(".*\\s(\\d+)$", "\\1", fcover_subset$class)

# Delete the year from the class column
fcover_subset$class <- sub("\\s+[^\\s]+$", "", fcover_subset$class)

# extract yod from disturbance map
fcover_subset$yod <- extract(disturbance_map, fcover_subset[, c("x", "y")])
gc()


# Function to clean 'share' values in a data frame
rescale <- function(table) {
  # Step 1: Replace negative 'share' values with 0
  table <- table %>%
    mutate(share = ifelse(share < 0, 0, share))
  
  # Step 2: Cap 'share' values at 10000
  table <- table %>%
    mutate(share = ifelse(share > 10000, 10000, share))
  
  return(table)
}

# Call the function and store the result in a new data frame
fcover_subset <- rescale(fcover_subset)

# just go with one observation per ID (for e.g. plotting)
fcover_subset_unique <- fcover_subset %>% distinct(ID, .keep_all = TRUE)

write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X30Y29.csv", row.names=FALSE)
write.csv(fcover_subset_unique, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X30Y29_unique.csv", row.names=FALSE)

rm(fcover_subset)
rm(fcover_subset_unique)
rm(new_column_names)
rm(columns_to_rename)
rm(new_column_names)
rm(new_column_names_final)
rm(new_column_names_with_year)
rm(raster_files)
rm(sampled_ids)
rm(starting_year)
rm(years)
gc()


#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0032_Y0028")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)
gc()

# Convert raster_stack to data frame
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Remove NAs from select
raster_df <- na.omit(raster_df)

# remove raster_stack to save RAM
rm(raster_stack)
gc()

# Get the names of all columns starting from the 3rd column
columns_to_rename <- names(raster_df)[3:length(names(raster_df))]

# New column names
new_column_names <- c("artificial land", "bare land", "water", "grassland", "shrubland", "coniferous woodland", "broadleaved wodland")

# Rename the columns
names(raster_df)[3:length(names(raster_df))] <- new_column_names

### add year
# Calculate the number of years based on the number of columns and the assumption that each year occupies 7 columns (= 7 classes)
num_years <- (ncol(raster_df) - 2) %/% 7

# Generate the years based on the starting year (1986) and store them in a vector
starting_year <- 1986
years <- starting_year + seq_len(num_years) - 1

# Create a list to store the new column names with the corresponding years
new_column_names_with_year <- vector("list", num_years)

# Populate the list with the new column names including the years
for (i in seq_len(num_years)) {
  new_column_names_with_year[[i]] <- paste0(new_column_names, " ", years[i])
}

# Unlist the list to get the final set of column names with years
new_column_names_final <- unlist(new_column_names_with_year)


# Rename the columns in the raster_df
names(raster_df)[3:length(names(raster_df))] <- new_column_names_final

# Add a new column with unique IDs for each x value
raster_df <- raster_df %>%
  group_by(x, y) %>%
  mutate(ID = cur_group_id())


fcover_long <- pivot_longer(raster_df,
                            cols = 3:268,
                            names_to = "class",
                            values_to = "share")

rm(raster_df)
gc()

# Check the number of unique IDs
num_unique_ids <- length(unique(fcover_long$ID))

# Ensure there are at least 10,000 unique IDs
if (num_unique_ids < 10000) {
  stop("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
}

# Set seed for reproducibility
set.seed(123)

# Randomly sample 10,000 unique IDs
sampled_ids <- sample(unique(fcover_long$ID), 10000)

# Filter the data frame to include only the sampled IDs
fcover_subset <- fcover_long %>%
  filter(ID %in% sampled_ids)

rm(fcover_long)
gc()

# Extract the year information from the last part after the last empty space
fcover_subset$year <- sub(".*\\s(\\d+)$", "\\1", fcover_subset$class)

# Delete the year from the class column
fcover_subset$class <- sub("\\s+[^\\s]+$", "", fcover_subset$class)

# extract yod from disturbance map
fcover_subset$yod <- extract(disturbance_map, fcover_subset[, c("x", "y")])
gc()


# Function to clean 'share' values in a data frame
rescale <- function(table) {
  # Step 1: Replace negative 'share' values with 0
  table <- table %>%
    mutate(share = ifelse(share < 0, 0, share))
  
  # Step 2: Cap 'share' values at 10000
  table <- table %>%
    mutate(share = ifelse(share > 10000, 10000, share))
  
  return(table)
}

# Call the function and store the result in a new data frame
fcover_subset <- rescale(fcover_subset)

# just go with one observation per ID (for e.g. plotting)
fcover_subset_unique <- fcover_subset %>% distinct(ID, .keep_all = TRUE)

write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X32Y28.csv", row.names=FALSE)
write.csv(fcover_subset_unique, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X32Y28_unique.csv", row.names=FALSE)

rm(fcover_subset)
rm(fcover_subset_unique)
rm(new_column_names)
rm(columns_to_rename)
rm(new_column_names)
rm(new_column_names_final)
rm(new_column_names_with_year)
rm(raster_files)
rm(sampled_ids)
rm(starting_year)
rm(years)
gc()


#-------------------------------------------------------------------------------

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0032_Y0029")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)
gc()

# Convert raster_stack to data frame
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Remove NAs from select
raster_df <- na.omit(raster_df)

# remove raster_stack to save RAM
rm(raster_stack)
gc()

# Get the names of all columns starting from the 3rd column
columns_to_rename <- names(raster_df)[3:length(names(raster_df))]

# New column names
new_column_names <- c("artificial land", "bare land", "water", "grassland", "shrubland", "coniferous woodland", "broadleaved wodland")

# Rename the columns
names(raster_df)[3:length(names(raster_df))] <- new_column_names

### add year
# Calculate the number of years based on the number of columns and the assumption that each year occupies 7 columns (= 7 classes)
num_years <- (ncol(raster_df) - 2) %/% 7

# Generate the years based on the starting year (1986) and store them in a vector
starting_year <- 1986
years <- starting_year + seq_len(num_years) - 1

# Create a list to store the new column names with the corresponding years
new_column_names_with_year <- vector("list", num_years)

# Populate the list with the new column names including the years
for (i in seq_len(num_years)) {
  new_column_names_with_year[[i]] <- paste0(new_column_names, " ", years[i])
}

# Unlist the list to get the final set of column names with years
new_column_names_final <- unlist(new_column_names_with_year)


# Rename the columns in the raster_df
names(raster_df)[3:length(names(raster_df))] <- new_column_names_final

# Add a new column with unique IDs for each x value
raster_df <- raster_df %>%
  group_by(x, y) %>%
  mutate(ID = cur_group_id())


fcover_long <- pivot_longer(raster_df,
                            cols = 3:268,
                            names_to = "class",
                            values_to = "share")

rm(raster_df)
gc()

# Check the number of unique IDs
num_unique_ids <- length(unique(fcover_long$ID))

# Ensure there are at least 10,000 unique IDs
if (num_unique_ids < 10000) {
  stop("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
}

# Set seed for reproducibility
set.seed(123)

# Randomly sample 10,000 unique IDs
sampled_ids <- sample(unique(fcover_long$ID), 10000)

# Filter the data frame to include only the sampled IDs
fcover_subset <- fcover_long %>%
  filter(ID %in% sampled_ids)

rm(fcover_long)
gc()

# Extract the year information from the last part after the last empty space
fcover_subset$year <- sub(".*\\s(\\d+)$", "\\1", fcover_subset$class)

# Delete the year from the class column
fcover_subset$class <- sub("\\s+[^\\s]+$", "", fcover_subset$class)

# extract yod from disturbance map
fcover_subset$yod <- extract(disturbance_map, fcover_subset[, c("x", "y")])
gc()


# Function to clean 'share' values in a data frame
rescale <- function(table) {
  # Step 1: Replace negative 'share' values with 0
  table <- table %>%
    mutate(share = ifelse(share < 0, 0, share))
  
  # Step 2: Cap 'share' values at 10000
  table <- table %>%
    mutate(share = ifelse(share > 10000, 10000, share))
  
  return(table)
}

# Call the function and store the result in a new data frame
fcover_subset <- rescale(fcover_subset)

# just go with one observation per ID (for e.g. plotting)
fcover_subset_unique <- fcover_subset %>% distinct(ID, .keep_all = TRUE)

write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X32Y29.csv", row.names=FALSE)
write.csv(fcover_subset_unique, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X32Y29_unique.csv", row.names=FALSE)

rm(fcover_subset)
rm(fcover_subset_unique)
rm(new_column_names)
rm(columns_to_rename)
rm(new_column_names)
rm(new_column_names_final)
rm(new_column_names_with_year)
rm(raster_files)
rm(sampled_ids)
rm(starting_year)
rm(years)
gc()

#-------------------------------------------------------------------------------

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0033_Y0027")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)
gc()

# Convert raster_stack to data frame
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Remove NAs from select
raster_df <- na.omit(raster_df)

# remove raster_stack to save RAM
rm(raster_stack)
gc()

# Get the names of all columns starting from the 3rd column
columns_to_rename <- names(raster_df)[3:length(names(raster_df))]

# New column names
new_column_names <- c("artificial land", "bare land", "water", "grassland", "shrubland", "coniferous woodland", "broadleaved wodland")

# Rename the columns
names(raster_df)[3:length(names(raster_df))] <- new_column_names

### add year
# Calculate the number of years based on the number of columns and the assumption that each year occupies 7 columns (= 7 classes)
num_years <- (ncol(raster_df) - 2) %/% 7

# Generate the years based on the starting year (1986) and store them in a vector
starting_year <- 1986
years <- starting_year + seq_len(num_years) - 1

# Create a list to store the new column names with the corresponding years
new_column_names_with_year <- vector("list", num_years)

# Populate the list with the new column names including the years
for (i in seq_len(num_years)) {
  new_column_names_with_year[[i]] <- paste0(new_column_names, " ", years[i])
}

# Unlist the list to get the final set of column names with years
new_column_names_final <- unlist(new_column_names_with_year)


# Rename the columns in the raster_df
names(raster_df)[3:length(names(raster_df))] <- new_column_names_final

# Add a new column with unique IDs for each x value
raster_df <- raster_df %>%
  group_by(x, y) %>%
  mutate(ID = cur_group_id())


fcover_long <- pivot_longer(raster_df,
                            cols = 3:268,
                            names_to = "class",
                            values_to = "share")

rm(raster_df)
gc()

# Check the number of unique IDs
num_unique_ids <- length(unique(fcover_long$ID))

# Ensure there are at least 10,000 unique IDs
if (num_unique_ids < 10000) {
  stop("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
}

# Set seed for reproducibility
set.seed(123)

# Randomly sample 10,000 unique IDs
sampled_ids <- sample(unique(fcover_long$ID), 10000)

# Filter the data frame to include only the sampled IDs
fcover_subset <- fcover_long %>%
  filter(ID %in% sampled_ids)

rm(fcover_long)
gc()

# Extract the year information from the last part after the last empty space
fcover_subset$year <- sub(".*\\s(\\d+)$", "\\1", fcover_subset$class)

# Delete the year from the class column
fcover_subset$class <- sub("\\s+[^\\s]+$", "", fcover_subset$class)

# extract yod from disturbance map
fcover_subset$yod <- extract(disturbance_map, fcover_subset[, c("x", "y")])
gc()


# Function to clean 'share' values in a data frame
rescale <- function(table) {
  # Step 1: Replace negative 'share' values with 0
  table <- table %>%
    mutate(share = ifelse(share < 0, 0, share))
  
  # Step 2: Cap 'share' values at 10000
  table <- table %>%
    mutate(share = ifelse(share > 10000, 10000, share))
  
  return(table)
}

# Call the function and store the result in a new data frame
fcover_subset <- rescale(fcover_subset)

# just go with one observation per ID (for e.g. plotting)
fcover_subset_unique <- fcover_subset %>% distinct(ID, .keep_all = TRUE)

write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X33Y27.csv", row.names=FALSE)
write.csv(fcover_subset_unique, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X33Y27_unique.csv", row.names=FALSE)

rm(fcover_subset)
rm(fcover_subset_unique)
rm(new_column_names)
rm(columns_to_rename)
rm(new_column_names)
rm(new_column_names_final)
rm(new_column_names_with_year)
rm(raster_files)
rm(sampled_ids)
rm(starting_year)
rm(years)
gc()

#-------------------------------------------------------------------------------

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0033_Y0028")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)
gc()

# Convert raster_stack to data frame
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Remove NAs from select
raster_df <- na.omit(raster_df)

# remove raster_stack to save RAM
rm(raster_stack)
gc()

# Get the names of all columns starting from the 3rd column
columns_to_rename <- names(raster_df)[3:length(names(raster_df))]

# New column names
new_column_names <- c("artificial land", "bare land", "water", "grassland", "shrubland", "coniferous woodland", "broadleaved wodland")

# Rename the columns
names(raster_df)[3:length(names(raster_df))] <- new_column_names

### add year
# Calculate the number of years based on the number of columns and the assumption that each year occupies 7 columns (= 7 classes)
num_years <- (ncol(raster_df) - 2) %/% 7

# Generate the years based on the starting year (1986) and store them in a vector
starting_year <- 1986
years <- starting_year + seq_len(num_years) - 1

# Create a list to store the new column names with the corresponding years
new_column_names_with_year <- vector("list", num_years)

# Populate the list with the new column names including the years
for (i in seq_len(num_years)) {
  new_column_names_with_year[[i]] <- paste0(new_column_names, " ", years[i])
}

# Unlist the list to get the final set of column names with years
new_column_names_final <- unlist(new_column_names_with_year)


# Rename the columns in the raster_df
names(raster_df)[3:length(names(raster_df))] <- new_column_names_final

# Add a new column with unique IDs for each x value
raster_df <- raster_df %>%
  group_by(x, y) %>%
  mutate(ID = cur_group_id())


fcover_long <- pivot_longer(raster_df,
                            cols = 3:268,
                            names_to = "class",
                            values_to = "share")

rm(raster_df)
gc()

# Check the number of unique IDs
num_unique_ids <- length(unique(fcover_long$ID))

# Ensure there are at least 10,000 unique IDs
if (num_unique_ids < 10000) {
  stop("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
}

# Set seed for reproducibility
set.seed(123)

# Randomly sample 10,000 unique IDs
sampled_ids <- sample(unique(fcover_long$ID), 10000)

# Filter the data frame to include only the sampled IDs
fcover_subset <- fcover_long %>%
  filter(ID %in% sampled_ids)

rm(fcover_long)
gc()

# Extract the year information from the last part after the last empty space
fcover_subset$year <- sub(".*\\s(\\d+)$", "\\1", fcover_subset$class)

# Delete the year from the class column
fcover_subset$class <- sub("\\s+[^\\s]+$", "", fcover_subset$class)

# extract yod from disturbance map
fcover_subset$yod <- extract(disturbance_map, fcover_subset[, c("x", "y")])
gc()


# Function to clean 'share' values in a data frame
rescale <- function(table) {
  # Step 1: Replace negative 'share' values with 0
  table <- table %>%
    mutate(share = ifelse(share < 0, 0, share))
  
  # Step 2: Cap 'share' values at 10000
  table <- table %>%
    mutate(share = ifelse(share > 10000, 10000, share))
  
  return(table)
}

# Call the function and store the result in a new data frame
fcover_subset <- rescale(fcover_subset)

# just go with one observation per ID (for e.g. plotting)
fcover_subset_unique <- fcover_subset %>% distinct(ID, .keep_all = TRUE)

write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X33Y28.csv", row.names=FALSE)
write.csv(fcover_subset_unique, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X33Y28_unique.csv", row.names=FALSE)

rm(fcover_subset)
rm(fcover_subset_unique)
rm(new_column_names)
rm(columns_to_rename)
rm(new_column_names)
rm(new_column_names_final)
rm(new_column_names_with_year)
rm(raster_files)
rm(sampled_ids)
rm(starting_year)
rm(years)
gc()

#-------------------------------------------------------------------------------

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0033_Y0029")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)
gc()

# Convert raster_stack to data frame
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Remove NAs from select
raster_df <- na.omit(raster_df)

# remove raster_stack to save RAM
rm(raster_stack)
gc()

# Get the names of all columns starting from the 3rd column
columns_to_rename <- names(raster_df)[3:length(names(raster_df))]

# New column names
new_column_names <- c("artificial land", "bare land", "water", "grassland", "shrubland", "coniferous woodland", "broadleaved wodland")

# Rename the columns
names(raster_df)[3:length(names(raster_df))] <- new_column_names

### add year
# Calculate the number of years based on the number of columns and the assumption that each year occupies 7 columns (= 7 classes)
num_years <- (ncol(raster_df) - 2) %/% 7

# Generate the years based on the starting year (1986) and store them in a vector
starting_year <- 1986
years <- starting_year + seq_len(num_years) - 1

# Create a list to store the new column names with the corresponding years
new_column_names_with_year <- vector("list", num_years)

# Populate the list with the new column names including the years
for (i in seq_len(num_years)) {
  new_column_names_with_year[[i]] <- paste0(new_column_names, " ", years[i])
}

# Unlist the list to get the final set of column names with years
new_column_names_final <- unlist(new_column_names_with_year)


# Rename the columns in the raster_df
names(raster_df)[3:length(names(raster_df))] <- new_column_names_final

# Add a new column with unique IDs for each x value
raster_df <- raster_df %>%
  group_by(x, y) %>%
  mutate(ID = cur_group_id())


fcover_long <- pivot_longer(raster_df,
                            cols = 3:268,
                            names_to = "class",
                            values_to = "share")

rm(raster_df)
gc()

# Check the number of unique IDs
num_unique_ids <- length(unique(fcover_long$ID))

# Ensure there are at least 10,000 unique IDs
if (num_unique_ids < 10000) {
  stop("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
}

# Set seed for reproducibility
set.seed(123)

# Randomly sample 10,000 unique IDs
sampled_ids <- sample(unique(fcover_long$ID), 10000)

# Filter the data frame to include only the sampled IDs
fcover_subset <- fcover_long %>%
  filter(ID %in% sampled_ids)

rm(fcover_long)
gc()

# Extract the year information from the last part after the last empty space
fcover_subset$year <- sub(".*\\s(\\d+)$", "\\1", fcover_subset$class)

# Delete the year from the class column
fcover_subset$class <- sub("\\s+[^\\s]+$", "", fcover_subset$class)

# extract yod from disturbance map
fcover_subset$yod <- extract(disturbance_map, fcover_subset[, c("x", "y")])
gc()


# Function to clean 'share' values in a data frame
rescale <- function(table) {
  # Step 1: Replace negative 'share' values with 0
  table <- table %>%
    mutate(share = ifelse(share < 0, 0, share))
  
  # Step 2: Cap 'share' values at 10000
  table <- table %>%
    mutate(share = ifelse(share > 10000, 10000, share))
  
  return(table)
}

# Call the function and store the result in a new data frame
fcover_subset <- rescale(fcover_subset)

# just go with one observation per ID (for e.g. plotting)
fcover_subset_unique <- fcover_subset %>% distinct(ID, .keep_all = TRUE)

write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X33Y29.csv", row.names=FALSE)
write.csv(fcover_subset_unique, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X33Y29_unique.csv", row.names=FALSE)

rm(fcover_subset)
rm(fcover_subset_unique)
rm(new_column_names)
rm(columns_to_rename)
rm(new_column_names)
rm(new_column_names_final)
rm(new_column_names_with_year)
rm(raster_files)
rm(sampled_ids)
rm(starting_year)
rm(years)
gc()

#-------------------------------------------------------------------------------

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0034_Y0027")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)
gc()

# Convert raster_stack to data frame
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Remove NAs from select
raster_df <- na.omit(raster_df)

# remove raster_stack to save RAM
rm(raster_stack)
gc()

# Get the names of all columns starting from the 3rd column
columns_to_rename <- names(raster_df)[3:length(names(raster_df))]

# New column names
new_column_names <- c("artificial land", "bare land", "water", "grassland", "shrubland", "coniferous woodland", "broadleaved wodland")

# Rename the columns
names(raster_df)[3:length(names(raster_df))] <- new_column_names

### add year
# Calculate the number of years based on the number of columns and the assumption that each year occupies 7 columns (= 7 classes)
num_years <- (ncol(raster_df) - 2) %/% 7

# Generate the years based on the starting year (1986) and store them in a vector
starting_year <- 1986
years <- starting_year + seq_len(num_years) - 1

# Create a list to store the new column names with the corresponding years
new_column_names_with_year <- vector("list", num_years)

# Populate the list with the new column names including the years
for (i in seq_len(num_years)) {
  new_column_names_with_year[[i]] <- paste0(new_column_names, " ", years[i])
}

# Unlist the list to get the final set of column names with years
new_column_names_final <- unlist(new_column_names_with_year)


# Rename the columns in the raster_df
names(raster_df)[3:length(names(raster_df))] <- new_column_names_final

# Add a new column with unique IDs for each x value
raster_df <- raster_df %>%
  group_by(x, y) %>%
  mutate(ID = cur_group_id())


fcover_long <- pivot_longer(raster_df,
                            cols = 3:268,
                            names_to = "class",
                            values_to = "share")

rm(raster_df)
gc()

# Check the number of unique IDs
num_unique_ids <- length(unique(fcover_long$ID))

# Ensure there are at least 10,000 unique IDs
if (num_unique_ids < 10000) {
  stop("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
}

# Set seed for reproducibility
set.seed(123)

# Randomly sample 10,000 unique IDs
sampled_ids <- sample(unique(fcover_long$ID), 10000)

# Filter the data frame to include only the sampled IDs
fcover_subset <- fcover_long %>%
  filter(ID %in% sampled_ids)

rm(fcover_long)
gc()

# Extract the year information from the last part after the last empty space
fcover_subset$year <- sub(".*\\s(\\d+)$", "\\1", fcover_subset$class)

# Delete the year from the class column
fcover_subset$class <- sub("\\s+[^\\s]+$", "", fcover_subset$class)

# extract yod from disturbance map
fcover_subset$yod <- extract(disturbance_map, fcover_subset[, c("x", "y")])
gc()


# Function to clean 'share' values in a data frame
rescale <- function(table) {
  # Step 1: Replace negative 'share' values with 0
  table <- table %>%
    mutate(share = ifelse(share < 0, 0, share))
  
  # Step 2: Cap 'share' values at 10000
  table <- table %>%
    mutate(share = ifelse(share > 10000, 10000, share))
  
  return(table)
}

# Call the function and store the result in a new data frame
fcover_subset <- rescale(fcover_subset)

# just go with one observation per ID (for e.g. plotting)
fcover_subset_unique <- fcover_subset %>% distinct(ID, .keep_all = TRUE)

write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X34Y27.csv", row.names=FALSE)
write.csv(fcover_subset_unique, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X34Y27_unique.csv", row.names=FALSE)

rm(fcover_subset)
rm(fcover_subset_unique)
rm(new_column_names)
rm(columns_to_rename)
rm(new_column_names)
rm(new_column_names_final)
rm(new_column_names_with_year)
rm(raster_files)
rm(sampled_ids)
rm(starting_year)
rm(years)
gc()

#-------------------------------------------------------------------------------

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0034_Y0028")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)
gc()

# Convert raster_stack to data frame
raster_df <- as.data.frame(raster_stack, xy = TRUE)

# Remove NAs from select
raster_df <- na.omit(raster_df)

# remove raster_stack to save RAM
rm(raster_stack)
gc()

# Get the names of all columns starting from the 3rd column
columns_to_rename <- names(raster_df)[3:length(names(raster_df))]

# New column names
new_column_names <- c("artificial land", "bare land", "water", "grassland", "shrubland", "coniferous woodland", "broadleaved wodland")

# Rename the columns
names(raster_df)[3:length(names(raster_df))] <- new_column_names

### add year
# Calculate the number of years based on the number of columns and the assumption that each year occupies 7 columns (= 7 classes)
num_years <- (ncol(raster_df) - 2) %/% 7

# Generate the years based on the starting year (1986) and store them in a vector
starting_year <- 1986
years <- starting_year + seq_len(num_years) - 1

# Create a list to store the new column names with the corresponding years
new_column_names_with_year <- vector("list", num_years)

# Populate the list with the new column names including the years
for (i in seq_len(num_years)) {
  new_column_names_with_year[[i]] <- paste0(new_column_names, " ", years[i])
}

# Unlist the list to get the final set of column names with years
new_column_names_final <- unlist(new_column_names_with_year)


# Rename the columns in the raster_df
names(raster_df)[3:length(names(raster_df))] <- new_column_names_final

# Add a new column with unique IDs for each x value
raster_df <- raster_df %>%
  group_by(x, y) %>%
  mutate(ID = cur_group_id())


fcover_long <- pivot_longer(raster_df,
                            cols = 3:268,
                            names_to = "class",
                            values_to = "share")

rm(raster_df)
gc()

# Check the number of unique IDs
num_unique_ids <- length(unique(fcover_long$ID))

# Ensure there are at least 10,000 unique IDs
if (num_unique_ids < 10000) {
  stop("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
}

# Set seed for reproducibility
set.seed(123)

# Randomly sample 10,000 unique IDs
sampled_ids <- sample(unique(fcover_long$ID), 10000)

# Filter the data frame to include only the sampled IDs
fcover_subset <- fcover_long %>%
  filter(ID %in% sampled_ids)

rm(fcover_long)
gc()

# Extract the year information from the last part after the last empty space
fcover_subset$year <- sub(".*\\s(\\d+)$", "\\1", fcover_subset$class)

# Delete the year from the class column
fcover_subset$class <- sub("\\s+[^\\s]+$", "", fcover_subset$class)

# extract yod from disturbance map
fcover_subset$yod <- extract(disturbance_map, fcover_subset[, c("x", "y")])
gc()


# Function to clean 'share' values in a data frame
rescale <- function(table) {
  # Step 1: Replace negative 'share' values with 0
  table <- table %>%
    mutate(share = ifelse(share < 0, 0, share))
  
  # Step 2: Cap 'share' values at 10000
  table <- table %>%
    mutate(share = ifelse(share > 10000, 10000, share))
  
  return(table)
}

# Call the function and store the result in a new data frame
fcover_subset <- rescale(fcover_subset)

# just go with one observation per ID (for e.g. plotting)
fcover_subset_unique <- fcover_subset %>% distinct(ID, .keep_all = TRUE)

write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X34Y28.csv", row.names=FALSE)
write.csv(fcover_subset_unique, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X34Y28_unique.csv", row.names=FALSE)

rm(fcover_subset)
rm(fcover_subset_unique)
rm(new_column_names)
rm(columns_to_rename)
rm(new_column_names)
rm(new_column_names_final)
rm(new_column_names_with_year)
rm(raster_files)
rm(sampled_ids)
rm(starting_year)
rm(years)
gc()

#-------------------------------------------------------------------------------
