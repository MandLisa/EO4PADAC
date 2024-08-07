# Load required libraries
library(raster)
library(tidyr)
library(dplyr)
library(sp)
library(sf)
library(ggplot2)
library(terra)
library(spatial)
library(spatialEco)
library(readr)
library(spatstat)

###-----------------------------------------------------------------------------
# to do: align disturbance mask with fcover stack
# create an edge pixel mask
# re-compute delta t
# compute severity

# Set the working directory to where your raster files are located
setwd("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0029_Y0028")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
#print(raster_files)
raster_stack <- stack(raster_files)

#num_pixels <- ncell(raster_stack)/31
#print(num_pixels)

# import disturbance mask
disturbance_map <- raster("~/eo_nas/EO4Alps/gis/disturbances_alps/dist_aligned.tif")
edge_mask <- stack("~/eo_nas/EO4Alps/gis/disturbances_alps/disturbance_map_edge_core.tif")

# Select the second band (index 2)
#edge_mask <- edge_mask[[2]]

# Set 0 values to NA
#disturbance_map[disturbance_map == 0] <- NA

# Drop the first layer from the edge_mask

#plot(disturbance_map)
#plot(edge_mask)

#-------------------------------------------------------------------------------
# compute edge/core pixel mask
#-------------------------------------------------------------------------------

# Remove pixels with values less than 1987
disturbance_map[disturbance_map < 1986] <- NA

# Create a function to check if a given neighborhood is a core or edge pixel
check_core_edge <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  sum(x, na.rm = TRUE) == 9  # Check if there are 8 other pixels + the center pixel within the neighborhood
}

# Create a circular neighborhood matrix with a radius of 30 meters
radius <- 30  # Radius in meters
neigh <- focalWeight(disturbance_map, d = radius, type = "circle")

# Use the focal function to calculate the number of neighbors for each pixel
neighbor_count <- focal(disturbance_map, w = neigh, fun = check_core_edge, pad = TRUE)

# Assign 1 to core pixels and 0 to edge pixels
core_edge_pixels <- ifelse(neighbor_count == 1, 1, 0)

# Add the new band to the original raster
disturbance_map_with_band <- disturbance_map
disturbance_map_with_band[[2]] <- core_edge_pixels

# this give only core pixels, edge pixels were removed, this is why we do: 

# Function to add missing raster cells and fill with value 2
add_missing_cells <- function(raster1, raster2) {
  # Ensure both rasters have the same extent and resolution
  extent1 <- extent(raster1)
  extent2 <- extent(raster2)
  if (!identical(extent1, extent2)) {
    stop("Raster extents do not match.")
  }
  
  res1 <- res(raster1)
  res2 <- res(raster2)
  if (!identical(res1, res2)) {
    stop("Raster resolutions do not match.")
  }
  
  # Create a new raster with the extent and resolution of the input rasters
  output_raster <- raster(extent = extent1, resolution = res1)
  
  # Set the values of the output raster to 2
  values(output_raster) <- 2
  
  # Use 'mask' to keep the values from raster2 that overlap with raster1
  output_raster <- mask(output_raster, raster2)
  
  return(output_raster)
}


# Add missing cells from raster2 to raster1 and fill with value 2
edge_core <- add_missing_cells(disturbance_map_with_band, disturbance_map)

# Save the result with the new band to a new raster file (optional)
output_raster_file <- "~/eo_nas/EO4Alps/gis/disturbances_alps/edge_mask.tif"
writeRaster(disturbance_map_with_band, output_raster_file, overwrite = TRUE)



#-------------------------------------------------------------------------------
# Convert raster to df function
#-------------------------------------------------------------------------------

raster_to_df <- function(raster_stack) {
  # Convert raster_stack to data frame
  raster_df <- as.data.frame(raster_stack, xy = TRUE)
  
  # Remove NAs from select
  raster_df <- na.omit(raster_df)
  
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
  

  #column_names <- names(raster_df)
  #print(column_names)
  
  # there is somehow one row too much, remove it
  raster_df <- raster_df %>% select(-last_col())
  
  # Rename the columns in the raster_df
  names(raster_df)[3:length(names(raster_df))] <- new_column_names_final
  
  # Add a new column with unique IDs for each x value
  raster_df1 <- raster_df %>%
    group_by(x, y) %>%
    mutate(ID = cur_group_id())
  
  raster_df <- raster_df1
  
  return(raster_df)
}

# Call the function and store the result in a new data frame -> works!
fcover_df_X29Y28 <- raster_to_df(raster_stack)


#-------------------------------------------------------------------------------
### compute edge/core mask
#-------------------------------------------------------------------------------
# does the observation emerge from an edge pixel?
# this function needs some improvement -> slow!
radius = 31

check_core_pixels <- function(raster_df, radius) {
  # Function to compute distance between two points
  euclidean_distance <- function(x1, y1, x2, y2) {
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
  }
  
  # Create a new column to store the core pixel values
  raster_df$core_pixel <- 0
  
  # Loop through each observation
  for (i in seq(nrow(raster_df))) {
    # Extract the x and y coordinates of the current observation
    x_i <- raster_df$x[i]
    y_i <- raster_df$y[i]
    
    # Compute the number of surrounding observations within the specified radius
    num_surrounding <- sum(euclidean_distance(x_i, y_i, raster_df$x, raster_df$y) <= radius) - 1
    
    # Assign the core pixel value based on whether it's surrounded by exactly 4 observations
    # surrounded by 4 observations = core pixel (value 1), less than 4 surrounding
    # observations = edge pixel (value 0)
    raster_df$core_pixel[i] <- ifelse(num_surrounding == 4, 1, 0)
  }
  
  # Count the number of core pixels (surrounded observations)
  num_core_pixels <- sum(raster_df$core_pixel == 1)
  
  # Count the number of edge pixels (not surrounded observations)
  num_edge_pixels <- sum(raster_df$core_pixel == 0)
  
  # Print the results
  cat("Number of core pixels:", num_core_pixels, "\n")
  cat("Number of edge pixels:", num_edge_pixels, "\n")
  
  return(raster_df)
}

# run function
edge_core$core_pixel <- check_core_pixels(raster_df, radius)


#-------------------------------------------------------------------------------
# pivot data frame and extract year
#-------------------------------------------------------------------------------

pivot_df <- function(raster_df) {
  # Pivoting columns 3 to 268 (?) into 7 new columns (since there are 7 classes)
  fcover_long <- pivot_longer(raster_df,
                              cols = 3:268,
                              names_to = "class",
                              values_to = "share")
  
  # Extract the year information from the last part after the last empty space
  fcover_long$year <- sub(".*\\s(\\d+)$", "\\1", fcover_long$class)
  
  # Delete the year from the class column
  # Using sub() to remove everything after the last empty space
  fcover_long$class <- sub("\\s+[^\\s]+$", "", fcover_long$class)
  
  return(fcover_long)
}

# Call the function and store the result in a new data frame
fcover_long_X28Y28 <- pivot_df(raster_df)

#-------------------------------------------------------------------------------
# extract yod from the disturbance mask and edge/core pixel info from the other
# raster
#-------------------------------------------------------------------------------
extract_yod_edge <- function(disturbance_map, fcover_long, edge_mask) {
  # Extract disturbance_map values for fcover_long_X4Y3 coordinates and store in fcover_long_X4Y3$yod
  fcover_long$yod <- extract(disturbance_map, fcover_long[, c("x", "y")])
  
  # Extract edge_mask values for fcover_long_X4Y3 coordinates and store in fcover_long_X4Y3$edge
  fcover_long$edge <- extract(edge_mask, fcover_long[, c("x", "y")])
  
  # Replace 0 = core pixel with 1 = core pixel in fcover_long_X4Y3$edge
  fcover_long$edge[fcover_long$edge == 0] <- 1
  
  # Replace NA with 2 = edge pixel in fcover_long_X4Y3$edge
  fcover_long$edge[is.na(fcover_long$edge)] <- 2
  
  # Convert fcover_long_X4Y3$year from character to numeric
  fcover_long$year <- as.numeric(fcover_long$year)
  
  # Return the modified fcover_long_X4Y3 data frame
  return(fcover_long)
}

fcover_long_X4Y3 <- extract_yod_edge(disturbance_map, fcover_long_X4Y3, edge_mask)

str(fcover_long_X4Y3)

#-------------------------------------------------------------------------------
# random selection function
#-------------------------------------------------------------------------------

# number of observations selected
num_total_to_select <- 10000

sampled_data <- function(fcover_long, num_total_to_select) {
  # Step 1: Split the dataframe into two groups based on core_pixel value
  #zero_core_pixel_df <- fcover_long[fcover_long$edge == 1, ]
  #non_zero_core_pixel_df <- fcover_long[fcover_long$edge != 1, ]
  
  # Step 2: Calculate the number of observations needed for core_pixel == 1 (50% of total)
  num_zero_core_pixel_to_select <- round(0.5 * num_total_to_select)
  
  # Step 3: Sample the required number of observations from each group
  selected_zero_core_pixel <- sample(unique(zero_core_pixel_df$ID), num_zero_core_pixel_to_select, replace = FALSE)
  selected_non_zero_core_pixel <- sample(unique(non_zero_core_pixel_df$ID), num_total_to_select - num_zero_core_pixel_to_select, replace = FALSE)
  
  # Step 4: Combine the sampled observations from both groups
  selected_ids <- c(selected_zero_core_pixel, selected_non_zero_core_pixel)
  
  # (Optional) If you want the final selection to be in the original order of fcover_long$ID:
  selected_ids <- selected_ids[order(match(selected_ids, fcover_long$ID))]
  
  # Step 5: Extract the selected IDs into a new dataframe
  subset <- fcover_long %>%
    filter(ID %in% selected_ids)
  
  return(subset)
}

# Call the function and store the result in a new data frame
subset_X4Y3 <- sampled_data(fcover_long_X4Y3, num_total_to_select)


### wihtout the core/edge thing
# Randomly sample 10,000 unique IDs
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

# rescale and clean data
#-------------------------------------------------------------------------------

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


write.csv(fcover_subset, "~/eo_nas/EO4Alps/00_analysis/_data/fcover_X28Y29.csv", row.names=FALSE)

#-------------------------------------------------------------------------------

# Remove non-finite values
fcover_subset1 <- fcover_X28Y28 %>%
  filter(is.finite(yod))


# plot distribution of year --> looks good!
yod <- ggplot(fcover_subset1, aes(x = yod)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  scale_x_continuous(breaks = seq(1986, 2023, by = 2)) +
  labs(title = "Histogram of yod", x = "yod", y = "Frequency")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/yod_sappada.png", units="in", width=10, height=6, res=300)
yod
dev.off()

#-------------------------------------------------------------------------------
# function to compute how long it takes until min tree cover is reached after
# disturbance
#-------------------------------------------------------------------------------

# compute min value of tree share after 5 years after disturbance
# Step 1: Filter the data for class == "trees"
df_trees <- fcover_subset1 %>%
  filter(class == "trees")

time_to_min <- function(df_trees) {
  # Find the minimum tree share value and the year when the minimum is reached within the 5-year window after the year of disturbance for each time series (ID)
  min_share_in_window <- df_trees %>%
    group_by(ID, yod) %>%
    filter(year >= yod, year <= yod + 5) %>%
    slice(which.min(share)) %>%
    ungroup()
  
  # Calculate the duration until the minimum is reached for each time series (ID)
  result <- min_share_in_window %>%
    group_by(ID) %>%
    mutate(time_to_min = year - yod) %>%
    ungroup() %>%
    mutate(across(everything(), ~ if (is.numeric(.)) . else first(.))) 
  
  # Keep only one row per time series (ID) with the minimum tree share
  result <- result %>%
    group_by(ID) %>%
    slice(1) %>%
    ungroup()
  
  # Select the necessary columns and remove duplicates to keep only one row per time series (ID)
  result <- result %>%
    select(ID, x, y, yod, min_year = year, time_to_min, min_tree_share = share) %>%
    distinct()
  
  # Step 3: Calculate the duration until the minimum is reached for each time series (ID)
  result <- min_share_in_window %>%
    group_by(ID) %>%
    mutate(time_to_min = year - yod) %>%
    ungroup() %>%
    mutate(across(everything(), ~ if (is.numeric(.)) . else first(.))) 
  
  result$share <- result$share/100
  result$min_year <- result$year
  result$year <- NULL
  result$min_tree_cover <- result$share
  result$share <- NULL
  result$core_pixel <- ifelse(result$edge == 1, "core pixel", "edge pixel")
  
  return(result)
}

# Call the function and store the result in a new data frame
t_min <- time_to_min(df_trees)


### works!
ggplot(t_min, aes(x = time_to_min)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  #geom_density() +
  scale_x_continuous(breaks = c(0,1,2,3,4,5)) +
  labs(title = "Time until min tree cover is reached", x = "years after disturbance", y = "Frequency")


png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/min_sappada.png", units="in", width=10, height=6, res=300)
min
dev.off()


# plot density
ggplot(t_min, aes(x=min_tree_cover, group=core_pixel, fill=core_pixel)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60), limits = c(0, 60)) 

# write time_to_min
write.csv(t_min, "/mnt/public/Projects/DataCube/projects/foreco/alps/level3_predictions/v3_dist/_csv/t_min_X4Y3_n.csv", row.names=FALSE)

#-------------------------------------------------------------------------------
# severity function
#-------------------------------------------------------------------------------
df_trees$share <- df_trees$share/100

calculate_severity <- function(df_trees) {
  # Calculate the average tree share in a 5-year time window before the disturbance event for each time series (ID)
  avg_tree_share_5y_before <- df_trees %>%
    group_by(ID) %>%
    arrange(year) %>%
    mutate(tree_share_before = mean(share[year >= (yod - 5) & year < yod], na.rm = TRUE))
  
  #avg_tree_share_5y_before$tree_share_before <- avg_tree_share_5y_before$tree_share_before/100
  
  # Merge the "result" dataframe with the average tree share in a 5-year time window before the disturbance
  result_with_avg <- left_join(t_min, avg_tree_share_5y_before, by = c("x", "y"))
  
  # Calculate the severity of the disturbance as absolute numbers from the average tree share before
  severity <- result_with_avg %>%
    mutate(severity = tree_share_before - min_tree_cover) %>%
    select(ID.x, x, y, yod.x, min_year = min_year, time_to_min, min_tree_cover, tree_share_before, severity, core_pixel)
  
  # rename column headers
  names(severity)[names(severity) == "ID.x"] <- "ID"
  names(severity)[names(severity) == "yod.x"] <- "yod"
  
  # Step 5: Calculate the severity of the disturbance as a percentage from the average tree share before
  #severity <- result_with_avg %>%
    #mutate(severity = ((tree_share_before - min_tree_cover) / tree_share_before) * 100) %>%
    #select(ID.x, x, y, yod.x, min_year = min_year, time_to_min, min_tree_cover, tree_share_before, severity, core_pixel)
  
  return(severity)
}

# Call the function and store the result in a new data frame
severity <- calculate_severity(df_trees)

### look better
ggplot(severity, aes(x=severity, group=core_pixel,fill=core_pixel)) + 
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 80, 90, 100), limits = c(0, 100))

# just go with one obserbation per ID (=time series)
severity_unique <- severity %>%
  distinct(ID, .keep_all = TRUE)

# smoothed
ggplot(severity_unique, aes(x=severity, group=core_pixel,fill=core_pixel)) + 
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 80, 90, 100), limits = c(0, 100)) 


# write severtiy
write.csv(severity, "/mnt/public/Projects/DataCube/projects/foreco/alps/level3_predictions/v3_dist/_csv/severity_all_X4Y3_n.csv", row.names=FALSE)

# write severtiy
write.csv(severity_unique, "/mnt/public/Projects/DataCube/projects/foreco/alps/level3_predictions/v3_dist/_csv/severity_unique_X4Y3_n.csv", row.names=FALSE)

#-------------------------------------------------------------------------------
# compute recovery rates
#-------------------------------------------------------------------------------
#df_trees$share <- df_trees$share/100

# Step 1: Compute average tree share for the last 5 years before yod
recovery <- df_trees %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(tree_share_before = max(share[year >= (yod - 5) & year < yod], na.rm = TRUE))

# Step 2: Compute 80% of the tree_share_before for each time series
recovery <- recovery %>%
  mutate(tree_share_80 = tree_share_before * 0.8)

# Step 3: Find the year when share exceeds tree_share_80 for the first time after yod
recovery <- recovery %>%
  group_by(ID) %>%
  mutate(
    reached_80 = cumsum(if_else(year > (yod + 3) & share >= tree_share_80, 1, 0)),
    year_recov = case_when(any(reached_80 > 0) ~ year[reached_80 == 1][1],
                           TRUE ~ NA_integer_)
  ) %>%
  ungroup() %>%
  select(-reached_80)

# Step 4: Compute the recovery_rate
recovery <- recovery %>%
  mutate(
    recovery_rate = if_else(!is.na(year_recov), year_recov - yod, 100)
  )

recovery <- recovery %>%
  mutate(edge = ifelse(edge == 1, "core_pixel", "edge_pixel"))

# check
ggplot(recovery, aes(x=recovery_rate, group=edge,fill=edge)) + 
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(2, 4,6,8,10,12,14,16), limits = c(2, 18)) 

# write recovery
write.csv(recovery, "/mnt/public/Projects/DataCube/projects/foreco/alps/level3_predictions/v3_dist/_csv/recovery80all_X4Y3_n.csv", row.names=FALSE)

# just go with one obserbation per ID (=time series)
recovery_unique <- recovery %>%
  distinct(ID, .keep_all = TRUE)

# write severtiy
write.csv(recovery_unique, "/mnt/public/Projects/DataCube/projects/foreco/alps/level3_predictions/v3_dist/_csv/recovery80_unique_X4Y3_n.csv", row.names=FALSE)


# check --> look better!
ggplot(recovery_unique, aes(x=recovery_rate, group=edge,fill=edge)) + 
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20), limits = c(0, 20))


#-------------------------------------------------------------------------------
# all plots together:
#-------------------------------------------------------------------------------
# all plots:
# when did the disturbance happen?
dist_year <- ggplot(t_min, aes(x = yod)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Histogram of yod", x = "yod", y = "Frequency")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/yod_sappada.png", units="in", width=5, height=2.5, res=300)
dist_year
dev.off()

# min tree cover reached
min_tre_cov <- ggplot(t_min, aes(x = time_to_min)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  #geom_density() +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  labs(title = "Time until min tree cover is reached", x = "years after disturbance", y = "Density")

# as geom_density
min_tree_cov <- ggplot(t_min, aes(x=time_to_min, group=core_pixel, fill=core_pixel)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  labs(title = "Time until min tree cover is reached", x = "years after disturbance", y = "Density")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/min_tree_cov_alps.png", units="in", width=5, height=2.5, res=300)
min_tree_cov
dev.off()

#avg min tree cover after disturbance
min_tree_cov_after <- ggplot(severity, aes(x=min_tree_cover, group=core_pixel, fill=core_pixel)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = seq(0, 20, by = 2)) +
  labs(title = "Min tree cover [%] in a 5-year time window after disturbance", 
       x = "tree cover [%]", y = "Density")

min_tree_cov_before <- ggplot(severity, aes(x=min_tree_cover, group=core_pixel, fill=core_pixel)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  labs(title = "Average tree cover 5 years before disturbance", x = "tree cover [%]", y = "Density")


png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/avg_tree_cov_after_sappada.png", units="in", width=5, height=2.5, res=300)
min_tree_cov_after
dev.off()

#avg min tree cover before disturbance
avg_tree_cov_before <- ggplot(severity, aes(x=tree_share_before, group=core_pixel, fill=core_pixel)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(title = "Average tree cover 5 years before disturbance", x = "tree cover [%]", y = "Density")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/avg_tree_cov_before_sappada.png", units="in", width=5, height=2.5, res=300)
avg_tree_cov_before
dev.off()

# severity
severity <- ggplot(severity, aes(x=severity, group=core_pixel,fill=core_pixel)) + 
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 80, 90, 100), limits = c(0, 100)) +
  labs(title = "Severity", x = "severity (impact of the disturbance)", y = "Density")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/severity.png", units="in", width=5, height=2.5, res=300)
severity
dev.off()


#-------------------------------------------------------------------------------
# merge alldataframes and create plots
#-------------------------------------------------------------------------------

setwd("/mnt/public/Projects/DataCube/projects/foreco/alps/level3_predictions/v3_dist/_csv")

# Step 2: List CSV files with "severity" in their file name
csv_files <- list.files(pattern = "severity_all", ignore.case = TRUE)

# Step 3: Read the CSV files into a list
csv_data_list <- list()

for (file in csv_files) {
  csv_data_list[[file]] <- read.csv(file, header = TRUE)
}

#-------------------------------------------------------------------------------
# Access the data frame from the first CSV file
X4Y3 <- csv_data_list[[csv_files[1]]]

X2Y2 <- csv_data_list[[csv_files[2]]]

X4Y3 <- csv_data_list[[csv_files[3]]]

X3Y2 <- csv_data_list[[csv_files[4]]]

X4Y0 <- csv_data_list[[csv_files[5]]]

X4Y3 <- csv_data_list[[csv_files[6]]]

X4Y3 <- csv_data_list[[csv_files[7]]]

X5Y0 <- csv_data_list[[csv_files[8]]]

X4Y3 <- csv_data_list[[csv_files[9]]]

X4Y3 <- csv_data_list[[csv_files[10]]]

X6Y0 <- csv_data_list[[csv_files[11]]]

X4Y3 <- csv_data_list[[csv_files[12]]]

X4Y3 <- csv_data_list[[csv_files[13]]]

X7Y0 <- csv_data_list[[csv_files[14]]]

X4Y3 <- csv_data_list[[csv_files[15]]]

#-------------------------------------------------------------------------------

# Combine all data frames into a single data frame using bind_rows()
severity_combined <- bind_rows(csv_data_list)

severity_combined_unique <- severity_combined %>%
  distinct(ID, .keep_all = TRUE)

# all plots:
# when did the disturbance happen?
dist_year <- ggplot(severity_combined_unique, aes(x = yod)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3)) +
  labs(title = "Histogram of yod", x = "yod", y = "Frequency")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/yod_alps.png", units="in", width=5, height=2.5, res=300)
dist_year
dev.off()

# min tree cover reached
min_tre_cov <- ggplot(severity_combined_unique, aes(x = time_to_min, group = core_pixel, fill = core_pixel)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  #geom_density() +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  labs(title = "Time until min tree cover is reached", x = "years after disturbance", y = "Density")


min_tree_cov <- ggplot(severity_combined, aes(x = time_to_min, fill = core_pixel)) + 
  geom_bar(alpha=.7) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5)) +
  labs(title = "Time until min tree cover is reached", x = "years after disturbance", y = "Frequency")



# as geom_density
min_tree_cov <- ggplot(severity_combined_unique, aes(x=time_to_min, group=core_pixel, fill=core_pixel)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  labs(title = "Time until min tree cover is reached", x = "years after disturbance", y = "Density")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/min_tree_cov_alps.png", units="in", width=5, height=2.5, res=300)
min_tree_cov
dev.off()

#avg min tree cover after disturbance
min_tree_cov_after <- ggplot(severity_combined_unique, aes(x=min_tree_cover, group=core_pixel, fill=core_pixel)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(0,10,20,30,40,50,60), limits = c(0, 60)) +
  labs(title = "Min tree cover in a 5-year time window after disturbance", x = "tree cover [%]", y = "Density")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/avg_tree_cov_after_alps.png", units="in", width=5, height=2.5, res=300)
min_tree_cov_after
dev.off()

#avg min tree cover before disturbance
avg_tree_cov_before <- ggplot(severity_combined_unique, aes(x=tree_share_before, group=core_pixel, fill=core_pixel)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(title = "Average tree cover 5 years before disturbance", x = "tree cover [%]", y = "Density")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/avg_tree_cov_before_alps.png", units="in", width=5, height=2.5, res=300)
avg_tree_cov_before
dev.off()

# severity
severity <- ggplot(severity_combined, aes(x=severity, group=core_pixel,fill=core_pixel)) + 
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 80, 90, 100), limits = c(0, 100)) +
  labs(title = "Severity", x = "severity (impact of the disturbance)", y = "Density")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/severity_alps.png", units="in", width=5, height=2.5, res=300)
severity
dev.off()

# recovery
# Step 2: List CSV files with "severity" in their file name
csv_files <- list.files(pattern = "recovery80all", ignore.case = TRUE)

# Step 3: Read the CSV files into a list
csv_data_list <- list()

for (file in csv_files) {
  csv_data_list[[file]] <- read.csv(file, header = TRUE)
}

#-------------------------------------------------------------------------------
# Access the data frame from the first CSV file
X4Y3 <- csv_data_list[[csv_files[1]]]

X2Y2 <- csv_data_list[[csv_files[2]]]

X4Y3 <- csv_data_list[[csv_files[3]]]

X3Y2 <- csv_data_list[[csv_files[4]]]

X4Y0 <- csv_data_list[[csv_files[5]]]

X4Y3 <- csv_data_list[[csv_files[6]]]

X4Y3 <- csv_data_list[[csv_files[7]]]

X5Y0 <- csv_data_list[[csv_files[8]]]

X4Y3 <- csv_data_list[[csv_files[9]]]

X4Y3 <- csv_data_list[[csv_files[10]]]

X6Y0 <- csv_data_list[[csv_files[11]]]

X4Y3 <- csv_data_list[[csv_files[12]]]

X4Y3 <- csv_data_list[[csv_files[13]]]

X7Y0 <- csv_data_list[[csv_files[14]]]

X4Y3 <- csv_data_list[[csv_files[15]]]



#-------------------------------------------------------------------------------

# Combine all data frames into a single data frame using bind_rows()
recovery_combined <- bind_rows(csv_data_list)

recovery_combined_unique <- recovery_combined %>%
  distinct(ID, .keep_all = TRUE)


recovery80 <- ggplot(recovery_combined_unique, aes(x=recovery_rate, group=edge,fill=edge)) + 
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(0,2,4,6,8,10,12,14,16,18,20), limits = c(0, 20)) +
  labs(title = "Baseline-normalized recovery", x = "recovery rate [years]", y = "Density")


png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/recovery80_alps.png", units="in", width=5, height=2.5, res=300)
recovery80
dev.off()

# write severtiy
write.csv(recovery_combined_unique, "/mnt/public/Projects/DataCube/projects/foreco/alps/level3_predictions/v3_dist/_csv/recovery80_combined_unique.csv", row.names=FALSE)


#-------------------------------------------------------------------------------
# GAM fitting
#-------------------------------------------------------------------------------

# Subset the data for class == "trees" and year > yod
trees_data <- subset(table, class == "trees" & year > yod)

# Group the data by ID
grouped_trees_data <- split(trees_data, trees_data$ID)

# Create a function to fit GAM and extract smoothed time series
fit_gam <- function(data) {
  if (nrow(data) >= 4) {
    # Sufficient data points, fit GAM model with reduced spline complexity
    # does not work with higher complexity, e.g., k=5
    # We are modeling the variable 'share' as a function of the term s(year - yod, k = 3). 
    # The s() function represents a smooth term, and in this case, it's used to 
    # model the effect of the difference between 'year' and 'yod'. The k = 3 
    # argument specifies the complexity of the spline used to model the smooth term.
    gam_model <- gam(share ~ s(year - yod, k = 3), data = data)
  } else {
    # Insufficient data points, use a linear model
    lm_model <- lm(share ~ year - yod, data = data)
  }
  
  # Create a prediction dataframe for the smoothed time series
  prediction_data <- data.frame(year = seq(min(data$year) + 1, max(data$year)))
  
  if (exists("gam_model")) {
    # Predict smoothed time series using GAM model
    prediction_data$smoothed_share <- predict(gam_model, newdata = prediction_data)
  } else if (exists("lm_model")) {
    # Predict smoothed time series using linear model
    prediction_data$smoothed_share <- predict(lm_model, newdata = prediction_data)
  }
  
  # Add the additional columns from the original data
  additional_cols <- setdiff(names(data), names(prediction_data))
  prediction_data <- cbind(prediction_data, data[1, additional_cols])
  
  return(prediction_data)
}


# Apply the function to each group
tree_cover_gam_list <- lapply(grouped_trees_data, fit_gam)



# Combine the results into a single dataframe
smoothed_time_series_df <- do.call(rbind, smoothed_time_series_list)

#smoothed_time_series_df$share <- smoothed_time_series_df$share/100


# Merge the smoothed_time_series_df with df based on matching x, y, and ID
df_smoothed <- merge(df, smoothed_time_series_df, by = c("x", "y", "ID"), all.x = TRUE)

# Create the share_smoothed column with smoothed values where available, and original values otherwise
df_smoothed$share_smoothed <- ifelse(!is.na(df_smoothed$smoothed_share), df_smoothed$smoothed_share, df_smoothed$share)

# Remove the intermediate columns if needed
df_smoothed <- subset(df_smoothed, select = -c(smoothed_share))




# Print the smoothed time series for each ID
print(smoothed_time_series_df)

table_GAM <- smoothed_time_series_df

write.csv(smoothed_time_series_df, "F:/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/_data/tree_cover_GAM.csv", row.names=FALSE)
















