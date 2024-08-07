# Load required libraries
library(raster)
library(tidyr)
library(dplyr)
library(sp)
library(sf)
library(ggplot2)
library(imager)
library(terra)
library(spatial)
library(rgeos)
library(spatialEco)
library(readr)
library(spatstat)

###-----------------------------------------------------------------------------
# to do: align disturbance mask with fcover stack
# create an edge pixel mask
# re-compute delta t
# compute severity

# Set the working directory to where your raster files are located
setwd("/mnt/public/Projects/DataCube/projects/foreco/alps/level3_predictions/v3_dist/mosaics")

# Task 1: Read in 31 raster files as a raster stack
raster_files <- list.files(pattern = "\\.tif$")  # Assuming your raster files have a .tif extension
print(raster_files)
raster_stack <- stack(raster_files)

num_pixels <- ncell(raster_stack)/31
print(num_pixels)

# import disturbance mask
#disturbance_map <- raster("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/_data/disturbance_map_final_LAEA_clip.tif")
disturbance_map <- raster("/mnt/public/Projects/DataCube/projects/foreco/alps/gis/dist_aligned_2807_1.tif")
edge_mask <- stack("/mnt/public/Projects/DataCube/projects/foreco/alps/gis/disturbance_map_edge_core.tif")

#-------------------------------------------------------------------------------
# randomly select 100,000 pixel
#-------------------------------------------------------------------------------
# Get the number of pixels in the raster stack
num_pixels <- ncell(raster_stack)

# Set the number of pixels you want to randomly select
num_to_select <- 50000

# Create a random sample of unique cell indices
sample_indices <- sample(1:num_pixels, num_to_select, replace = FALSE)

# Get the spatial coordinates (x, y) of the selected pixels
random_pixel_locations <- xyFromCell(raster_stack, sample_indices)

# Create a new raster stack with the selected pixels
subset_raster_stack <- raster_stack
subset_raster_stack[] <- NA  # Set all values in the new raster stack to NA

# Assign the selected pixel values to the new raster stack
for (i in 1:num_to_select) {
  subset_raster_stack[[i]] <- raster_stack[[sample_indices[i]]]
}

#-------------------------------------------------------------------------------
# compute edge/core pixel mask
#-------------------------------------------------------------------------------

# Remove pixels with values less than 1987
disturbance_map[disturbance_map < 1987] <- NA

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

#add disturbance map with band
disturbance_map_with_band <- raster("/mnt/public/Projects/DataCube/projects/foreco/alps/gis/disturbance_map_edge_core.tif")

# Add missing cells from raster2 to raster1 and fill with value 2
edge_core <- add_missing_cells(disturbance_map_with_band, disturbance_map)



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
  new_column_names <- c("grassland/shrub", "trees", "bare land")
  
  # Rename the columns
  names(raster_df)[3:length(names(raster_df))] <- new_column_names
  
  ### add year
  # Calculate the number of years based on the number of columns and the assumption that each year occupies 3 columns
  num_years <- (ncol(raster_df) - 2) %/% 3
  
  # Generate the years based on the starting year (1990) and store them in a vector
  starting_year <- 1990
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
  
  return(raster_df)
}


# Call the function and store the result in a new data frame -> works!
fcover_df <- raster_to_df(random_pixels)


#-------------------------------------------------------------------------------
# edge/core pixel function
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
fcover_df$core_pixel <- check_core_pixels(raster_df, radius)

#-------------------------------------------------------------------------------
# pivot data frame and extract year
#-------------------------------------------------------------------------------

pivot_df <- function(raster_df) {
  # Pivoting columns 3 to 98 into three new columns
  fcover_long <- pivot_longer(raster_df,
                              cols = 3:98,
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
fcover_long <- pivot_df(fcover_df)

#-------------------------------------------------------------------------------
# extract yod from the disturbance mask and edge/core pixel info from the other
# raster
#-------------------------------------------------------------------------------
fcover_long$yod <- extract(disturbance_map, fcover_long[, c("x", "y")])

edge_mask <- dropLayer(edge_mask, 1)

fcover_long$edge <- extract(edge_mask, fcover_long[, c("x", "y")])

# Replace 0 = core pixel with 1 = core pixel
fcover_long$edge[fcover_long$edge == 0] <- 1

# Replace NA with 2 = edge pixel
fcover_long$edge[is.na(fcover_long$edge)] <- 2

# convert fcover_long$year from character to numeric
fcover_long$year <- as.numeric(fcover_long$year)

print(fcover_long$edge)
#-------------------------------------------------------------------------------
# random selection function
#-------------------------------------------------------------------------------

# number of observations selected
num_total_to_select <- 10000

sampled_data <- function(fcover_long, num_total_to_select) {
  # Step 1: Split the dataframe into two groups based on core_pixel value
  zero_core_pixel_df <- fcover_long[fcover_long$edge == 1, ]
  non_zero_core_pixel_df <- fcover_long[fcover_long$edge != 1, ]
  
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
subset <- sampled_data(fcover_long, num_total_to_select)

#write.csv(selected_dataframe, "/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/_data/fcover_10000_stratified.csv", row.names=FALSE)

#-------------------------------------------------------------------------------

# plot distribution of year --> looks good!
yod <- ggplot(subset, aes(x = yod)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Histogram of yod", x = "yod", y = "Frequency")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/yod_sappada.png", units="in", width=10, height=6, res=300)
yod
dev.off()

#-------------------------------------------------------------------------------
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
subset <- rescale(subset)


#-------------------------------------------------------------------------------
# function to compute how long it takes until min tree cover is reached after
# disturbance
#-------------------------------------------------------------------------------

# compute min value of tree share after 5 years after disturbance
# Step 1: Filter the data for class == "trees"
df_trees <- subset %>%
  filter(class == "trees")

df_trees$year <- as.numeric(df_trees$year)

time_to_min <- function(df_trees) {
  # Step 2: Find the minimum tree share value and the year when the minimum is reached within the 5-year window after the year of disturbance for each time series (ID)
  min_share_in_window <- df_trees %>%
    group_by(ID, yod) %>%
    filter(year >= yod, year <= yod + 5) %>%
    slice(which.min(share)) %>%
    ungroup()
  
  # Step 3: Calculate the duration until the minimum is reached for each time series (ID)
  result <- min_share_in_window %>%
    group_by(ID) %>%
    mutate(time_to_min = year - yod) %>%
    ungroup() %>%
    mutate(across(everything(), ~ if (is.numeric(.)) . else first(.))) 
  
  # Step 4: Keep only one row per time series (ID) with the minimum tree share
  result <- result %>%
    group_by(ID) %>%
    slice(1) %>%
    ungroup()
  
  # Step 5: Select the necessary columns and remove duplicates to keep only one row per time series (ID)
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
  result$core_pixel <- ifelse(result$edge == 1, "edge pixel", "core pixel")
  
  return(result)
}

# Call the function and store the result in a new data frame
time_to_min <- time_to_min(df_trees)


### works!
min <- ggplot(time_to_min, aes(x = time_to_min)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  #geom_density() +
  scale_x_continuous(breaks = c(0,1,2,3,4,5)) +
  labs(title = "Time until min tree cover is reached", x = "years after disturbance", y = "Frequency")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/min_sappada.png", units="in", width=10, height=6, res=300)
min
dev.off()


# plot density
ggplot(time_to_min, aes(x=min_tree_cover, group=core_pixel, fill=core_pixel)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 80, 90, 100)) 


#-------------------------------------------------------------------------------
# severity function
#-------------------------------------------------------------------------------

calculate_severity <- function(df_trees) {
  # Step 2: Calculate the average tree share in a 5-year time window before the disturbance event for each time series (ID)
  avg_tree_share_5y_before <- df_trees %>%
    group_by(ID, yod) %>%
    filter(year >= yod - 10 & year < yod) %>%
    summarise(avg_tree_share_before = max(share))
  
  # Step 3: Merge the "result" dataframe with the average tree share in a 5-year time window before the disturbance
  result_with_avg <- left_join(time_to_min, avg_tree_share_5y_before, by = c("ID", "yod"))
  result_with_avg$avg_tree_share_before <- result_with_avg$avg_tree_share_before/100
  
  # Step 4: Calculate the severity of the disturbance as absolute numbers from the average tree share before
  severity <- result_with_avg %>%
    mutate(severity = avg_tree_share_before - min_tree_cover) %>%
    select(ID, x, y, yod, min_year = min_year, time_to_min, min_tree_cover, avg_tree_share_before, severity, core_pixel)
  
  # Step 5: Calculate the severity of the disturbance as a percentage from the average tree share before
  severity <- result_with_avg %>%
    mutate(severity = ((avg_tree_share_before - min_tree_cover) / avg_tree_share_before) * 100) %>%
    select(ID, x, y, yod, min_year = min_year, time_to_min, min_tree_cover, avg_tree_share_before, severity, core_pixel)
  
  return(severity)
}

# Call the function and store the result in a new data frame
severity <- calculate_severity(df_trees)


### final severity plot
severity <- ggplot(severity, aes(x=severity, group=core_pixel,fill=core_pixel)) + 
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(-200, -100, 0, 10, 20, 30, 40, 50, 60, 70, 80, 80, 90, 100), limits = c(-200, 100)) 

#-------------------------------------------------------------------------------
# all plots together:
#-------------------------------------------------------------------------------
# all plots:
# when did the disturbance happen?
dist_year <- ggplot(table, aes(x = yod)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Histogram of yod", x = "yod", y = "Frequency")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/yod_sappada.png", units="in", width=5, height=2.5, res=300)
dist_year
dev.off()

# min tree cover reached
min_tre_cov <- ggplot(result, aes(x = time_to_min)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  #geom_density() +
  scale_x_continuous(breaks = c(0,1,2,3,4,5)) +
  labs(title = "Time until min tree cover is reached", x = "years after disturbance", y = "Density")

# as geom_density
min_tree_cov <- ggplot(result, aes(x=time_to_min, group=core_pixel, fill=core_pixel)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5)) +
  labs(title = "Time until min tree cover is reached", x = "years after disturbance", y = "Density")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/min_tree_cov.png", units="in", width=5, height=2.5, res=300)
min_tree_cov
dev.off()

#avg min tree cover after disturbance
min_tree_cov_after <- ggplot(result, aes(x=min_tree_cov, group=core_pixel, fill=core_pixel)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 80, 90, 100)) +
  labs(title = "Min tree cover [%] in a 5-year time window after disturbance", x = "tree cover [%]", y = "Density")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/avg_tree_cov_after_sappada.png", units="in", width=5, height=2.5, res=300)
min_tree_cov_after
dev.off()

#avg min tree cover before disturbance
avg_tree_cov_before <- ggplot(result_with_avg, aes(x=avg_tree_share_before, group=core_pixel, fill=core_pixel)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 80, 90, 100)) +
  labs(title = "Average tree cover 5 years before disturbance", x = "tree cover [%]", y = "Density")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/avg_tree_cov_before_sappada.png", units="in", width=5, height=2.5, res=300)
avg_tree_cov_before
dev.off()

# severity
severity <- ggplot(result_with_severity, aes(x=severity, group=core_pixel,fill=core_pixel)) + 
  geom_density(adjust=1.5, alpha=.4) +
  scale_x_continuous(breaks = c(-200, -100, 0, 10, 20, 30, 40, 50, 60, 70, 80, 80, 90, 100), limits = c(-100, 100)) +
  labs(title = "Severity", x = "severity (impact of the disturbance)", y = "Density")

png("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/severity.png", units="in", width=5, height=2.5, res=300)
severity
dev.off()




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
















