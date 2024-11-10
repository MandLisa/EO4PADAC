library(readr)
library(terra)
library(raster)
library(sf)
library(tidyverse)
library(mgcv)

### extract tree cover values per year
# Directory containing the rasters (assuming filenames follow a consistent naming pattern)
raster_dir <- "~/eo_nas/EO4Alps/level3_predictions/l2_mask/tree_only/"
years <- 1986:2023  # Define the range of years

# Load the points shapefile
points_shapefile <- st_read("~/eo_nas/EO4Alps/GEDI/GEDI_filt.shp")

# Check and match CRS of the shapefile and the rasters
first_raster <- raster(file.path(raster_dir, paste0("treecover", years[1], ".tif")))
if (st_crs(points_shapefile) != st_crs(first_raster)) {
  points_shapefile <- st_transform(points_shapefile, crs(first_raster))
}

# Convert the sf object to a Spatial object
points_spatial <- as(points_shapefile, "Spatial")

# Loop through each year, load the raster, extract values, and add them as a new column
for (year in years) {
  # Construct the file path for the current year
  raster_path <- file.path(raster_dir, paste0("treecover", year, ".tif"))
  
  # Load the raster
  treecover_raster <- raster(raster_path)
  
  # Extract values at each point
  values <- extract(treecover_raster, points_spatial)
  
  # Add the extracted values as a new column in the shapefile's data
  points_shapefile[[paste0("treecover_", year)]] <- values
}

# View the result (first few columns)
head(points_shapefile)




# Remove columns by index, e.g., removing columns 3, 4, and 5
points_shapefile <- points_shapefile %>% select(-c(5,6,7,18,20,22,23,25,26,28,29,
                                                   31,32,33,34,35,36,37,38,39,
                                                   40,41,42,53,,54,55,56,57,58,
                                                   59,60,61,62,63,64,65))

# Remove rows where "tree_cover" column is NA
points_shapefile_filt <- points_shapefile_filt %>%
  filter(!is.na(tr_1996))

# Add a unique ID column for each row
points_shapefile_filt <- points_shapefile_filt %>%
  mutate(ID = row_number())

# Optional: Save the updated shapefile with all extracted values
st_write(points_shapefile, "~/eo_nas/EO4Alps/GEDI/GEDI_filt_treecover_wide_filt.shp")


### apply pivot_longer
# Load the points shapefile
points_shapefile_filt <- st_read("~/eo_nas/EO4Alps/GEDI/GEDI_filt_treecover_wide1.shp")


# Pivot to long format, keeping the ID and year information
points_shapefile_long <- points_shapefile_filt %>%
  pivot_longer(
    cols = starts_with("tr_"),      # Select columns that start with "treecover_"
    names_to = "year",                     # New column for years
    names_prefix = "tr_",           # Remove the "treecover_" prefix from names
    names_transform = list(year = as.integer),  # Convert year to integer
    values_to = "tree_cover"               # New column for tree cover values
  )

# Rename a column
points_shapefile_long <- points_shapefile_long %>% rename(yod = dist_yr)

# Extract x and y coordinates (assuming the shapefile contains points)
points_shapefile_long_df <- points_shapefile_long %>%
  mutate(x = st_coordinates(.)[, 1],
         y = st_coordinates(.)[, 2]) %>%
  st_set_geometry(NULL)  # Drop geometry column to keep only attributes and coordinates

# Save as CSV
write.csv(points_shapefile_long_df, "~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_points.csv", row.names = FALSE)

# Optional: Save the updated shapefile with all extracted values
st_write(points_shapefile_long, "~/eo_nas/EO4Alps/GEDI/GEDI_long.shp")


# Define a function to fit models per ID
fit_tree_cover <- function(data) {
  # Split data into two parts: one for year > yod and one for year <= yod
  data_after_yod <- data %>% filter(year > yod)
  data_before_yod <- data %>% filter(year <= yod)
  
  # Check if there are enough years to fit a GAM (let’s assume at least 3 years)
  if (nrow(data_after_yod) >= 5) {
    # Fit a GAM to smooth out intra-year variations for years > yod, with limited k to avoid issues
    model <- gam(tree_cover ~ s(year, k = min(5, nrow(data_after_yod) - 1)), data = data_after_yod)
  } else {
    # If too few years, fit a linear model instead
    model <- lm(tree_cover ~ year, data = data_after_yod)
  }
  
  # Predict the smoothed/adjusted tree cover values for years > yod
  data_after_yod$smoothed_tree_cover <- predict(model, newdata = data_after_yod)
  
  # For years <= yod, keep the original tree_cover values as smoothed_tree_cover
  data_before_yod <- data_before_yod %>%
    mutate(smoothed_tree_cover = tree_cover)
  
  # Combine the two subsets back together
  data_combined <- bind_rows(data_before_yod, data_after_yod) %>%
    arrange(year)  # Ensure rows are in chronological order
  
  return(data_combined)
}

# Apply the function to each ID
points_shapefile_long_GAM <- points_shapefile_long %>%
  group_by(ID) %>%
  group_modify(~ fit_tree_cover(.x)) %>%
  ungroup()


write.csv(points_shapefile_long_GAM, "~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_GAM.csv", row.names = FALSE)



#-------------------------------------------------------------------------------
### prepare VPD data

# Directory containing the VPD rasters (adjust path as needed)
vpd_dir <- "~/path_to_vpd_rasters/"
years <- 1986:2018  # Range of years for which you have VPD rasters

# Ensure your main data frame has unique IDs and years from 1986-2023 for each location
# Assuming your main data frame is points_shapefile_long_smoothed

# Initialize a list to store extracted VPD values along with IDs and years
vpd_values_list <- list()

# Loop through each year, load the corresponding VPD raster, and extract values
for (year in years) {
  # Construct the file path for the current VPD raster
  raster_path <- file.path(vpd_dir, paste0("VPD_", year, ".tif"))
  
  # Load the VPD raster for the current year
  vpd_raster <- raster(raster_path)
  
  # Filter the data frame for the current year
  current_year_data <- points_shapefile_long_smoothed %>%
    filter(year == year)
  
  # Convert the filtered data to an `sf` object with the correct CRS, then to `Spatial` for compatibility
  locations <- st_as_sf(current_year_data, coords = c("longitude", "latitude"), crs = st_crs(vpd_raster)) %>%
    as("Spatial")
  
  # Extract VPD values at the locations for the current year
  vpd_values <- extract(vpd_raster, locations)
  
  # Store the results in a temporary data frame with ID, year, and extracted VPD values
  vpd_values_df <- current_year_data %>%
    select(ID, year) %>%
    mutate(VPD_anomalies = vpd_values)
  
  # Add the results to the list
  vpd_values_list[[as.character(year)]] <- vpd_values_df
}

# Combine all years into a single data frame
vpd_values_combined <- bind_rows(vpd_values_list)

# Join the extracted VPD values back to the main data frame by ID and year
points_shapefile_long_smoothed <- points_shapefile_long_smoothed %>%
  left_join(vpd_values_combined, by = c("ID", "year"))

# View the result
head(points_shapefile_long_smoothed)


