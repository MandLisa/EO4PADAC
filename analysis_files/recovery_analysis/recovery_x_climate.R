# Load required libraries
library(raster)
library(tidyr)
library(dplyr)
library(sp)
library(sf)
library(ggplot2)
library(terra)
library(spatial)
library(readr)
library(spatstat)
library(pryr)
library(mgcv)
library(purrr)
library(readr)
library(mgcv)
library(stringr)
library(data.table)


### load recory_climate.csv
recovery_VPD <- read.csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_VPD.csv")


### load recory_climate.csv
recovery_climate <- read.csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate.csv")

# convert CRS of climate data
# Load the reference raster (assuming the reference raster file path is 'reference_raster.tif')
reference_raster <- raster("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0028_Y0028/PREDICTION_l2_1986_v3_HL_ML_MLP.tif")

# Extract the CRS from the reference raster
target_crs <- crs(reference_raster)

# List of raster files to be reprojected (update the paths as needed)
raster_files <- list.files(path = "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies", pattern = "\\.tif$", full.names = TRUE)

# Function to reproject a raster with multiple bands
reproject_raster <- function(raster_path, target_crs) {
  raster_layer <- brick(raster_path)  # Use brick to handle multi-band rasters
  reprojected_raster <- projectRaster(raster_layer, crs = target_crs)
  return(reprojected_raster)
}

# Reproject each raster file and save the results
output_dir <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies"
lapply(raster_files, function(raster_path) {
  reprojected_raster <- reproject_raster(raster_path, target_crs)
  
  # Create the new filename with _LAEA suffix
  base_name <- tools::file_path_sans_ext(basename(raster_path))
  new_filename <- file.path(output_dir, paste0(base_name, "_LAEA.tif"))
  
  # Save the reprojected raster
  writeRaster(reprojected_raster, filename = new_filename, format = "GTiff", overwrite = TRUE)
})

#-------------------------------------------------------------------------------
### extract VPD anomalie values

# Assume recovery_df has columns: 'longitude' and 'latitude'
recovery_sf <- st_as_sf(recovery_all_fractions, coords = c("x", "y"), crs = st_crs(reference_raster))


# Function to extract VPD anomaly values from each raster band
extract_vpd_anomalies <- function(raster_path, recovery_sf) {
  raster_layer <- brick(raster_path)  # Use brick to handle multi-band rasters
  
  # Extract values for each band
  vpd_values <- extract(raster_layer, as(recovery_sf, "Spatial"))
  
  return(vpd_values)
}

# List of reprojected raster files (update the paths as needed)
raster_files <- list.files(path = "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod", pattern = "_mod\\.tif$", full.names = TRUE)

# Extract VPD anomaly values for each reprojected raster
vpd_anomalies <- lapply(raster_files, extract_vpd_anomalies, recovery_sf = recovery_sf)

# Combine extracted values into a single dataframe
vpd_anomalies_df <- do.call(cbind, vpd_anomalies)
colnames(vpd_anomalies_df) <- paste0("VPD_anomaly_band", 1:ncol(vpd_anomalies_df))

# Add VPD anomaly values to recovery_df
recovery_climate <- cbind(recovery, vpd_anomalies_df)

# Count the number of NA values in the 'value' column
num_na <- sum(is.na(recovery_climate$VPD_anomaly_band3))

print(num_na)


# Define column ranges for each month
col_ranges <- list(
  Apr = 19:57,
  May = 58:96,
  Jun = 97:135,
  Jul = 136:174,
  Aug = 175:213,
  Sep = 214:252,
  Oct = 253:291
)

# Function to generate new column names
generate_column_names <- function(start_col, end_col, month, start_year, end_year) {
  years <- start_year:end_year
  num_years <- length(years)
  num_cols <- end_col - start_col + 1
  names <- character(num_cols)
  
  for (i in 1:num_cols) {
    year_index <- ((i - 1) %% num_years) + 1
    names[i] <- paste0("VPD_", month, "_", years[year_index])
  }
  
  return(names)
}

# Define year range
start_year <- 1980
end_year <- 2018

# Generate new column names for each month
new_column_names <- unlist(lapply(names(col_ranges), function(month) {
  start_col <- col_ranges[[month]][1]
  end_col <- col_ranges[[month]][length(col_ranges[[month]])]
  generate_column_names(start_col, end_col, month, start_year, end_year)
}))

# Number of columns in the dataframe
num_cols <- ncol(recovery_climate)

# Create new column names for columns 19 to 291
new_colnames <- paste0("new_name_", seq(1, num_cols - 18))

# Check the length of new column names
if (length(new_colnames) == (num_cols - 18)) {
  # Get the original column names
  original_colnames <- colnames(recovery_climate)
  
  # Rename columns from 19 to 291
  colnames(recovery_climate) <- c(original_colnames[1:18], new_column_names)
  
  # Verify the changes
  head(colnames(recovery_climate), 30)  # View the first few column names for verification
} else {
  stop("Number of new column names does not match the number of columns to rename.")
}


# Count the number of NA values in the 'value' colum
num_na <- sum(is.na(recovery_climate$VPD_Oct_2018))

print(num_na)

### write
write.csv(recovery_climate, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate.csv", row.names=FALSE)


# Remove columns with years 1980 to 1985
cols_to_remove <- grep("VPD_(Apr|May|Jun|Jul|Aug|Sep|Oct)_(1980|1981|1982|1983|1984|1985)", names(recovery_climate))
recovery_climate <- recovery_climate[ , -cols_to_remove]

### write
write.csv(recovery_climate, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate.csv", row.names=FALSE)


#-------------------------------------------------------------------------------
# reorder data
# Perform the pivot_longer operation
recovery_climate_long <- recovery_climate %>%
  pivot_longer(
    cols = matches("VPD_(Apr|May|Jun|Jul|Aug|Sep|Oct)_\\d{4}"),  # Select columns with both month and year
    names_to = c("Month", "Year"),   # Create new columns for "Month" and "Year"
    names_pattern = "VPD_(\\w+)_(\\d+)",    # Regex to extract "Month" and "Year"
    values_to = "VPD_value"                 # The values will go into this column
  ) %>%
  mutate(Year = as.integer(Year)) %>%       # Convert "Year" column to integer
  pivot_wider(
    names_from = "Month",                   # Spread "Month" into columns
    values_from = "VPD_value"               # Use the values from "VPD_value" column
  )




#------------------------------------------------------------------------------
### do the same for absolute VPD

# convert CRS of climate data
# Load the reference raster (assuming the reference raster file path is 'reference_raster.tif')
reference_raster <- raster("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0028_Y0028/PREDICTION_l2_1986_v3_HL_ML_MLP.tif")

# Extract the CRS from the reference raster
target_crs <- crs(reference_raster)

# List of raster files to be reprojected (update the paths as needed)
raster_files <- list.files(path = "~/eo_nas/EO4Alps/climate_data/VPD_summer_averages", pattern = "\\.tif$", full.names = TRUE)


# Reproject each raster file and save the results
output_dir <- "~/eo_nas/EO4Alps/climate_data/VPD_summer_averages"
lapply(raster_files, function(raster_path) {
  reprojected_raster <- reproject_raster(raster_path, target_crs)
  
  # Create the new filename with _LAEA suffix
  base_name <- tools::file_path_sans_ext(basename(raster_path))
  new_filename <- file.path(output_dir, paste0(base_name, "_LAEA.tif"))
  
  # Save the reprojected raster
  writeRaster(reprojected_raster, filename = new_filename, format = "GTiff", overwrite = TRUE)
})

#-------------------------------------------------------------------------------
### extract VPD anomalie values

# Assume recovery_df has columns: 'longitude' and 'latitude'
recovery_sf <- st_as_sf(recovery_all_fractions, coords = c("x", "y"), crs = st_crs(reference_raster))


# Function to extract VPD anomaly values from a single-band raster
extract_vpd_anomalies <- function(raster_path, recovery_sf) {
  raster_layer <- raster(raster_path)  # Use raster to handle single-band rasters
  
  # Extract values for the raster
  vpd_values <- extract(raster_layer, as(recovery_sf, "Spatial"))
  
  return(vpd_values)
}

# List of reprojected raster files 
raster_files <- list.files(path = "~/eo_nas/EO4Alps/climate_data/VPD_summer_averages", pattern = "*_LAEA.tif$", full.names = TRUE)

# Extract VPD anomaly values for each reprojected raster
vpd_anomalies <- lapply(raster_files, extract_vpd_anomalies, recovery_sf = recovery_sf)

# Combine extracted values into a single dataframe
vpd_df <- do.call(cbind, vpd_anomalies)
colnames(vpd_df) <- paste0("VPD_absolute", 1:ncol(vpd_df))

# Add VPD anomaly values to recovery_df
recovery_VPD <- cbind(recovery_all_fractions, vpd_df)

# Count the number of NA values in the 'value' column
num_na <- sum(is.na(recovery_VPD$VPD_absolute33))

print(num_na)

# Assuming your data frame is named recovery_df
start_year <- 1986
end_year <- 2018

# Generate the new column names
new_column_names <- paste0("VPD_absolute_", seq(start_year, end_year))

# Assign the new names to columns 52 to 84
colnames(recovery_VPD)[129:161] <- new_column_names

### write
write.csv(recovery_VPD, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_VPD_2608.csv", row.names=FALSE)

# Remove specific columns by name
recovery_VPD <- recovery_VPD %>% select(-VPD_consecutive, -season, -VPD_Apr, -VPD_May,
                                          -VPD_Jun, -VPD_Jul, -VPD_Aug, -VPD_Sep, -VPD_Oct)


recovery_VPD <- recovery_VPD %>% select(-class, -min_year, -min_tree_share, -severity_absolute,
                                        -tree_share_80, -month)

recovery_VPD <- recovery_VPD %>% select(-class, -min_year, -min_tree_share, -severity_absolute,
                                        -tree_share_80, -month)


#-------------------------------------------------------------------------------
# Perform the pivot_longer operation for the year data

# Assuming your original data frame is called recovery_unique
recovery_VPD <- as.data.frame(recovery_VPD)

recovery_VPD <- recovery_VPD %>%
  group_by(x, y) %>%
  mutate(ID_new = cur_group_id()) %>%
  ungroup()  # Ungroup after assigning IDs


recovery_VPD1 <- recovery_VPD  %>%
  distinct(ID_new, year, .keep_all = TRUE) 



# Assuming your dataframe is called recovery_VPD
recovery_VPD_long <- recovery_VPD %>%
  pivot_longer(
    cols = starts_with("VPD_absolute_"),     # Select all columns that start with "VPD_absolute_"
    names_to = "Year",                       # Create a new column "Year"
    names_prefix = "VPD_absolute_",          # Remove the "VPD_absolute_" prefix to leave just the year
    values_to = "VPD_absolute"               # Place the VPD values into a new column "VPD_absolute"
  ) %>%
  mutate(Year = as.integer(Year)) %>%        # Convert the Year column to integer for proper matching
  filter(year == Year) %>%                   # Filter rows where the year column matches the Year extracted from the column name
  select(-Year)                              # Drop the temporary Year column



### write
write.csv(recovery_VPD_long, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_2608.csv", row.names=FALSE)


recovery_2608 <- recovery_2608 %>% select(-VPD_yod, -VPD_yod1, -VPD_yod2, -severity_absolute,
                                        -tree_share_80, -month)


recovery_2608_1 <- recovery_2608 %>%
  group_by(ID_new) %>%                              # Group the data by ID
  mutate(
    VPD_absolute_yod = ifelse(ysd == 0, VPD_absolute, NA),
    VPD_absolute_yod1 = ifelse(ysd == 1, VPD_absolute, NA),
    VPD_absolute_yod2 = ifelse(ysd == 2, VPD_absolute, NA),
    VPD_absolute_yod3 = ifelse(ysd == 3, VPD_absolute, NA),
    VPD_absolute_yod4 = ifelse(ysd == 4, VPD_absolute, NA),
    VPD_absolute_yod5 = ifelse(ysd == 5, VPD_absolute, NA)
  ) %>%
  ungroup()    


recovery_2608_1 <- recovery_2608 %>%
  group_by(ID_new) %>%  # Group by ID
  mutate(
    VPD_absolute_yod0 = case_when(ysd == 0 ~ VPD_absolute),
    VPD_absolute_yod1 = case_when(ysd == 1 ~ VPD_absolute),
    VPD_absolute_yod2 = case_when(ysd == 2 ~ VPD_absolute),
    VPD_absolute_yod3 = case_when(ysd == 3 ~ VPD_absolute),
    VPD_absolute_yod4 = case_when(ysd == 4 ~ VPD_absolute),
    VPD_absolute_yod5 = case_when(ysd == 5 ~ VPD_absolute)
  ) %>%
  ungroup()


str(recovery_2608)
head(recovery_2608)


recovery_2608_test <- recovery_2608 %>%
  filter(year == yod) %>%  # Filter for rows where year equals yod
  select(ID_new, VPD_absolute) %>%  # Select necessary columns
  rename(VPD_absolute_yod = VPD_absolute)

# Now, join this back with the original data frame
recovery_2608_1 <- recovery_2608 %>%
  left_join(recovery_2608_test, by = "ID_new")

recovery_2608_test <- recovery_2608 %>%
  filter(year == yod+1) %>%  # Filter for rows where year equals yod
  select(ID_new, VPD_absolute) %>%  # Select necessary columns
  rename(VPD_absolute_yod1 = VPD_absolute)

# Now, join this back with the original data frame
recovery_2608_1 <- recovery_2608_1 %>%
  left_join(recovery_2608_test, by = "ID_new")

recovery_2608_test <- recovery_2608 %>%
  filter(year == yod+2) %>%  # Filter for rows where year equals yod
  select(ID_new, VPD_absolute) %>%  # Select necessary columns
  rename(VPD_absolute_yod2 = VPD_absolute)

# Now, join this back with the original data frame
recovery_2608_1 <- recovery_2608_1 %>%
  left_join(recovery_2608_test, by = "ID_new")


recovery_2608_test <- recovery_2608 %>%
  filter(year == yod+3) %>%  # Filter for rows where year equals yod
  select(ID_new, VPD_absolute) %>%  # Select necessary columns
  rename(VPD_absolute_yod3 = VPD_absolute)

# Now, join this back with the original data frame
recovery_2608_1 <- recovery_2608_1 %>%
  left_join(recovery_2608_test, by = "ID_new")


recovery_2608_test <- recovery_2608 %>%
  filter(year == yod+4) %>%  # Filter for rows where year equals yod
  select(ID_new, VPD_absolute) %>%  # Select necessary columns
  rename(VPD_absolute_yod4 = VPD_absolute)

# Now, join this back with the original data frame
recovery_2608_1 <- recovery_2608_1 %>%
  left_join(recovery_2608_test, by = "ID_new")


recovery_2608_test <- recovery_2608 %>%
  filter(year == yod+5) %>%  # Filter for rows where year equals yod
  select(ID_new, VPD_absolute) %>%  # Select necessary columns
  rename(VPD_absolute_yod5 = VPD_absolute)

# Now, join this back with the original data frame
recovery_2608_1 <- recovery_2608_1 %>%
  left_join(recovery_2608_test, by = "ID_new")



# Rename columns
colnames(recovery_2608_1)[colnames(recovery_2608_1) == "VPD_autumn_yod+1"] <- "VPD_autumn_yod1"
colnames(recovery_2608_1)[colnames(recovery_2608_1) == "VPD_autumn_yod+2"] <- "VPD_autumn_yod2"
colnames(recovery_2608_1)[colnames(recovery_2608_1) == "VPD_autumn_yod+3"] <- "VPD_autumn_yod3"
colnames(recovery_2608_1)[colnames(recovery_2608_1) == "VPD_autumn_yod+4"] <- "VPD_autumn_yod4"
colnames(recovery_2608_1)[colnames(recovery_2608_1) == "VPD_autumn_yod+5"] <- "VPD_autumn_yod5"

colnames(recovery_2608_1)[colnames(recovery_2608_1) == "VPD_spring_yod+1"] <- "VPD_spring_yod1"
colnames(recovery_2608_1)[colnames(recovery_2608_1) == "VPD_spring_yod+2"] <- "VPD_spring_yod2"
colnames(recovery_2608_1)[colnames(recovery_2608_1) == "VPD_spring_yod+3"] <- "VPD_spring_yod3"
colnames(recovery_2608_1)[colnames(recovery_2608_1) == "VPD_spring_yod+4"] <- "VPD_spring_yod4"
colnames(recovery_2608_1)[colnames(recovery_2608_1) == "VPD_spring_yod+5"] <- "VPD_spring_yod5"

colnames(recovery_2608_1)[colnames(recovery_2608_1) == "VPD_summer_yod+1"] <- "VPD_summer_yod1"
colnames(recovery_2608_1)[colnames(recovery_2608_1) == "VPD_summer_yod+2"] <- "VPD_summer_yod2"
colnames(recovery_2608_1)[colnames(recovery_2608_1) == "VPD_summer_yod+3"] <- "VPD_summer_yod3"
colnames(recovery_2608_1)[colnames(recovery_2608_1) == "VPD_summer_yod+4"] <- "VPD_summer_yod4"
colnames(recovery_2608_1)[colnames(recovery_2608_1) == "VPD_summer_yod+5"] <- "VPD_summer_yod5"

### write
write.csv(recovery_2608_1, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_2608.csv", row.names=FALSE)

# Remove rows where recovery_rate > 38
recovery_2608_filt <- recovery_2608_1[recovery_2608_1$recovery_rate <= 38, ]



recovery_2608_unique <- recovery_2608_filt  %>%
  distinct(ID_new, .keep_all = TRUE) 

recovery_2308_unique <- recovery_2308  %>%
  distinct(ID, .keep_all = TRUE) 

# Select the specific columns from df1
df_selected <- recovery_2308_unique[, c("ID", "agent", "agent_name", "geoloc")]

# Merge df1_selected with df2 based on the 'ID' column
recovery_2608_filt1 <- merge(recovery_2608_filt, df_selected, by = "ID", all.x = TRUE)

### write
write.csv(recovery_2608_filt1, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_2608_filt_geoloc.csv", row.names=FALSE)


# Create a scatterplot
ggplot(
  data = recovery_2608_filt1 %>%
    filter(
      severity_class == "stand-replacing" &
        !is.na(geoloc)),
      aes(x = VPD_absolute_yod1, y = recovery_rate,
          color = cut(temp, c(0, 5, 10, 15)))) +
  geom_point() +
  labs(title = "",
       x = "VPD anomalies 1-year post-disturbance",
       y = "Recovery Rate") +
  facet_wrap(cut(temp, c(0, 5, 10, 15))~geoloc) +
  theme_minimal()


# Create a scatterplot
ggplot(
  data = recovery_2608_filt1 %>%
    filter(
      severity_class == "stand-replacing" &
        !is.na(geoloc)),
  aes(x = VPD_absolute_yod1, y = recovery_rate,
      color = cut(prec, c(400, 1000, 2000, 3200)))) +
  geom_point() +
  labs(title = "",
       x = "VPD anomalies 1-year post-disturbance",
       y = "Recovery Rate") +
  facet_wrap(cut(prec, c(400, 1000, 2000, 3200))~geoloc) +
  theme_minimal()




ggplot(
  data = recovery_2608_filt1 %>%
    filter(
      severity_class == "stand-replacing" &
        !is.na(geoloc)),
      #) %>%
    #filter(
      #yod %in% c(2001, 2002, 2003, 2004)
      #), 
  aes(
    y = recovery_rate, 
    x =`VPD_absolute_yod1`, 
    color = factor(yod)
    )
  ) +
  geom_point() +
  facet_wrap(~geoloc, scales = "free_y") +
  labs(title = "",
       x = "absolute VPD 1-year post-disturbance",
       y = "Recovery Rate") 
  #geom_smooth(
    #method = "gam",
    #se = FALSE
    #)


ggplot(
  data = recovery_2608_filt1 %>%
    filter(
      severity_class == "stand-replacing" &
        !is.na(geoloc)),
  aes(
    y = recovery_rate, 
    x =`VPD_absolute_yod1`, 
    color = cut(height, c(0, 1000, 2000, 3000))
    )
  ) +
  geom_point() +
  facet_wrap(cut(height, c(0, 1000, 2000, 3000))~geoloc) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x)
  )


ggplot(
  data = recovery_2608_filt1 %>%
    filter(
      severity_class == "stand-replacing" &
        !is.na(geoloc)),
  aes(
    y = recovery_rate, 
    x =`VPD_absolute_yod1`, 
    color = cut(temp, c(0, 5, 10, 20))
  )
) +
  geom_point() +
  facet_wrap(cut(temp, c(0, 5, 10, 20))~geoloc) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x)
  )



