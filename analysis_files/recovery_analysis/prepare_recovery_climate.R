# Load the 1986 raster
raster_apr_1986 <- raster("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/04/1986.tif")


# Filter the dataframe for the year 1986
recovery_1986 <- recovery %>% filter(year == 1986)

# Extract VPD values at the x and y coordinates
vpd_values <- extract(raster_apr_1986, recovery_1986[, c("x", "y")])

# Add the extracted values as a new column to the filtered dataframe
recovery_1986$VPD_apr <- vpd_values


# Load the 1986 raster
raster_may_1986 <- raster("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/05/1986.tif")

# Extract VPD values at the x and y coordinates
vpd_values <- extract(raster_may_1986, recovery_1986[, c("x", "y")])

# Add the extracted values as a new column to the filtered dataframe
recovery_1986$VPD_may <- vpd_values



# Load the 1986 raster
raster_jun_1986 <- raster("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/06/1986.tif")


# Extract VPD values at the x and y coordinates
vpd_values <- extract(raster_jun_1986, recovery_1986[, c("x", "y")])

# Add the extracted values as a new column to the filtered dataframe
recovery_1986$VPD_jun <- vpd_values


# Load the 1986 raster
raster_jul_1986 <- raster("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/07/1986.tif")


# Extract VPD values at the x and y coordinates
vpd_values <- extract(raster_jul_1986, recovery_1986[, c("x", "y")])

# Add the extracted values as a new column to the filtered dataframe
recovery_1986$VPD_jul <- vpd_values


# Load the 1986 raster
raster_aug_1986 <- raster("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/08/1986.tif")


# Extract VPD values at the x and y coordinates
vpd_values <- extract(raster_aug_1986, recovery_1986[, c("x", "y")])

# Add the extracted values as a new column to the filtered dataframe
recovery_1986$VPD_aug <- vpd_values


# Load the 1986 raster
raster_sep_1986 <- raster("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/09/1986.tif")


# Extract VPD values at the x and y coordinates
vpd_values <- extract(raster_sep_1986, recovery_1986[, c("x", "y")])

# Add the extracted values as a new column to the filtered dataframe
recovery_1986$VPD_sep <- vpd_values


# Load the 1986 raster
raster_oct_1986 <- raster("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/10/1986.tif")


# Extract VPD values at the x and y coordinates
vpd_values <- extract(raster_oct_1986, recovery_1986[, c("x", "y")])

# Add the extracted values as a new column to the filtered dataframe
recovery_1986$VPD_oct <- vpd_values


#-----------------------------------------

library(raster)
library(dplyr)

###
# 1987
###


# Define the months you want to process
months <- c("04", "05", "06", "07", "08", "09", "10")

# Filter the dataframe for the year 1986
recovery_1986 <- points_shapefile_long_GAM %>% filter(year == 1986)

# Loop over each month, load the corresponding raster, and extract the VPD values
for (month in months) {
  # Construct the file path based on the month
  raster_path <- paste0("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/", month, "/1987.tif")
  
  # Load the raster
  raster_data <- raster(raster_path)
  
  # Extract VPD values at the x and y coordinates
  vpd_values <- extract(raster_data, recovery_1987[, c("x", "y")])
  
  # Create a column name dynamically, e.g., "VPD_apr" for "04"
  col_name <- paste0("VPD_", tolower(month.abb[as.integer(month)]))
  
  # Add the extracted values as a new column to the filtered dataframe
  recovery_1987[[col_name]] <- vpd_values
}


###
# 1988
###


# Define the months you want to process
months <- c("04", "05", "06", "07", "08", "09", "10")

# Filter the dataframe for the year 1986
recovery_1988 <- recovery %>% filter(year == 1988)

# Loop over each month, load the corresponding raster, and extract the VPD values
for (month in months) {
  # Construct the file path based on the month
  raster_path <- paste0("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/", month, "/1988.tif")
  
  # Load the raster
  raster_data <- raster(raster_path)
  
  # Extract VPD values at the x and y coordinates
  vpd_values <- extract(raster_data, recovery_1988[, c("x", "y")])
  
  # Create a column name dynamically, e.g., "VPD_apr" for "04"
  col_name <- paste0("VPD_", tolower(month.abb[as.integer(month)]))
  
  # Add the extracted values as a new column to the filtered dataframe
  recovery_1988[[col_name]] <- vpd_values
}



###
# 1989
###


# Define the months you want to process
months <- c("04", "05", "06", "07", "08", "09", "10")

# Filter the dataframe for the year 1986
recovery_2003 <- recovery %>% filter(year == 2003)

# Loop over each month, load the corresponding raster, and extract the VPD values
for (month in months) {
  # Construct the file path based on the month
  raster_path <- paste0("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/", month, "/2003.tif")
  
  # Load the raster
  raster_data <- raster(raster_path)
  
  # Extract VPD values at the x and y coordinates
  vpd_values <- extract(raster_data, recovery_2003[, c("x", "y")])
  
  # Create a column name dynamically, e.g., "VPD_apr" for "04"
  col_name <- paste0("VPD_", tolower(month.abb[as.integer(month)]))
  
  # Add the extracted values as a new column to the filtered dataframe
  recovery_2003[[col_name]] <- vpd_values
}



# merge dfs
# Define the range of years
years <- 1986:2018

# Create a vector of data frame names
df_names <- paste0("recovery_", years)

# Use mget to retrieve the data frames into a list
df_list <- mget(df_names)

# Combine all data frames using do.call and rbind
recovery_climate <- do.call(rbind, df_list)


# Filter the new data frame for years between 2019 and 2023
recovery_2019_2023 <- recovery %>%
  filter(year >= 2019 & year <= 2023)


# Combine the filtered new data frame with df_combined
recovery_climate <- bind_rows(recovery_climate, recovery_2019_2023)

recovery_climate <- recovery_climate1


### write
write.csv(recovery_climate, "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/recovery_climate.csv", row.names=FALSE)
