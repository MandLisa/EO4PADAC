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

### load df


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



#-------------------------------------------------------------------

### write --> that is the df for subsequent analysis!
write.csv(recovery_2608_filt1, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_2608_filt_geoloc.csv", row.names=FALSE)


ggplot(
  data = recovery_2608_filt1 %>%
    filter(
      severity_class == "stand-replacing" &
        !is.na(geoloc)),
  aes(x = VPD_summer_yod1, y = recovery_rate, color = geoloc)) +
  geom_point() +
  labs(title = "",
       x = "VPD anomalies 1-year post-disturbance",
       y = "Recovery Rate") +
  facet_wrap(~year) +
  theme_minimal()



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



