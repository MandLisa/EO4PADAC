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
library(randomForest)
library(broom)

# load recovery x climate df
recovery_all <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_all.csv")
fcover_all <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/fcover_all.csv")

# Convert monthly VPD anomalies to a long format
# recovery_climate_topo = df
# recovery_long = df_long
recovery_long <- recovery_climate_topo %>%
  pivot_longer(cols = starts_with("VPD_"), names_to = "month", values_to = "VPD_anomaly") %>%
  mutate(month = factor(month, levels = c("VPD_apr", "VPD_may", "VPD_jun", "VPD_jul", "VPD_aug", "VPD_sep", "VPD_oct")))

# Compute the average VPD_anomaly value for each ID across all years before yod
pre_dist_VPD <- recovery_long %>%
  filter(year < yod) %>%  # Step 1: Filter rows where year is less than yod
  group_by(ID) %>%  # Step 2: Group by ID
  summarize(mean_VPD = mean(VPD_anomaly, na.rm = TRUE))  


# Merge the pre-disturbance VPD anomaly back into the original df by ID
recovery_long1 <- recovery_long %>%
  left_join(pre_dist_VPD, by = "ID")  # Perform a left join to add mean_VPD_anomaly column from df_summary

# Assign the mean_VPD_anomaly only to the rows where year <= yod
recovery_long1 <- recovery_long1 %>%
  mutate(pre_disturbance_VPD_anomaly = ifelse(year < yod, mean_VPD, NA))


# Compute the average VPD_anomaly value for each ID across all years before yod
post_dist_VPD <- recovery_long %>%
  filter(year >= yod) %>%  # Step 1: Filter rows where year is less than yod
  group_by(ID) %>%  # Step 2: Group by ID
  summarize(mean_VPD_post = mean(VPD_anomaly, na.rm = TRUE))


# Merge the pre-disturbance VPD anomaly back into the original df by ID
recovery_long2 <- recovery_long1 %>%
  left_join(post_dist_VPD, by = "ID")


# Delete column 27 from df_merged
recovery_long2 <- recovery_long2 %>%
  select(-27)  # Select all columns except the 27th column

# Rename the 26th column of df_merged
names(recovery_long2)[26] <- "mean_VPD_pre"  


# Function to compute mean VPD_anomaly for different time windows
compute_means <- function(df) {
  yod <- unique(df$yod)  # Get the disturbance year for the current ID
  
  data.frame(
    mean_VPD_yod = mean(df$VPD_anomaly[df$year %in% (yod + 0)], na.rm = TRUE),
    mean_VPD_post_1_year = mean(df$VPD_anomaly[df$year %in% (yod + 1)], na.rm = TRUE),
    mean_VPD_post_2_year = mean(df$VPD_anomaly[df$year %in% (yod + 1):(yod + 2)], na.rm = TRUE),
    mean_VPD_post_3_year = mean(df$VPD_anomaly[df$year %in% (yod + 1):(yod + 3)], na.rm = TRUE),
    mean_VPD_post_4_year = mean(df$VPD_anomaly[df$year %in% (yod + 1):(yod + 4)], na.rm = TRUE),
    mean_VPD_post_5_year = mean(df$VPD_anomaly[df$year %in% (yod + 1):(yod + 5)], na.rm = TRUE)
  )
}

# Group by ID and compute the means
VPD_time_windows <- recovery_long2 %>%
  group_by(ID) %>%
  do(compute_means(.)) %>%
  ungroup() %>%
  rename(
    mean_VPD_yod = mean_VPD_yod,
    mean_VPD_post_1_year = mean_VPD_post_1_year,
    mean_VPD_post_2_year = mean_VPD_post_2_year,
    mean_VPD_post_3_year = mean_VPD_post_3_year,
    mean_VPD_post_4_year = mean_VPD_post_4_year,
    mean_VPD_post_5_year = mean_VPD_post_5_year
  )

# Merge the summary back into the original dataframe
recovery_long2 <- recovery_long2 %>%
  left_join(VPD_time_windows, by = "ID")




# Map month names to numeric values
month_mapping <- c("VPD_apr" = 1, "VPD_may" = 2, "VPD_jun" = 3,
                   "VPD_jul" = 4, "VPD_aug" = 5, "VPD_sep" = 6, "VPD_oct" = 7)

# Add numeric month values to the dataframe
recovery_long2 <- recovery_long2 %>%
  mutate(month_num = recode(month, !!!month_mapping))


# Function to compute consecutive positive VPD anomaly months within each year
compute_consecutive_positive_within_year <- function(df) {
  df <- df %>%
    group_by(ID, year) %>%  # Group by ID and year
    arrange(ID, year, month_num) %>%  # Sort within each year
    mutate(VPD_consecutive = {
      consecutive_months <- 0
      max_consecutive <- 0
      
      # Iterate through each month's VPD anomaly for the year
      for (i in seq_len(n())) {
        # Handle NA values (skip or treat as non-positive)
        if (!is.na(VPD_anomaly[i]) && VPD_anomaly[i] > 0) {
          # If positive, increment consecutive count
          consecutive_months <- consecutive_months + 1
        } else {
          # If non-positive or NA, reset consecutive count
          consecutive_months <- 0
        }
        # Track the maximum consecutive count within the year
        max_consecutive <- max(max_consecutive, consecutive_months)
      }
      
      max_consecutive
    }) %>% 
    ungroup() %>% 
    distinct(ID, year, VPD_consecutive)  # Keep one record per ID and year
  
  return(df)
}

# Filter for years greater than yod, compute within-year consecutive VPD months
df_consecutive <- recovery_long2 %>%
  filter(year >= yod) %>%  # Keep rows where year is greater than yod
  compute_consecutive_positive_within_year()

# Merge the consecutive VPD counts back into the original dataframe
recovery_long3 <- recovery_long2 %>%
  left_join(df_consecutive, by = c("ID", "year"))


# Add new columns for VPD_consecutive for ysd = 1 to 5
recovery_long3 <- recovery_long3 %>%
  group_by(ID) %>%  # Group by ID to handle this per ID
  mutate(
    VPD_consecutive_yod = ifelse(ysd == 0, VPD_consecutive, NA),
    VPD_consecutive_1y = ifelse(ysd == 1, VPD_consecutive, NA),
    VPD_consecutive_2y = ifelse(ysd == 2, VPD_consecutive, NA),
    VPD_consecutive_3y = ifelse(ysd == 3, VPD_consecutive, NA),
    VPD_consecutive_4y = ifelse(ysd == 4, VPD_consecutive, NA),
    VPD_consecutive_5y = ifelse(ysd == 5, VPD_consecutive, NA)
  ) %>%
  # Fill down within each group to ensure the correct value is filled
  fill(VPD_consecutive_yod, VPD_consecutive_1y, VPD_consecutive_2y, VPD_consecutive_3y,
       VPD_consecutive_4y, VPD_consecutive_5y, .direction = "downup") %>%
  ungroup()  # Ungroup after mutation


recovery_long3 <- recovery_long3 %>%
  mutate(recovery_status = if_else(recovery_rate == 100, "Not Recovered", "Recovered"))

### write
write.csv(recovery_long3, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate_topo_2.0.csv", row.names=FALSE)

# Exclude observations where recovery_rate = 100
recovery_long3_filtered <- recovery_long3 %>% filter(recovery_rate < 100)

### write
write.csv(recovery_long3_filtered, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate_topo_2.0_filt.csv", row.names=FALSE)

# Keep the first observation for each ID
recovery_long3_filt_unique <- recovery_long3_filtered %>%
  distinct(ID, .keep_all = TRUE)

### write
write.csv(recovery_long3_filt_unique, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate_topo_2.0_filt_unique.csv", row.names=FALSE)


# Create a binary variable indicating whether recovery is complete
recovery_long3_unique <- recovery_long3_unique %>%
  mutate(recovery_status = if_else(recovery_rate == 100, "Not Recovered", "Recovered"))

recovery_long3 <- recovery_long3 %>%
  mutate(recovery_status = if_else(recovery_rate == 100, "Not Recovered", "Recovered"))


### compute spring/summer/autumn VPD anomalies

# Create a new column 'season' based on the month_num
recovery_all <- recovery_all %>%
  mutate(season = case_when(
    month_num %in% c(1, 2) ~ 'VPD_spring',   # April, May
    month_num %in% c(3, 4, 5) ~ 'VPD_summer', # June, July, August
    month_num %in% c(6, 7) ~ 'VPD_autumn'     # September, October
  ))

# Calculate mean VPD values for each ID, year, and season
result <- recovery_all %>%
  group_by(ID, year, season) %>%
  summarise(mean_VPD = mean(VPD_anomaly, na.rm = TRUE)) %>%
  ungroup()

# Reshape the data to have seasons as columns
result1 <- result %>%
  pivot_wider(names_from = season, values_from = mean_VPD)

# Merge the newly created seasonal columns with the original dataframe
recovery_all <- recovery_all %>%
  left_join(result1, by = c("ID", "year"))

# Exclude observations where recovery_rate = 100
recovery_all_filtered <- recovery_all %>% filter(recovery_rate < 100)


# Keep the first observation for each ID
recovery_all_unique <- recovery_all_filtered %>%
  distinct(ID, .keep_all = TRUE)



### write
write.csv(recovery_all_unique, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_all_unique.csv", row.names=FALSE)
write.csv(recovery_all_filtered, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_all_filtered.csv", row.names=FALSE)
write.csv(recovery_all, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_all.csv", row.names=FALSE)


### add fractions from other endmembers
str(fcover_all_wide)
str(recovery_all)


# pivot wider
fcover_all_wide <- fcover_all %>%
  pivot_wider(names_from = class, values_from = share)

colnames(recovery_all)[colnames(recovery_all) == "GAM_share"] <- "tree_share_GAM"
colnames(recovery_all)[colnames(recovery_all) == "share"] <- "tree_share"

colnames(fcover_all_wide)[colnames(fcover_all_wide) == "artificial"] <- "artifical_land_share"
colnames(fcover_all_wide)[colnames(fcover_all_wide) == "bare"] <- "bare_land_share"
colnames(fcover_all_wide)[colnames(fcover_all_wide) == "water"] <- "water_share"
colnames(fcover_all_wide)[colnames(fcover_all_wide) == "grassland"] <- "grassland_share"
colnames(fcover_all_wide)[colnames(fcover_all_wide) == "shrubland"] <- "shrubland_share"
colnames(fcover_all_wide)[colnames(fcover_all_wide) == "coniferous"] <- "coniferous_woodland_share"
colnames(fcover_all_wide)[colnames(fcover_all_wide) == "broadleaved"] <- "broadleaved_woodland_share"

# Assuming your data frame is called df and you want to move column "col_name" to position 2
recovery_all <- recovery_all %>%
  select(x, y, ID, year, month_num, yod, everything())


# Ensure the data is sorted
recovery_all_sorted <- recovery_all %>%
  arrange(ID, year, month_num)

# Pivot the data
recovery_wide <- recovery_all_sorted %>%
  pivot_wider(
    id_cols = c(ID, year),         # Keep these columns as identifiers
    names_from = month_num,        # Spread month numbers into columns
    values_from = VPD_anomaly,     # Fill the new columns with the corresponding values
    values_fn = list(VPD_anomaly = ~ first(.))  # Take the first non-null value (if any)
  )

# Step 1: Remove columns indexed 5 to 11
recovery_reduced <- recovery_all_wide %>%
  select(-c(5:11))

# Step 2: Keep only one observation per year and ID
recovery_reduced1 <- recovery_reduced %>%
  distinct(ID, year, .keep_all = TRUE)


# Perform a left join
recovery_final <- recovery_reduced1 %>%
  left_join(recovery_wide, by = c("ID", "year"))

# Rename the columns
recovery_final <- recovery_final %>%
  rename(
    VPD_Apr = `1`,
    VPD_May = `2`,
    VPD_Jun = `3`,
    VPD_Jul = `4`,
    VPD_Aug = `5`,
    VPD_Sep = `6`,
    VPD_Oct = `7`
  )

### write
write.csv(recovery_final, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_1808.csv", row.names=FALSE)

# Subset the data frame and remove columns indexed 1, 2, and 5
fcover_all_wide_subset <- fcover_all_wide %>%
  select(-c(1, 2, 5))


# Perform a left join
recovery_final1 <- recovery_final %>%
  left_join(fcover_all_wide_subset, by = c("ID", "year"))

# Keep only one observation per ID and year
recovery_final2 <- recovery_final1 %>%
  distinct(ID, year, .keep_all = TRUE)

# Apply the unique function to each list element in the relevant columns
recovery_final2$artifical_land_share <- sapply(recovery_final2$artifical_land_share, function(x) paste(unique(unlist(x)), collapse = ","))
recovery_final2$bare_land_share <- sapply(recovery_final2$bare_land_share, function(x) paste(unique(unlist(x)), collapse = ","))
recovery_final2$water_share <- sapply(recovery_final2$water_share, function(x) paste(unique(unlist(x)), collapse = ","))
recovery_final2$grassland_share <- sapply(recovery_final2$grassland_share, function(x) paste(unique(unlist(x)), collapse = ","))
recovery_final2$shrubland_share <- sapply(recovery_final2$shrubland_share, function(x) paste(unique(unlist(x)), collapse = ","))
recovery_final2$coniferous_woodland_share <- sapply(recovery_final2$coniferous_woodland_share, function(x) paste(unique(unlist(x)), collapse = ","))
recovery_final2$broadleaved_woodland_share <- sapply(recovery_final2$broadleaved_woodland_share, function(x) paste(unique(unlist(x)), collapse = ","))



### write
write.csv(recovery_final2, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_all_fractions.csv", row.names=FALSE)

# Keep only one observation per ID and year
recovery_final2_unique <- recovery_all_fractions %>%
  distinct(ID, .keep_all = TRUE)


### write
write.csv(recovery_final2_unique, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_all_fractions_unique.csv", row.names=FALSE)


# compute pre-disturbed land cover fractions
pre_dist_lc <- recovery_all_fractions %>%
  filter(year < yod) %>%  # Step 1: Filter rows where year is less than yod
  group_by(ID) %>%  # Step 2: Group by ID
  summarize(across(c(artifical_land_share, bare_land_share, water_share, 
                     grassland_share, shrubland_share, coniferous_woodland_share, 
                     broadleaved_woodland_share), 
                   \(x) mean(x, na.rm = TRUE), .names = "mean_pre_dist_{col}"))

# Perform the left join
recovery_all_fractions1 <- recovery_all_fractions %>%
  left_join(pre_dist_lc, by = "ID")


post_land_cover_shares <- recovery_all_fractions1 %>%
  filter(year == yod) %>%  # Filter for rows where year equals yod
  select(ID, artifical_land_share, bare_land_share, water_share, 
         grassland_share, shrubland_share, coniferous_woodland_share, 
         broadleaved_woodland_share) %>%  # Select necessary columns
  rename(artifical_land_share_yod = artifical_land_share,
         bare_land_share_yod = bare_land_share,
         water_share_yod = water_share,
         grassland_share_yod = grassland_share,
         shrubland_share_yod = shrubland_share,
         coniferous_woodland_share_yod = coniferous_woodland_share,
         broadleaved_woodland_share_yod = broadleaved_woodland_share)  # Rename columns

# Now, join this back with the original data frame
recovery_all_fractions2 <- recovery_all_fractions1 %>%
  left_join(post_land_cover_shares, by = "ID")

post_land_cover_shares <- recovery_all_fractions1 %>%
  filter(year == yod+1) %>%  # Filter for rows where year equals yod
  select(ID, artifical_land_share, bare_land_share, water_share, 
         grassland_share, shrubland_share, coniferous_woodland_share, 
         broadleaved_woodland_share) %>%  # Select necessary columns
  rename("artifical_land_share_yod+1" = artifical_land_share,
         "bare_land_share_yod+1" = bare_land_share,
         "water_share_yod+1" = water_share,
         "grassland_share_yod+1" = grassland_share,
         "shrubland_share_yod+1" = shrubland_share,
         "coniferous_woodland_share_yod+1" = coniferous_woodland_share,
         "broadleaved_woodland_share_yod+1" = broadleaved_woodland_share)  # Rename columns

# Now, join this back with the original data frame
recovery_all_fractions2 <- recovery_all_fractions2 %>%
  left_join(post_land_cover_shares, by = "ID")


post_land_cover_shares <- recovery_all_fractions1 %>%
  filter(year == yod+2) %>%  # Filter for rows where year equals yod
  select(ID, artifical_land_share, bare_land_share, water_share, 
         grassland_share, shrubland_share, coniferous_woodland_share, 
         broadleaved_woodland_share) %>%  # Select necessary columns
  rename("artifical_land_share_yod+2" = artifical_land_share,
         "bare_land_share_yod+2" = bare_land_share,
         "water_share_yod+2" = water_share,
         "grassland_share_yod+2" = grassland_share,
         "shrubland_share_yod+2" = shrubland_share,
         "coniferous_woodland_share_yod+2" = coniferous_woodland_share,
         "broadleaved_woodland_share_yod+2" = broadleaved_woodland_share)  # Rename columns

# Now, join this back with the original data frame
recovery_all_fractions2 <- recovery_all_fractions2 %>%
  left_join(post_land_cover_shares, by = "ID")


post_land_cover_shares <- recovery_all_fractions1 %>%
  filter(year == yod+3) %>%  # Filter for rows where year equals yod
  select(ID, artifical_land_share, bare_land_share, water_share, 
         grassland_share, shrubland_share, coniferous_woodland_share, 
         broadleaved_woodland_share) %>%  # Select necessary columns
  rename("artifical_land_share_yod+3" = artifical_land_share,
         "bare_land_share_yod+3" = bare_land_share,
         "water_share_yod+3" = water_share,
         "grassland_share_yod+3" = grassland_share,
         "shrubland_share_yod+3" = shrubland_share,
         "coniferous_woodland_share_yod+3" = coniferous_woodland_share,
         "broadleaved_woodland_share_yod+3" = broadleaved_woodland_share)  # Rename columns

# Now, join this back with the original data frame
recovery_all_fractions2 <- recovery_all_fractions2 %>%
  left_join(post_land_cover_shares, by = "ID")


post_land_cover_shares <- recovery_all_fractions1 %>%
  filter(year == yod+4) %>%  # Filter for rows where year equals yod
  select(ID, artifical_land_share, bare_land_share, water_share, 
         grassland_share, shrubland_share, coniferous_woodland_share, 
         broadleaved_woodland_share) %>%  # Select necessary columns
  rename("artifical_land_share_yod+4" = artifical_land_share,
         "bare_land_share_yod+4" = bare_land_share,
         "water_share_yod+4" = water_share,
         "grassland_share_yod+4" = grassland_share,
         "shrubland_share_yod+4" = shrubland_share,
         "coniferous_woodland_share_yod+4" = coniferous_woodland_share,
         "broadleaved_woodland_share_yod+4" = broadleaved_woodland_share)  # Rename columns

# Now, join this back with the original data frame
recovery_all_fractions2 <- recovery_all_fractions2 %>%
  left_join(post_land_cover_shares, by = "ID")


post_land_cover_shares <- recovery_all_fractions1 %>%
  filter(year == yod+5) %>%  # Filter for rows where year equals yod
  select(ID, artifical_land_share, bare_land_share, water_share, 
         grassland_share, shrubland_share, coniferous_woodland_share, 
         broadleaved_woodland_share) %>%  # Select necessary columns
  rename("artifical_land_share_yod+5" = artifical_land_share,
         "bare_land_share_yod+5" = bare_land_share,
         "water_share_yod+5" = water_share,
         "grassland_share_yod+5" = grassland_share,
         "shrubland_share_yod+5" = shrubland_share,
         "coniferous_woodland_share_yod+5" = coniferous_woodland_share,
         "broadleaved_woodland_share_yod+5" = broadleaved_woodland_share)  # Rename columns

# Now, join this back with the original data frame
recovery_all_fractions2 <- recovery_all_fractions2 %>%
  left_join(post_land_cover_shares, by = "ID")

# compute spring/summer/autumn atmospheric dryness for yod, yod+1, yod+2
VPD_season_yod <- recovery_all_fractions %>%
  filter(year == yod) %>%  # Filter for rows where year equals yod
  select(ID, VPD_autumn, VPD_spring, VPD_summer) %>%  # Select necessary columns
  rename(VPD_autumn_yod = VPD_autumn,
         VPD_spring_yod = VPD_spring,
         VPD_summer_yod = VPD_summer)  # Rename columns

# Now, join this back with the original data frame
recovery_all_fractions1 <- recovery_all_fractions %>%
  left_join(VPD_season_yod, by = "ID")


# compute spring/summer/autumn atmospheric dryness for yod, yod+1, yod+2
VPD_season_yod1 <- recovery_all_fractions %>%
  filter(year == yod+1) %>%  # Filter for rows where year equals yod
  select(ID, VPD_autumn, VPD_spring, VPD_summer) %>%  # Select necessary columns
  rename("VPD_autumn_yod+1" = VPD_autumn,
         "VPD_spring_yod+1" = VPD_spring,
         "VPD_summer_yod+1" = VPD_summer)  # Rename columns

# Now, join this back with the original data frame
recovery_all_fractions1 <- recovery_all_fractions1 %>%
  left_join(VPD_season_yod1, by = "ID")


# compute spring/summer/autumn atmospheric dryness for yod, yod+1, yod+2
VPD_season_yod2 <- recovery_all_fractions %>%
  filter(year == yod+2) %>%  # Filter for rows where year equals yod
  select(ID, VPD_autumn, VPD_spring, VPD_summer) %>%  # Select necessary columns
  rename("VPD_autumn_yod+2" = VPD_autumn,
         "VPD_spring_yod+2" = VPD_spring,
         "VPD_summer_yod+2" = VPD_summer)  # Rename columns

# Now, join this back with the original data frame
recovery_all_fractions1 <- recovery_all_fractions1 %>%
  left_join(VPD_season_yod2, by = "ID")


# compute spring/summer/autumn atmospheric dryness for yod, yod+1, yod+2
VPD_season_yod3 <- recovery_all_fractions %>%
  filter(year == yod+3) %>%  # Filter for rows where year equals yod
  select(ID, VPD_autumn, VPD_spring, VPD_summer) %>%  # Select necessary columns
  rename("VPD_autumn_yod+3" = VPD_autumn,
         "VPD_spring_yod+3" = VPD_spring,
         "VPD_summer_yod+3" = VPD_summer)  # Rename columns

# Now, join this back with the original data frame
recovery_all_fractions1 <- recovery_all_fractions1 %>%
  left_join(VPD_season_yod3, by = "ID")


# compute spring/summer/autumn atmospheric dryness for yod, yod+1, yod+2
VPD_season_yod4 <- recovery_all_fractions %>%
  filter(year == yod+4) %>%  # Filter for rows where year equals yod
  select(ID, VPD_autumn, VPD_spring, VPD_summer) %>%  # Select necessary columns
  rename("VPD_autumn_yod+4" = VPD_autumn,
         "VPD_spring_yod+4" = VPD_spring,
         "VPD_summer_yod+4" = VPD_summer)  # Rename columns

# Now, join this back with the original data frame
recovery_all_fractions1 <- recovery_all_fractions1 %>%
  left_join(VPD_season_yod4, by = "ID")


# compute spring/summer/autumn atmospheric dryness for yod, yod+1, yod+2
VPD_season_yod5 <- recovery_all_fractions %>%
  filter(year == yod+5) %>%  # Filter for rows where year equals yod
  select(ID, VPD_autumn, VPD_spring, VPD_summer) %>%  # Select necessary columns
  rename("VPD_autumn_yod+5" = VPD_autumn,
         "VPD_spring_yod+5" = VPD_spring,
         "VPD_summer_yod+5" = VPD_summer)  # Rename columns

# Now, join this back with the original data frame
recovery_all_fractions1 <- recovery_all_fractions1 %>%
  left_join(VPD_season_yod5, by = "ID")

recovery_all_fractions <- recovery_all_fractions1


### write
write.csv(recovery_all_fractions, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_all_fractions.csv", row.names=FALSE)

recovery_all_fractions_recov <- recovery_all_fractions %>%
  filter(recovery_rate != 100)



# Keep only one observation per ID and year
recovery_all_fractions_unique <- recovery_all_fractions_recov  %>%
  distinct(ID, .keep_all = TRUE)

### write
write.csv(recovery_all_fractions_unique, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_all_fractions_unique.csv", row.names=FALSE)

#--------------------------------------------------------------------------------


#-------------------------------------------------------------------------------

# Scatter plot
ggplot(recovery_long3_unique, aes(x = recovery_rate, y = mean_VPD_post, color = recovery_status, shape = recovery_status)) +
  geom_point() +
  labs(title = "Scatter Plot of Recovery Rate vs Mean VPD Post",
       x = "Recovery Rate",
       y = "Mean VPD Post") +
  scale_color_manual(values = c("Recovered" = "blue", "Not Recovered" = "red")) +
  scale_shape_manual(values = c("Recovered" = 16, "Not Recovered" = 17)) +
  theme_minimal()


# Exclude observations where recovery_rate = 100
recovery_long3_filtered <- recovery_long3 %>% filter(recovery_rate < 100)
recovery_long3_filtered_SR <- recovery_long3_filtered %>% filter(severity_class == "stand-replacing")


# Scatter plot
ggplot(recovery_long3_unique, aes(x = recovery_rate, y = mean_VPD_post_1_year)) +
  geom_point() +
  labs(title = "Scatter Plot of Recovery Time vs Mean VPD Post",
       x = "Recovery Time",
       y = "Mean VPD Post") +
  theme_minimal()


# Compute correlation
correlation <- cor(recovery_long3_unique$recovery_rate, recovery_long3_unique$mean_VPD_post_1_year)
print(paste("Correlation coefficient:", correlation))


library(stats)


# Fit the model
model <- lm(recovery_rate ~ severity_relative + slope + height +
              aspect +
              mean_VPD_pre +
              VPD_consecutive_1y +
              VPD_consecutive_2y,
            data = recovery_long3_unique)
summary(model)


# Standardize the predictors
recovery_standardized <- recovery_long3_unique %>%
  mutate(across(c(severity_relative, slope, height, aspect, mean_VPD_pre,
                  VPD_consecutive_1y, VPD_consecutive_2y, VPD_consecutive_3y), scale))

# Fit the standardized model
model <- lm(recovery_rate ~ severity_relative + slope + height +
              aspect +
              mean_VPD_pre +
              VPD_consecutive_1y +
              VPD_consecutive_2y +
              VPD_consecutive_3y,
            data = recovery_standardized)

summary(model)


# Extract the coefficients, standard errors, and confidence intervals
model_summary <- broom::tidy(model)

# Exclude the intercept
model_summary <- model_summary[model_summary$term != "(Intercept)", ]

# Reorder the predictors for better plotting (if needed)
model_summary$term <- factor(model_summary$term, levels = model_summary$term)

# Plot using ggplot2 with predictors on the y-axis and coefficients on the x-axis
ggplot(model_summary, aes(y = term, x = estimate)) +
  geom_point(size = 3) +  # Points for the estimates
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error), height = 0.2) +  # Horizontal error bars for standard errors
  theme_minimal() +
  labs(x = "Estimated Coefficient", y = "Predictor",
       title = "Coefficient Plot of the Linear Model (Without Intercept)") +
  theme(axis.text.y = element_text(hjust = 1))  # Adjust y-axis labels for readability





# Fit a Random Forest model
# Mean impute missing values for numeric columns only
recovery_imputed <- recovery_long3_unique %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Fit the Random Forest model on the cleaned data
set.seed(123)  # For reproducibility
rf_model <- randomForest(recovery_rate ~ severity_relative + slope + height +
                           aspect + mean_VPD_pre + VPD_consecutive_1y +
                           VPD_consecutive_2y + VPD_consecutive_3y,
                         data = recovery_imputed, 
                         importance = TRUE)


# Extract feature importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)

# Rename columns for clarity
colnames(importance_df) <- c("MeanDecreaseGini", "MeanDecreaseAccuracy", "Feature")

# Select the metric you want to plot, e.g., MeanDecreaseGini
importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini)) %>%  # Sort by importance
  mutate(Feature = factor(Feature, levels = Feature))  # Reorder factor levels for plotting

# Plot feature importance using ggplot2
ggplot(importance_df, aes(y = Feature, x = MeanDecreaseGini)) +
  geom_col(fill = "skyblue") +  # Bar plot with sky blue color
  theme_minimal() +
  labs(x = "Mean Decrease in Gini Index", y = "Feature",
       title = "Feature Importance from Random Forest Model") +
  theme(axis.text.y = element_text(size = 12))  # Adjust text size if needed



# Extract feature importance
importance_scores <- importance(rf_model)
importance_df <- as.data.frame(importance_scores)
importance_df$Feature <- rownames(importance_df)


# Plot feature importance
ggplot(importance_df, aes(x = reorder(Feature, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance from Random Forest",
       x = "Feature",
       y = "Importance (IncMSE)") +
  theme_minimal()



#-------------------------------------------------------------------------------
### add absolute VPD values -> average from June, July, August


# Folder containing the .tif files
tif_folder <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/06"

# Convert the data frame to an sf object
recvoery_all_sf <- st_as_sf(recovery_all, coords = c("x", "y"), crs = st_crs(reference_raster))

# Reproject the sf object to the target CRS if necessary
recvoery_all_sf <- st_transform(recvoery_all_sf, crs = target_crs)


# List all .tif files in the folder
tif_files <- list.files(tif_folder, pattern = "\\.tif$", full.names = TRUE)

# Create an empty list to store the extracted values
extracted_values_list <- list()

# Loop over each tif file
for (tif_file in tif_files) {
  # Load the raster
  raster <- raster(tif_file)
  
  # Project the raster to the target CRS
  raster_proj <- projectRaster(raster, crs = target_crs)
  
  # Extract the values at the points using the SpatialPointsDataFrame or coordinates
  values <- extract(raster_proj, recvoery_all_sf)
  
  # Get the raster name (without the file extension)
  raster_name <- tools::file_path_sans_ext(basename(tif_file))
  
  # Store the extracted values in the list with the raster name as the key
  extracted_values_list[[raster_name]] <- values
}

# Combine the extracted values into a data frame
extracted_values_df <- as.data.frame(extracted_values_list)

# Combine the original df with the extracted values
recovery_all <- cbind(recovery_all@data, extracted_values_df)







