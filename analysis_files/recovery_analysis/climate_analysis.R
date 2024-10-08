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


# load recovery x climate df
recovery_climate_topo <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate_topo.csv")


# Convert monthly VPD anomalies to a long format
# recovery_climate_topo = df
# recovery_long = df_long
recovery_long <- recovery_climate_topo %>%
  pivot_longer(cols = starts_with("VPD_"), names_to = "month", values_to = "VPD_anomaly") %>%
  mutate(month = factor(month, levels = c("VPD_apr", "VPD_may", "VPD_jun", "VPD_jul", "VPD_aug", "VPD_sep", "VPD_oct")))

# Compute the average VPD_anomaly value for each ID across all years before yod
pre_dist_VPD <- recovery_long %>%
  filter(year <= yod) %>%  # Step 1: Filter rows where year is less than yod
  group_by(ID) %>%  # Step 2: Group by ID
  summarize(mean_VPD = mean(VPD_anomaly, na.rm = TRUE))  


# Merge the pre-disturbance VPD anomaly back into the original df by ID
recovery_long1 <- recovery_long %>%
  left_join(pre_dist_VPD, by = "ID")  # Perform a left join to add mean_VPD_anomaly column from df_summary

# Assign the mean_VPD_anomaly only to the rows where year <= yod
recovery_long1 <- recovery_long1 %>%
  mutate(pre_disturbance_VPD_anomaly = ifelse(year <= yod, mean_VPD, NA))


# Compute the average VPD_anomaly value for each ID across all years before yod
post_dist_VPD <- recovery_long %>%
  filter(year > yod) %>%  # Step 1: Filter rows where year is less than yod
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
  filter(year > yod) %>%  # Keep rows where year is greater than yod
  compute_consecutive_positive_within_year()

# Merge the consecutive VPD counts back into the original dataframe
recovery_long3 <- recovery_long2 %>%
  left_join(df_consecutive, by = c("ID", "year"))


# Add new columns for VPD_consecutive for ysd = 1 to 5
recovery_long3 <- recovery_long3 %>%
  group_by(ID) %>%  # Group by ID to handle this per ID
  mutate(
    VPD_consecutive_1y = ifelse(ysd == 1, VPD_consecutive, NA),
    VPD_consecutive_2y = ifelse(ysd == 2, VPD_consecutive, NA),
    VPD_consecutive_3y = ifelse(ysd == 3, VPD_consecutive, NA),
    VPD_consecutive_4y = ifelse(ysd == 4, VPD_consecutive, NA),
    VPD_consecutive_5y = ifelse(ysd == 5, VPD_consecutive, NA)
  ) %>%
  # Fill down within each group to ensure the correct value is filled
  fill(VPD_consecutive_1y, VPD_consecutive_2y, VPD_consecutive_3y,
       VPD_consecutive_4y, VPD_consecutive_5y, .direction = "downup") %>%
  ungroup()  # Ungroup after mutation

### write
write.csv(recovery_long3, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate_topo_2.0.csv", row.names=FALSE)


# Keep the first observation for each ID
recovery_long3_unique <- recovery_long3 %>%
  distinct(ID, .keep_all = TRUE)

### write
write.csv(recovery_long3_unique, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate_topo_2.0_unique.csv", row.names=FALSE)


# Create a binary variable indicating whether recovery is complete
recovery_long3_unique <- recovery_long3_unique %>%
  mutate(recovery_status = if_else(recovery_rate == 100, "Not Recovered", "Recovered"))

recovery_long3 <- recovery_long3 %>%
  mutate(recovery_status = if_else(recovery_rate == 100, "Not Recovered", "Recovered"))


#-------------------------------------------------------------------------------

# Scatter plot
ggplot(recovery_long3, aes(x = recovery_rate, y = mean_VPD_post, color = recovery_status, shape = recovery_status)) +
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
ggplot(recovery_long3_filtered, aes(x = recovery_rate, y = mean_VPD_post_1_year)) +
  geom_point() +
  labs(title = "Scatter Plot of Recovery Time vs Mean VPD Post",
       x = "Recovery Time",
       y = "Mean VPD Post") +
  theme_minimal()


# Compute correlation
correlation <- cor(recovery_long3$recovery_rate, recovery_long3$mean_VPD_post_1_year)
print(paste("Correlation coefficient:", correlation))



install.packages("stats")
library(stats)


# Fit the model
model <- lm(recovery_rate ~ severity_relative + slope + height +
              aspect +
              mean_VPD_pre +
              VPD_consecutive_1y +
              VPD_consecutive_2y,
              data = recovery_long3)
summary(model)


# Standardize the predictors
recovery_standardized <- recovery_long3 %>%
  mutate(across(c(severity_relative, slope, height, aspect, mean_VPD_pre,
                  VPD_consecutive_1y, VPD_consecutive_2y), scale))

# Fit the standardized model
model <- lm(recovery_rate ~ severity_relative + slope + height +
              aspect +
              mean_VPD_pre +
              VPD_consecutive_1y +
              VPD_consecutive_2y,
            data = recovery_standardized)

summary(model)



# Load necessary library
install.packages("randomForest")
library(randomForest)

# Fit Random Forest model
rf_model <- randomForest(recovery_rate ~ severity_relative + slope + height +
                           aspect +
                           mean_VPD_pre +
                           VPD_consecutive_1y +
                           VPD_consecutive_2y, data = recovery_long3_filtered, importance = TRUE)

# Display feature importance
importance(rf_model)



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

