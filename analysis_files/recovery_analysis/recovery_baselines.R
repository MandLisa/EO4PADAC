library(readr)
library(terra)
library(raster)
library(sf)
library(tidyverse)
library(mgcv)
library(furrr)
library(ggplot2)

recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_random.csv")


# Function to calculate time to min tree share
time_to_min <- function(df_trees) {
  
  # Find the minimum tree share value and the year when the minimum is reached within the 5-year window after the year of disturbance for each time series (ID)
  min_share_in_window <- df_trees %>%
    group_by(ID, yod) %>%
    filter(year >= yod - 3, year <= yod + 3) %>%  # 3 years before and 3 years after
    slice(which.min(smoothed_tree_cover)) %>%
    ungroup()
  
  # Calculate the duration until the minimum is reached for each time series (ID)
  result <- min_share_in_window %>%
    mutate(time_to_min = year - yod) %>%
    select(ID, yod, min_year = year, time_to_min, min_tree_share = smoothed_tree_cover) %>%
    distinct()
  
  # Convert share to proportion
  result <- result %>%
    mutate(min_tree_share = min_tree_share / 100)
  
  # Join the result back with the original data to keep all columns
  result_full <- df_trees %>%
    left_join(result, by = c("ID", "yod"))
  
  return(result_full)
}

# Call the function and store the result in a new data frame
t_min <- time_to_min(recovery_random)

# Remove columns 27 and 28
t_min <- t_min[, -c(19,20,21)]

# Rename columns (example: renaming column 'old_name1' to 'new_name1' and 'old_name2' to 'new_name2')
t_min <- t_min %>%
  rename(
    min_year = min_year.y,
    time_to_min = time_to_min.y,
    min_tree_share = min_tree_share.y
  )



# Calculate the average tree share in a 5-year time window before the disturbance event for each time series (ID)
# compute avg tree share pre-dist
recovery <- recovery %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(
    # Compute the average smoothed_tree_cover for all years before yod
    tree_share_before = ifelse(
      year < yod, 
      mean(smoothed_tree_cover[year < yod], na.rm = TRUE), 
      NA
    )
  ) %>%
  ungroup()

# compute relative severoty
recovery <- recovery %>%
  group_by(ID) %>% 
  mutate(
    severity_relative = ifelse(
      tree_share_before > 0,
      ((tree_share_before - min_tree_share) / tree_share_before) * 100,
      NA_real_
    )
  ) %>%
  ungroup()




# Compute 80% of the tree_share_before for each time series
recovery <- recovery %>% #severity
  mutate(tree_share_80 = tree_share_before * 0.80)

# fill
recovery <- recovery %>%
  group_by(ID) %>%
  fill(tree_share_before, severity_relative, tree_share_80, .direction = "downup") %>%
  ungroup()

# compute recovery intervals
recovery1 <- recovery %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(
    # Find years after the min_year when tree_share_80 is met
    condition_met = year > min_year & smoothed_tree_cover >= tree_share_80,
    
    # Check if the condition is met for two consecutive years
    next_year_condition = lead(condition_met),
    two_consecutive_recovery = condition_met & next_year_condition
  ) %>%
  # Find the first year where recovery happens
  mutate(
    year_recov = ifelse(two_consecutive_recovery, year, NA_integer_)
  ) %>%
  summarise(
    year_recov = ifelse(all(is.na(year_recov)), NA_integer_, min(year_recov, na.rm = TRUE)),
    min_year = first(min_year)  # Retain min_year for use in recovery_rate calculation
  ) %>%
  ungroup() %>%
  # Compute recovery rate
  mutate(
    recovery_rate = if_else(!is.na(year_recov), year_recov - min_year, NA_integer_)
  )

# Step 2: Apply distinct to recovery
recovery1 <- recovery1 %>%
  distinct(ID, .keep_all = TRUE)



# Step 3: Join recovery with severity
recovery <- recovery %>%
  left_join(recovery1, by = "ID")

# Step 4: Fill NA values in recovery_rate within each ID
recovery <- recovery %>%
  group_by(ID) %>%
  mutate(
    recovery_rate.y = if_else(is.na(recovery_rate.y), first(na.omit(recovery_rate.y)), recovery_rate.y)
  ) %>%
  ungroup()


# Remove columns 27 and 28
recovery <- recovery[, -c(27,28,29)]

# Rename columns (example: renaming column 'old_name1' to 'new_name1' and 'old_name2' to 'new_name2')
recovery  <- recovery %>%
  rename(
    min_year = min_year.y,
    year_recov = year_recov.y,
    recovery_rate = recovery_rate.y
  )


# Reclassify severity
recovery <- recovery  %>%
  mutate(severity_class = case_when(
    severity_relative <= 75 ~ "NSR",
    severity_relative > 75 ~ "SR",
    TRUE ~ NA_character_  # Ensure that NA values are handled
  ))

# Assuming `recovery` already contains `recovery_rate`
recovery <- recovery %>%
  mutate(
    recovery_status = if_else(!is.na(recovery_rate), "recovered", "not recovered")
  )


# Create the recovery_10y and recovery_10y_num columns
recovery <- recovery %>%
  group_by(ID) %>%
  mutate(
    recovery_10y = if_else(any(recovery_status == "recovered" & year <= min_year + 10), 1, 0)
  ) %>%
  ungroup()

recovery <- recovery %>%
  group_by(ID) %>%
  mutate(
    VPD_yod = VPD_anomaly[year == yod],
    VPD_yod1 = VPD_anomaly[year == yod + 1],
    VPD_yod2 = VPD_anomaly[year == yod + 2],
    VPD_yod3 = VPD_anomaly[year == yod + 3]
  ) %>%
  ungroup()

#-------------------------------------------------------------------------------
### plot
# Filter the data to exclude recovery_rate >= 100
recovery_recovered <- severity %>%
  filter(recovery_rate < 100)

# one observation per ID
recovery_unique <- recovery_recovered %>%
  distinct(ID, .keep_all = TRUE)

# Reclassify `severity_class` where it is NA
recovery_unique <- recovery_unique %>%
  mutate(
    severity_class = if_else(is.na(severity_class), "non stand-replacing", severity_class)
  )

# plot
ggplot(recovery_unique, aes(x = recovery_rate, fill = severity_class)) +
  geom_density(alpha = 0.7, adjust = 2) + # Density plot with transparency
  scale_fill_manual(values = c("non stand-replacing" = "#458C91", "stand-replacing" = "#E2A800")) +
  labs(
    title = "",
    x = "Recovery Rate [Years]",
    y = "Density",
    fill = "Severity Class"
  )





### write df
write.csv(recovery, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_random.csv", row.names = FALSE)

### write df
write.csv(recovery_unique, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_random_unique_recovered.csv", row.names = FALSE)

### write df
write.csv(recovery_recovered, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_random_recovered.csv", row.names = FALSE)




# Create the recovery_10y and recovery_10y_num columns
severity <- severity %>%
  group_by(ID) %>%
  mutate(
    recovery_10y = if_else(any(recovery_status == "recovered" & year <= min_year + 10), 1, 0)
  ) %>%
  ungroup()



# Create new columns for VPD values at yod, yod+1, yod+2, and yod+3
severity <- severity %>%
  group_by(ID) %>%
  mutate(
    VPD_yod = VPD_anomaly[year == yod],
    VPD_yod1 = VPD_anomaly[year == yod + 1],
    VPD_yod2 = VPD_anomaly[year == yod + 2],
    VPD_yod3 = VPD_anomaly[year == yod + 3]
  ) %>%
  ungroup()


# if  I wanna control for aspect, I should reclassify aspect values
severity <- severity %>%
  mutate(aspect_cat = case_when(
    (aspect >= 315 | aspect < 45) ~ "N",
    aspect >= 45 & aspect < 135 ~ "O",
    aspect >= 135 & aspect < 225 ~ "S",
    aspect >= 225 & aspect < 315 ~ "W"
  ))













