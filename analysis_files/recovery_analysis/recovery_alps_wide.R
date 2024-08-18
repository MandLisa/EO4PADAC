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

# Define the path to the folder containing the CSV files
folder_path <- "~/eo_nas/EO4Alps/00_analysis/_data"

# List all CSV files in the folder, excluding those with "unique" in their filenames
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
csv_files <- csv_files[!grepl("unique", csv_files)]

# Initialize an empty list to store the dataframes
df_list <- list()

# Loop through the list of CSV files and read each one into a dataframe
for (file in csv_files) {
  df <- read.csv(file)
  df_list <- append(df_list, list(df))
}

# Row-bind all dataframes into a single dataframe
fcover_all <- bind_rows(df_list)

### write
write.csv(fcover_all, "~/eo_nas/EO4Alps/00_analysis/_recovery/fcover_all.csv", row.names=FALSE)


# compute min value of tree share after 5 years after disturbance
# Step 1: Filter the data for class == "trees"
df_trees <- fcover_all %>%
  filter(class %in% c("coniferous", "broadleaved"))

df_trees <- fcover_all %>%
  filter(class %in% c("coniferous", "broadleaved")) %>%
  mutate(class_l1 = "trees")

# as numeric
df_trees$year <- as.numeric(df_trees$year)
df_trees$yod <- as.numeric(df_trees$yod)

# Create ID_new column based on unique combinations of x and y
df_trees <- df_trees %>%
  group_by(x, y) %>%
  mutate(ID_new = as.numeric(cur_group_id())) %>%  # Convert ID_new to numeric
  ungroup()

# sum broadleaved and coniferous
df_trees <- df_trees %>%
  # Group by the columns that should remain unchanged
  group_by(x, y, ID_new, year, yod) %>%
  # Summarize by summing shares and assigning "trees" to the class
  summarise(
    share = sum(share, na.rm = TRUE),
    class = "trees",
    .groups = 'drop'
  ) %>%
  # Cap the share values at 100
  mutate(share = pmin(share, 10000))

# Calculate the number of unique IDs
num_unique_ids <- length(unique(df_trees$ID_new))



#-------------------------------------------------------------------------------
### time to min tree cover after disturbance
#-------------------------------------------------------------------------------

time_to_min <- function(df_trees) {
  
  #Find the minimum tree share value and the year when the minimum is reached within the 5-year window after the year of disturbance for each time series (ID)
  min_share_in_window <- df_trees %>%
    group_by(ID_new, yod) %>%
    filter(year >= yod, year <= yod + 3) %>%
    slice(which.min(share)) %>%
    ungroup()
  
  #Calculate the duration until the minimum is reached for each time series (ID)
  result <- min_share_in_window %>%
    mutate(time_to_min = year - yod) %>%
    select(ID_new, x, y, class, yod, min_year = year, time_to_min, min_tree_share = share) %>%
    distinct()
  
  # Convert share to proportion
  result <- result %>%
    mutate(min_tree_share = min_tree_share / 100)
  
  return(result)
}

# Call the function and store the result in a new data frame
t_min <- time_to_min(df_trees)

### works!
min <- ggplot(t_min, aes(x = time_to_min)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  #geom_density() +
  scale_x_continuous(breaks = c(0,1,2,3,4,5)) +
  labs(title = "Time until min tree cover is reached", x = "years after disturbance", y = "Frequency")


# Select only the relevant columns from t_min
t_min_selected <- t_min %>%
  select(ID_new, time_to_min, min_tree_share, min_year)

# Perform the left join on ID only, including only selected columns from t_min
df_trees_tmin <- df_trees %>%
  left_join(t_min_selected, by = "ID_new")


#-------------------------------------------------------------------------------
# fit GAM
#-------------------------------------------------------------------------------
# Adjust the share column by dividing by 100
df_trees_tmin <- df_trees_tmin %>%
  mutate(share = share / 100)

# Filter the data to include only relevant observations where year >= min_year
df_filtered <- df_trees_tmin %>%
  group_by(ID_new) %>%
  filter(year >= min_year) %>%
  ungroup()

# Function to fit models and get fitted values
fit_models_and_get_fitted <- function(data) {
  # Count the number of observations
  num_observations <- nrow(data)
  
  # Fit model based on number of observations
  if (num_observations < 20) {
    # Fit a linear model if observations are too few
    model <- lm(share ~ year, data = data)
  } else {
    # Fit a GAM if there are enough observations
    model <- gam(share ~ s(year, k = 3), data = data)
  }
  
  # Add fitted values to the data
  data <- data %>%
    mutate(GAM = predict(model, newdata = data))
  
  return(data)
}

# Apply the function to each group and combine results
GAM <- df_filtered %>%
  group_by(ID_new) %>%
  do(fit_models_and_get_fitted(.)) %>%
  ungroup()

# Assuming your data frame is named df
GAM <- GAM %>%
  mutate(GAM = case_when(
    GAM > 100 ~ 100,
    GAM < 0 ~ 0,
    TRUE ~ GAM
  ))


# Merge the complete dataframe with the GAM fitted values
df_combined <- df_trees_tmin %>%
  left_join(GAM, by = c("ID_new", "year")) %>%
  left_join(df_trees_tmin, by = c("ID_new", "year"))

# Replace NA in GAM_Share with the original Share value
df_combined <- df_combined %>%
  mutate(GAM_share = ifelse(is.na(GAM), share, GAM))

# Select the relevant columns
GAM <- df_combined %>%
  select(x, y, ID_new, year, yod, share, GAM_share, class, time_to_min, min_year, min_tree_share)


GAM_fitted <- GAM

GAM_fitted$share <- GAM_fitted$share * 10000
GAM_fitted$GAM_share <- GAM_fitted$GAM_share * 10000

# set GAM_share values > 100 to 100
# Set values in 'col' greater than 100 to 100
GAM_fitted$GAM_share[GAM_fitted$GAM_share > 100] <- 100


#-------------------------------------------------------------------------------
### severity
#-------------------------------------------------------------------------------

#Calculate the average tree share in a 5-year time window before the disturbance event for each time series (ID)
severity <- GAM_fitted %>%
  group_by(ID_new) %>%
  arrange(year) %>%
  mutate(tree_share_before = quantile(GAM_share[year > (yod - 20) & year <= yod], 
                                      probs = 0.9, 
                                      na.rm = TRUE))

# Step 4: Calculate the severity of the disturbance
severity <- severity %>%
  filter(!is.na(yod)) %>%
  mutate(
    severity_absolute = tree_share_before - min_tree_share,
    severity_relative = ifelse(tree_share_before == 0, NA, ((tree_share_before - min_tree_share) / tree_share_before) * 100)
  ) %>%
  select(x, y, ID_new, year, yod, share, GAM_share, class, time_to_min, min_year, 
         min_tree_share, tree_share_before, severity_absolute, severity_relative)


#Step 2: Compute 80% of the tree_share_before for each time series
severity <- severity %>% #severity
  mutate(tree_share_80 = tree_share_before * 0.85)


# Step 1: Compute whether the recovery condition is met
recovery <- severity %>%
  group_by(ID_new) %>%
  mutate(
    # Check if the condition is met
    condition_met = year > yod & GAM_share > tree_share_80
  ) %>%
  ungroup()

# Step 2: Identify the year when the recovery condition is met in two consecutive years
recovery <- recovery %>%
  group_by(ID_new) %>%
  arrange(ID_new, year) %>%  # Ensure data is sorted by year within each ID
  mutate(
    # Shift the condition_met column to check the next year
    next_year_condition = lead(condition_met, order_by = year),
    # Compute whether both the current and next year meet the condition
    consecutive_recovery = condition_met & next_year_condition
  ) %>%
  # Identify the first year where the consecutive recovery condition is true
  mutate(
    year_recov = if_else(consecutive_recovery & !duplicated(consecutive_recovery),
                         year,
                         NA_integer_)
  ) %>%
  # Remove intermediate columns
  select(-condition_met, -next_year_condition, -consecutive_recovery) %>%
  ungroup()

# Step 3: Compute the recovery rate
recovery <- recovery %>%
  mutate(
    recovery_rate = if_else(!is.na(year_recov), year_recov - yod, 100)
  )


# Reclassify severity
recovery <- recovery %>%
  mutate(severity_class = case_when(
    severity_relative <= 80 ~ "non stand-replacing",
    severity_relative > 80 ~ "stand-replacing",
    TRUE ~ NA_character_  # Ensure that NA values are handled
  ))

# Step 1: Subset for recovery_rate < 100
recovery_subset <- recovery %>% filter(recovery_rate < 100)

# Step 2: Remove duplicate rows based on the ID column, keeping the first occurrence
recovery_unique <- recovery_subset %>% distinct(ID_new, .keep_all = TRUE)


# just go with one obserbation per ID (=time series)
recovery_unique <- recovery %>%
  distinct(ID_new, .keep_all = TRUE)

### plot
# Remove rows with NA in severity_class
recovery_filtered <- recovery %>%
  filter(!is.na(severity_class))

# Define custom colors for severity_class
custom_colors <- c(
  "non stand-replacing" = "blue",
  "stand-replacing" = "red"
)

# Create a density plot for severity_relative
ggplot(recovery_filtered, aes(x = recovery_rate, color = severity_class, fill = severity_class)) +
  geom_density(alpha = 0.5, bw = 0.75) +
  labs(
    title = "Recovery rate by severity level",
    x = "Recovery Rate (%)",
    y = "Density"
  ) +
  xlim(0, 38) + 
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()

# Remove rows with NA in severity_class
recovery_unique_filtered <- recovery_unique %>%
  filter(!is.na(severity_class))

ggplot(recovery_unique_filtered, aes(x = recovery_rate, color = severity_class, fill = severity_class)) +
  geom_density(alpha = 0.5, bw = 0.75) +
  labs(
    title = "Recovery rate by severity level",
    x = "Recovery Rate (%)",
    y = "Density"
  ) +
  xlim(0, 38) + 
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()


### write
### write
write.csv(recovery_unique, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_0608_unique.csv", row.names=FALSE)
