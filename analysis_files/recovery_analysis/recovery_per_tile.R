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
library(pryr)
library(mgcv)
library(purrr)
library(readr)
install.packages("mgcv")
library(mgcv)


#-------------------------------------------------------------------------------
### preprocessing done
### continue with:  ~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_0608.csv

recovery_IV <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_0608.csv")


###load df
fcover_X29Y28 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X29Y28.csv")


# compute min value of tree share after 5 years after disturbance
# Step 1: Filter the data for class == "trees"
df_trees <- fcover_X29Y28 %>%
  filter(class %in% c("coniferous", "broadleaved"))

df_trees <- fcover_X29Y28 %>%
  filter(class %in% c("coniferous", "broadleaved")) %>%
  mutate(class_l1 = "trees")

# as numeric
df_trees$year <- as.numeric(df_trees$year)
df_trees$yod <- as.numeric(df_trees$yod)

# sum broadleaved and coniferous
df_trees <- df_trees %>%
  # Group by the columns that should remain unchanged
  group_by(x, y, ID, year, yod) %>%
  # Summarize by summing shares and assigning "trees" to the class
  summarise(
    share = sum(share, na.rm = TRUE),
    class = "trees",
    .groups = 'drop'
  ) %>%
  # Cap the share values at 100
  mutate(share = pmin(share, 10000))

#-------------------------------------------------------------------------------
### time to min tree cover after disturbance
#-------------------------------------------------------------------------------

time_to_min <- function(df_trees) {
  
  #Find the minimum tree share value and the year when the minimum is reached within the 5-year window after the year of disturbance for each time series (ID)
  min_share_in_window <- df_trees %>%
    group_by(ID, yod) %>%
    filter(year >= yod, year <= yod + 3) %>%
    slice(which.min(share)) %>%
    ungroup()
  
  #Calculate the duration until the minimum is reached for each time series (ID)
  result <- min_share_in_window %>%
    mutate(time_to_min = year - yod) %>%
    select(ID, x, y, class, yod, min_year = year, time_to_min, min_tree_share = share) %>%
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
  select(ID, time_to_min, min_tree_share, min_year)

# Perform the left join on ID only, including only selected columns from t_min
df_trees_tmin <- df_trees %>%
  left_join(t_min_selected, by = "ID")


#-------------------------------------------------------------------------------
# fit GAM
#-------------------------------------------------------------------------------
# Adjust the share column by dividing by 100
df_trees_tmin <- df_trees_tmin %>%
  mutate(share = share / 100)

# Filter the data to include only relevant observations where year >= min_year
df_filtered <- df_trees_tmin %>%
  group_by(ID) %>%
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
  group_by(ID) %>%
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
  left_join(GAM, by = c("ID", "year")) %>%
  left_join(df_trees_tmin, by = c("ID", "year"))

# Replace NA in GAM_Share with the original Share value
df_combined <- df_combined %>%
  mutate(GAM_share = ifelse(is.na(GAM), share, GAM))

# Select the relevant columns
GAM <- df_combined %>%
  select(x, y, ID, year, yod, share, GAM_share, class, time_to_min, min_year, min_tree_share)


GAM_fitted <- GAM
#-------------------------------------------------------------------------------
# severity function
#-------------------------------------------------------------------------------

#Calculate the average tree share in a 5-year time window before the disturbance event for each time series (ID)
avg_tree_share_5y_before <- GAM_fitted %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(avg_tree_share_before = mean(GAM_share[year >= (min_year - 20) & year < min_year],
                                    na.rm = TRUE))

# Step 4: Calculate the severity of the disturbance
severity <- avg_tree_share_5y_before %>%
  filter(!is.na(yod)) %>%
  mutate(
    severity_absolute = avg_tree_share_before - min_tree_share,
    severity_relative = ifelse(avg_tree_share_before == 0, NA, ((avg_tree_share_before - min_tree_share) / avg_tree_share_before) * 100)
  ) %>%
  select(x, y, ID, year, yod, share, GAM_share, class, time_to_min, min_year, 
          min_tree_share, avg_tree_share_before, severity_absolute, severity_relative)
  
  
# Create a density plot for severity_relative
  ggplot(severity, aes(x = severity_relative)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(
      title = "Density Plot of Severity Relative",
      x = "Severity Relative (%)",
      y = "Density"
    ) +
    xlim(0, 100) + 
    theme_minimal()

#-------------------------------------------------------------------------------
### recovery
#-------------------------------------------------------------------------------

recovery <- severity %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(tree_share_before = mean(GAM_share[year > (yod - 20) & year <= yod], 
                                    na.rm = TRUE))
  
  
#Step 2: Compute 80% of the tree_share_before for each time series
recovery <- recovery %>% #severity
  mutate(tree_share_80 = tree_share_before * 0.8)
  
  
# Step 3: Find the year when share exceeds tree_share_80 for at least two consecutive years after yod
recovery <- recovery %>%
  group_by(ID) %>%
  mutate(
    reached_80 = cumsum(if_else(year > (yod) & GAM_share > tree_share_80, 
                                1, 0)),
    year_recov = case_when(
      any(reached_80 >= 3) ~ year[which.max(reached_80 > 2)],
      TRUE ~ NA_integer_
    )
  ) %>%
    ungroup() %>%
    select(-reached_80)
  
# Step 4: Compute the recovery_rate
recovery <- recovery %>%
  mutate(
    recovery_rate = if_else(!is.na(year_recov), year_recov - yod, 100)
  )

# Create a density plot for severity_relative
ggplot(recovery, aes(x = recovery_rate)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(
    title = "Density Plot of Severity Relative",
    x = "Severity Relative (%)",
    y = "Density"
  ) +
  xlim(0, 38) + 
  theme_minimal()

# reclassify severity
recovery <- recovery %>%
  mutate(severity_class = case_when(
    severity_relative <= 80 ~ "non stand-replacing",
    severity_relative > 80 ~ "stand-replacing" 
  ))


proportions <- recovery %>%
  group_by(severity_class) %>%
  summarise(num_observations = n()) %>%
  mutate(proportion = num_observations / sum(num_observations))

print(proportions)


recovery_unique <- recovery %>%
  group_by(ID) %>%
  distinct(ID, .keep_all = TRUE) %>%
  ungroup()

# write
write.csv(recovery, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_X29Y28.csv", row.names=FALSE)
write.csv(recovery_unique_X29Y28, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_X29Y28_unique.csv", row.names=FALSE)


#-------------------------------------------------------------------------------
fcover_X28Y28 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X28Y28.csv")
fcover_X28Y29 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X28Y29.csv")
fcover_X29Y28 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X29Y28.csv")
fcover_X29Y29 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X29Y29.csv")
fcover_X29Y30 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X29Y30.csv")
fcover_X30Y27 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X30Y27.csv")
fcover_X30Y28 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X30Y28.csv")
fcover_X30Y29 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X30Y29.csv")
fcover_X31Y27 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X31Y27.csv")

# rowbind
# List of data frame names
data_frames <- list(fcover_X28Y28, fcover_X28Y29, fcover_X29Y29, fcover_X29Y28,
                    fcover_X29Y30, fcover_X30Y27, fcover_X30Y28, fcover_X30Y29, fcover_X31Y27)  # Replace with the actual names of your data frames

# Combine data frames
combined_data <- bind_rows(data_frames)



# compute min value of tree share after 5 years after disturbance
# Step 1: Filter the data for class == "trees"
df_trees <- combined_data %>%
  filter(class %in% c("coniferous", "broadleaved"))

df_trees <- combined_data %>%
  filter(class %in% c("coniferous", "broadleaved")) %>%
  mutate(class_l1 = "trees")

# as numeric
df_trees$year <- as.numeric(df_trees$year)
df_trees$yod <- as.numeric(df_trees$yod)

# sum broadleaved and coniferous
df_trees <- df_trees %>%
  # Group by the columns that should remain unchanged
  group_by(x, y, ID, year, yod) %>%
  # Summarize by summing shares and assigning "trees" to the class
  summarise(
    share = sum(share, na.rm = TRUE),
    class = "trees",
    .groups = 'drop'
  ) %>%
  # Cap the share values at 100
  mutate(share = pmin(share, 10000))

#-------------------------------------------------------------------------------
### time to min tree cover after disturbance
#-------------------------------------------------------------------------------

time_to_min <- function(df_trees) {
  
  #Find the minimum tree share value and the year when the minimum is reached within the 5-year window after the year of disturbance for each time series (ID)
  min_share_in_window <- df_trees %>%
    group_by(ID, yod) %>%
    filter(year >= yod, year <= yod + 3) %>%
    slice(which.min(share)) %>%
    ungroup()
  
  #Calculate the duration until the minimum is reached for each time series (ID)
  result <- min_share_in_window %>%
    mutate(time_to_min = year - yod) %>%
    select(ID, x, y, class, yod, min_year = year, time_to_min, min_tree_share = share) %>%
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
  select(ID, time_to_min, min_tree_share, min_year)

# Perform the left join on ID only, including only selected columns from t_min
df_trees_tmin <- df_trees %>%
  left_join(t_min_selected, by = "ID")


#-------------------------------------------------------------------------------
# fit GAM
#-------------------------------------------------------------------------------
# Adjust the share column by dividing by 100
df_trees_tmin <- df_trees_tmin %>%
  mutate(share = share / 100)

# Filter the data to include only relevant observations where year >= min_year
df_filtered <- df_trees_tmin %>%
  group_by(ID) %>%
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
  group_by(ID) %>%
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
  left_join(GAM, by = c("ID", "year")) %>%
  left_join(df_trees_tmin, by = c("ID", "year"))

# Replace NA in GAM_Share with the original Share value
df_combined <- df_combined %>%
  mutate(GAM_share = ifelse(is.na(GAM), share, GAM))

# Select the relevant columns
GAM <- df_combined %>%
  select(x, y, ID, year, yod, share, GAM_share, class, time_to_min, min_year, min_tree_share)


GAM_fitted <- GAM
#-------------------------------------------------------------------------------
# severity function
#-------------------------------------------------------------------------------

#Calculate the average tree share in a 5-year time window before the disturbance event for each time series (ID)
avg_tree_share_5y_before <- GAM_fitted %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(avg_tree_share_before = mean(GAM_share[year >= (min_year - 20) & year < min_year],
                                      na.rm = TRUE))

# Step 4: Calculate the severity of the disturbance
severity <- avg_tree_share_5y_before %>%
  filter(!is.na(yod)) %>%
  mutate(
    severity_absolute = avg_tree_share_before - min_tree_share,
    severity_relative = ifelse(avg_tree_share_before == 0, NA, ((avg_tree_share_before - min_tree_share) / avg_tree_share_before) * 100)
  ) %>%
  select(x, y, ID, year, yod, share, GAM_share, class, time_to_min, min_year, 
         min_tree_share, avg_tree_share_before, severity_absolute, severity_relative)


# Create a density plot for severity_relative
ggplot(severity, aes(x = severity_relative)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(
    title = "Density Plot of Severity Relative",
    x = "Severity Relative (%)",
    y = "Density"
  ) +
  xlim(0, 100) + 
  theme_minimal()

#-------------------------------------------------------------------------------
### recovery
#-------------------------------------------------------------------------------

recovery <- severity %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(tree_share_before = max(GAM_share[year > (yod - 20) & year <= yod], 
                                  na.rm = TRUE))

recovery <- severity %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(tree_share_before = quantile(GAM_share[year > (yod - 20) & year <= yod], 
                                      probs = 0.75, 
                                      na.rm = TRUE))


#Step 2: Compute 80% of the tree_share_before for each time series
recovery <- recovery %>% #severity
  mutate(tree_share_80 = tree_share_before * 0.8)


# Step 3: Find the year when share exceeds tree_share_80 for at least two consecutive years after yod
recovery <- recovery %>%
  group_by(ID) %>%
  mutate(
    reached_80 = cumsum(if_else(year > (yod) & GAM_share > tree_share_80, 
                                1, 0)),
    year_recov = case_when(
      any(reached_80 >= 3) ~ year[which.max(reached_80 > 2)],
      TRUE ~ NA_integer_
    )
  ) %>%
  ungroup() %>%
  select(-reached_80)

# Step 4: Compute the recovery_rate
recovery <- recovery %>%
  mutate(
    recovery_rate = if_else(!is.na(year_recov), year_recov - yod, 100)
  )

# Create a density plot for severity_relative
ggplot(recovery, aes(x = recovery_rate)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(
    title = "Density Plot of Severity Relative",
    x = "Severity Relative (%)",
    y = "Density"
  ) +
  xlim(0, 38) + 
  theme_minimal()

# reclassify severity
recovery <- recovery %>%
  mutate(severity_class = case_when(
    severity_relative <= 80 ~ "non stand-replacing",
    severity_relative > 80 ~ "stand-replacing" 
  ))


proportions <- recovery %>%
  group_by(severity_class) %>%
  summarise(num_observations = n()) %>%
  mutate(proportion = num_observations / sum(num_observations))

print(proportions)


recovery_unique <- recovery %>%
  group_by(ID) %>%
  distinct(ID, .keep_all = TRUE) %>%
  ungroup()

# write
write.csv(recovery, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_I.csv", row.names=FALSE)
write.csv(recovery_unique_X29Y28, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_I_unique.csv", row.names=FALSE)


#-------------------------------------------------------------------------------
fcover_X31Y28 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X31Y28.csv")
fcover_X31Y29 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X31Y29.csv")
fcover_X32Y27 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X32Y27.csv")
fcover_X32Y28 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X32Y28.csv")
fcover_X32Y29 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X32Y29.csv")
fcover_X33Y27 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X33Y27.csv")
fcover_X33Y28 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X33Y28.csv")
fcover_X33Y29 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X33Y29.csv")
fcover_X34Y27 <- read_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X34Y27.csv")

# rowbind
# List of data frame names
data_frames <- list(fcover_X31Y28, fcover_X31Y29, fcover_X32Y27, fcover_X32Y28,
                    fcover_X32Y29, fcover_X33Y27, fcover_X33Y28, fcover_X33Y29, fcover_X34Y27)  # Replace with the actual names of your data frames

# Combine data frames
combined_data_II <- bind_rows(data_frames)



# compute min value of tree share after 5 years after disturbance
# Step 1: Filter the data for class == "trees"
df_trees <- combined_data_II %>%
  filter(class %in% c("coniferous", "broadleaved"))

df_trees <- combined_data_II %>%
  filter(class %in% c("coniferous", "broadleaved")) %>%
  mutate(class_l1 = "trees")

# as numeric
df_trees$year <- as.numeric(df_trees$year)
df_trees$yod <- as.numeric(df_trees$yod)

# sum broadleaved and coniferous
df_trees <- df_trees %>%
  # Group by the columns that should remain unchanged
  group_by(x, y, ID, year, yod) %>%
  # Summarize by summing shares and assigning "trees" to the class
  summarise(
    share = sum(share, na.rm = TRUE),
    class = "trees",
    .groups = 'drop'
  ) %>%
  # Cap the share values at 100
  mutate(share = pmin(share, 10000))

#-------------------------------------------------------------------------------
### time to min tree cover after disturbance
#-------------------------------------------------------------------------------

time_to_min <- function(df_trees) {
  
  #Find the minimum tree share value and the year when the minimum is reached within the 5-year window after the year of disturbance for each time series (ID)
  min_share_in_window <- df_trees %>%
    group_by(ID, yod) %>%
    filter(year >= yod, year <= yod + 3) %>%
    slice(which.min(share)) %>%
    ungroup()
  
  #Calculate the duration until the minimum is reached for each time series (ID)
  result <- min_share_in_window %>%
    mutate(time_to_min = year - yod) %>%
    select(ID, x, y, class, yod, min_year = year, time_to_min, min_tree_share = share) %>%
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
  select(ID, time_to_min, min_tree_share, min_year)

# Perform the left join on ID only, including only selected columns from t_min
df_trees_tmin <- df_trees %>%
  left_join(t_min_selected, by = "ID")


#-------------------------------------------------------------------------------
# fit GAM
#-------------------------------------------------------------------------------
# Adjust the share column by dividing by 100
df_trees_tmin <- df_trees_tmin %>%
  mutate(share = share / 100)

# Filter the data to include only relevant observations where year >= min_year
df_filtered <- df_trees_tmin %>%
  group_by(ID) %>%
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
  group_by(ID) %>%
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
  left_join(GAM, by = c("ID", "year")) %>%
  left_join(df_trees_tmin, by = c("ID", "year"))

# Replace NA in GAM_Share with the original Share value
df_combined <- df_combined %>%
  mutate(GAM_share = ifelse(is.na(GAM), share, GAM))

# Select the relevant columns
GAM <- df_combined %>%
  select(x, y, ID, year, yod, share, GAM_share, class, time_to_min, min_year, min_tree_share)


GAM_fitted <- GAM
#-------------------------------------------------------------------------------
# severity function
#-------------------------------------------------------------------------------

#Calculate the average tree share in a 5-year time window before the disturbance event for each time series (ID)
avg_tree_share_5y_before <- GAM_fitted %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(avg_tree_share_before = mean(GAM_share[year >= (min_year - 20) & year < min_year],
                                      na.rm = TRUE))

# Step 4: Calculate the severity of the disturbance
severity <- avg_tree_share_5y_before %>%
  filter(!is.na(yod)) %>%
  mutate(
    severity_absolute = avg_tree_share_before - min_tree_share,
    severity_relative = ifelse(avg_tree_share_before == 0, NA, ((avg_tree_share_before - min_tree_share) / avg_tree_share_before) * 100)
  ) %>%
  select(x, y, ID, year, yod, share, GAM_share, class, time_to_min, min_year, 
         min_tree_share, avg_tree_share_before, severity_absolute, severity_relative)


# Create a density plot for severity_relative
ggplot(severity, aes(x = severity_relative)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(
    title = "Density Plot of Severity Relative",
    x = "Severity Relative (%)",
    y = "Density"
  ) +
  xlim(0, 100) + 
  theme_minimal()

#-------------------------------------------------------------------------------
### recovery
#-------------------------------------------------------------------------------

recovery <- severity %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(tree_share_before = max(GAM_share[year > (yod - 20) & year <= yod], 
                                 na.rm = TRUE))

recovery_II <- severity %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(tree_share_before = quantile(GAM_share[year > (yod - 20) & year <= yod], 
                                      probs = 0.75, 
                                      na.rm = TRUE))


#Step 2: Compute 80% of the tree_share_before for each time series
recovery_II <- recovery_II %>% #severity
  mutate(tree_share_80 = tree_share_before * 0.8)


# Step 3: Find the year when share exceeds tree_share_80 for at least two consecutive years after yod
recovery_II <- recovery_II %>%
  group_by(ID) %>%
  mutate(
    reached_80 = cumsum(if_else(year > (yod) & GAM_share > tree_share_80, 
                                1, 0)),
    year_recov = case_when(
      any(reached_80 >= 3) ~ year[which.max(reached_80 > 2)],
      TRUE ~ NA_integer_
    )
  ) %>%
  ungroup() %>%
  select(-reached_80)

# Step 4: Compute the recovery_rate
recovery_II <- recovery_II %>%
  mutate(
    recovery_rate = if_else(!is.na(year_recov), year_recov - yod, 100)
  )

# Create a density plot for severity_relative
ggplot(recovery_II, aes(x = recovery_rate)) +
  geom_density(fill = "blue", alpha = 0.5, bw = 0.75) +
  labs(
    title = "Density Plot of Severity Relative",
    x = "Severity Relative (%)",
    y = "Density"
  ) +
  xlim(0, 38) + 
  theme_minimal()

# reclassify severity
recovery_II <- recovery_II %>%
  mutate(severity_class = case_when(
    severity_relative <= 80 ~ "non stand-replacing",
    severity_relative > 80 ~ "stand-replacing" 
  ))


proportions <- recovery %>%
  group_by(severity_class) %>%
  summarise(num_observations = n()) %>%
  mutate(proportion = num_observations / sum(num_observations))

print(proportions)


recovery_unique <- recovery %>%
  group_by(ID) %>%
  distinct(ID, .keep_all = TRUE) %>%
  ungroup()

# write
write.csv(recovery, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_X29Y28.csv", row.names=FALSE)
write.csv(recovery_unique_X29Y28, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_X29Y28_unique.csv", row.names=FALSE)

#-------------------------------------------------------------------------------
# List of data frame names
data_frames <- list(recovery,recovery_II)  # Replace with the actual names of your data frames

# Combine data frames
recovery_all <- bind_rows(data_frames)


ggplot(recovery_all, aes(x = recovery_rate, color = severity_class, fill = severity_class)) +
  geom_density(alpha = 0.5, bw = 0.7) +  # Adjust the `alpha` parameter as needed
  labs(
    title = "Density Plot of Severity Relative by Severity Level",
    x = "Severity Relative (%)",
    y = "Density"
  ) +
  xlim(0, 38) + 
  theme_minimal()



#-------------------------------------------------------------------------------

#Calculate the average tree share in a 5-year time window before the disturbance event for each time series (ID)
avg_tree_share_5y_before <- recovery_all %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(avg_tree_share_before = quantile(GAM_share[year >= (yod - 20) & year < yod],
                                          probs = 0.9, 
                                          na.rm = TRUE))

# Step 4: Calculate the severity of the disturbance
severity <- avg_tree_share_5y_before %>%
  filter(!is.na(yod)) %>%
  mutate(
    severity_absolute = avg_tree_share_before - min_tree_share,
    severity_relative = ifelse(avg_tree_share_before == 0, NA, ((avg_tree_share_before - min_tree_share) / avg_tree_share_before) * 100)
  ) %>%
  select(x, y, ID, year, yod, share, GAM_share, class, time_to_min, min_year, 
         min_tree_share, avg_tree_share_before, severity_absolute, severity_relative)


# Create a density plot for severity_relative
ggplot(severity, aes(x = severity_relative)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(
    title = "Density Plot of Severity Relative",
    x = "Severity Relative (%)",
    y = "Density"
  ) +
  xlim(0, 100) + 
  theme_minimal()

#-------------------------------------------------------------------------------
### recovery
#-------------------------------------------------------------------------------

#recovery <- severity %>%
  #group_by(ID) %>%
  #arrange(year) %>%
  #mutate(tree_share_before = max(GAM_share[year > (yod - 20) & year <= yod], 
                                 #na.rm = TRUE))

recovery_II <- severity %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(tree_share_before = quantile(GAM_share[year > (yod - 20) & year <= yod], 
                                      probs = 0.9, 
                                      na.rm = TRUE))


#Step 2: Compute 80% of the tree_share_before for each time series
recovery_II <- recovery_II %>% #severity
  mutate(tree_share_80 = tree_share_before * 0.85)


# Step 1: Compute whether the recovery condition is met
recovery_III <- recovery_II %>%
  group_by(ID) %>%
  mutate(
    # Check if the condition is met
    condition_met = year > yod & GAM_share > tree_share_80
  ) %>%
  ungroup()

# Step 2: Identify the year when the recovery condition is met in two consecutive years
recovery_III <- recovery_III %>%
  group_by(ID) %>%
  arrange(ID, year) %>%  # Ensure data is sorted by year within each ID
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
recovery_III <- recovery_III %>%
  mutate(
    recovery_rate = if_else(!is.na(year_recov), year_recov - yod, 100)
  )


# Reclassify severity
recovery_III <- recovery_III %>%
  mutate(severity_class = case_when(
    severity_relative <= 80 ~ "non stand-replacing",
    severity_relative > 80 ~ "stand-replacing",
    TRUE ~ NA_character_  # Ensure that NA values are handled
  ))

### write
write.csv(recovery_III, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_all.csv", row.names=FALSE)

#-------------------------------------------------------------------------------

# Remove rows with NA in severity_class
recovery_filtered <- recovery %>%
  filter(!is.na(severity_class))

# Define custom colors for severity_class
custom_colors <- c(
  "non stand-replacing" = "blue",
  "stand-replacing" = "red"
)

# Create a density plot for severity_relative
png("~/eo_nas/EO4Alps/00_analysis/figs/rcovery_intervals_severity.png", 
    units="in", width=8, height=4, res=300)
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
dev.off()

recovery_unique <- recovery_III %>%
  group_by(ID) %>%
  distinct(ID, .keep_all = TRUE) %>%
  ungroup()


# Remove rows with NA in severity_class
recovery_unique_filtered <- recovery_unique %>%
  filter(!is.na(severity_class))

# Define custom colors for severity_class
custom_colors <- c(
  "non stand-replacing" = "blue",
  "stand-replacing" = "red"
)

# Create a density plot for severity_relative
ggplot(recovery_unique_filtered, aes(x = recovery_rate, color = severity_class, fill = severity_class)) +
  geom_density(alpha = 0.5, bw = 0.75) +
  labs(
    title = "Density Plot of Recovery Rate by Severity Class",
    x = "Recovery Rate (%)",
    y = "Density"
  ) +
  xlim(0, 38) + 
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()


### mean recovery intervals
recovery_mean <- recovery_III %>%
  filter(recovery_rate <= 38) %>%
  group_by(severity_class) %>%
  summarize(mean = mean(recovery_rate))



# Filter the data for pixels that meet the baseline condition within 10 years
recovery_10y <- recovery_III %>%
  filter(GAM_share >= tree_share_80, year <= yod + 10, year >= yod)

# Count the total number of observations within each severity_class
total_counts <- recovery_III %>%
  filter(year <= yod + 10, year >= yod) %>%
  group_by(severity_class) %>%
  summarise(total_observations = n())

# Count the number of observations that meet the baseline condition within 10 years
baseline_counts <- recovery_10y %>%
  group_by(severity_class) %>%
  summarise(num_observations = n())

# Join total counts with baseline counts and calculate percentages
result <- baseline_counts %>%
  left_join(total_counts, by = "severity_class") %>%
  mutate(
    percentage = (num_observations / total_observations) * 100
  ) %>%
  select(severity_class, percentage)

# Print the result
print(result)

# View the result
print(result)

# 24% of non-stand replcaing disturbances recover within 10y
# only 9% of stand-replacing disturbances recover within 10 years


# Calculate the percentage of observations in each severity class
severity_percentage <- recovery_III %>%
  group_by(severity_class) %>%
  summarise(
    count = n()  # Count the number of observations in each severity class
  ) %>%
  mutate(
    total = sum(count),  # Calculate the total number of observations
    percentage = (count / total) * 100  # Calculate the percentage
  ) %>%
  select(severity_class, count, percentage)  # Select relevant columns

# Print the result
print(severity_percentage)



# proportino of all disturbances per severity categroy
# Step 1: Compute the proportion of observations for each severity category
proportions <- recovery %>%
  group_by(severity_class) %>%
  summarise(num_observations = n()) %>%
  mutate(proportion = num_observations / sum(num_observations))


# compute standard error
se_by_strata <- recovery_III %>%
  filter(recovery_rate <= 38) %>%
  group_by(severity_class) %>%
  summarize(standard_error = sd(recovery_rate, na.rm = TRUE) / sqrt(sum(!is.na(recovery_rate))))

print(se_by_strata)
# non-sr: 0.0531
# sr: 0.0505

sd_by_strata <- recovery_III %>%
  filter(recovery_rate <= 38) %>%
  group_by(severity_class) %>%
  summarize(standard_deviation = sd(recovery_rate, na.rm = TRUE))

print(sd_by_strata)
# non sr: 7.76
# sr: 7.22

#-------------------------------------------------------------------------------
### ysd

# Compute years since disturbance (ysd)
recovery_IV <- recovery_III %>%
  mutate(ysd = year - yod)

# Filter out only the records where ysd is positive and less than or equal to 38
recovery_IV_filtered <- recovery_IV %>%
  filter(ysd >= 1 & ysd <= 37)

# Count the number of pixels with each recovery rate
recovery_counts <- recovery_IV_filtered %>%
  count(recovery_rate) %>%
  rename(ysd = recovery_rate, count = n)

# Calculate the total number of observations
total_observations <- nrow(recovery_IV_filtered)

# Calculate the cumulative count and cumulative percentage
recovery_cumulative <- recovery_counts %>%
  arrange(ysd) %>%
  mutate(cumulative_count = cumsum(count)) %>%
  mutate(cumulative_perc = (cumulative_count / total_observations) * 100)

# Create the final data frame
recovery_within <- recovery_cumulative %>%
  select(ysd, cumulative_perc)

# plot
ggplot(recovery_within, aes(x = ysd, y = cumulative_perc)) +
  #geom_line() +
  geom_smooth(method = "auto", se = TRUE,size = 0.5, span = 1.25) +  # Facet by 'type' column in two columns
  labs(x = "Years since disturbance", y = "Percentage of \nrecovered disturbances", color = "Recovery") +
  scale_x_continuous(
    limits = c(0, 38),  # Set the range of values shown on the x-axis
    breaks = seq(0, 38, by = 5))




# keep severity class

# Filter out only the records where ysd is positive and less than or equal to 38
recovery_IV_filtered <- recovery_IV %>%
  filter(ysd >= 1 & ysd <= 37)

# Count the number of pixels with each recovery rate and severity class
recovery_counts <- recovery_IV_filtered %>%
  group_by(severity_class, recovery_rate) %>%
  count() %>%
  rename(ysd = recovery_rate, count = n) %>%
  ungroup() %>%
  group_by(severity_class) %>%
  mutate(total_pixels = sum(count)) %>%
  ungroup()

# Calculate cumulative count and percentage within each severity class
recovery_cumulative <- recovery_counts %>%
  arrange(severity_class, ysd) %>%
  group_by(severity_class) %>%
  mutate(cumulative_count = cumsum(count)) %>%
  mutate(cumulative_perc = (cumulative_count / total_pixels) * 100) %>%
  ungroup()


# Create the final data frame with severity class
recovery_within_I <- recovery_cumulative %>%
  select(severity_class, ysd, cumulative_perc)

# Multiply cumulative_perc by 100
recovery_within_I <- recovery_cumulative %>%
  mutate(cumulative_perc = cumulative_perc * 100) %>%
  select(severity_class, ysd, cumulative_perc)


recovery_within_I <- recovery_within_I %>%
  filter(!is.na(severity_class))

# plot
png("~/eo_nas/EO4Alps/00_analysis/figs/recovered_within.png", 
    units="in", width=6, height=4, res=300)
ggplot(recovery_within_I, aes(x = ysd, y = cumulative_perc, color = severity_class)) +
  #geom_line() +
  geom_smooth(method = "auto", se = TRUE,size = 0.5, span = 1.25) +  # Facet by 'type' column in two columns
  labs(x = "Years since disturbance", y = "Percentage of \nrecovered disturbances", color = "Recovery") +
  scale_y_continuous(limits = c(0, 100)) + 
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_x_continuous(
    limits = c(0, 38),  # Set the range of values shown on the x-axis
    breaks = seq(0, 38, by = 2)) +
  theme_bw()
dev.off()

write.csv(recovery_within_I, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_within.csv", row.names=FALSE)
write.csv(recovery_IV, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_0608.csv", row.names=FALSE)

# seems not to work...
recovery_unique <- recovery_IV %>%
  distinct(ID, .keep_all = TRUE)

# Filter the data frame to keep rows where year equals yod
recovery_unique <- recovery_IV %>%
  filter(year == yod)


write.csv(recovery_unique, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_unique_0608.csv", row.names=FALSE)


