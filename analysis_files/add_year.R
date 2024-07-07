# Load necessary library
library(dplyr)
library(ggplot2)
library(zoo)

# Set the directory containing the files
setwd("/data/eo/EO4Alps/level3/NDVI/features")

# List all text files in the directory
file_list <- list.files(pattern = "features_\\d{4}\\.txt")

# Function to process each file
process_file <- function(file_name) {
  # Extract the year from the file name
  year <- gsub("features_(\\d{4})\\.txt", "\\1", file_name)
  
  # Read the data from the file
  data <- read.table(file_name, header = FALSE, stringsAsFactors = FALSE)
  
  # Add the year column
  data <- data %>% mutate(Year = year)
  
  # Write the modified data back to the file
  write.table(data, file_name, row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
}

# Process each file in the list
lapply(file_list, process_file)


# rowbind all feature files
# List all text files in the directory
file_list <- list.files(pattern = "features_\\d{4}\\.txt")

# Initialize an empty list to store the data frames
data_list <- list()

# Function to read each file and add to the list
read_file <- function(file_name) {
  # Read the data from the file
  data <- read.table(file_name, header = FALSE, stringsAsFactors = FALSE, sep = ",")
  
  # Add the data frame to the list
  data_list <<- append(data_list, list(data))
}

# Read each file in the list
lapply(file_list, read_file)

# Combine all data frames into one
features_combined <- bind_rows(data_list)

# Write the combined data to a new file (optional)
write.table(features_combined, "combined_features.csv", row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)

# View the combined data frame (optional)
print(combined_data)


# Read the data from CSV file
combined_features <- read.csv("/data/eo/EO4Alps/level3/NDVI/combined_features.csv")

# Create the ID column based on unique combinations of X and Y
combined_features <- combined_features %>%
  group_by(X, Y) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()

# write
write.csv(combined_features, "/data/eo/EO4Alps/level3/NDVI/combined_features.csv", row.names = FALSE)

# Remove negative values and set them to NA for interpolation
combined_features_clean <- combined_features %>%
  mutate(value = ifelse(value < 0, NA, value))

# Perform linear interpolation by ID
combined_features_clean <- combined_features_clean %>%
  group_by(ID) %>%
  mutate(value = na.approx(value, na.rm = FALSE)) %>%
  ungroup()


# Filter out IDs with negative values
combined_features_clean <- combined_features %>%
  group_by(ID) %>%
  filter(all(value >= 0)) %>%
  ungroup()




# plot
ggplot(combined_features_clean, aes(x = year, y = value, group = ID, color = as.factor(ID))) + 
  geom_line() + 
  facet_wrap(~ class) + 
  theme_minimal() + 
  labs(x = "Year", y = "Value", title = "Values Over Time by Class and ID") +
  theme(legend.position = "none")  # Optionally remove legend if there are many IDs


summary_stats <- combined_features_clean %>%
  group_by(class) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE)) %>%
  ungroup()


# Plot the cleaned data with mean and ribbon
ggplot(combined_features_clean, aes(x = year, y = value, group = ID, color = as.factor(ID))) + 
  geom_line(alpha = 0.5) +  # Add lines for individual IDs with transparency
  geom_line(data = summary_stats, aes(y = mean_value, group = class), color = "black", size = 1.5) +  # Add mean line per class
  #geom_ribbon(data = summary_stats, aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value, group = class), alpha = 0.2) +  # Add ribbon for variability
  facet_wrap(~ class) + 
  theme_minimal() + 
  labs(x = "Year", y = "Value", title = "Values Over Time with Mean and Variability by Class") +
  theme(legend.position = "none")






