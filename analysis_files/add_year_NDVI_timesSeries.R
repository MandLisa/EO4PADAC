# Load necessary library
library(dplyr)
library(ggplot2)
library(zoo)
library(plotly)


### load files (then the next steps are not needed anymore!)
combined_features <- read_csv("/data/eo/EO4Alps/level3/NDVI/combined_features.csv")
combined_features_interpolated <- read_csv("/data/eo/EO4Alps/level3/NDVI/combined_features_interpolated.csv")

# unstable candidates removed
combined_features_cleaned <- read_csv("/data/eo/EO4Alps/level3/NDVI/combined_features_cleaned.csv")
combined_features_cleaned_classname <- read_csv("/data/eo/EO4Alps/level3/NDVI/combined_features_cleaned_classname.csv")

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


# interpolate in case value is negative
combined_features_clean1 <- combined_features %>%
  arrange(ID, year)

# Interpolate missing values and handle negative values
combined_features_clean1 <- combined_features_clean1 %>%
  group_by(ID, class) %>%
  mutate(value = ifelse(value < 0, NA, value),  # Convert negative values to NA
         value = na.approx(value, na.rm = FALSE, maxgap = Inf)) %>%
  ungroup()


# write
write.csv(combined_features_clean1, "/data/eo/EO4Alps/level3/NDVI/combined_features_interpolated.csv", row.names = FALSE)



# plot with all data points
ggplot(combined_features, aes(x = year, y = value, group = ID, color = as.factor(ID))) + 
  geom_line() + 
  facet_wrap(~ class) + 
  theme_bw() + 
  labs(x = "Year", y = "Value", title = "Values Over Time by Class and ID") +
  theme(legend.position = "none")  # Optionally remove legend if there are many IDs




# plot with cleaned dataset
ggplot(combined_features_clean, aes(x = year, y = value, group = ID, color = as.factor(ID))) + 
  geom_line() + 
  facet_wrap(~ class) + 
  theme_bw() + 
  labs(x = "Year", y = "Value", title = "Temporal stability of candidate pixels") +
  theme(legend.position = "none")  # Optionally remove legend if there are many IDs


# Calculate summary statistics
summary_stats <- combined_features_clean1 %>%
  group_by(class, year) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE)) %>%
  ungroup()

# Plot the cleaned data with mean and ribbon
ggplot() + 
  geom_line(data = combined_features_clean1, aes(x = year, y = value, group = ID, color = as.factor(ID)), alpha = 0.5) +  # Add lines for individual IDs with transparency
  geom_line(data = summary_stats, aes(x = year, y = mean_value, group = class), color = "black", size = 1.5) +  # Add mean line per class
  geom_ribbon(data = summary_stats, aes(x = year, ymin = mean_value - sd_value, ymax = mean_value + sd_value, group = class), fill = "grey", alpha = 0.55) +  # Add ribbon for variability
  facet_wrap(~ class) + 
  theme_bw() + 
  labs(x = "Year", y = "Value", title = "Temporal stability of candidate pixels") +
  theme(legend.position = "none")

# interactive plot

p <- ggplot() + 
  geom_line(data = combined_features_clean1, aes(x = year, y = value, group = ID, color = as.factor(ID)), alpha = 0.5) +  # Add lines for individual IDs with transparency
  geom_line(data = summary_stats, aes(x = year, y = mean_value, group = class), color = "black", size = 1.5) +  # Add mean line per class
  geom_ribbon(data = summary_stats, aes(x = year, ymin = mean_value - sd_value, ymax = mean_value + sd_value, group = class), fill = "grey", alpha = 0.55) +  # Add ribbon for variability
  facet_wrap(~ class) + 
  theme_bw() + 
  labs(x = "Year", y = "Value", title = "Temporal stability of candidate pixels") +
  theme(legend.position = "none")

# Convert to plotly interactive plot
interactive_plot <- ggplotly(p)

# Print interactive plot
print(interactive_plot)


# exclude ID outliers
# Define the IDs to exclude
ids_to_exclude <- c(406, 818, 103,457,597,289,326,127,824,189,821,289,6,2,3,1,517,20,
                    179,20,511,517,544,738,143,10,154,167,166,416,260,264,386,327,508,724,524,
                    235,254,525,307,112,76,264,809,341,172,175,177,336,171,172,490,63,599,169,598,
                    60,64,163,476,566,181,606,654,476,385,691,185,521,185,222,310,308,768,
                    47,81,497,123,69,73,226,659,668,400,269,168,43,94,65,375,303,714,
                    723,283,87,550,541,334,546,496,263,741,679,790,736,686,276,280,331,
                    690,643,692)

# Filter out the rows with these IDs
combined_features_clean2 <- combined_features_clean1 %>%
  filter(!ID %in% ids_to_exclude)


p <- ggplot() + 
  geom_line(data = combined_features_clean2, aes(x = year, y = value, group = ID, color = as.factor(ID)), alpha = 0.5) +  # Add lines for individual IDs with transparency
  geom_line(data = summary_stats, aes(x = year, y = mean_value, group = class), color = "black", size = 1.5) +  # Add mean line per class
  geom_ribbon(data = summary_stats, aes(x = year, ymin = mean_value - sd_value, ymax = mean_value + sd_value, group = class), fill = "grey", alpha = 0.55) +  # Add ribbon for variability
  facet_wrap(~ class) + 
  theme_bw() + 
  labs(x = "Year", y = "Value", title = "Temporal stability of candidate pixels") +
  theme(legend.position = "none")

# Convert to plotly interactive plot
interactive_plot <- ggplotly(p)

# Print interactive plot
print(interactive_plot)


# Define the IDs to exclude
ids_to_exclude <- c(291,391,823,696,213,826,610,150,115,116,360,763,359,41,526,
                    464,466,625,138,645,138,822,630,5,111,134,165,155,108,807,609,
                    268,760,311,330,554,281,397,252,180,56,441,
                    107,393,392,529,408,709,492,619,234,158,156,170,234,615,
                    614,158,170,632,338,342,340,337,346,77,161,557,218,547,577,784,445,
                    446,265,570,244,128,110,407,79,426,274,516,109,102,520,
                    59,219,629,37,31,669)

# Filter out the rows with these IDs
combined_features_clean3 <- combined_features_clean2 %>%
  filter(!ID %in% ids_to_exclude)


p <- ggplot() + 
  geom_line(data = combined_features_clean3, aes(x = year, y = value, group = ID, color = as.factor(ID)), alpha = 0.5) +  # Add lines for individual IDs with transparency
  geom_line(data = summary_stats, aes(x = year, y = mean_value, group = class), color = "black", size = 1.5) +  # Add mean line per class
  geom_ribbon(data = summary_stats, aes(x = year, ymin = mean_value - sd_value, ymax = mean_value + sd_value, group = class), fill = "grey", alpha = 0.55) +  # Add ribbon for variability
  facet_wrap(~ class) + 
  theme_bw() + 
  labs(x = "Year", y = "Value", title = "Temporal stability of candidate pixels") +
  theme(legend.position = "none")

# Convert to plotly interactive plot
interactive_plot <- ggplotly(p)

# Print interactive plot
print(interactive_plot)


# Define the IDs to exclude
ids_to_exclude <- c(38,149,661,727,793,344,316,404,600,450)

# Filter out the rows with these IDs
combined_features_clean4 <- combined_features_clean3 %>%
  filter(!ID %in% ids_to_exclude)


p <- ggplot() + 
  geom_line(data = combined_features_clean4, aes(x = year, y = value, group = ID, color = as.factor(ID)), alpha = 0.5) +  # Add lines for individual IDs with transparency
  geom_line(data = summary_stats, aes(x = year, y = mean_value, group = class), color = "black", size = 1.5) +  # Add mean line per class
  geom_ribbon(data = summary_stats, aes(x = year, ymin = mean_value - sd_value, ymax = mean_value + sd_value, group = class), fill = "grey", alpha = 0.55) +  # Add ribbon for variability
  facet_wrap(~ class) + 
  theme_bw() + 
  labs(x = "Year", y = "Value", title = "Temporal stability of candidate pixels") +
  theme(legend.position = "none")

# Convert to plotly interactive plot
interactive_plot <- ggplotly(p)

# Print interactive plot
print(interactive_plot)


# Define the IDs to exclude
ids_to_exclude <- c(379,467,589,523)

# Filter out the rows with these IDs
combined_features_clean5 <- combined_features_clean4 %>%
  filter(!ID %in% ids_to_exclude)

# Step 1: Merge classes 5 and 6 into a single class (class 5)
combined_features_clean5 <- combined_features_clean5 %>%
  mutate(class = ifelse(class == 6, 5, class))

# Step 2: Reclassify subsequent classes by decrementing their class number by 1
combined_features_clean5 <- combined_features_clean5 %>%
  mutate(class = ifelse(class > 5, class - 1, class))

# Create a named vector with class names corresponding to each new class number
class_mapping <- c(
  "1" = "Artificial Land",
  "2" = "Cropland",
  "3" = "Broadleaved woodland",
  "4" = "Coniferous woodland",
  "5" = "Shrubland",  # Merged class
  "6" = "Grassland",
  "7" = "Bare land",
  "8" = "Water bodies"
)

# Add the class_name column based on the class column
combined_features_clean5 <- combined_features_clean5 %>%
  mutate(class_name = class_mapping[as.character(class)])

# unique IDs
# Remove duplicate IDs
combined_features_clean5_unique <- combined_features_clean5 %>%
  distinct(ID, .keep_all = TRUE)

# write
write.csv(combined_features_clean5, "/data/eo/EO4Alps/level3/NDVI/combined_features_cleaned_classname.csv", row.names = FALSE)
write.csv(combined_features_clean5_unique, "/data/eo/EO4Alps/level3/NDVI/combined_features_cleaned_classname_unique.csv", row.names = FALSE)



# Calculate summary statistics
summary_stats <- combined_features_clean5 %>%
  group_by(class_name, year) %>%
  summarise(mean_value = mean(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE)) %>%
  ungroup()


# Define the order of the classes
class_order <- c("Artificial Land", "Water bodies", "Bare land", "Coniferous woodland", 
                 "Broadleaved woodland", "Shrubland", "Grassland", "Cropland")

# Convert class_name to a factor with the specified order
summary_stats <- summary_stats %>%
  mutate(class_name = factor(class_name, levels = class_order))

# Create the plot with geom_ribbon filled according to the class variable and with a black border
p<-ggplot() + 
  geom_ribbon(data = summary_stats, 
              aes(x = year, ymin = mean_value - sd_value, ymax = mean_value + sd_value, 
                  fill = class_name, group = class_name), 
              color = "black", alpha = 0.55) +  # Add ribbon for variability with black border
  facet_wrap(~ class_name) + 
  theme_bw() + 
  labs(x = "Year", y = "Value", title = "Temporal stability of candidate pixels") +
  theme(legend.position = "none")

# Save the plot as a JPG file
ggsave("/data/eo/EO4Alps/figs/spectral_stability.jpg", plot = p, dpi = 300, width = 8, height = 6, units = "in")



# Calculate the count of IDs per class
class_counts <- combined_features_clean5 %>%
  group_by(class_name) %>%
  summarise(num_ids = n_distinct(ID)) %>%
  ungroup()

# Plot the bar chart
ggplot(class_counts, aes(x = class_name, y = num_ids)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = num_ids), vjust = -0.5, color = "black", size = 4) +  # Add labels with num_ids values
  theme_minimal() +
  labs(x = "Class", y = "Number of IDs", title = "Number of IDs per Class")




