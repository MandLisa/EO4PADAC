library(ggplot2)
library(tidyverse)
library(ggforce)
library(sf)
library(gridExtra)
library(purrr)
library(jcolors)
library(GGally)
library(randomForest)
library(pdp)
library(patchwork)
library(spgwr)
library(scales)
library(GWmodel)
library(sf)
library(spdep)
library(pheatmap)
library(DataEditR)
library(readr)
library(spgwr)
library(kableExtra)
library(dplyr)

recovery_hex <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_hex.csv")
recovery_hex_yearly <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_hex_yearly.csv")
recovery_sf_temporal2 <- st_read("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_MC.shp")
hex_data_MC_0402 <- st_read("~/eo_nas/EO4Alps/00_analysis/_recovery/hex_data_MC_0402.shp")

recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_GWR.csv")

# Filter the dataset and compute the new column
recovery_filt <- recovery %>%
  group_by(ID) %>%
  filter(yod < 2013) %>%
  mutate(recov_10 = ifelse(recovery_rate <= 10, 1, 0)) %>%
  ungroup()

# convert recovery df to sf object
recovery_sf <- st_as_sf(recovery_filt, coords = c("x", "y"), crs = 3035)


# load hexagons and recovery df
hexagons <- st_read("~/eo_nas/EO4Alps/gis/hexagons/hex_500.shp")


# just use GRID_ID for subsequent joins
hexagons_selected <- hexagons %>%
  select(GRID_ID)

recovery_sf <- st_join(recovery_sf, hexagons_selected, join = st_intersects)



# Compute yearly recovery success per GRID_ID
recovery_hex_yearly <- recovery_sf %>%
  group_by(GRID_ID, yod) %>%  # Group by year of disturbance (yod)
  summarize(
    total_observations = n(),
    total_recovered = sum(recovery_10yn, na.rm = TRUE),
    percent_recovered = (total_recovered / total_observations) * 100,
    .groups = "drop"
  )

# group data by GRID_ID and yod ctegory
recovery_hex <- recovery_hex_yearly %>%
  mutate(yod_category = ifelse(yod < 2000, "before_2000", "after_2000")) %>%
  group_by(GRID_ID, yod_category) %>%
  summarize(
    mean_recovery = mean(percent_recovered, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = yod_category, values_from = mean_recovery) %>%
  group_by(GRID_ID) %>%  # Ensure only one row per GRID_ID
  summarize(
    before_2000 = mean(before_2000, na.rm = TRUE),
    after_2000 = mean(after_2000, na.rm = TRUE),
    observed_difference = after_2000 - before_2000,
    .groups = "drop"
  )

sum(is.na(recovery_hex$observed_difference))  
recovery_hex <- recovery_hex %>% drop_na(observed_difference)


write.csv(recovery_hex_yearly, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_hex_yearly.csv", row.names = FALSE)
write.csv(recovery_hex, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_hex.csv", row.names = FALSE)


set.seed(42)
n_permutations <- 5000
p_values_list <- list()


# Drop geometry from data to prevent vector errors
recovery_hex_yearly <- st_drop_geometry(recovery_hex_yearly)

# Drop geometry from data to prevent vector errors
recovery_hex <- st_drop_geometry(recovery_hex)

sum(is.na(recovery_hex_yearly$GRID_ID))  # Count NA values in GRID_ID
sum(is.na(recovery_hex$GRID_ID))  # Check in the aggregated dataset

# Remove NA GRID_ID values before processing
recovery_hex_yearly <- recovery_hex_yearly %>% filter(!is.na(GRID_ID))
recovery_hex <- recovery_hex %>% filter(!is.na(GRID_ID))


for (grid in unique(recovery_hex$GRID_ID)) {
  
  if (is.na(grid)) {
    print("Skipping NA GRID_ID")
    next
  }
  
  # Extract observed difference for this grid
  grid_data <- recovery_hex %>%
    filter(GRID_ID == grid) %>%
    st_drop_geometry()
  
  if (nrow(grid_data) == 0 || any(is.na(grid_data$observed_difference))) {
    print(paste("Skipping GRID_ID:", grid, "due to missing observed difference"))
    next
  }
  
  observed_difference <- grid_data$observed_difference
  
  # Extract original recovery data for this grid
  hex_data <- recovery_hex_yearly %>%
    filter(GRID_ID == grid) %>%
    st_drop_geometry()
  
  if (nrow(hex_data) == 0) {
    print(paste("Skipping GRID_ID:", grid, "due to empty hex_data"))
    next
  }
  
  # Monte Carlo Permutation Test
  random_differences <- replicate(n_permutations, {
    
    shuffled_data <- hex_data %>%
      mutate(yod = sample(yod, replace = FALSE)) %>%
      mutate(yod_category = ifelse(yod < 2000, "before_2000", "after_2000")) %>%
      group_by(GRID_ID, yod_category) %>%
      summarize(mean_recovery = mean(percent_recovered, na.rm = TRUE), .groups = "drop")
    
    if (nrow(shuffled_data) == 0 || !"yod_category" %in% colnames(shuffled_data)) {
      print(paste("Warning: 'shuffled_data' missing for GRID_ID:", grid))
      return(NA)
    }
    
    shuffled_data <- shuffled_data %>%
      pivot_wider(names_from = yod_category, values_from = mean_recovery, values_fill = list(mean_recovery = NA)) %>%
      mutate(
        before_2000 = coalesce(before_2000, 0),
        after_2000 = coalesce(after_2000, 0),
        shuffled_difference = after_2000 - before_2000
      )
    
    if (!"shuffled_difference" %in% colnames(shuffled_data) || !is.numeric(shuffled_data$shuffled_difference)) {
      print(paste("Skipping GRID_ID:", grid, "due to missing shuffled_difference"))
      return(NA)
    }
    
    shuffled_data$shuffled_difference
  })
  
  # Remove NAs before computing p-value
  random_differences <- na.omit(random_differences)
  if (length(random_differences) == 0) {
    print(paste("Skipping GRID_ID:", grid, "due to empty random_differences"))
    next
  }
  
  # Compute p-value (Compare observed vs. randomized differences)
  p_value <- (sum(abs(random_differences) >= abs(observed_difference), na.rm = TRUE) + 1) / (n_permutations + 1)
  
  # Store results
  p_values_list[[grid]] <- tibble(GRID_ID = grid, observed_difference = observed_difference, p_value = p_value)
}

# Combine results
recovery_sf_temporal1 <- bind_rows(p_values_list)


# Ensure both datasets are compatible for joining
if (!inherits(recovery_sf_temporal1, "sf")) {
  print("Converting recovery_sf_temporal1 into an sf object...")
  
  # Merge recovery_sf_temporal1 with hexagon geometries
  recovery_sf_temporal2 <- hexagons_selected %>%
    left_join(recovery_sf_temporal1, by = "GRID_ID")  # Join by GRID_ID
  
  # Ensure the result is an sf object
  recovery_sf_temporal2 <- st_as_sf(recovery_sf_temporal1)
}


st_write(recovery_sf_temporal2, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_MC.shp", driver = "ESRI Shapefile")




# Plot p-value map
map_pvalues <- ggplot(recovery_sf_temporal2) +
  geom_sf(aes(fill = p_value), color = "black") +
  scale_fill_viridis_c(option = "mako", direction = -1, name = "P-Value") +
  labs(title = "Significance of Recovery Success Changes per Grid",
       subtitle = "p-value < 0.05 indicates systematic change",
       fill = "P-Value") +
  theme_minimal()

print(map_pvalues)



library(ggplot2)

ggplot(hex_data_MC_0402, aes(x = p_smooth_2)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.6, color = "black") +
  geom_vline(xintercept = 0.15, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Smoothed P-Values",
       x = "Smoothed P-Value",
       y = "Count") +
  theme_minimal()


ggplot(hex_data_MC_0402, aes(x = factor(geoloc), y = p_smooth_2, fill = factor(geoloc))) +
  geom_boxplot() +
  scale_fill_viridis_d() +
  labs(title = "P-Value Variation Across Regions",
       x = "Region",
       y = "Smoothed P-Value") +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(hex_data_MC_0402, aes(x = obsrvd_, y = p_smooth_2)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "P-Value vs. Recovery Success Difference",
       x = "Recovery Success Change (Before vs. After 2000)",
       y = "Smoothed P-Value") +
  theme_minimal()


ggplot(hex_data_MC_0402, aes(x = obsrvd_, fill = p_smooth_2 < 0.05)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("red", "blue"), labels = c("Non-Significant", "Significant")) +
  labs(title = "Density Plot: Recovery Success Change by Significance",
       x = "Recovery Success Change",
       y = "Density",
       fill = "Significance") +
  theme_minimal()




















# Ensure recovery_sf_temporal1 is an sf object
hex_data <- recovery_sf_temporal2  # Your hexagonal grid with p-values

hex_data <- hex_data %>%
  mutate(p_value = ifelse(is.na(p_value) | is.infinite(p_value), mean(p_value, na.rm = TRUE), p_value))



# Compute neighbors based on hexagon adjacency
nb <- poly2nb(hex_data)  # Finds adjacent hexagons
lw <- nb2listw(nb, style = "W", zero.policy = TRUE) 

# Compute spatially smoothed p-values as the weighted average of neighbors
hex_data$p_value_smoothed <- lag.listw(lw, hex_data$p_value, zero.policy = TRUE)

# Replace NAs (hexagons with no neighbors) with original p-value
hex_data$p_value_smoothed <- ifelse(is.na(hex_data$p_value_smoothed), hex_data$p_value, hex_data$p_value_smoothed)

hex_data$p_value_smoothed1 <- hex_data$p_value_smoothed * 0.5

# Plot smoothed values
ggplot(hex_data) +
  geom_sf(aes(fill = p_value_smoothed1), color = "black") +
  scale_fill_viridis_c(option = "mako", direction = -1, name = "Smoothed P-Value") +
  labs(title = "Smoothed Recovery Success Changes per Hexagon") +
  theme_minimal()



st_write(hex_data, "~/eo_nas/EO4Alps/00_analysis/_recovery/hex_data_MC.shp", driver = "ESRI Shapefile")














# Convert random differences to a dataframe for plotting
random_differences_df <- data.frame(random_differences = as.vector(random_differences))

# Histogram of the null distribution vs. observed differences
hist_null_distribution <- ggplot(random_differences_df, aes(x = random_differences)) +
  geom_histogram(bins = 100, alpha = 0.6, fill = "gray") +
  geom_vline(data = recovery_sf_temporal, aes(xintercept = observed_difference), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Null Distribution of Recovery Success Differences",
       subtitle = "Red line = Observed Difference per Grid",
       x = "Randomized Recovery Success Difference", y = "Frequency") +
  theme_bw()

print(hist_null_distribution)


