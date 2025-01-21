# Ensure ggpattern is loaded
library(ggpattern)
library(tidyverse)
library(sf)
library(DataEditR)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(osmdata)
library(ggridges)

library(readr)
recovery <- read_csv("eo_nas/EO4Alps/00_analysis/_recovery/recovery_312025.csv")

recovery <- recovery_312025

# Aggregate recovery metrics per geolocation
recovery_summary <- recovery %>%
  group_by(geoloc) %>%
  summarise(
    total_dist_area = unique(total_dist_area),
    total_forest_area = unique(total_forest_area),
    percentage_disturbed = (unique(total_dist_area) / unique(total_forest_area)) * 100,
    total_pixels = n(),
    recovered_pixels = sum(recovery_10y == 1, na.rm = TRUE),
    percentage_recovered_within_disturbed = (recovered_pixels / total_pixels) * percentage_disturbed
  ) %>%
  mutate(
    percentage_not_recovered = percentage_disturbed - percentage_recovered_within_disturbed
  )

# Prepare data for stacked bar plot
recovery_stacked <- recovery_summary %>%
  select(geoloc, percentage_recovered_within_disturbed, percentage_not_recovered) %>%
  pivot_longer(
    cols = c(percentage_recovered_within_disturbed, percentage_not_recovered),
    names_to = "metric",
    values_to = "percentage"
  ) %>%
  mutate(
    pattern = ifelse(metric == "percentage_recovered_within_disturbed", "stripe", "none")
  )

# Reorder geoloc levels for consistency
recovery_stacked <- recovery_stacked %>%
  mutate(geoloc = factor(geoloc, levels = c(
    "eastern alps - north", 
    "eastern alps - central", 
    "eastern alps - south", 
    "western alps - north", 
    "western alps - south"
  )))

# Remove NA values from df_long
recovery_stacked <- recovery_stacked %>%
  filter(!is.na(geoloc))

# Launch the interactive editor window
recovery_stacked <- data_edit(recovery_stacked)

# Plot stacked bar chart with patterns
p4 <- ggplot(recovery_stacked, aes(x = geoloc, y = percentage, fill = "Disturbed")) +
  geom_col_pattern(
    aes(pattern = pattern),
    position = "stack",
    color = "black",
    fill = "#76A8AD",  # Solid fill for disturbed area
    pattern_color = "black",
    pattern_fill = "#76A8AD",
    pattern_density = 0.5,
    pattern_spacing = 0.05,
    pattern_angle = 45
  ) +
  scale_pattern_manual(
    values = c("stripe" = "stripe", "none" = "none"),
    labels = c("Disturbed area [%]", "Recovered area [%]")
  ) +
  labs(
    title = "",
    x = "",
    y = "Percentage [%]",
    fill = "",
    pattern = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  )

# Display the plot
print(p4)


#-------------------------------------------------------------------------------
recovery_distinct <- recovery %>%
  distinct(ID, .keep_all = TRUE)


# Group data by 'geolocation' and 'severity_class' to define local baselines
local_baseline <- recovery_distinct %>%
  group_by(geoloc, severity_class, height_class, forest_type, VPD_class_yod1) %>%
  summarise(
    mean_recovery_interval = mean(recovery_rate, na.rm = TRUE),
    sd_recovery_interval = sd(recovery_rate, na.rm = TRUE)
  )

# Merge baseline metrics back into the original dataframe
recovery_distinct <- recovery_distinct %>%
  left_join(local_baseline, by = c("geoloc", "seberity_class")) %>%
  mutate(
    z_score = (recovery_rate - mean_recovery_interval) / sd_recovery_interval
  )


ggplot(recovery_distinct, aes(x = severity_class, y = z_score, fill = severity_class)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Recovery Z-Scores by Severity Class",
    x = "Severity Class",
    y = "Z-Score"
  ) +
  theme_minimal()


ggplot(recovery_distinct, aes(x = z_score)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  labs(
    title = "Density Plot of Local Recovery Z-Scores",
    x = "Z-Score",
    y = "Density"
  ) +
  theme_minimal()


# alps-wide vs. local
national_mean <- mean(recovery$recovery_rate, na.rm = TRUE)
national_sd <- sd(recovery$recovery_rate, na.rm = TRUE)
recovery <- recovery %>%
  mutate(
    national_z_score = (recovery_rate - national_mean) / national_sd
  )


ggplot(recovery_distinct, aes(x = national_z_score, y = z_score, color = severity_class)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "",
    x = "Alps-wide z-score",
    y = "local z-score"
  ) +
  theme_minimal()



# Convert to sf object using latitude and longitude
recovery_sf <- st_as_sf(recovery_distinct, coords = c("x", "y"), crs = 3035)

# Classify slow recovery
recovery_sf <- recovery_sf %>%
  mutate(
    recovery_type = case_when(
      z_score > 0.8 ~ "Slow Recovery",
      z_score < -0.75 ~ "Fast Recovery",
      TRUE ~ "Normal Recovery"
    )
  )


ggplot() +
  geom_sf(data = recovery_sf, aes(color = recovery_type), size = 0.5, alpha = 0.7) +
  scale_color_manual(
    values = c(
      "Slow Recovery" = "red",
      "Fast Recovery" = "blue",
      "Normal Recovery" = "grey"
    ),
    name = "Recovery Type"
  ) +
  labs(
    title = "",
    subtitle = "",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()


#------------------------------------------------------------------------------

# Load the hexagon shapefile
hex_sf <- st_read("~/eo_nas/EO4Alps/gis/hexagons/hex_500.shp")

# Verify the CRS
st_crs(hex_sf)
st_crs(recovery_sf)

# Reproject if necessary to ensure both layers have the same CRS
if (st_crs(hex_sf) != st_crs(df_sf)) {
  hex_sf <- st_transform(hex_sf, crs = st_crs(df_sf))
}


# Perform the spatial join
df_hex_aggregated <- st_join(recovery_sf, hex_sf, join = st_within) %>%
  group_by(GRID_ID) %>%  # Replace 'hex_id' with the identifier column in your hexagon shapefile
  summarise(
    mean_z_score = mean(z_score, na.rm = TRUE),
    count_slow = sum(recovery_type == "Slow Recovery", na.rm = TRUE),
    count_fast = sum(recovery_type == "Fast Recovery", na.rm = TRUE),
    count_normal = sum(recovery_type == "Normal Recovery", na.rm = TRUE),
    total_points = n(),
    .groups = "drop"
  )


# Remove geometry from aggregated data
df_hex_aggregated <- df_hex_aggregated %>%
  st_drop_geometry()

# Join aggregated data to hexagon geometries
hex_sf <- hex_sf %>%
  left_join(df_hex_aggregated, by = "GRID_ID")


ggplot() +
  geom_sf(data = hex_sf, aes(fill = mean_z_score), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey80",
    name = "Mean Z-Score"
  ) +
  labs(
    title = "Mean Z-Score Across Hexagons",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()



# Group data by 'geolocation' and 'severity_class' to define local baselines
local_baseline <- recovery_distinct %>%
  group_by(geoloc, severity_class, height_class, forest_type, VPD_class_yod1) %>%
  summarise(
    mean_recovery_interval = mean(recovery_rate, na.rm = TRUE),
    sd_recovery_interval = sd(recovery_rate, na.rm = TRUE)
  )


local_baseline <- local_baseline %>%
  filter(!is.na(forest_type) & !is.na(geoloc))

summary(local_baseline$mean_recovery_interval)
max(local_baseline$mean_recovery_interval, na.rm = TRUE)

# Remove rows where VPD_class_yod1 is "undefined"
local_baseline <- local_baseline %>%
  filter(VPD_class_yod1 != "undefined")

# Remove rows where VPD_class_yod1 is "undefined"
local_baseline <- local_baseline %>%
  filter(forest_type != "undefined")



ggplot(local_baseline, aes(x = geoloc, y = mean_recovery_interval, fill = mean_recovery_interval)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_viridis_c(option = "plasma", name = "Mean Recovery Interval") +
  labs(
    title = "Mean Recovery Interval by Geolocation",
    x = "Geolocation",
    y = "Mean Recovery Interval"
  ) +
  theme_minimal()

ggplot(local_baseline, aes(x = severity_class, y = mean_recovery_interval, fill = severity_class)) +
  geom_boxplot() +
  facet_wrap(~ geoloc) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Recovery Intervals by Severity Class and Geolocation",
    x = "Severity Class",
    y = "Mean Recovery Interval"
  ) +
  theme_minimal()



ggplot(local_baseline, aes(x = VPD_class_yod1, y = mean_recovery_interval, fill = VPD_class_yod1)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ geoloc) +
  scale_fill_viridis_d(option = "inferno") +
  labs(
    title = "Mean Recovery Interval by VPD Class Across Geolocations",
    x = "VPD Class (Year of Disturbance +1)",
    y = "Mean Recovery Interval"
  ) +
  theme_minimal()


# Group data by 'geolocation' and 'severity_class' to define local baselines
local_baseline <- recovery_distinct %>%
  group_by(VPD_class_yod, height_class, severity_class, forest_type) %>%
  summarise(
    mean_recovery_interval = mean(recovery_rate, na.rm = TRUE),
    sd_recovery_interval = sd(recovery_rate, na.rm = TRUE)
  )

# Remove rows where VPD_class_yod1 is "undefined"
local_baseline <- local_baseline %>%
  filter(VPD_class_yod != "undefined")

local_baseline <- local_baseline %>%
  filter(forest_type != "undefined")

local_baseline <- local_baseline %>%
  filter(!is.na(geoloc))


local_baseline$height_class <- factor(local_baseline$height_class, 
                                      levels = c("<800m", "<800-1200m", ">1200m"))


ggplot(local_baseline, aes(x = height_class, y = mean_recovery_interval, fill = severity_class)) +
  geom_col(position = "dodge") +
  facet_grid(VPD_class_yod ~ forest_type) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Interaction Between Height, Severity, VPD, and Forest Type on Recovery",
    x = "Height Class",
    y = "Mean Recovery Interval"
  ) +
  theme_bw()





# Group data by 'geolocation' and 'severity_class' to define local baselines
local_baseline <- recovery_distinct %>%
  group_by(geoloc, VPD_cat_yod) %>%
  summarise(
    mean_recovery_interval = mean(recovery_rate, na.rm = TRUE),
    sd_recovery_interval = sd(recovery_rate, na.rm = TRUE)
  )

# Remove rows where VPD_class_yod1 is "undefined"
local_baseline <- local_baseline %>%
  filter(VPD_cat_yod != "NA")

local_baseline <- local_baseline %>%
  filter(!is.na(geoloc))

local_baseline$mean_recovery_interval <- local_baseline$mean_recovery_interval * 2
local_baseline$sd_recovery_interval <- local_baseline$sd_recovery_interval * 1.25



ggplot(local_baseline, aes(x = VPD_cat_yod, y = mean_recovery_interval, fill = VPD_cat_yod)) +
  geom_col(position = "dodge") +  # Bar plot
  geom_errorbar(
    aes(ymin = mean_recovery_interval - sd_recovery_interval,
        ymax = mean_recovery_interval + sd_recovery_interval),
    width = 0.2,                   # Width of the error bar caps
    position = position_dodge(0.9), # Align error bars with bars
    color = "black"                # Make error bars visible
  ) +
  facet_wrap(~ geoloc) +
  scale_fill_manual(
    values = c(
      "Dry" = "#900C3F",     # Custom color for Dry
      "Average" = "#F5F2A9", # Custom color for Average
      "Wet" = "#087984"      # Custom color for Wet
    )
  ) +
  labs(
    title = "",
    x = "",
    y = "Mean recovery interval [years]",
    fill = "VPD category"
  ) +
  ylim(0, 30) +
  theme_bw(base_size = 17) +
  theme(legend.position = "none")




# Save the last plotted ggplot
ggsave(
  filename = "~/eo_nas/EO4Alps/figs/recovery_intervals_VPD.png",    
  width = 8,                     
  height = 6,                    
  dpi = 300)        



# Remove rows where VPD_class_yod1 is "undefined"
recovery_distinct1 <- recovery_distinct %>%
  filter(VPD_cat_yod != "NA")

recovery_distinct1 <- recovery_distinct1 %>%
  filter(!is.na(geoloc))

recovery_distinct1$recovery_rate <- recovery_distinct1$recovery_rate * 2

# Reorder geoloc manually
recovery_distinct1 <- recovery_distinct1 %>%
  mutate(
    geoloc = factor(geoloc, levels = c(
      "eastern alps - north",
      "eastern alps - central",
      "eastern alps - south",
      "western alps - south",
      "western alps - north"
    ))
  )

# Reorder geoloc manually
recovery_distinct1 <- recovery_distinct1 %>%
  mutate(
    height_class = factor(height_class, levels = c(
      "<800m",
      ">800-1200m",
      ">1200m"
    ))
  )



ggplot(recovery_distinct1, aes(x = recovery_rate, fill = VPD_cat_yod, color = VPD_cat_yod)) +
  geom_density(alpha = 0.25, bw = 1.5, adjust = 1.5, trim = TRUE) +
  facet_wrap(~ geoloc) +
  scale_fill_manual(
    values = c(
      "Dry" = "#900C3F",     # Custom color for Dry
      "Average" = "#797D7F", # Custom color for Average
      "Wet" = "#087984"      # Custom color for Wet
    )
  ) +
  scale_color_manual(
    values = c(
      "Dry" = "#900C3F",     
      "Average" = "#797D7F", 
      "Wet" = "#087984"      
    )
  ) +
  coord_cartesian(xlim = c(0, 38)) +
  labs(
    title = "",
    x = "Mean recovery interval [years]",
    y = "Density",
    fill = "VPD category",
    color = "VPD category"
  ) +
  geom_vline(aes(xintercept = mean(recovery_rate)), linetype = "dashed", color = "black") +
  theme_bw(base_size = 17) +
  theme(legend.position = "right")







# Save the last plotted ggplot
ggsave(
  filename = "~/eo_nas/EO4Alps/figs/recovery_intervals_height_ridges.png",    
  width = 14,                     
  height = 5,                    
  dpi = 300)   




# Group data by 'geolocation' and 'severity_class' to define local baselines
local_baseline <- recovery_distinct %>%
  group_by(geoloc, height_class) %>%
  summarise(
    mean_recovery_interval = mean(recovery_rate, na.rm = TRUE),
    sd_recovery_interval = sd(recovery_rate, na.rm = TRUE)
  )

local_baseline$mean_recovery_interval <- local_baseline$mean_recovery_interval * 2
local_baseline$sd_recovery_interval <- local_baseline$sd_recovery_interval * 1.25


hist(recovery_distinct$VPD_yod,
     main = "Distribution of VPD_class_yod",
     xlab = "VPD_class_yod",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")


# Calculate quantiles
quantiles <- quantile(recovery_distinct$VPD_yod, probs = c(0.33, 0.66), na.rm = TRUE)

# Define thresholds
dry_threshold <- quantiles[1]   # 33rd percentile
wet_threshold <- quantiles[2]   # 66th percentile

# Classify data
recovery_distinct$VPD_cat_yod <- cut(recovery_distinct$VPD_yod1,
                       breaks = c(-Inf, dry_threshold, wet_threshold, Inf),
                       labels = c("Dry", "Average", "Wet"))




### add temp_yod column to df
# Add temp_yod column for all observations of the same ID
recovery1 <- recovery %>%
  group_by(ID) %>%
  mutate(temp_yod = first(temp[year == yod])) %>%
  ungroup()

recovery <- recovery1






