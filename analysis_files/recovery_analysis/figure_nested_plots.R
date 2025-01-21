library(ggforce)
library(tidyverse)
library(ggplot2)
library(sf)
library(terra)
library(ggh4x)

#-------------------------------------------------------------------------------
### load dfs
recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_random.csv")
mean_temperature <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/mean_temperature.csv")
mean_temperature_nested <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/mean_temperature_nested.csv")
recovery_summary <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_summary_2311.csv")
recovery_summary <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_summary_nested.csv")

# reclassify severity
recovery <- recovery %>%
  mutate(severity_class = if_else(
    severity_relative > 75, 
    "SR", 
    "NSR"
  ))

# add height classes
recovery <- recovery %>%
  mutate(
    height_class = case_when(
      height >= 0 & height <= 800 ~ "<800m",
      height > 800 & height <= 1200 ~ ">800-1200m",
      height > 1200 ~ ">1200m",
      TRUE ~ NA_character_  
    )
  )

# compute recov_10 metric
recovery_summary <- recovery %>%
  group_by(yod, geoloc, severity_class, height_class) %>%
  summarize(
    total_disturbances = n(),
    #percent_recovered_within_5y = (sum(recovery_5y, na.rm = TRUE) / total_disturbances) * 100,
    recov_10y_perc = (sum(recovery_10y, na.rm = TRUE) / total_disturbances) * 100
  )


### prepare temperature data

# Calculate mean temperature for each geoloc, year, and sea_level
mean_temperature <- recovery %>%
  group_by(geoloc, height_class,year) %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE), .groups = "drop")

# Normalize temperature for each geoloc and sea_level
mean_temperature <- mean_temperature %>%
  group_by(geoloc, height_class) %>%
  mutate(
    norm_temp = (mean_temp - min(mean_temp, na.rm = TRUE)) / 
      (max(mean_temp, na.rm = TRUE) - min(mean_temp, na.rm = TRUE)), 
    temp_mid = mean(mean_temp, na.rm = TRUE)  
  ) %>%
  ungroup()

# Define the temperature range
temp_min <- min(mean_temperature$mean_temp, na.rm = TRUE)
temp_max <- max(mean_temperature$mean_temp, na.rm = TRUE)

# Define the maximum recovery value 
recovery_max <- 100

# Define a scaling function to map temperature to recovery scale
scale_to_recovery <- function(temp) {
  (temp - temp_min) / (temp_max - temp_min) * recovery_max
}

# Define a reverse scaling function for the secondary y-axis
scale_to_temperature <- function(recovery) {
  recovery / recovery_max * (temp_max - temp_min) + temp_min
}



### Reorder height classes and geolocation both the recovery and temp df
recovery_summary_2211 <- recovery_summary_2211 %>%
  mutate(height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")))

mean_temperature <- mean_temperature %>%
  mutate(height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")))

# Reorder height_class in the dataset
recovery_summary_2211 <- recovery_summary_2211 %>%
  mutate(geoloc = factor(geoloc, levels = c("eastern alps - north", "eastern alps - central",
                                            "eastern alps - south", "western alps - north",
                                            "western alps - south")))

mean_temperature <- mean_temperature %>%
  mutate(geoloc = factor(geoloc, levels = c("eastern alps - north", "eastern alps - central",
                                            "eastern alps - south", "western alps - north",
                                            "western alps - south")))


#-------------------------------------------------------------------------------
### create plots

# prepare temp data
mean_temperature_nested <- recovery %>%
  group_by(geoloc, year, height_class, severity_class) %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE), .groups = "drop")

# Step 2: Normalize temperature for each geoloc and sea_level
mean_temperature_nested <- mean_temperature_nested %>%
  group_by(geoloc, height_class,severity_class) %>%
  mutate(
    norm_temp = (mean_temp - min(mean_temp, na.rm = TRUE)) / 
      (max(mean_temp, na.rm = TRUE) - min(mean_temp, na.rm = TRUE)), # Normalize 0-1
    temp_mid = mean(mean_temp, na.rm = TRUE)  # Optional midpoint for color gradient
  ) %>%
  ungroup()


# Define the temperature range
temp_min <- min(mean_temperature_nested$mean_temp, na.rm = TRUE)
temp_max <- max(mean_temperature_nested$mean_temp, na.rm = TRUE)

# Define the maximum recovery value (70 in your case)
recovery_max <- 100

# Define a scaling function to map temperature to recovery scale
scale_to_recovery <- function(temp) {
  (temp - temp_min) / (temp_max - temp_min) * recovery_max
}

# Define a reverse scaling function for the secondary y-axis
scale_to_temperature <- function(recovery) {
  recovery / recovery_max * (temp_max - temp_min) + temp_min
}

mean_temperature_nested <- mean_temperature_nested %>%
  mutate(height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")))


mean_temperature_nested <- mean_temperature_nested %>%
  mutate(geoloc = factor(geoloc, levels = c("eastern alps - north", "eastern alps - central",
                                            "eastern alps - south", "western alps - north",
                                            "western alps - south")))

### nested plot
ggplot() +
  geom_bar(
    data = recovery_summary_2211,
    aes(x = yod, y = recov_10y_perc, fill = severity_class), 
    stat = "identity", position = "stack", alpha = 1
  ) +
  geom_smooth(
    data = recovery_summary_2211, 
    aes(x = yod, y = recov_10y_perc, group = interaction(geoloc, , severity_class)), 
    method = "lm", linetype = "dashed", color = "black", fill = "#65A0A7", se = TRUE, size = 0.4
  ) +
  # Line plot for mean temperature trends 
  geom_line(
  data = mean_temperature_nested, 
  aes(
  x = year, 
  y = scale_to_recovery(mean_temp), 
  color = norm_temp,  
  group = interaction(geoloc, height_class, severity_class) 
  ), 
  size = 0.75
  ) +
  facet_nested(
    height_class ~ geoloc + severity_class, 
    scales = "free_x", 
    space = "free_x"
  ) +
  scale_x_continuous(
    name = "Year of Disturbance",
    limits = c(1986, 2012), 
    breaks = seq(1986, 2012, by = 10) 
  ) +
  scale_y_continuous(
    name = "Percentage recovered 10y post-disturbance",
    limits = c(0, 100), 
    sec.axis = sec_axis(~ scale_to_temperature(.), name = "Mean Temperature (°C)")  # Secondary y-axis for temperature
  ) +
  scale_color_distiller(
    palette = "YlOrRd", 
    direction = 1,  
    name = "Temperature (°C)",
    limits = c(0, 1)  
  ) +
  scale_fill_manual(
    values = c("NSR" = "#BCBFC0", "SR" = "#5B5B5B"),
    name = ""
  ) +
  labs(
    x = "Year of Disturbance",
    fill = ""
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom", 
    axis.title.y.left = element_text(color = "black"),  
    axis.title.y.right = element_text(color = "black"), 
    strip.text.x = element_text(size = 10, face = "bold"),  
    strip.text.y = element_text(size = 10, face = "bold")
  )



#-------------------------------------------------------------------------------
# plot stacked instead of nested
mean_temperature <- mean_temperature %>%
  mutate(height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")))


mean_temperature <- mean_temperature %>%
  mutate(geoloc = factor(geoloc, levels = c("eastern alps - north", "eastern alps - central",
                                            "eastern alps - south", "western alps - north",
                                            "western alps - south")))

ggplot() +
  # Bar plot for recovery trends (filtered for stand-replacing)
  geom_bar(data = recovery_summary_2211,
           aes(x = yod, y = recov_10y_perc, fill = severity_class), 
           stat = "identity", position = "stack", alpha =1) +
  # Set geom_smooth line to black, grouped by facet
  geom_smooth(data = recovery_summary_2211, 
              aes(x = yod, y = recov_10y_perc, group = interaction(geoloc, height_class, severity_class)), 
              method = "lm", linetype = "dashed", color = "black", fill = "#65A0A7", se = TRUE, size = 0.4) +
  # Line plot for mean temperature trends (colored by normalized temperature for independent scaling)
  geom_line(data = mean_temperature, 
            aes(
              x = year, 
              y = scale_to_recovery(mean_temp), 
              color = norm_temp,  # Use normalized temperature
              group = interaction(geoloc, height_class)
            ), 
            size = 0.75) +
  # Add horizontal line at y = 100
  #geom_hline(yintercept = 100, linetype = "dashed", color = "darkblue", size = 0.5) +
  # Facet grid
  facet_grid(height_class ~ geoloc) +
  scale_x_continuous(
    name = "Year of Disturbance",
    limits = c(1986, 2012), # Set the range of the x-axis
    breaks = seq(1986, 2012, by = 10) # Define the interval for the labels
  ) +
  scale_y_continuous(
    name = "Percentage recovered 10y post-disturbance",
    limits = c(0, 200), # Ensure recovery values stay within range
    sec.axis = sec_axis(~ scale_to_temperature(.), name = "Mean Temperature (°C)")  # Secondary y-axis for temperature
  ) +
  # Apply RdBu palette
  scale_color_distiller(
    palette = "YlOrRd", 
    direction = 1,  # Reverse so blue is for low values and red is for high values
    name = "Temperature (°C)",
    limits = c(0, 1)  # Normalize scale to 0-1
  ) +
  scale_fill_manual(
    values = c("NSR" = "#BCBFC0", "SR" = "#5B5B5B"),
    name = ""
  ) +
  labs(
    x = "Year of Disturbance",
    fill = ""
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom", # Position legend below the plot
    axis.title.y.left = element_text(color = "black"),  # Primary y-axis
    axis.title.y.right = element_text(color = "black") # Secondary y-axis
  )



#-------------------------------------------------------------------------------
# Compute the share of severity levels for each yod
severity_share <- recovery %>%
  group_by(yod, severity_class, geoloc) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(yod) %>%
  mutate(share = count / sum(count)) %>%
  arrange(yod, severity_class)

# Remove NAs
severity_share <- severity_share %>%
  filter(!is.na(geoloc))

# plot
p3 <- ggplot(severity_share, aes(x = yod, y = share*100, fill = severity_class)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "Year of distrubance",
       y = "Share",
       fill = "Severity Class") +
  scale_fill_manual(
    values = c("NSR" = "#BCBFC0", "SR" = "#5B5B5B"),
    name = ""
  ) +
  facet_wrap(~ geoloc) +
  theme_bw()


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/severity_over_time.png", plot = p3, width = 7, height =4, dpi = 300)



# Compute development of pre-dist tree share over time
avg_tree_cover_before <- recovery  %>%
  group_by(yod, geoloc, severity_class) %>%
  summarise(
    mean_tree_share_before = mean(tree_share_before, na.rm = TRUE),
    se_tree_share_before = sd(tree_share_before, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()


# Plot the data as a bar plot using ggplot2
p4 <-ggplot(avg_tree_cover_before, aes(x = factor(yod), y = mean_tree_share_before)) +
  geom_bar(stat = "identity", position = "identity") +
  #facet_wrap(~ geoloc) +
  ylim(0,100) + 
  scale_x_discrete(
    breaks = c("1990", "2000", "2010", "2018"), # Specify breaks
    labels = c("1990", "2000", "2010", "2018") # Custom labels
  ) +# Define the interval for the labels
  labs(
    title = "",
    x = "Year of distrubance",
    y = "Avg pre-disturbance tree share",
    fill = "Severity Class"
  ) +
  theme_bw()



