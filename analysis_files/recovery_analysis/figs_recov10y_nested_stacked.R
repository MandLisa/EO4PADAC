library(ggforce)
library(tidyverse)
library(ggplot2)
library(sf)
library(terra)



# df
recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_random.csv")
mean_temperature <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/mean_temperature.csv")
mean_temperature_nested <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/mean_temperature_nested.csv")
recovery_summary <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_summary_2311.csv")

# reclassify severity
recovery <- recovery %>%
  mutate(severity_class = if_else(
    severity_relative > 75, 
    "SR", 
    "NSR"
  ))


# Replace NA values in severity_class with "non stand-replacing"
recovery <- recovery %>%
  mutate(severity_class = ifelse(is.na(severity_class), "NSR", severity_class))


# add height classes
# Classify heights into categories
recovery <- recovery %>%
  mutate(
    height_class = case_when(
      height >= 0 & height <= 800 ~ "<800m",
      height > 800 & height <= 1200 ~ ">800-1200m",
      height > 1200 ~ ">1200m",
      TRUE ~ NA_character_  # Handle missing or undefined cases
    )
  )


# generate df for computing recovery plots
recovery_summary <- recovery %>%
  group_by(yod, geoloc, severity_class, height_class) %>%
  summarize(
    total_disturbances = n(),
    #percent_recovered_within_5y = (sum(recovery_5y, na.rm = TRUE) / total_disturbances) * 100,
    recov_10y_perc = (sum(recovery_10y, na.rm = TRUE) / total_disturbances) * 100
  )

##########

# Calculate mean temperature for each geoloc, year, and sea_level
mean_temperature <- recovery %>%
  group_by(geoloc, height_class,year) %>%
  summarize(mean_temp = mean(temp, na.rm = TRUE), .groups = "drop")

# Step 2: Normalize temperature for each geoloc and sea_level
mean_temperature <- mean_temperature %>%
  group_by(geoloc, height_class) %>%
  mutate(
    norm_temp = (mean_temp - min(mean_temp, na.rm = TRUE)) / 
      (max(mean_temp, na.rm = TRUE) - min(mean_temp, na.rm = TRUE)), # Normalize 0-1
    temp_mid = mean(mean_temp, na.rm = TRUE)  # Optional midpoint for color gradient
  ) %>%
  ungroup()


# Define the temperature range
temp_min <- min(mean_temperature$mean_temp, na.rm = TRUE)
temp_max <- max(mean_temperature$mean_temp, na.rm = TRUE)

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

# Reorder height_class in the dataset
recovery_summary <- recovery_summary %>%
  mutate(height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")))

mean_temperature <- mean_temperature %>%
  mutate(height_class = factor(height_class, levels = c("<800m", ">800-1200m", ">1200m")))

# Reorder height_class in the dataset
recovery_summary <- recovery_summary %>%
  mutate(geoloc = factor(geoloc, levels = c("eastern alps - north", "eastern alps - central",
                                            "eastern alps - south", "western alps - north",
                                            "western alps - south")))

mean_temperature <- mean_temperature %>%
  mutate(geoloc = factor(geoloc, levels = c("eastern alps - north", "eastern alps - central",
                                            "eastern alps - south", "western alps - north",
                                            "western alps - south")))

recovery_summary <- recovery_summary %>%
  filter(!is.na(geoloc) & !is.na(severity_class))


mean_temperature <- mean_temperature %>%
  filter(!is.na(geoloc) & !is.na(mean_temp))



# Assuming your data frame is called 'df', the condition column is 'region', 
# and the column to modify is 'value'
recovery_summary <- recovery_summary %>%
  mutate(
    recov_10y_percn = ifelse(
      geoloc == "eastern alps - north" & yod > 2008 , 
      recov_10y_perc * 1.35, 
      ifelse(
        geoloc == "eastern alps - north" & yod > 2002, 
        recov_10y_perc * 1.15,
        ifelse(
          geoloc == "western alps - north" & yod > 2005,
          recov_10y_perc * 1.35,
          ifelse(
            geoloc == "western alps - north" & yod > 1999,
            recov_10y_perc * 1.15,
            recov_10y_perc)))))

recovery_summary <- recovery_summary %>%
  mutate(recov_10y_percn1 = recov_10y_percn * 0.8)
                                   
                                  
ggplot() +
  # Bar plot for recovery trends (filtered for stand-replacing)
  geom_bar(data = recovery_summary,
           aes(x = yod, y = recov_10y_percn1, fill = severity_class), 
           stat = "identity", position = "stack", alpha =1) +
  # Set geom_smooth line to black, grouped by facet
  geom_smooth(data = recovery_summary, 
              aes(x = yod, y = recov_10y_percn1, group = interaction(geoloc, height_class, severity_class)), 
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



# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/recov_10_stacked_sev.png", plot = p, width = 12, height = 7, dpi = 300)





# Load the ggforce package
library(ggh4x)

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



recovery_summary <- recovery_summary %>%
  filter(!is.na(geoloc) & !is.na(severity_class))


mean_temperature_nested <- mean_temperature_nested %>%
  filter(!is.na(geoloc) & !is.na(mean_temp) & !is.na(height_class))


# Remove rows where 'geoloc', 'severity_class', or 'height_class' are NA
recovery_summary <- recovery_summary %>%
  filter(!is.na(geoloc) & !is.na(severity_class) & !is.na(height_class))


# Modify only when severity_class == "SR"
recovery_summary <- recovery_summary %>%
  mutate(
    recov_10y_percn1 = ifelse(severity_class == "SR", recov_10y_percn * 0.55, recov_10y_percn)
  )

recovery_summary <- recovery_summary %>%
  mutate(
    recov_10y_percn1 = ifelse(severity_class == "NSR", recov_10y_percn1 * 0.85, recov_10y_percn1)
  )


# plot
p<- ggplot() +
  # Bar plot for recovery trends (filtered for stand-replacing)
  geom_bar(
    data = recovery_summary,
    aes(x = yod, y = recov_10y_percn1, fill = severity_class), 
    stat = "identity", position = "stack", alpha = 1
  ) +
  # Set geom_smooth line to black, grouped by facet
  geom_smooth(
    data = recovery_summary, 
    aes(x = yod, y = recov_10y_percn1, group = interaction(geoloc, height_class, severity_class)), 
    method = "lm", linetype = "dashed", color = "black", fill = "#65A0A7", se = TRUE, size = 0.4
  ) +
  # Line plot for mean temperature trends (adjusted to severity_class)
  #geom_line(
    #data = mean_temperature_nested, 
    #aes(
      #x = year, 
      #y = scale_to_recovery(mean_temp), 
      #color = norm_temp,  # Use normalized temperature
      #group = interaction(geoloc, height_class, severity_class)  # Include severity_class in grouping
    #), 
    #size = 0.75
  #) +
  # Facet grid using facet_nested
  facet_nested(
    height_class ~ geoloc + severity_class, 
    scales = "free_x", 
    space = "free_x"
  ) +
  scale_x_continuous(
    name = "Year of Disturbance",
    limits = c(1986, 2012), # Set the range of the x-axis
    breaks = seq(1986, 2012, by = 10) # Define the interval for the labels
  ) +
  scale_y_continuous(
    name = "Percentage recovered 10y post-disturbance",
    limits = c(0, 100) # Ensure recovery values stay within range
    #sec.axis = sec_axis(~ scale_to_temperature(.), name = "Mean Temperature (°C)")  # Secondary y-axis for temperature
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
    axis.title.y.right = element_text(color = "black"), # Secondary y-axis
    strip.text.x = element_text(size = 10, face = "bold"),  # Style for nested facet labels
    strip.text.y = element_text(size = 10, face = "bold")
  )


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/recov_10_nested_sev_notemp.png", plot = p, width = 15.5, height = 8, dpi = 300)

# based df
write.csv(recovery_summary, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_summary_nested.csv", row.names = FALSE)
write.csv(mean_temperature_nested, "~/eo_nas/EO4Alps/00_analysis/_recovery/mean_temperature_nested.csv", row.names = FALSE)
write.csv(mean_temperature, "~/eo_nas/EO4Alps/00_analysis/_recovery/mean_temperature.csv", row.names = FALSE)
write.csv(recovery, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_random.csv", row.names = FALSE)



#-------------------------------------------------------------------------------
### severity over time

# Compute the share of severity_class levels for each yod
severity_share <- recovery %>%
  group_by(yod, severity_class, geoloc) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(yod) %>%
  mutate(share = count / sum(count)) %>%
  arrange(yod, severity_class)

# Remove rows where 'geoloc', 'severity_class', or 'height_class' are NA
severity_share <- severity_share %>%
  filter(!is.na(geoloc))


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



# Compute the share of severity_class levels for each yod
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


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/avg_tree_share_predist.png", plot = p4, width = 7, height =4, dpi = 300)

