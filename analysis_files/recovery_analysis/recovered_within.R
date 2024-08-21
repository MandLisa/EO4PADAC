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
library(mgcv)


recovery_all <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_all.csv")

### ysd

# Filter out only the records where ysd is positive and less than or equal to 38
recovery_filtered <- recovery_all %>%
  filter(ysd >= 1 & ysd <= 37)

# Count the number of pixels with each recovery rate
recovery_counts <- recovery_filtered %>%
  count(recovery_rate) %>%
  rename(ysd = recovery_rate, count = n)

# Calculate the total number of observations
total_observations <- nrow(recovery_filtered)

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

# Count the number of pixels with each recovery rate and severity class
recovery_counts <- recovery_filtered %>%
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

# Define custom colors for severity_class
custom_colors <- c(
  "non stand-replacing" = "blue",
  "stand-replacing" = "red"
)

# plot
png("~/eo_nas/EO4Alps/figs/recovered_within_severity.png", 
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


# keep agent information


# Count the number of pixels with each recovery rate and severity class
recovery_counts <- recovery_filtered %>%
  group_by(agent_name, recovery_rate) %>%
  count() %>%
  rename(ysd = recovery_rate, count = n) %>%
  ungroup() %>%
  group_by(agent_name) %>%
  mutate(total_pixels = sum(count)) %>%
  ungroup()

# Calculate cumulative count and percentage within each severity class
recovery_cumulative <- recovery_counts %>%
  arrange(agent_name, ysd) %>%
  group_by(agent_name) %>%
  mutate(cumulative_count = cumsum(count)) %>%
  mutate(cumulative_perc = (cumulative_count / total_pixels) * 100) %>%
  ungroup()


# Create the final data frame with severity class
recovery_within_I <- recovery_cumulative %>%
  select(agent_name, ysd, cumulative_perc)

# Multiply cumulative_perc by 100
recovery_within_I <- recovery_cumulative %>%
  mutate(cumulative_perc = cumulative_perc * 100) %>%
  select(agent_name, ysd, cumulative_perc)


recovery_within_I <- recovery_within_I %>%
  filter(!is.na(agent_name))

# Define custom colors
custom_colors <- c("other" = "blue",
                   "Bark Beetle/Wind" ="#f4d03f",        # Example color for "Low"
                   "Fire" = "#78281f") # Example color for "Medium"

# plot
png("~/eo_nas/EO4Alps/figs/recovered_within_agent.png", 
    units="in", width=6, height=4, res=300)
ggplot(recovery_within_I, aes(x = ysd, y = cumulative_perc, color = agent_name)) +
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




### geoloc
# Filter out only the records where ysd is positive and less than or equal to 38

# Count the number of pixels with each recovery rate and severity class
recovery_counts <- recovery_filtered %>%
  group_by(geoloc, recovery_rate) %>%
  count() %>%
  rename(ysd = recovery_rate, count = n) %>%
  ungroup() %>%
  group_by(geoloc) %>%
  mutate(total_pixels = sum(count)) %>%
  ungroup()

# Calculate cumulative count and percentage within each severity class
recovery_cumulative <- recovery_counts %>%
  arrange(geoloc, ysd) %>%
  group_by(geoloc) %>%
  mutate(cumulative_count = cumsum(count)) %>%
  mutate(cumulative_perc = (cumulative_count / total_pixels) * 100) %>%
  ungroup()


# Create the final data frame with severity class
recovery_within_I <- recovery_cumulative %>%
  select(geoloc, ysd, cumulative_perc)

# Multiply cumulative_perc by 100
recovery_within_I <- recovery_cumulative %>%
  mutate(cumulative_perc = cumulative_perc * 100) %>%
  select(geoloc, ysd, cumulative_perc)


recovery_within_I <- recovery_within_I %>%
  filter(!is.na(geoloc))

# Define custom colors
custom_colors <- c("Northern West Alps" = "#2471a3",
                   "Southern West Alps" ="#dc7633",        # Example color for "Low"
                   "Northern East Alps" = "#27ae60",
                   "Central Alps" = "#34495e",
                   "Southern East Alps" = "#f1c40f") 



# plot
png("~/eo_nas/EO4Alps/figs/recovered_within_geoloc.png", 
    units="in", width=6, height=4, res=300)
ggplot(recovery_within_I, aes(x = ysd, y = cumulative_perc, color = geoloc)) +
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





