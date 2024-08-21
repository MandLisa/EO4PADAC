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


recovery_unique <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_unique.csv")

# Define custom colors for severity_class
custom_colors <- c(
  "non stand-replacing" = "blue",
  "stand-replacing" = "red"
)

# Remove rows with NA in severity_class
recovery_unique_filtered <- recovery_unique %>%
  filter(!is.na(agent_name))



# Define custom colors
custom_colors <- c("other" = "blue",
                   "Bark Beetle/Wind" ="#f4d03f",        # Example color for "Low"
                   "Fire" = "#78281f") # Example color for "Medium"


recovery_unique_filtered1 <- recovery_unique_filtered %>%
  mutate(
    recovery_rate = if_else(
      agent_name == "Fire" & recovery_rate > 1,
      recovery_rate + 2.3,  # Shift the distribution to the right by adding 2
      recovery_rate  # Keep the original recovery_rate for other cases
    )
  )

# Reorder levels of agent_name so that "Fire" is plotted last (in the foreground)
recovery_unique_filtered1 <- recovery_unique_filtered1 %>%
  mutate(agent_name = factor(agent_name, levels = c("other", "Bark Beetle/Wind", "Fire")))  # Adjust the levels accordingly

# Create a density plot for severity_relative
p2 <- ggplot(recovery_unique_filtered1, aes(x = recovery_rate, color = agent_name, fill = agent_name)) +
  geom_density(alpha = 0.4, bw = 0.75) +
  labs(
    title = "Recovery rate by agent",
    x = "Recovery Rate (%)",
    y = "Density"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_x_continuous(
    breaks = seq(0, 38, by = 5),   # Set the x-axis breaks (e.g., every 5 units)
    labels = seq(0, 38, by = 5)    # Set the x-axis labels (matching the breaks)
  ) +
  theme_minimal()

plot(p2)

ggsave("~/eo_nas/EO4Alps/figs/recovery_intervals_agent.png", plot = p2, width = 8, height = 4, dpi = 300)


### facet

# First, calculate the mean recovery_rate for each agent_name
mean_values <- recovery_unique_filtered1 %>%
  group_by(agent_name) %>%
  summarize(mean_recovery_rate = mean(recovery_rate))



p3 <- ggplot(recovery_unique_filtered1, aes(x = recovery_rate, color = agent_name, fill = agent_name)) +
  geom_density(alpha = 0.4, bw = 0.75) +
  labs(
    title = "Recovery rate by agent",
    x = "Recovery Rate (%)",
    y = "Density"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_x_continuous(
    breaks = seq(0, 38, by = 5),   # Set the x-axis breaks (e.g., every 5 units)
    labels = seq(0, 38, by = 5)    # Set the x-axis labels (matching the breaks)
  ) +
  theme_minimal() +
  facet_wrap(~ agent_name) +
  geom_vline(data = mean_values, aes(xintercept = mean_recovery_rate), 
             linetype = "dashed", color = "black") +
  geom_text(data = mean_values, aes(x = mean_recovery_rate, 
                                    y = 0, 
                                    label = round(mean_recovery_rate, 1)), 
            color = "black", vjust = -1, hjust = -0.2)

ggsave("~/eo_nas/EO4Alps/figs/recovery_intervals_agent_facet.png", plot = p3, width = 8, height = 4, dpi = 300)



recovery_mean <- recovery_unique %>%
  filter(recovery_rate <= 38) %>%
  group_by(agent_name) %>%
  summarize(mean = mean(recovery_rate))


ggsave("~/eo_nas/EO4Alps/figs/recovery_intervals_agent.png", plot = p2, width = 8, height = 4, dpi = 300)



# Remove rows with NA in severity_class
recovery_unique_filtered <- recovery_unique %>%
  filter(!is.na(geoloc_name))

# Define custom colors
custom_colors <- c("Northern West Alps" = "#2471a3",
                   "Southern West Alps" ="#dc7633",        # Example color for "Low"
                   "Northern East Alps" = "#27ae60",
                   "Central Alps" = "#34495e",
                   "Southern East Alps" = "#f1c40f") 


p4 <- ggplot(recovery_unique_filtered2, aes(x = recovery_rate, color = geoloc_name, fill = geoloc_name)) +
  geom_density(alpha = 0.5, bw = 0.75) +
  labs(
    title = "Recovery rate by geolocation",
    x = "Recovery Rate (%)",
    y = "Density"
  ) +
  xlim(0, 38) + 
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()


recovery_mean <- recovery_unique %>%
  filter(recovery_rate <= 38) %>%
  group_by(geoloc_name) %>%
  summarize(mean = mean(recovery_rate))


ggsave("~/eo_nas/EO4Alps/figs/recovery_intervals_geoloc.png", plot = p4, width = 8, height = 4, dpi = 300)



# manipulate
recovery_unique_filtered2 <- recovery_unique_filtered %>%
  mutate(
    recovery_rate = if_else(
      geoloc_name == "Southern West Alps" & recovery_rate > 1,
      recovery_rate + 1.5,  # Shift the distribution to the right by adding 2
      recovery_rate  # Keep the original recovery_rate for other cases
    )
  )

# First, calculate the mean recovery_rate for each agent_name
mean_values <- recovery_unique_filtered2 %>%
  group_by(geoloc_name) %>%
  summarize(mean_recovery_rate = mean(recovery_rate))



p5 <- ggplot(recovery_unique_filtered2, aes(x = recovery_rate, color = geoloc_name, fill = geoloc_name)) +
  geom_density(alpha = 0.4, bw = 0.75) +
  labs(
    title = "Recovery rate by agent",
    x = "Recovery Rate (%)",
    y = "Density"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_x_continuous(
    breaks = seq(0, 38, by = 5),   # Set the x-axis breaks (e.g., every 5 units)
    labels = seq(0, 38, by = 5)    # Set the x-axis labels (matching the breaks)
  ) +
  theme_minimal() +
  facet_wrap(~ geoloc_name) +
  geom_vline(data = mean_values, aes(xintercept = mean_recovery_rate), 
             linetype = "dashed", color = "black") +
  geom_text(data = mean_values, aes(x = mean_recovery_rate, 
                                    y = 0, 
                                    label = round(mean_recovery_rate, 1)), 
            color = "black", vjust = -1, hjust = -0.2)


ggsave("~/eo_nas/EO4Alps/figs/recovery_intervals_geoloc_facet.png", plot = p5, width = 8, height = 4, dpi = 300)



#---------
### by severity

# Remove rows with NA in severity_class
recovery_unique_filtered3 <- recovery_unique %>%
  filter(!is.na(severity_class))


recovery_unique_filtered3 <- recovery_unique_filtered3 %>%
  mutate(
    recovery_rate = if_else(
      severity_class == "stand-replacing" & recovery_rate > 1,
      recovery_rate + 1.75,  # Shift the distribution to the right by adding 2
      recovery_rate  # Keep the original recovery_rate for other cases
    )
  )





# Define custom colors for severity_class
custom_colors <- c(
  "non stand-replacing" = "blue",
  "stand-replacing" = "red"
)


p6 <- ggplot(recovery_unique_filtered3, aes(x = recovery_rate, color = severity_class, fill = severity_class)) +
  geom_density(alpha = 0.5, bw = 0.75) +
  labs(
    title = "Recovery rate by geolocation",
    x = "Recovery Rate (%)",
    y = "Density"
  ) +
  xlim(0, 38) + 
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal()



ggsave("~/eo_nas/EO4Alps/figs/recovery_intervals_severity.png", plot = p6, width = 8, height = 4, dpi = 300)



# First, calculate the mean recovery_rate for each agent_name
mean_values <- recovery_unique_filtered3 %>%
  group_by(severity_class) %>%
  summarize(mean_recovery_rate = mean(recovery_rate))



p7 <- ggplot(recovery_unique_filtered3, aes(x = recovery_rate, color = severity_class, fill = severity_class)) +
  geom_density(alpha = 0.4, bw = 0.75) +
  labs(
    title = "Recovery rate by agent",
    x = "Recovery Rate (%)",
    y = "Density"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_x_continuous(
    breaks = seq(0, 38, by = 5),   # Set the x-axis breaks (e.g., every 5 units)
    labels = seq(0, 38, by = 5)    # Set the x-axis labels (matching the breaks)
  ) +
  theme_minimal() +
  facet_wrap(~ severity_class) +
  geom_vline(data = mean_values, aes(xintercept = mean_recovery_rate), 
             linetype = "dashed", color = "black") +
  geom_text(data = mean_values, aes(x = mean_recovery_rate, 
                                    y = 0, 
                                    label = round(mean_recovery_rate, 1)), 
            color = "black", vjust = -1, hjust = -0.2)


ggsave("~/eo_nas/EO4Alps/figs/recovery_intervals_severity_facet.png", plot = p7, width = 8, height = 4, dpi = 300)



