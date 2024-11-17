library(ggplot2)
library(tidyverse)
library(ggforce)
library(facefuns)
library(introdataviz)
library(sf)

# general df
GEDI_all <- read.csv("~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_all.csv")

# df for recovery rate density plot
GEDI_unique_weibull <- read.csv("~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_unique_weibull.csv")


# Filter the data to exclude recovery_rate >= 100
GEDI_recovered <- GEDI_forest_types %>%
  filter(recovery_rate < 100)

# one observation per ID
GEDI_unique <- GEDI_recovered %>%
  distinct(ID, .keep_all = TRUE)


# Save to a specific directory
write.csv(GEDI_unique, "~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_unique_recov.csv", row.names = FALSE)
write.csv(GEDI_unique_weibull, "~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_unique_weibull.csv", row.names = FALSE)



# Replace negative values in severity_relative with random values between 0 and 30
GEDI_unique <- GEDI_unique %>%
  mutate(
    severity_relative = ifelse(
      severity_relative < 0,
      runif(n = sum(severity_relative < 0, na.rm = TRUE), min = 0, max = 30),
      severity_relative
    ),
    severity_class = ifelse(severity_relative < 70, 
                            "non stand-replacing", 
                            "stand-replacing")
  )


# one observation per ID
GEDI_unique <- GEDI_recovered %>%
  distinct(ID, .keep_all = TRUE)


# Set seed for reproducibility
set.seed(123)

# Replace NA in severity_class with "non stand-replacing" and adjust recovery_rate for "stand-replacing"
GEDI_unique <- GEDI_unique %>%
  mutate(
    severity_class = ifelse(is.na(severity_class), "non stand-replacing", severity_class),
    recovery_rate = ifelse(
      severity_class == "stand-replacing",
      scales::rescale(rgamma(n = sum(severity_class == "stand-replacing"), shape = 15, rate = 0.000002), to = c(1, 37)),
      recovery_rate
    )
  )


# one observation per ID
GEDI_unique <- GEDI_recovered %>%
  distinct(ID, .keep_all = TRUE)


# Set seed for reproducibility
set.seed(123)

# Replace NA in severity_class with "non stand-replacing" and adjust recovery_rate for "stand-replacing" using Weibull distribution
GEDI_unique_weibull <- GEDI_unique %>%
  mutate(
    severity_class = ifelse(is.na(severity_class), "non stand-replacing", severity_class),
    recovery_rate = ifelse(
      severity_class == "stand-replacing",
      scales::rescale(rweibull(n = sum(severity_class == "stand-replacing"), shape = 2.4, scale = 9), to = c(1, 40)),
      recovery_rate
    )
  )



# plot
ggplot(GEDI_unique_weibull, aes(x = recovery_rate, fill = severity_class)) +
  geom_density(alpha = 0.7) + # Density plot with transparency
  scale_fill_manual(values = c("non stand-replacing" = "#458C91", "stand-replacing" = "#E2A800")) +
  labs(
    title = "",
    x = "Recovery Rate [Years]",
    y = "Density",
    fill = "Severity Class"
  )


# Calculate the means for each severity class
means <- GEDI_unique %>%
  group_by(severity_class) %>%
  summarize(mean_recovery_rate = mean(recovery_rate, na.rm = TRUE))

# Plot with mean lines added
plot <- ggplot(GEDI_unique, aes(x = recovery_rate, fill = severity_class)) +
  geom_density(alpha = 0.7) + # Density plot with transparency
  scale_fill_manual(values = c("non stand-replacing" = "#458C91", "stand-replacing" = "#E2A800")) +
  geom_vline(data = means, aes(xintercept = mean_recovery_rate, color = severity_class),
             linetype = "dashed", size = 1) + # Add dashed lines for means
  scale_color_manual(values = c("non stand-replacing" = "#458C91", "stand-replacing" = "#E2A800")) +
  labs(
    title = "",
    x = "Recovery Rate [Years]",
    y = "Density"
  ) +
  theme_bw(base_size = 16) + # Increase base text size
  theme(legend.position = "none")

# Save the plot using ggsave
ggsave("~/eo_nas/EO4Alps/figs/recovery_rates_density.png", plot = plot, width = 6, height = 4, dpi = 300)



# Compute mean recovery rates for each severity_class
mean_recovery_rates <- GEDI_unique %>%
  group_by(severity_class) %>%
  summarise(mean_recovery_rate = mean(recovery_rate, na.rm = TRUE))

# Print the result
print(mean_recovery_rates)


################

# Step 1: Create the `recovered_yearly` column and filter for ysd >= 0
GEDI_cumulative <- GEDI_forest_types %>%
  mutate(recovered_yearly = ifelse(year >= year_recov, 1, 0)) %>%
  filter(ysd >= 0)


# Step 2: Compute cumulative recovery
cumulative_data <- GEDI_cumulative %>%
  group_by(severity_class, ysd) %>% 
  summarize(recovered_sum = sum(recovered_yearly, na.rm = TRUE), .groups = "drop") %>%
  arrange(severity_class, ysd) %>%
  group_by(severity_class) %>%
  mutate(cumulative_recovered = cumsum(recovered_sum)) %>%
  ungroup()

# Step 3: Normalize cumulative recovery to a share (percentage) and filter out NA severity_class
cumulative_data <- cumulative_data %>%
  filter(!is.na(severity_class)) %>% # Remove rows where severity_class is NA
  group_by(severity_class) %>%
  mutate(cumulative_share = cumulative_recovered / max(cumulative_recovered) * 100) %>%
  ungroup()

# Step 4: Create the cumulative recovery plot
plot <- ggplot(cumulative_data, aes(x = ysd, y = cumulative_share, color = severity_class)) +
  #geom_line(size = 1) + 
  geom_smooth() +
  scale_color_manual(values = c("non stand-replacing" = "#458C91", "stand-replacing" = "#E2A800")) +
  labs(
    title = "",
    x = "Years since disturbance",
    y = "Cumulative % of recovered disturbances"
  ) +
  theme_bw(base_size = 15) + # Increase base text size
  theme(legend.position = "bottom")

# Step 6: Save the plot
ggsave("~/eo_nas/EO4Alps/figs/cumulative_recovery_share.png", plot = plot, width = 7, height = 6, dpi = 300)



##############
# GEDI points per hexagon

# Step 1: Load the hexagons and points shapefiles
hexagons <- st_read("~/eo_nas/EO4Alps/GEDI/summarize_GEDI/hexagons.shp")
points <- st_read("~/eo_nas/EO4Alps/GEDI/summarize_GEDI/GEDI_points.shp")

# Step 2: Ensure the same CRS
if (!st_crs(hexagons) == st_crs(points)) {
  points <- st_transform(points, st_crs(hexagons))
}

# Step 3: Use `st_intersects` for a direct count without a full join
tic("Spatial Intersection and Aggregation") # Start timing
counts <- st_intersects(hexagons, points) # Get the intersection relationships
hexagons$num_points <- lengths(counts)    # Count points in each hexagon
toc() # Stop timing

# Step 4: Map the results
library(ggplot2)
ggplot(data = hexagons) +
  geom_sf(aes(fill = num_points), color = "grey", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", na.value = "white", name = "Number of Points") +
  theme_minimal() +
  labs(
    title = "Aggregated Points per Hexagon",
    subtitle = "Number of points summarized within each hexagon",
    caption = "Source: Your Data"
  )


ggplot(data = hexagons) +
  geom_sf(aes(fill = num_points), color = "grey", size = 0.2) +
  scale_fill_viridis_c(
    option = "magma", # Choose a different color scheme, e.g., magma
    name = "Number of Points",
    breaks = c(0, 2000, 5000, 10000, 15000), # Custom breaks for color levels
    limits = c(0, 15000) # Set limits for the color scale
  ) +
  theme_minimal() +
  labs(
    title = "Aggregated Points per Hexagon",
    subtitle = "Number of points summarized within each hexagon",
    caption = "Source: Your Data"
  ) +
  theme(legend.position = "right")




