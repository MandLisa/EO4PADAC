# Aggregate data
heatmap_data <- recovery_distinct1 %>%
  group_by(geoloc, VPD_cat_yod) %>%
  summarise(mean_recovery_rate = mean(recovery_rate, na.rm = TRUE))

# Plot
ggplot(heatmap_data, aes(x = VPD_cat_yod, y = geoloc, fill = mean_recovery_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#F5F2A9", high = "#900C3F") +
  labs(
    title = "Mean Recovery Rate Heatmap",
    x = "VPD Category",
    y = "Geolocation",
    fill = "Mean Recovery Rate"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold")
  )



# Aggregate data
heatmap_data <- recovery_distinct1 %>%
  group_by(geoloc, height_class) %>%
  summarise(mean_recovery_rate = mean(recovery_rate, na.rm = TRUE))

# Plot
ggplot(heatmap_data, aes(x = height_class, y = geoloc, fill = mean_recovery_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#F5F2A9", high = "#900C3F") +
  labs(
    title = "Mean Recovery Rate Heatmap",
    x = "VPD Category",
    y = "Geolocation",
    fill = "Mean Recovery Rate"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold")
  )


# Aggregate data
heatmap_data <- recovery_distinct1 %>%
  group_by(geoloc, severity_class) %>%
  summarise(mean_recovery_rate = mean(recovery_rate, na.rm = TRUE))

# Plot
ggplot(heatmap_data, aes(x = severity_class, y = geoloc, fill = mean_recovery_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#F5F2A9", high = "#900C3F") +
  labs(
    title = "Mean Recovery Rate Heatmap",
    x = "VPD Category",
    y = "Geolocation",
    fill = "Mean Recovery Rate"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold")
  )



# Aggregate data
heatmap_data <- recovery_distinct1 %>%
  group_by(geoloc, forest_type) %>%
  summarise(mean_recovery_rate = mean(recovery_rate, na.rm = TRUE))

# Plot
ggplot(heatmap_data, aes(x = forest_type, y = geoloc, fill = mean_recovery_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#F5F2A9", high = "#900C3F") +
  labs(
    title = "",
    x = "Severity level",
    y = "Geolocation",
    fill = "Mean recovery interval"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold")
  )




heatmap_data <- recovery_distinct1 %>%
  group_by(geoloc, height_class, severity_class, VPD_cat_yod, forest_type) %>%  # Replace driver/category with your specific variables
  summarise(mean_recovery_interval = mean(recovery_rate, na.rm = TRUE)) %>%
  ungroup()

# Reorder geoloc manually
heatmap_data <- heatmap_data %>%
  mutate(
    height_class = factor(height_class, levels = c(
      "<800m",
      ">800-1200m",
      ">1200m"
    ))
  )

# Remove rows where VPD_class_yod1 is "undefined"
heatmap_data <- heatmap_data %>%
  filter(forest_type != "undefined")


heatmap_data <- recovery_distinct1 %>%
  pivot_longer(
    cols = c(severity_class, forest_type, height_class, VPD_cat_yod),
    names_to = "driver",
    values_to = "category"
  ) %>%
  group_by(geoloc, driver, category) %>%
  summarise(mean_recovery_interval = mean(recovery_rate, na.rm = TRUE)) %>%
  ungroup()


# Reorder geoloc manually
heatmap_data <- heatmap_data %>%
  mutate(
    geoloc = factor(geoloc, levels = c(
      "western alps - north",
      "western alps - south",
      "eastern alps - south",
      "eastern alps - central",
      "eastern alps - north"
    ))
  )

# Remove rows where VPD_class_yod1 is "undefined"
heatmap_data <- heatmap_data %>%
  filter(forest_type != "undefined")


# Explicitly set levels for each driver
heatmap_data <- heatmap_data %>%
  mutate(category = case_when(
    driver == "height_class" ~ factor(category, levels = c("<800m", ">800-1200m", ">1200m")),
    driver == "forest_type" ~ factor(category, levels = c("broadleaved-dominant", "coniferous-dominant", "mixed", "undefined")),
    driver == "VPD_cat_yod" ~ factor(category, levels = c("Dry", "Average", "Wet")),
    driver == "severity_class" ~ factor(category, levels = c("NSR", "SR")),
    TRUE ~ as.factor(category)  # Fallback for other potential drivers
  ))


# Reorder driver levels
heatmap_data <- heatmap_data %>%
  mutate(driver = fct_relevel(driver, 
                              "severity_class", "height_class", "forest_type", "VPD_cat_yod"))


# Define custom facet labels
driver_labels <- c(
  "forest_type" = "Forest Type",
  "height_class" = "Height Class",
  "severity_class" = "Severity Class",
  "VPD_cat_yod" = "VPD Category"
)


# Remove rows where forest_type is "undefined"
heatmap_data <- heatmap_data %>%
  filter(!(driver == "forest_type" & category == "undefined"))



ggplot(heatmap_data, aes(x = category, y = geoloc, fill = mean_recovery_interval)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#FDFD96", high = "#900C3F") +  # Yellow to dark red
  facet_wrap(~ driver, scales = "free_x", nrow = 2, 
             labeller = labeller(driver = driver_labels)) +  # Create one heatmap per driver
  labs(
    title = "",
    x = "",
    y = "",
    fill = "Mean recovery interval [year]"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines")
  ) +
  theme_bw(base_size = 14)


# Save the last plotted ggplot
ggsave(
  filename = "~/eo_nas/EO4Alps/figs/heatmap.png",    
  width = 13,                     
  height = 8,                    
  dpi = 300)   
