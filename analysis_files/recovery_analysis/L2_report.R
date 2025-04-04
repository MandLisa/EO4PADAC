library(readr)
library(ggplot2)
library(dplyr)


metadata_sensor <- read_delim("eo_nas/EO4Alps/metadata_sensor.csv", 
                              delim = ",", escape_double = FALSE, trim_ws = TRUE)

# Reorder the sensor factor levels
metadata_sensor$sensor <- factor(metadata_sensor$sensor, levels = c("LT04", "LT05", "LE07", "LC08", "LC09", "S2A", "S2B"))
metadata_sensor$sensor <- factor(metadata_sensor$sensor, levels = c("S2A", "S2B", "LC09", "LC08", "LE07", "LT05", "LT04"))



# Create the plot
ggplot(metadata_sensor, aes(x = Year, y = number1, fill = sensor)) +
  geom_bar(stat = "identity", position = "stack") + # Stacked bar chart
  labs(
    x = "Year",
    y = "# of L1 Images",
    fill = "Sensor"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels
  )



ggplot(metadata_sensor, aes(x = Year, y = number1, fill = sensor)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis_d(option = "magma") + # Options: "viridis", "magma", "plasma", "cividis"
  labs(
    x = "Year",
    y = "# of L1 Images",
    fill = "Sensor"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



p1 <- ggplot(metadata_sensor, aes(x = Year, y = number1, fill = sensor)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set3") + 
  scale_x_continuous(breaks = seq(1986, 2023, by = 5)) +  
  labs(
    x = "Year",
    y = "# of L1 images",
    fill = "Sensor"
  ) +
  theme_bw(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save the plot
ggsave("~/eo_nas/EO4Alps/figs/L1_data.png", plot = p1, width = 8, height = 4.5, dpi = 300)



library(ggplot2)
library(viridis)

# Create a data frame for the gradient
gradient_df <- data.frame(x = seq(0, 1, length.out = 100), y = 1)

# Create the plot
legend_plot <- ggplot(gradient_df, aes(x = x, y = y, fill = x)) +
  geom_tile() +  # Create the gradient
  scale_fill_viridis_c(option = "mako", name = "Legend Title") +  # Use viridis continuous scale
  labs(x = "", y = "") +  # Remove axis labels
  theme_void() +  # Remove all other elements
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),  # Adjust the width of the legend
    legend.key.height = unit(0.5, "cm")  # Adjust the height of the legend
  )

# Display the legend
print(legend_plot)



# Define the range for the gradient
gradient_df <- data.frame(x = seq(0, 1, length.out = 100), y = 1)

# Create the horizontal legend
legend_plot <- ggplot(gradient_df, aes(x = x, y = y, fill = x)) +
  geom_tile() +  # Create the gradient bar
  scale_fill_gradientn(colors = c("#440154FF", "#21908CFF", "#edf6b5"), name = "Legend Title") + # Define colors
  labs(x = "", y = "") +  # Remove axis labels
  theme_void() +  # Remove other plot elements
  theme(
    legend.position = "bottom",  # Place the legend horizontally
    legend.key.width = unit(2, "cm"),  # Adjust legend size
    legend.key.height = unit(0.5, "cm")
  )

# Display the plot
print(legend_plot)





