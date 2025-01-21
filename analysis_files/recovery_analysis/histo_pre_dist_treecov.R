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
library(readr)

recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_312025.csv")

# one observation per ID
recovery_unique <- recovery %>%
  distinct(ID, .keep_all = TRUE)


# Create the histogram
ggplot(recovery_unique, aes(x = tree_share_before)) +
  geom_histogram(
    binwidth = 2, 
    aes(fill = ..count..), 
    color = "black", 
    boundary = 0
  ) +
  scale_fill_gradientn(
    colors = c("#5E2D7F", "#2599C1", "#79CBBC", "#EBF8B2"),
    name = "Frequency"
  ) +
  labs(
    title = "",
    x = "Pre-disturbance tree cover (%)",
    y = "Frequency"
  ) +
  theme_minimal()



# Adjust the fill gradient to increase with x-axis
ggplot(recovery_unique, aes(x = tree_share_before)) +
  geom_histogram(
    binwidth = 2, 
    aes(fill = ..x..), # Map the fill to x-axis values
    color = "black", 
    boundary = 0
  ) +
  scale_fill_gradientn(
    colors = c("#EDF8B1","#C7E9B4", "#41B6C4", "#A80084", "#73004C"),
    name = "Tree Cover"
  ) +
  labs(
    title = "",
    x = "Pre-disturbance tree cover (%)",
    y = "Frequency"
  ) +
  theme(legend.position = "none") +
  theme_minimal(base_size = 30)


# Save the last plotted ggplot
ggsave(
  filename = "~/eo_nas/EO4Alps/figs/histo_pre_dist_tree_cover.png",    
  width = 12,                     
  height = 6,                    
  dpi = 300)   




# Create a dummy plot to generate the legend
dummy_plot <- ggplot(data.frame(x = c(0, 1), y = c(0, 1), z = c(0, 100)), aes(x = x, y = y, fill = z)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = c("#EDF8B1", "#C7E9B4", "#41B6C4", "#A80084", "#73004C"),
    name = "Tree Cover (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.5, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Extract the legend
legend <- cowplot::get_legend(dummy_plot)

# Display the legend separately
cowplot::plot_grid(legend, ncol = 1)




