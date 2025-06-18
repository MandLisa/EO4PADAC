library(ggplot2)
library(gganimate)
library(dplyr)
library(readr)
library(dplyr)
library(tidyr)

land_cover_share <- read_csv("Y:/01_PhD/00_thesis/defense/land_cover_share.csv")

df_long <- land_cover_share %>%
  pivot_longer(cols = c(tree, bare, grassland),
               names_to = "class",
               values_to = "share")

# Smooth the data manually using loess per class
df_smooth <- df_long %>%
  group_by(class) %>%
  arrange(year) %>%
  mutate(smoothed = loess(share ~ year, span = 0.25)$fitted)

p <- ggplot(df_smooth, aes(x = year, y = smoothed, color = class, group = class)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 2003, linetype = "dashed", color = "black", size = 1.5) + 
  annotate("text", x = 2003, y = 80, label = "Year of disturbance", 
           angle = 90, vjust = -0.5, size = 6, fontface = "bold") +
  scale_color_manual(values = c(
    "tree" = "#0DFE14",
    "grassland" = "#D92B0A",
    "bare" = "#4E23FF"
  )) +
  ylim(0, 100) +
  labs(x = "Year", y = "Fractional share (%)", color = "Endmembers") +
  theme_minimal(base_size = 22) +
  transition_reveal(year)

animate(p, width = 800, height = 400, duration = 17, fps = 2, renderer = gifski_renderer())

animate(p,
        width = 1920,        # Full HD
        height = 1080,
        fps = 2,
        duration = 17,
        res = 150,          
        renderer = gifski_renderer("Y:/01_PhD/00_thesis/defense/line_plot_animations.gif"))




p <- ggplot(df_smooth, aes(x = year, y = smoothed, color = class, group = class)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 2002, linetype = "dashed", color = "black", size = 1.5) + 
  annotate("text", x = 2002, y = 80, label = "Year of disturbance", 
           angle = 90, vjust = -0.5, size = 6, fontface = "bold") +
  scale_color_manual(
    values = c(
      "tree" = "#0DFE14",       # bright green
      "grassland" = "#D92B0A",  # reddish
      "bare" = "#4E23FF"        # blue-purple
    ),
    labels = c(
      "tree" = "Tree cover",
      "grassland" = "Grassland",
      "bare" = "Bare ground"
    )
  ) +
  labs(
    x = "Year",
    y = "Fractional land cover (%)",
    color = "Endmembers"
  ) +
  ylim(0, 100) +
  theme_minimal(base_size = 22) +
  theme(legend.position = "bottom") +
  transition_reveal(year)



