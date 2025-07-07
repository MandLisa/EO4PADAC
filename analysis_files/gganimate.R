library(ggplot2)
library(gganimate)
library(dplyr)




p <- ggplot(df, aes(x = year, y = share, color = class, group = class)) +
  geom_line(size = 1.2) +
  labs(x = "Year", y = "Land Cover Share (%)", color = "Land Cover") +
  theme_minimal(base_size = 14) +
  ylim(0, 100) +
  transition_reveal(along = year)  # This makes the line animate over x-axis (time)

# Render animation
animate(p, width = 800, height = 500, duration = 5, fps = 20, renderer = gifski_renderer())



