library(ggplot2)
library(dplyr)
library(tidyr)

# Example structure of your data:
# spectral_profiles should have columns like: class, BLUE, GREEN, RED, NIR, SWIR1, SWIR2
spectral_profiles <- read_csv("/mnt/eo/EO4Alps/00_analysis/_recovery/spectral_profiles.csv")

# Reshape data to long format
spectral_long <- spectral_profiles %>%
  pivot_longer(cols = c(BLUE, GREEN, RED, NIR, SWIR1, SWIR2),
               names_to = "band",
               values_to = "reflectance")

# Ensure correct band order
spectral_profiles$band <- factor(spectral_profiles$band,
                             levels = c("BLUE", "GREEN", "RED", "NIR", "SWIR1", "SWIR2"))

# Plot spectral profiles
ggplot(spectral_profiles, aes(x = band, y = value, group = class, color = class)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~class) +
  labs(x = "Spectral Band", y = "Reflectance", title = "Spectral Profiles by Class") +
  theme_bw(base_size = 14) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())






# --- Add artificial ribbon widths ---
# Define ribbon size by class (arbitrary example)
library(dplyr)
# 1. Define ribbon widths explicitly (shrubland gets the largest)
ribbon_widths <- data.frame(
  class = c("coniferous", "broadleaved", "shrubland", "bramble"),
  ribbon = c(50, 100, 200, 120)  # shrubland gets ±200
)

spectral_profiles <- spectral_profiles %>%
  dplyr::select(band, value, class) %>%
  left_join(ribbon_widths, by = "class") %>%
  mutate(
    ymin = value - ribbon,
    ymax = value + ribbon,
    band_num = as.numeric(factor(band, levels = c("BLUE", "GREEN", "RED", "NIR", "SWIR1", "SWIR2")))
  )

spectral_profiles <- spectral_profiles %>%
  mutate(class = factor(class, levels = c("coniferous", "broadleaved", "shrubland", "bramble")))


ggplot(spectral_profiles, aes(x = band_num, y = value, group = class)) +
  #geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = class), alpha = 0.2, color = NA) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 2) +
  facet_wrap(~class) +
  scale_x_continuous(breaks = 1:6, labels = c("BLUE", "GREEN", "RED", "NIR", "SWIR1", "SWIR2")) +
  labs(
    x = "Spectral band",
    y = "Reflectance (STMs)",
    title = ""
  ) +
  ylim(0, 3500) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none", 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


ggplot(spectral_profiles, aes(x = band_num, y = value, group = class, color = class, fill = class)) +
  #geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 1:6,
    labels = c("BLUE", "GREEN", "RED", "NIR", "SWIR1", "SWIR2")
  ) +
  labs(
    x = "Spectral band",
    y = "Reflectance (STMs)",
    title = ""
  ) +
  ylim(0, 3200) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

spectral_profiles <- spectral_profiles %>%
  mutate(class = case_when(
    class == "shrubland" ~ "mountain pine",
    TRUE ~ class
  ))

spectral_profiles$class <- factor(
  spectral_profiles$class,
  levels = c("coniferous", "broadleaved", "mountain pine", "bramble")
)

ggplot(spectral_profiles, aes(x = band, y = value, group = class, color = class)) +
  #geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = class), alpha = 0.15, color = NA) +
  geom_line(aes(alpha = class), linewidth = 0.75) +
  geom_point(aes(alpha = class), size = 2) +
  scale_x_discrete(
    limits = c("BLUE", "GREEN", "RED", "NIR", "SWIR1", "SWIR2")
  ) +
  # Set custom colors
  scale_color_manual(
    values = c(
      "coniferous" = "#014C00",  # dark green
      "broadleaved" = "#88c341", # orange
      "bramble" = "#d95f02",     # purple
      "mountain pine" = "#6C0000"    # pink
    )
  ) +
  # Set fill colors to match line colors
  scale_fill_manual(
    values = c(
      "coniferous" = "#014C00",  # dark green
      "broadleaved" = "#88c341", # orange
      "bramble" = "#d95f02",     # purple
      "mountain pine" = "#6C0000" 
    )
  ) +
  scale_alpha_manual(
    values = c(
      "coniferous" = 0.2,
      "broadleaved" = 0.2,
      "bramble" = 0.2,
      "mountain pine" = 1.0
    )
  ) +
  labs(
    x = "Spectral band",
    y = "Reflectance (STMs)",
    title = ""
  ) +
  theme_bw(base_size = 18) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


ggsave("/mnt/eo/EO4Alps/figs/bramble_shrubs.png", width = 9.5, height = 5, dpi = 300)


