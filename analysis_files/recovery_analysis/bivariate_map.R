library(biscale)
library(ggplot2)
library(sf)
library(dplyr)
library(viridis)

# Read your shapefile (already in sf format)
hex_data <- st_read("path_to_your_hexagons.shp")

# Check columns
head(hexagons_recov10)

hex_bi <- bi_class(hexagons_recov10, x = mean_temp, y = mean_percent_recovered, style = "quantile", dim = 3)

bi_map <- ggplot() +
  geom_sf(data = hex_bi, aes(fill = bi_class), color = "white", size = 0.1) +
  bi_scale_fill(pal = "GrPink2", dim = 3) +  # You can change palette: "DkBlue", "GrPink", "TealGrn", etc.
  labs(title = "") +
  theme_void() +
  theme(legend.position = "none")

bi_legend <- bi_legend(pal = "GrPink2",
                       dim = 3,
                       xlab = "High VPD anomalies",
                       ylab = "High recovery success",
                       size = 8)

# Combine map and legend using patchwork
library(patchwork)
bi_map + inset_element(bi_legend, left = 0.7, bottom = 0.05, right = 1, top = 0.35)

ggsave("~/eo_nas/EO4Alps/figs/bivariate_plot.png", width = 6, height = 4, units = "in", dpi = 300)



