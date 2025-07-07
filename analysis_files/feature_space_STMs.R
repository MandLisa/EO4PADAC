# Definition der Bänder und Indizes
bands <- c("BLUE", "GREEN", "RED", "NIR", "SWIR1", "SWIR2")
indices <- c("NDVI", "EVI", "NBR", "NDMI", "SAVI", "NDWI", "NDSI")

# Jeweilige Statistiken
band_stats <- c("Q25", "Q50", "Q75")  # 3 pro Band
index_stats <- c("AVG", "STD", "Q25", "Q50", "Q75")  # 5 pro Index

# Spaltennamen erzeugen
band_names <- as.vector(outer(bands, band_stats, paste, sep = "_"))
index_names <- as.vector(outer(indices, index_stats, paste, sep = "_"))
col_names <- c(band_names, index_names)

# Einlesen und Spaltennamen zuweisen
# Einlesen und benennen
features <- read.table("/mnt/eo/EO4Alps/level3_samples/features/features_combined.txt", header = FALSE)
labels <- read.table("/mnt/eo/EO4Alps/level3_samples/response/response_combined.txt", header = FALSE, col.names = "class")

stopifnot(ncol(features) == length(col_names))  # sollte 53 ergeben
colnames(features) <- col_names

stopifnot(ncol(features) == length(col_names)) 
colnames(features) <- col_names

data <- cbind(features, class = labels$class)


library(plotly)

# Beispiel: drei gute STMs auswählen
x_feature <- "SWIR1_Q50"
y_feature <- "NIR_Q50"
z_feature <- "NBR_Q50"

plot_ly(
  data,
  x = ~get(x_feature),
  y = ~get(y_feature),
  z = ~get(z_feature),
  color = ~class,
  colors = "Set1",
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 3, opacity = 0.7)
) %>%
  layout(
    scene = list(
      xaxis = list(title = x_feature),
      yaxis = list(title = y_feature),
      zaxis = list(title = z_feature)
    ),
    title = "3D Feature Space of Selected STMs"
  )



# Compute SWIR ratio
# Step 1: Create SWIR ratio
data$SWIR_ratio <- data$SWIR2_Q50 / data$SWIR1_Q50

# Step 2: Subset only classes 2, 4, 5
subset_data <- subset(data, class %in% c(2, 4, 5))

# Step 3: Plot NDVI vs. SWIR ratio
library(ggplot2)

ggplot(subset_data, aes(x = NDVI_Q50, y = SWIR_ratio, color = factor(class))) +
  geom_point(alpha = 0.6, size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    x = "NDVI (Q50)",
    y = "SWIR Ratio (SWIR2 / SWIR1)",
    title = "NDVI vs. SWIR Ratio Feature Space",
    color = "Class"
  )


ggplot(data, aes(x = NDVI_Q50, y = SWIR_ratio, color = factor(class), fill = factor(class))) +
  stat_density_2d(geom = "polygon", alpha = 0.3, contour_var = "density") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(
    title = "2D Density Contours in Feature Space",
    x = "NDVI (Q50)",
    y = "SWIR Ratio (SWIR2 / SWIR1)",
    fill = "Class", color = "Class"
  )



ggplot(subset_data, aes(x = NDVI_Q50, y = SWIR_ratio)) +
  stat_bin_2d(aes(fill = ..count..), bins = 50) +
  scale_fill_viridis_c() +
  theme_minimal() +
  facet_wrap(~ class) +
  labs(x = "NDVI (Q50)", y = "SWIR2 / SWIR1", title = "Density of STM Points per Class")


