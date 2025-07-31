library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)

# -----------------------------------------
# 1. Load one of the raster stacks
# -----------------------------------------
broadleaved <- rast("/mnt/eo/EO4Alps/level4_fcover/final_stacks/mosaic_broadleaved.tif")

# -----------------------------------------
# 2. Create 20 random sample points
# -----------------------------------------
set.seed(as.numeric(Sys.time()))  # for reproducibility
points <- spatSample(broadleaved, size = 10, method = "random", na.rm = TRUE, as.points = TRUE)
points$PointID <- paste0("P", 1:nrow(points))

# -----------------------------------------
# 3. Extract raster values at sample points
# -----------------------------------------
vals <- terra::extract(broadleaved, points)  # returns matrix: ID + bands
vals$PointID <- points$PointID

# -----------------------------------------
# 4. Reshape to long format
# -----------------------------------------
vals_long <- vals %>%
  select(-ID) %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "Year",
    names_prefix = "y",
    values_to = "value"
  ) %>%
  mutate(Year = as.integer(Year))

# -----------------------------------------
# 5. Plot time series per point
# -----------------------------------------
ggplot(vals_long, aes(x = Year, y = value)) +
  geom_line(color = "#1B9E77", size = 1) +
  facet_wrap(~ PointID) +  # separate subplot per point
  labs(
    title = "Fractional cover over time (Broadleaved)",
    x = "Year",
    y = "Fractional cover [%]"
  ) +
  ylim(0,100) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )



# -----------------------------------------
# 1. Load one of the raster stacks
# -----------------------------------------
coniferous <- rast("/mnt/eo/EO4Alps/level4_fcover/final_stacks/mosaic_coniferous.tif")

# -----------------------------------------
# 2. Create 20 random sample points
# -----------------------------------------
set.seed(as.numeric(Sys.time())) 
points <- spatSample(coniferous, size = 10, method = "random", na.rm = TRUE, as.points = TRUE)
points$PointID <- paste0("P", 1:nrow(points))

# -----------------------------------------
# 3. Extract raster values at sample points
# -----------------------------------------
vals <- terra::extract(coniferous, points)  # returns matrix: ID + bands
vals$PointID <- points$PointID

# -----------------------------------------
# 4. Reshape to long format
# -----------------------------------------
vals_long <- vals %>%
  select(-ID) %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "Year",
    names_prefix = "y",
    values_to = "value"
  ) %>%
  mutate(Year = as.integer(Year))

# -----------------------------------------
# 5. Plot time series per point
# -----------------------------------------
ggplot(vals_long, aes(x = Year, y = value)) +
  geom_line(color = "#1B9E77", size = 1) +
  facet_wrap(~ PointID) +  # separate subplot per point
  labs(
    title = "Fractional cover over time (coniferous)",
    x = "Year",
    y = "Fractional cover [%]"
  ) +
  ylim(0,100) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )



# -----------------------------------------
# 1. Load one of the raster stacks
# -----------------------------------------
shrubland <- rast("/mnt/eo/EO4Alps/level4_fcover/final_stacks/mosaic_shrubland.tif")

# -----------------------------------------
# 2. Create 20 random sample points
# -----------------------------------------
set.seed(as.numeric(Sys.time()))  # for reproducibility
points <- spatSample(shrubland, size = 10, method = "random", na.rm = TRUE, as.points = TRUE)
points$PointID <- paste0("P", 1:nrow(points))

# -----------------------------------------
# 3. Extract raster values at sample points
# -----------------------------------------
vals <- terra::extract(shrubland, points)  # returns matrix: ID + bands
vals$PointID <- points$PointID

# -----------------------------------------
# 4. Reshape to long format
# -----------------------------------------
vals_long <- vals %>%
  select(-ID) %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "Year",
    names_prefix = "y",
    values_to = "value"
  ) %>%
  mutate(Year = as.integer(Year))

# -----------------------------------------
# 5. Plot time series per point
# -----------------------------------------
ggplot(vals_long, aes(x = Year, y = value)) +
  geom_line(color = "#1B9E77", size = 1) +
  facet_wrap(~ PointID) +  # separate subplot per point
  labs(
    title = "Fractional cover over time (shrubland)",
    x = "Year",
    y = "Fractional cover [%]"
  ) +
  ylim(0,100) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )



# -----------------------------------------
# 1. Load one of the raster stacks
# -----------------------------------------
grassland <- rast("/mnt/eo/EO4Alps/level4_fcover/final_stacks/mosaic_grassland.tif")

# -----------------------------------------
# 2. Create 20 random sample points
# -----------------------------------------
set.seed(as.numeric(Sys.time()))  # for reproducibility
points <- spatSample(grassland, size = 10, method = "random", na.rm = TRUE, as.points = TRUE)
points$PointID <- paste0("P", 1:nrow(points))

# -----------------------------------------
# 3. Extract raster values at sample points
# -----------------------------------------
vals <- terra::extract(grassland, points)  # returns matrix: ID + bands
vals$PointID <- points$PointID

# -----------------------------------------
# 4. Reshape to long format
# -----------------------------------------
vals_long <- vals %>%
  select(-ID) %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "Year",
    names_prefix = "y",
    values_to = "value"
  ) %>%
  mutate(Year = as.integer(Year))

# -----------------------------------------
# 5. Plot time series per point
# -----------------------------------------
ggplot(vals_long, aes(x = Year, y = value)) +
  geom_line(color = "#1B9E77", size = 1) +
  facet_wrap(~ PointID) +  # separate subplot per point
  labs(
    title = "Fractional cover over time (grassland)",
    x = "Year",
    y = "Fractional cover [%]"
  ) +
  ylim(0,100) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )



# -----------------------------------------
# 1. Load one of the raster stacks
# -----------------------------------------
bare_ground <- rast("/mnt/eo/EO4Alps/level4_fcover/final_stacks/mosaic_bare_ground.tif")

# -----------------------------------------
# 2. Create 20 random sample points
# -----------------------------------------
set.seed(as.numeric(Sys.time()))  # for reproducibility
points <- spatSample(bare_ground, size = 10, method = "random", na.rm = TRUE, as.points = TRUE)
points$PointID <- paste0("P", 1:nrow(points))

# -----------------------------------------
# 3. Extract raster values at sample points
# -----------------------------------------
vals <- terra::extract(bare_ground, points)  # returns matrix: ID + bands
vals$PointID <- points$PointID

# -----------------------------------------
# 4. Reshape to long format
# -----------------------------------------
vals_long <- vals %>%
  select(-ID) %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "Year",
    names_prefix = "y",
    values_to = "value"
  ) %>%
  mutate(Year = as.integer(Year))

# -----------------------------------------
# 5. Plot time series per point
# -----------------------------------------
ggplot(vals_long, aes(x = Year, y = value)) +
  geom_line(color = "#1B9E77", size = 1) +
  facet_wrap(~ PointID) +  # separate subplot per point
  labs(
    title = "Fractional cover over time (bare_ground)",
    x = "Year",
    y = "Fractional cover [%]"
  ) +
  ylim(0,100) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )


#-------------------------------------------------------------------------------




# ----------------------------------------
# 1. Load your raster stack
# ----------------------------------------
broadleaved <- rast("/mnt/eo/EO4Alps/level4_fcover/final_stacks/mosaic_broadleaved.tif")

# ----------------------------------------
# 2. Sample 500 random points (no set.seed -> always new)
# ----------------------------------------
set.seed(as.numeric(Sys.time()))
points <- spatSample(broadleaved, size = 1000, method = "random", na.rm = TRUE, as.points = TRUE)
points$PointID <- paste0("P", 1:nrow(points))

# ----------------------------------------
# 3. Extract values at sample points
# ----------------------------------------
vals <- terra::extract(broadleaved, points)
vals$PointID <- points$PointID

# ----------------------------------------
# 4. Reshape to long format
# ----------------------------------------
vals_long <- vals %>%
  select(-ID) %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "Year",
    names_prefix = "y",
    values_to = "value"
  ) %>%
  mutate(Year = as.integer(Year))

# ----------------------------------------
# 5. Aggregate: mean and 95% confidence interval per year
# ----------------------------------------
summary_stats <- vals_long %>%
  group_by(Year) %>%
  summarise(
    mean_cover = mean(value, na.rm = TRUE),
    sd_cover = sd(value, na.rm = TRUE),
    n = n(),
    se = sd_cover / sqrt(n),
    lower_CI = mean_cover - qt(0.975, df = n - 1) * se,
    upper_CI = mean_cover + qt(0.975, df = n - 1) * se
  )

# ----------------------------------------
# 6. Plot with confidence interval ribbon
# ----------------------------------------
ggplot(summary_stats, aes(x = Year, y = mean_cover)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "#1B9E77", alpha = 0.3) +
  geom_line(color = "#1B9E77", size = 1) +
  labs(
    title = "Mean fractional broadleaved cover over time (n = 1,000 points)",
    x = "Year",
    y = "Mean fractional cover [%]"
  ) +
  theme_minimal(base_size = 14)



# ----------------------------------------
# 1. Load your raster stack
# ----------------------------------------
coniferous <- rast("/mnt/eo/EO4Alps/level4_fcover/final_stacks/mosaic_coniferous.tif")

# ----------------------------------------
# 2. Sample 500 random points (no set.seed -> always new)
# ----------------------------------------
points <- spatSample(coniferous, size = 200, method = "random", na.rm = TRUE, as.points = TRUE)
points$PointID <- paste0("P", 1:nrow(points))

# ----------------------------------------
# 3. Extract values at sample points
# ----------------------------------------
vals <- terra::extract(coniferous, points)
vals$PointID <- points$PointID

# ----------------------------------------
# 4. Reshape to long format
# ----------------------------------------
vals_long <- vals %>%
  select(-ID) %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "Year",
    names_prefix = "y",
    values_to = "value"
  ) %>%
  mutate(Year = as.integer(Year))

# ----------------------------------------
# 5. Aggregate: mean and 95% confidence interval per year
# ----------------------------------------
summary_stats <- vals_long %>%
  group_by(Year) %>%
  summarise(
    mean_cover = mean(value, na.rm = TRUE),
    sd_cover = sd(value, na.rm = TRUE),
    n = n(),
    se = sd_cover / sqrt(n),
    lower_CI = mean_cover - qt(0.975, df = n - 1) * se,
    upper_CI = mean_cover + qt(0.975, df = n - 1) * se
  )

# ----------------------------------------
# 6. Plot with confidence interval ribbon
# ----------------------------------------
ggplot(summary_stats, aes(x = Year, y = mean_cover)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "#1B9E77", alpha = 0.3) +
  geom_line(color = "#1B9E77", size = 1) +
  labs(
    title = "Mean fractional coniferous cover over time (n = 1,000 points)",
    x = "Year",
    y = "Mean fractional cover [%]"
  ) +
  theme_minimal(base_size = 14)



# ----------------------------------------
# 1. Load your raster stack
# ----------------------------------------
shrubland <- rast("/mnt/eo/EO4Alps/level4_fcover/final_stacks/mosaic_shrubland.tif")

# ----------------------------------------
# 2. Sample 500 random points (no set.seed -> always new)
# ----------------------------------------
points <- spatSample(shrubland, size = 200, method = "random", na.rm = TRUE, as.points = TRUE)
points$PointID <- paste0("P", 1:nrow(points))

# ----------------------------------------
# 3. Extract values at sample points
# ----------------------------------------
vals <- terra::extract(shrubland, points)
vals$PointID <- points$PointID

# ----------------------------------------
# 4. Reshape to long format
# ----------------------------------------
vals_long <- vals %>%
  select(-ID) %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "Year",
    names_prefix = "y",
    values_to = "value"
  ) %>%
  mutate(Year = as.integer(Year))

# ----------------------------------------
# 5. Aggregate: mean and 95% confidence interval per year
# ----------------------------------------
summary_stats <- vals_long %>%
  group_by(Year) %>%
  summarise(
    mean_cover = mean(value, na.rm = TRUE),
    sd_cover = sd(value, na.rm = TRUE),
    n = n(),
    se = sd_cover / sqrt(n),
    lower_CI = mean_cover - qt(0.975, df = n - 1) * se,
    upper_CI = mean_cover + qt(0.975, df = n - 1) * se
  )

# ----------------------------------------
# 6. Plot with confidence interval ribbon
# ----------------------------------------
ggplot(summary_stats, aes(x = Year, y = mean_cover)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "#1B9E77", alpha = 0.3) +
  geom_line(color = "#1B9E77", size = 1) +
  labs(
    title = "Mean fractional shrubland cover over time (n = 1,000 points)",
    x = "Year",
    y = "Mean fractional cover [%]"
  ) +
  theme_minimal(base_size = 14)


# ----------------------------------------
# 1. Load your raster stack
# ----------------------------------------
grassland <- rast("/mnt/eo/EO4Alps/level4_fcover/final_stacks/mosaic_grassland.tif")

# ----------------------------------------
# 2. Sample 500 random points (no set.seed -> always new)
# ----------------------------------------
points <- spatSample(grassland, size = 200, method = "random", na.rm = TRUE, as.points = TRUE)
points$PointID <- paste0("P", 1:nrow(points))

# ----------------------------------------
# 3. Extract values at sample points
# ----------------------------------------
vals <- terra::extract(grassland, points)
vals$PointID <- points$PointID

# ----------------------------------------
# 4. Reshape to long format
# ----------------------------------------
vals_long <- vals %>%
  select(-ID) %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "Year",
    names_prefix = "y",
    values_to = "value"
  ) %>%
  mutate(Year = as.integer(Year))

# ----------------------------------------
# 5. Aggregate: mean and 95% confidence interval per year
# ----------------------------------------
summary_stats <- vals_long %>%
  group_by(Year) %>%
  summarise(
    mean_cover = mean(value, na.rm = TRUE),
    sd_cover = sd(value, na.rm = TRUE),
    n = n(),
    se = sd_cover / sqrt(n),
    lower_CI = mean_cover - qt(0.975, df = n - 1) * se,
    upper_CI = mean_cover + qt(0.975, df = n - 1) * se
  )

# ----------------------------------------
# 6. Plot with confidence interval ribbon
# ----------------------------------------
ggplot(summary_stats, aes(x = Year, y = mean_cover)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "#1B9E77", alpha = 0.3) +
  geom_line(color = "#1B9E77", size = 1) +
  labs(
    title = "Mean fractional grassland cover over time (n = 1,000 points)",
    x = "Year",
    y = "Mean fractional cover [%]"
  ) +
  theme_minimal(base_size = 14)


# ----------------------------------------
# 1. Load your raster stack
# ----------------------------------------
bare_gorund <- rast("/mnt/eo/EO4Alps/level4_fcover/final_stacks/mosaic_bare_gorund.tif")

# ----------------------------------------
# 2. Sample 500 random points (no set.seed -> always new)
# ----------------------------------------
points <- spatSample(bare_gorund, size = 200, method = "random", na.rm = TRUE, as.points = TRUE)
points$PointID <- paste0("P", 1:nrow(points))

# ----------------------------------------
# 3. Extract values at sample points
# ----------------------------------------
vals <- terra::extract(bare_gorund, points)
vals$PointID <- points$PointID

# ----------------------------------------
# 4. Reshape to long format
# ----------------------------------------
vals_long <- vals %>%
  select(-ID) %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "Year",
    names_prefix = "y",
    values_to = "value"
  ) %>%
  mutate(Year = as.integer(Year))

# ----------------------------------------
# 5. Aggregate: mean and 95% confidence interval per year
# ----------------------------------------
summary_stats <- vals_long %>%
  group_by(Year) %>%
  summarise(
    mean_cover = mean(value, na.rm = TRUE),
    sd_cover = sd(value, na.rm = TRUE),
    n = n(),
    se = sd_cover / sqrt(n),
    lower_CI = mean_cover - qt(0.975, df = n - 1) * se,
    upper_CI = mean_cover + qt(0.975, df = n - 1) * se
  )

# ----------------------------------------
# 6. Plot with confidence interval ribbon
# ----------------------------------------
ggplot(summary_stats, aes(x = Year, y = mean_cover)) +
  geom_ribbon(aes(ymin = lower_CI, ymax = upper_CI), fill = "#1B9E77", alpha = 0.3) +
  geom_line(color = "#1B9E77", size = 1) +
  labs(
    title = "Mean fractional bare_gorund cover over time (n = 1,000 points)",
    x = "Year",
    y = "Mean fractional cover [%]"
  ) +
  theme_minimal(base_size = 14)


