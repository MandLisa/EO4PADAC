library(raster)
library(dplyr)
library(sf)
library(sp)
library(readr)
library(ggplot2)

recovery_0411 <- read_csv("eo_nas/EO4Alps/00_analysis/_recovery/recovery_0411.csv")

# Specify the path to your .vrt file
vrt_path <- "~/eo_nas/EO4Alps/GEDI/lang/ETH_GlobalCanopyHeight_10m_2020_mosaic_Map.vrt"

# Load the VRT file
chm_lang <- rast(vrt_path)

# Check the loaded raster data
print(chm_lang)
plot(chm_lang)



# Define the Alps extent
alps_extent <- ext(5.5, 15.5, 44.0, 48.5)

# Crop the raster to the Alps extent
alps_canopy <- crop(chm_lang, alps_extent)

# Save the cropped result
writeRaster(alps_canopy, "alps_canopy_height_2020.tif", format="GTiff")




# Set the reference year for GEDI data (e.g., midpoint 2020 for data from 2019-2022)
gedi_reference_year <- 2020


# Set the reference year for height data
height_reference_year <- 2020

# Calculate years since disturbance relative to 2020
recovery_0411 <- recovery_0411 %>%
  mutate(
    ysd_GEDI = height_reference_year - yod
  )

# Define recovery stages based on years since disturbance
recovery_0411 <- recovery_0411 %>%
  mutate(
    recovery_stage = case_when(
      years_since_disturbance <= 5 ~ "early",
      years_since_disturbance <= 10 ~ "intermediate",
      years_since_disturbance <= 20 ~ "late",
      TRUE ~ "very_late"
    )
  )

# Calculate mean canopy cover and height for each recovery stage
recovery_summary <- recovery_0411 %>%
  group_by(recovery_stage) %>%
  summarize(
    avg_canopy_cover = mean(regrown_percent, na.rm = TRUE),
    avg_height_2020 = mean(chm, na.rm = TRUE)
  )

print(recovery_summary)


ggplot(recovery_0411, aes(x = ysd_GEDI, y = chm)) +
  geom_point(alpha = 0.6) +  # Add scatter points with slight transparency
  theme_minimal()






# Calculate average GEDI height for each recovery stage
height_benchmarks <- recovery_0411 %>%
  group_by(recovery_stage) %>%
  summarize(avg_gedi_height = mean(chm, na.rm = TRUE))
print(height_benchmarks)


# Join the height benchmarks with the recovery data
recovery_0411 <- left_join(recovery_0411, height_benchmarks, by = "recovery_stage")

# Calculate a composite score, if desired, to combine spectral and height recovery
recovery_0411 <- recovery_0411 %>%
  mutate(
    recovery_composite_score = relative_regrowth_rate * (gedi_height / avg_gedi_height)
  )



# Boxplot of GEDI height by recovery stage
ggplot(recovery_df, aes(x = recovery_stage, y = gedi_height)) +
  geom_boxplot() +
  labs(title = "GEDI Height by Recovery Stage", x = "Recovery Stage", y = "Canopy Height (m)")

# Scatter plot of relative regrowth rate vs. GEDI height
ggplot(recovery_df, aes(x = relative_regrowth_rate, y = gedi_height, color = recovery_stage)) +
  geom_point() +
  labs(title = "Spectral Recovery vs. Canopy Height", x = "Relative Regrowth Rate", y = "GEDI Canopy Height (m)")




library(ggplot2)

# Line plot for tree cover and height over years since disturbance
ggplot(recovery_df, aes(x = years_since_disturbance)) +
  geom_line(aes(y = tree_cover_recovery, color = "Tree Cover Recovery")) +
  geom_line(aes(y = gedi_height, color = "Canopy Height")) +
  labs(title = "Spectral vs. Structural Recovery Over Time",
       x = "Years Since Disturbance",
       y = "Recovery Metric",
       color = "Recovery Type") +
  scale_color_manual(values = c("Tree Cover Recovery" = "blue", "Canopy Height" = "green"))



# Melt data for plotting if needed
library(reshape2)
recovery_melted <- melt(recovery_df, id.vars = "recovery_stage", measure.vars = c("tree_cover_recovery", "gedi_height"))

# Boxplot of tree cover and height by recovery stage
ggplot(recovery_melted, aes(x = recovery_stage, y = value, fill = variable)) +
  geom_boxplot() +
  labs(title = "Spectral vs. Structural Recovery by Stage",
       x = "Recovery Stage",
       y = "Recovery Value",
       fill = "Recovery Metric") +
  scale_fill_manual(values = c("tree_cover_recovery" = "blue", "gedi_height" = "green"))



# Scatter plot with correlation
ggplot(recovery_df, aes(x = tree_cover_recovery, y = gedi_height)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Benchmarking Spectral Recovery (Tree Cover) vs. Structural Recovery (Height)",
       x = "Tree Cover Recovery",
       y = "Canopy Height") +
  theme_minimal()

# Calculate correlation
correlation <- cor(recovery_df$tree_cover_recovery, recovery_df$gedi_height, use = "complete.obs")
print(paste("Correlation between Tree Cover Recovery and Canopy Height:", round(correlation, 2)))




# Compute ratio of tree cover recovery to canopy height for benchmarking
recovery_df <- recovery_df %>%
  mutate(recovery_ratio = tree_cover_recovery / gedi_height)

# Visualize the recovery ratio over time
ggplot(recovery_df, aes(x = years_since_disturbance, y = recovery_ratio)) +
  geom_line() +
  labs(title = "Spectral to Structural Recovery Ratio Over Time",
       x = "Years Since Disturbance",
       y = "Recovery Ratio (Tree Cover / Height)")















