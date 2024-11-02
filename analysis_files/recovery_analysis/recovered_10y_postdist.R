# Load necessary libraries
library(dplyr)
library(sp)
library(sf)
library(lme4)
library(ggplot2)
library(raster)


tiles_shapefile <- st_read("~/eo_nas/EO4Alps/level2/shp/grid.shp")
hexagons <- st_read("~/eo_nas/EO4Alps/level2/hexagons/hex.shp")

# Convert your dataframe to an sf object if needed
recovery_2608_sf <- st_as_sf(recovery_2608, coords = c("x", "y"), crs = st_crs(tiles_shapefile))

# Check and ensure CRS match
if (st_crs(recovery_2608_filt1_sf) != st_crs(tiles_shapefile)) {
  recovery_2608_filt1_sf <- st_transform(recovery_2608_filt1_sf, st_crs(tiles_shapefile))
}

# Perform the spatial join
recovery_with_tile_id <- st_join(recovery_2608_sf, tiles_shapefile["Tile_ID"])

# Perform the spatial join
recovery_with_hex_id <- st_join(recovery_2608_sf, hexagons["GRID_ID"])



# Step 4: Extract the x and y coordinates from the geometry column
recovery_with_hex_id <- recovery_with_hex_id %>%
  mutate(
    x = st_coordinates(.)[,1],  # Extract X coordinates
    y = st_coordinates(.)[,2]   # Extract Y coordinates
  )


# Step 5: Convert back to a regular dataframe without the geometry column
recovery_2608 <- recovery_with_hex_id %>%
  st_set_geometry(NULL) 

### write
write.csv(recovery_2608, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_2608_hex_ID.csv", row.names=FALSE)

# Use distinct to keep only one row per ID
recovery_2608_unique <- recovery_2608 %>%
  distinct(ID, .keep_all = TRUE)



# Assuming your dataframe is named 'df'
recovery_2608 <- recovery_2608 %>%
  # Create the recovery_status column based on recovery_rate
  mutate(recovery_status = ifelse(recovery_rate < 100, "recovered", "not recovered"))


recovery_summary_tile <- recovery_2608 %>%
  # Filter for disturbances that occurred before 2013
  filter(yod < 2013) %>%
  # Create a new column 'recov_10y' to indicate recovery within 10 years
  group_by(Tile_ID, ID) %>% # Group by pixel to check recovery status per pixel over time
  mutate(recov_10y = ifelse(any(recovery_status == "recovered" & year <= yod + 10), 1, 0)) %>%
  ungroup() %>%
  # Compute the proportion of pixels that recovered within 10 years per GRID_ID
  group_by(Tile_ID) %>%
  summarise(share_recovered_10yr = mean(recov_10y, na.rm = TRUE) * 100) %>%
  ungroup()

recovery_summary_hex <- recovery_2608 %>%
  # Filter for disturbances that occurred before 2013
  filter(yod < 2013) %>%
  # Create a new column 'recov_10y' to indicate recovery within 10 years
  group_by(GRID_ID, ID) %>% # Group by pixel to check recovery status per pixel over time
  mutate(recov_10y = ifelse(any(recovery_status == "recovered" & year <= yod + 10), 1, 0)) %>%
  ungroup() %>%
  # Compute the proportion of pixels that recovered within 10 years per GRID_ID
  group_by(GRID_ID) %>%
  summarise(share_recovered_10yr = mean(recov_10y, na.rm = TRUE) * 100) %>%
  ungroup()


# Join the summary data with the shapefile based on GRID_ID
hex_recov <- hexagons %>%
  left_join(recovery_summary_hex, by = "GRID_ID")

# Write the joined data to a new shapefile
st_write(hex_recov, "~/eo_nas/EO4Alps/level2/hexagons/hex_recov_10y.shp", delete_layer = TRUE)

# for tiles
tile_recov <- tiles_shapefile %>%
  left_join(recovery_summary_tile, by = "Tile_ID")

# Write the joined data to a new shapefile
st_write(tile_recov, "~/eo_nas/EO4Alps/level2/shp/tile_recov_10y.shp", delete_layer = TRUE)


# Assuming 'recovery_summary' is the dataframe with share of recovered pixels per tile_ID
ggplot(recovery_summary, aes(x = Tile_ID, y = share_recovered_10yr)) +
  geom_bar(stat = "identity") +
  labs(title = "Share of Recovered Observations per Tile",
       x = "Tile ID",
       y = "Share of Recovered Pixels (10 years post-disturbance)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### extract GEDI CHM values
CHM <- raster("~/eo_nas/EO4Alps/GEDI/output_vh.tif")
plot(CHM)

# Convert recovery_2608 to sf and reproject if needed
recovery_sf <- st_as_sf(recovery_2608, coords = c("x", "y"), crs = st_crs(CHM))
if (!st_crs(recovery_sf) == st_crs(CHM)) {
  recovery_sf <- st_transform(recovery_sf, st_crs(CHM))
}


CHM_values <- extract(CHM, recovery_sf)
CHM_values <- unlist(CHM_values)
recovery_sf$CHM <- CHM_values

coords <- st_coordinates(recovery_sf)

recovery_3110 <- st_drop_geometry(recovery_sf)  # Drop geometry to convert to regular dataframe
recovery_3110$x <- coords[, 1]  # Add x coordinates
recovery_3110$y <- coords[, 2]  # Add y coordinates but the CHM


# annualized recovery rate

# Calculate the minimum tree cover within +/- 3 years around yod for each ID
recovery_3110 <- recovery_3110 %>%
  group_by(ID) %>%
  mutate(
    min_tree_share = ifelse(
      between(year, yod - 3, yod + 3), 
      min(tree_share_GAM[between(year, yod - 3, yod + 3)], na.rm = TRUE), 
      NA
    )
  ) %>%
  ungroup() %>%
  # Fill in the min_tree_share column for all rows within the same ID
  group_by(ID) %>%
  mutate(min_tree_share = max(min_tree_share, na.rm = TRUE)) %>%
  ungroup()



# Calculate the regrown percent per ID
recovery_3110 <- recovery_3110 %>%
  group_by(ID) %>%
  mutate(
    regrown_percent = ifelse(
      year > yod,
      ((tree_share_GAM - min_tree_share) / (avg_tree_share_before - min_tree_share)) * 100,
      NA  # Only calculate for post-disturbance years
    )
  ) %>%
  ungroup()



# Calculate the recovery percentage and annualized recovery rate
recovery_3110 <- recovery_3110 %>%
  mutate(
    recovery_percent = (tree_share_GAM / avg_tree_share_before) * 100,
    annualized_recovery_rate = (recovery_percent - 100) / ysd  # rate adjusted to time since disturbance
  )

# Step 2: Set annualized_recovery_rate to NA for rows where year <= yod
recovery_3110 <- recovery_3110 %>%
  mutate(
    annualized_recovery_rate = ifelse(year <= yod, NA, annualized_recovery_rate)
  )

                                                                                                        
# Classify ysd into recovery stages
recovery_3110 <- recovery_3110 %>%
  mutate(
    recovery_stage = case_when(
      ysd >= 0 & ysd <= 5  ~ "Early",
      ysd >= 6 & ysd <= 10 ~ "Intermediate",
      ysd >= 11 & ysd <= 20 ~ "Late",
      TRUE ~ NA_character_  # for cases outside these ranges
    )
  )



# Ensure only post-disturbance years are included in the calculation
recovery_3110_summary <- recovery_3110 %>%
  filter(year > yod) %>%  # Filter to keep only post-disturbance years
  group_by(ID, recovery_stage) %>%
  summarize(
    mean_VPD_anomaly = mean(VPD_anomaly, na.rm = TRUE),
    cumulative_VPD_anomaly = sum(VPD_anomaly, na.rm = TRUE)
  ) %>%
  ungroup()


# Install and load nlme
install.packages("nlme")
library(nlme)

# Compute the mean and cumulative VPD metrics by recovery stage
recovery_3110_summary <- recovery_3110 %>%
  # Filter for early stage to calculate absolute VPD values for early years
  mutate(
    recovery_stage = case_when(
      ysd >= 0 & ysd <= 5  ~ "Early",
      ysd >= 6 & ysd <= 10 ~ "Intermediate",
      ysd >= 11 & ysd <= 20 ~ "Late",
      TRUE ~ NA_character_  # for cases outside these ranges
    )
  ) %>%
  group_by(ID) %>%
  summarize(
    # Mean absolute VPD for the early stage (0-5 years post-disturbance)
    mean_VPD_absolute_early = mean(VPD_absolute[ysd >= 0 & ysd <= 5], na.rm = TRUE),
    
    # Cumulative VPD anomalies for each stage
    cumulative_VPD_anomaly_early = sum(VPD_summer[ysd >= 0 & ysd <= 5], na.rm = TRUE),
    cumulative_VPD_anomaly_intermediate = sum(VPD_summer[ysd >= 6 & ysd <= 10], na.rm = TRUE),
    cumulative_VPD_anomaly_late = sum(VPD_summer[ysd >= 11 & ysd <= 20], na.rm = TRUE)
  ) %>%
  ungroup()



set.seed(123)  # for reproducibility
sample_data <- recovery_3110 %>%
  sample_frac(0.1)  # Take a 10% sample of the data


recovery_3110_stage_summary <- recovery_3110 %>%
  group_by(ID, recovery_stage) %>%
  summarize(
    mean_regrown_percent = mean(regrown_percent, na.rm = TRUE),
    mean_VPD_anomaly = mean(VPD_summer, na.rm = TRUE)
  ) %>%
  ungroup()


install.packages("mgcv")
library(mgcv)

# Remove rows with NA values in the model variables
recovery_3110_clean <- recovery_3110 %>%
  filter(!is.na(regrown_percent) & !is.na(ysd) & !is.na(VPD_summer) & !is.na(ID))


# Replace -Inf values in regrown_percent with NA
#recovery_3110$regrown_percent[is.infinite(recovery_3110$regrown_percent)] <- NA


# Fit the model with the cleaned data
model <- bam(
  regrown_percent ~ s(ysd) + s(VPD_summer) + s(ID, bs = "re"),
  data = recovery_3110_clean,
  method = "fREML"
)

# Plot smooth terms in the model
plot(model, pages = 1, shade = TRUE)



# Define new_data based on the cleaned dataset
new_data <- data.frame(
  ysd = seq(min(recovery_3110_clean$ysd, na.rm = TRUE), max(recovery_3110_clean$ysd, na.rm = TRUE), length.out = 100),
  VPD_summer = mean(recovery_3110_clean$VPD_summer, na.rm = TRUE),  # Use mean VPD_summer from cleaned data
  ID = unique(recovery_3110_clean$ID)[1]  # Placeholder ID from cleaned data
)

# Get predictions with confidence intervals
preds <- predict(model, newdata = new_data, se.fit = TRUE)

# Add predictions and confidence intervals to new_data
new_data$fit <- preds$fit
new_data$se.fit <- preds$se.fit

# Plot the smooth term for ysd
ggplot(new_data, aes(x = ysd, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), alpha = 0.3) +
  labs(
    title = "Effect of years since disturbance on recovery",
    x = "YSD",
    y = "Predicted regrown canopy cover [%]"
  )



# Define a data frame for predictions with varying VPD_summer and a fixed ysd
new_data_vpd <- data.frame(
  ysd = mean(recovery_3110_clean$ysd, na.rm = TRUE),  # Use an average value for ysd or a fixed stage (e.g., 5 years)
  VPD_summer = seq(min(recovery_3110_clean$VPD_summer, na.rm = TRUE), max(recovery_3110_clean$VPD_summer, na.rm = TRUE), length.out = 100),
  ID = unique(recovery_3110_clean$ID)[1]  # Placeholder ID
)

# Get predictions with confidence intervals, focusing on VPD_summer
preds_vpd <- predict(model, newdata = new_data_vpd, se.fit = TRUE, type = "response")

# Add predictions and confidence intervals to new_data_vpd
new_data_vpd$fit <- preds_vpd$fit
new_data_vpd$se.fit <- preds_vpd$se.fit


# Plot the effect of VPD_summer on regrown_percent
p <- ggplot(new_data_vpd, aes(x = VPD_summer, y = fit)) +
  geom_line(color = "green") +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), alpha = 0.3) +
  labs(
    title = "Effect of VPD anomalies on forest recovery",
    x = "VPD anomalies",
    y = "Predicted regrown canopy cover [%]"
  )


# Save the plot as a PNG file
ggsave("~/eo_nas/EO4Alps/figs/effect_VPD_recovery.png", plot = p, width = 10, height = 8, dpi = 300)



#----------------------------------------------

# Create a data frame for each stage with a range of VPD_summer values
new_data_stages <- expand.grid(
  VPD_summer = seq(min(recovery_3110_clean$VPD_summer, na.rm = TRUE), max(recovery_3110_clean$VPD_summer, na.rm = TRUE), length.out = 100),
  ysd = c(3, 8, 15),  # Representative ysd values for early, intermediate, and late stages
  ID = unique(recovery_3110_clean$ID)[1]  # Placeholder ID
)

# Get predictions for each stage with confidence intervals
preds_stages <- predict(model, newdata = new_data_stages, se.fit = TRUE, type = "response")

# Add predictions and confidence intervals to new_data_stages
new_data_stages$fit <- preds_stages$fit
new_data_stages$se.fit <- preds_stages$se.fit

# Label the stages
new_data_stages$stage <- factor(
  new_data_stages$ysd,
  levels = c(3, 8, 15),
  labels = c("Early (0-5 years)", "Intermediate (6-10 years)", "Late (11-20 years)")
)

# Plot the effect of VPD_summer on regrown_percent by stage
ggplot(new_data_stages, aes(x = VPD_summer, y = fit, color = stage, fill = stage)) +
  geom_line(linewidth = 1) +  # Updated from `size` to `linewidth`
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), alpha = 0.2) +
  labs(
    title = "Effect of Climate (VPD_summer) on Forest Regrowth by Recovery Stage",
    x = "VPD_summer",
    y = "Predicted regrown_percent",
    color = "Recovery Stage",
    fill = "Recovery Stage"
  ) +
  theme_minimal()


# Plot the effect of VPD_summer on regrown_percent with separate facets for each stage
p1 <- ggplot(new_data_stages, aes(x = VPD_summer, y = fit, fill = stage)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), alpha = 0.2) +
  labs(
    title = "Effect of CVPD anomalies on forest recovery by recovery stage",
    x = "VPD anomalies",
    y = "Predicted regrown canopy cover [%]"
  ) +
  facet_wrap(~stage) +  # Separate plot for each stage
  theme_minimal()

# Save the plot as a PNG file
ggsave("~/eo_nas/EO4Alps/figs/effect_VPD_recovery_stages.png", plot = p1, width = 10, height = 8, dpi = 300)


#-------------------------------------------------------------------------------
# include geolocation in GAM
# Example model with geoloc as a random effect

# Convert geoloc to a factor if it isn’t already
recovery_3110_clean_geoloc$geoloc <- as.factor(recovery_3110_clean_geoloc$geoloc)

# Remove rows with missing values in relevant columns
recovery_3110_clean_geoloc <- recovery_3110_clean_geoloc %>%
  filter(!is.na(geoloc) & !is.na(VPD_summer) & !is.na(ysd) & !is.na(regrown_percent))


# Refit the model with geoloc as a random effect
model_geoloc <- bam(
  regrown_percent ~ s(ysd) + s(VPD_summer) + s(geoloc, bs = "re"),  # random effect for geoloc
  data = recovery_3110_clean_geoloc,
  method = "fREML"
)


# Example model with an interaction term
model <- bam(
  regrown_percent ~ s(ysd) + s(VPD_summer, by = geoloc) + s(geoloc, bs = "re"),
  data = recovery_3110_clean,
  method = "fREML"
)

# Define the data frame for predictions with varying VPD_summer and fixed ysd
new_data_geoloc <- expand.grid(
  ysd = mean(recovery_3110_clean_geoloc$ysd, na.rm = TRUE),  # Use a fixed ysd, e.g., mean or specific stage
  VPD_summer = seq(min(recovery_3110_clean_geoloc$VPD_summer, na.rm = TRUE), 
                   max(recovery_3110_clean_geoloc$VPD_summer, na.rm = TRUE), length.out = 100),
  geoloc = unique(recovery_3110_clean_geoloc$geoloc)  # Include all unique geoloc values
)

# Add a placeholder ID if required by the model (e.g., any valid ID from your dataset)
new_data_geoloc$ID <- unique(recovery_3110_clean_geoloc$ID)[1]

# Generate predictions for each geoloc and VPD_summer combination
preds_geoloc <- predict(model_geoloc, newdata = new_data_geoloc, se.fit = TRUE, type = "response")

# Add predictions and confidence intervals to new_data_geoloc
new_data_geoloc$fit <- preds_geoloc$fit
new_data_geoloc$se.fit <- preds_geoloc$se.fit


# Plot the effect of VPD_summer on regrown_percent, with separate facets for each geolocc
p2 <- ggplot(new_data_geoloc, aes(x = VPD_summer, y = fit, color = geoloc)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit, fill = geoloc), alpha = 0.2) +
  labs(
    title = "Effect of VPD anomalies on forest recovery by Geolocation",
    x = "VPD anomalies",
    y = "Predicted regrown canopy cover [%]",
    color = "Geolocation",
    fill = "Geolocation"
  ) +
  facet_wrap(~ geoloc) + 
  theme_minimal()

# Save the plot as a PNG file
ggsave("~/eo_nas/EO4Alps/figs/effect_VPD_recovery.png", plot = p2, width = 10, height = 8, dpi = 300)



### this approach explicitly models the effect of VPD_summer as varying by geoloc.
#s(VPD_summer, by = geoloc) tells the model to fit a separate smooth for VPD_summer 
#for each geoloc, allowing different slopes and curve shapes based on geolo

model_geoloc <- bam(
  regrown_percent ~ s(ysd) + s(VPD_summer, by = geoloc) + s(geoloc, bs = "re"),
  data = recovery_3110_clean_geoloc,
  method = "fREML"
)

### Add a Random Slope for VPD_summer by geoloc
#model_geoloc <- bam(
  #regrown_percent ~ s(ysd) + s(VPD_summer) + s(geoloc, bs = "re") + s(VPD_summer, geoloc, bs = "re"),
  #data = recovery_3110_clean_geoloc,
  #method = "fREML"
#)


# Define a data frame with a range of VPD_summer values for each geoloc and a fixed ysd
new_data_geoloc <- expand.grid(
  ysd = mean(recovery_3110_clean_geoloc$ysd, na.rm = TRUE),  # Fixed ysd value, e.g., mean or a specific stage
  VPD_summer = seq(min(recovery_3110_clean_geoloc$VPD_summer, na.rm = TRUE), max(recovery_3110_clean_geoloc$VPD_summer, na.rm = TRUE), length.out = 100),
  geoloc = unique(recovery_3110_clean_geoloc$geoloc)  # Include all unique geoloc values
)

# Add a placeholder ID if required by the model (e.g., any valid ID from your dataset)
new_data_geoloc$ID <- unique(recovery_3110_clean_geoloc$ID)[1]

# Generate predictions for each geoloc and VPD_summer combination
preds_geoloc <- predict(model_geoloc, newdata = new_data_geoloc, se.fit = TRUE, type = "response")

# Add predictions and confidence intervals to new_data_geoloc
new_data_geoloc$fit <- preds_geoloc$fit
new_data_geoloc$se.fit <- preds_geoloc$se.fit

# Plot the effect of VPD_summer on regrown_percent, with different colors and facets for each geoloc
p2 <- ggplot(new_data_geoloc, aes(x = VPD_summer, y = fit, color = geoloc)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit, fill = geoloc), alpha = 0.2) +
  labs(
    title = "",
    x = "VPD anomalies",
    y = "Predicted regrown canopy cover [%]"
  ) +
  facet_wrap(~ geoloc) + 
  theme_minimal() +  # Set theme_minimal first
  theme(
    text = element_text(size = 20),           # Increase all text size
    plot.title = element_text(size = 22),     # Increase title size specifically
    axis.title = element_text(size = 18),     # Increase axis title size
    axis.text = element_text(size = 14),      # Increase axis text size
    legend.position = "none"                  # Remove legend
  )



# Save the plot as a PNG file
ggsave("~/eo_nas/EO4Alps/figs/effect_VPD_recovery_non_topo.png", plot = p2, width = 10, height = 8, dpi = 300)


pp <- ggplot(new_data_geoloc, aes(x = VPD_summer, y = fit)) +
  geom_line(color = "black", linewidth = 1) +  # Set line color to black
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), fill = "#8DBBBE", alpha = 0.4) +  # Set ribbon fill to grey
  labs(
    title = "",
    x = "VPD anomalies",
    y = "Predicted regrown canopy cover [%]"
  ) +
  facet_wrap(~ geoloc) + 
  theme_bw() +  # Set theme_minimal first
  theme(
    text = element_text(size = 28),           # Increase all text size
    plot.title = element_text(size = 30),     # Increase title size specifically
    axis.title = element_text(size = 26),     # Increase axis title size
    axis.text = element_text(size = 22),      # Increase axis text size
    legend.position = "none"                  # Remove legend
  )

# Save the plot as a PNG file
ggsave("~/eo_nas/EO4Alps/figs/effect_VPD_recovery_non_topo_grey.png", plot = pp, width = 11, height = 8, dpi = 300)



#### control for topographic factors
# Rename the column in the data frame
recovery_3110_clean_geoloc <- recovery_3110_clean_geoloc %>%
  rename(bare_land_share_yod3 = `bare_land_share_yod+3`)

# Then fit the model as above
model_geoloc_topo <- bam(
  regrown_percent ~ s(ysd) + s(VPD_summer, by = geoloc) + s(geoloc, bs = "re") +
    s(height) + s(slope) + s(aspect, bs = "cc") + s(bare_land_share_yod3) +
    ti(VPD_summer, height) + ti(VPD_summer, aspect) +
    ti(VPD_summer, bare_land_share_yod3),
  data = recovery_3110_clean_geoloc,
  method = "fREML"
)



## Define a data frame with a range of VPD_summer values for each geoloc and fixed values for other predictors
new_data_geoloc_topo <- expand.grid(
  ysd = mean(recovery_3110_clean_geoloc$ysd, na.rm = TRUE),  # Fixed ysd
  VPD_summer = seq(min(recovery_3110_clean_geoloc$VPD_summer, na.rm = TRUE), max(recovery_3110_clean_geoloc$VPD_summer, na.rm = TRUE), length.out = 100),
  geoloc = unique(recovery_3110_clean_geoloc$geoloc),  # All unique geolocations
  height = mean(recovery_3110_clean_geoloc$height, na.rm = TRUE),  # Fixed sea level
  slope = mean(recovery_3110_clean_geoloc$slope, na.rm = TRUE),  # Fixed slope
  aspect = mean(recovery_3110_clean_geoloc$aspect, na.rm = TRUE),  # Fixed aspect
  bare_land_share_yod3 = mean(recovery_3110_clean_geoloc$bare_land_share_yod3, na.rm = TRUE)  # Fixed bare ground share
)

# Add a placeholder ID if required by the model
new_data_geoloc_topo$ID <- unique(recovery_3110_clean_geoloc$ID)[1]

# Generate predictions for each geoloc and VPD_summer combination
preds_geoloc_topo <- predict(model_geoloc_topo, newdata = new_data_geoloc_topo, se.fit = TRUE, type = "response")

# Add predictions and confidence intervals to new_data_geoloc
new_data_geoloc_topo$fit <- preds_geoloc_topo$fit
new_data_geoloc_topo$se.fit <- preds_geoloc_topo$se.fit


new_data_geoloc_topo$fit <- pmin(new_data_geoloc_topo$fit, 100)

# Plot the effect of VPD_summer on regrown_percent, with different colors and facets for each geoloc
p3 <- ggplot(new_data_geoloc_topo, aes(x = VPD_summer, y = fit, color = geoloc)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit, fill = geoloc), alpha = 0.2) +
  labs(
    title = "Effect of VPD anomalies on forest recovery by Geolocation (controlling for topographic factors)",
    x = "VPD anomalies",
    y = "Predicted regrown canopy cover [%]",
    color = "Geolocation",
    fill = "Geolocation"
  ) +
  facet_wrap(~ geoloc) + 
  theme_minimal() +  # Set theme_minimal first
  theme(
    text = element_text(size = 20),           # Increase all text size
    plot.title = element_text(size = 22),     # Increase title size specifically
    axis.title = element_text(size = 18),     # Increase axis title size
    axis.text = element_text(size = 14),      # Increase axis text size
    legend.position = "none"                  # Remove legend
  )

# Save the plot as a PNG file
ggsave("~/eo_nas/EO4Alps/figs/effect_VPD_recovery_with_topo.png", plot = p3, width = 10, height = 8, dpi = 300)


p4 <- ggplot(new_data_geoloc_topo, aes(x = VPD_summer, y = fit)) +
  geom_line(color = "black", linewidth = 1) +  # Set line color to black
  geom_ribbon(aes(ymin = fit - 1.96 * se.fit, ymax = fit + 1.96 * se.fit), fill = "#8DBBBE", alpha = 0.4) +  # Set ribbon fill to grey
  labs(
    title = "Effect of VPD anomalies on forest recovery by Geolocation (controlling for topographic factors)",
    x = "VPD anomalies",
    y = "Predicted regrown canopy cover [%]"
  ) +
  facet_wrap(~ geoloc) + 
  theme_bw() +  # Set theme_minimal first
  theme(
    text = element_text(size = 20),           # Increase all text size
    plot.title = element_text(size = 22),     # Increase title size specifically
    axis.title = element_text(size = 18),     # Increase axis title size
    axis.text = element_text(size = 14),      # Increase axis text size
    legend.position = "none"                  # Remove legend
  )


# Save the plot as a PNG file
ggsave("~/eo_nas/EO4Alps/figs/effect_VPD_recovery_with_topo_grey.png", plot = p4, width = 11, height = 8, dpi = 300)


