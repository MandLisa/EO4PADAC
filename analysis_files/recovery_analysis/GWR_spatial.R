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
library(DataEditR)
library(readr)
library(spgwr)
library(kableExtra)
library(dplyr)

recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_GWR.csv")

# Filter the dataset and compute the new column
recovery_filt <- recovery %>%
  group_by(ID) %>%
  filter(yod < 2013) %>%
  mutate(recov_10 = ifelse(recovery_rate <= 10, 1, 0)) %>%
  ungroup()


# compute pre-disturbance mean broadleaved and mean coniferous shares and bare 
# ground shares

# Create new columns for pre-disturbance means
recovery_filt <- recovery_filt %>%
  group_by(ID) %>%  # Group by ID to calculate means within each ID
  mutate(
    pre_dist_coni = ifelse(year < yod, mean(coniferous[year < yod], na.rm = TRUE), NA),
    pre_dist_broadl = ifelse(year < yod, mean(broadleaved[year < yod], na.rm = TRUE), NA),
    post_dist_bare = ifelse(year < yod, mean(bare_ground[year > yod], na.rm = TRUE), NA)
  ) %>%
  ungroup()


# one observation per ID, enough for the spatial model
recovery_unique <- recovery_filt %>%
  distinct(ID, .keep_all = TRUE)


# convert recovery df to sf object
recovery_sf <- st_as_sf(recovery_filt, coords = c("x", "y"), crs = 3035)

# same for one ID per observation
recovery_unique_sf <- st_as_sf(recovery_unique, coords = c("x", "y"), crs = 3035)

# load hexagons and recovery df
hexagons <- st_read("~/eo_nas/EO4Alps/gis/hexagons/hex_500.shp")


# just use GRID_ID for subsequent joins
hexagons_selected <- hexagons %>%
  select(GRID_ID)

recovery_sf <- st_join(recovery_sf, hexagons_selected, join = st_intersects)
recovery_unique_sf <- st_join(recovery_unique_sf, hexagons_selected, join = st_intersects)


### spatial model
# Calculate percentage of recovered disturbances per GRID_ID
recovery_unique_sf_recov10 <- recovery_unique_sf %>%
  group_by(GRID_ID) %>%
  mutate(
    total_observations = n(),  # Total number of observations per GRID_ID
    total_recovered = sum(recov_10, na.rm = TRUE),  # Total recovered (recovery_10yn == 1)
    percent_recovered = (total_recovered / total_observations) * 100  # Percentage recovered
  ) %>%
  ungroup()


hexagon_predictors <- recovery_unique_sf_recov10 %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_elevation = mean(height, na.rm = TRUE),
    mean_severity = mean(severity_relative, na.rm = TRUE),
    mean_VPD = mean(mean_VPD10, na.rm = TRUE),
    mean_VPD_ano = mean(mean_VPD_ano10, na.rm = TRUE),
    mean_VPD_yod1 = mean(VPD_yod1, na.rm = TRUE),
    mean_prec = mean(mean_prec10, na.rm = TRUE),
    mean_temp = mean(mean_temp10, na.rm = TRUE),
    mean_prec_total = mean(mean_prec_total, na.rm = TRUE),
    mean_temp_total = mean(mean_temp_total, na.rm = TRUE),
    mean_recovery_rate = mean(recovery_rate, na.rm = TRUE),
    mean_percent_recovered = mean(percent_recovered, na.rm = TRUE),
    mean_broadleaved = mean(pre_dist_broadl, na.rm = TRUE),
    mean_coniferous = mean(pre_dist_coni, na.rm = TRUE),
    mean_bare = mean(post_dist_bare, na.rm = TRUE),
    dominant_forest_type = names(sort(table(forest_type), decreasing = TRUE))[1],
    .groups = "drop"
  )


hexagon_predictors <- recovery_unique_sf_recov10 %>%
  group_by(GRID_ID) %>%
  summarise(
    mean_elevation = mean(height, na.rm = TRUE),
    mean_severity = mean(severity_relative, na.rm = TRUE),
    mean_VPD = mean(mean_VPD10, na.rm = TRUE),
    mean_VPD_ano = mean(mean_VPD_ano10, na.rm = TRUE),
    mean_VPD_yod1 = mean(VPD_yod1, na.rm = TRUE),
    mean_prec = mean(mean_prec10, na.rm = TRUE),
    mean_temp = mean(mean_temp10, na.rm = TRUE),
    mean_prec_total = mean(mean_prec_total, na.rm = TRUE),
    mean_temp_total = mean(mean_temp_total, na.rm = TRUE),
    mean_recovery_rate = mean(recovery_rate, na.rm = TRUE),
    mean_percent_recovered = mean(percent_recovered, na.rm = TRUE),
    mean_broadleaved = mean(pre_dist_broadl, na.rm = TRUE),
    mean_coniferous = mean(pre_dist_coni, na.rm = TRUE),
    mean_bare = mean(post_dist_bare, na.rm = TRUE),
    dominant_forest_type = names(sort(table(forest_type), decreasing = TRUE))[1],  # Mode of forest type
    geolocation = names(sort(table(geoloc), decreasing = TRUE))[1],  # Mode of geolocation
    .groups = "drop"
  )



# Perform spatial join
hexagons_recov10 <- st_join(hexagons_selected, hexagon_predictors, join = st_intersects)


# Check for NAs in model variables
hexagons_recov10 %>%
  summarise(
    na_mean_recovery_rate = sum(is.na(mean_recovery_rate)),
    na_mean_percent_recovered = sum(is.na(mean_percent_recovered)),
    na_mean_elevation = sum(is.na(mean_elevation)),
    na_mean_severity = sum(is.na(mean_severity)),
    na_mean_VPD = sum(is.na(mean_VPD)),
    na_mean_VPD_ano = sum(is.na(mean_VPD_ano)),
    na_mean_VPD_yod1 = sum(is.na(mean_VPD_yod1)),
    na_mean_prec = sum(is.na(mean_prec)),
    na_mean_temp = sum(is.na(mean_temp)),
    na_mean_prec_total = sum(is.na(mean_prec_total)),
    na_mean_temp_total = sum(is.na(mean_temp_total)),
    na_mean_coniferouss = sum(is.na(mean_coniferous)),
    na_mean_broadleaved = sum(is.na(mean_broadleaved)),
    na_mean_bare = sum(is.na(mean_bare)),
    na_dominant_forest_type = sum(is.na(dominant_forest_type))
  )


hexagons_recov10 <- hexagons_recov10 %>%
  filter(
    !is.na(mean_recovery_rate) & 
      !is.na(mean_percent_recovered) &
      !is.na(mean_elevation) & 
      !is.na(mean_severity) & 
      !is.na(mean_VPD) & 
      !is.na(mean_VPD_ano) & 
      !is.na(mean_VPD_yod1) &
      !is.na(mean_prec) & 
      !is.na(mean_temp) & 
      !is.na(mean_prec_total) & 
      !is.na(mean_temp_total) &
      !is.na(dominant_forest_type) &
      !is.na(mean_coniferous) &
      !is.na(mean_broadleaved) &
      !is.na(mean_bare)
  )

hexagons_recov10 <- hexagons_recov10 %>%
  mutate(mean_pre_dist_tree_cover = mean_broadleaved + mean_coniferous) %>%
  dplyr::select(-mean_broadleaved, -mean_coniferous)  


#-------------------------------------------------------------------------------
### GWR modelling (spatial model)
#-------------------------------------------------------------------------------
### with GWmodel package

# Extract centroid coordinates
coords <- st_coordinates(st_centroid(hexagons_recov10))

# Convert data to Spatial format if required by GWR
hexagons_sp <- as(hexagons_recov10, "Spatial")

# determine bandwidth
bw_GW_spatial <- bw.gwr(
  mean_percent_recovered ~ mean_elevation + 
    mean_severity + 
    mean_VPD_yod1 + 
    mean_prec_total +
    mean_temp_total +
    mean_pre_dist_tree_cover + 
    mean_bare,
  data = hexagons_sp,
  kernel = "gaussian"
)

# fit GWR model
gwr_GW_spatial <- gwr.basic(
  mean_percent_recovered ~ mean_elevation + 
    mean_severity + 
    mean_VPD_yod1 + 
    mean_prec_total +
    mean_temp_total +
    mean_pre_dist_tree_cover + 
    mean_bare,
  data = hexagons_sp,
  bw = bw_GW_spatial, 
  kernel = "gaussian"
)

print(gwr_GW_spatial )
gwr_GW_spatial 

# determine degrees of freedom
dfree <- gwr_GW_spatial $GW.diagnostic$edf

# compute t-ratios from which p-values are derived
hexagons_sp$elevation.t <- gwr_GW_spatial $SDF@data$mean_elevation/gwr_GW_spatial $SDF@data$mean_elevation_SE
hexagons_sp$severity.t <- gwr_GW_spatial $SDF@data$mean_severity/gwr_GW_spatial $SDF@data$mean_severity_SE
hexagons_sp$VPD.t <- gwr_GW_spatial $SDF@data$mean_VPD_yod1/gwr_GW_spatial $SDF@data$mean_VPD_yod1_SE
hexagons_sp$prec.t <- gwr_GW_spatial $SDF@data$mean_prec_total/gwr_GW_spatial $SDF@data$mean_prec_total_SE
hexagons_sp$temp.t <- gwr_GW_spatial $SDF@data$mean_temp_total/gwr_GW_spatial $SDF@data$mean_temp_total_SE
hexagons_sp$predist_treecov.t <- gwr_GW_spatial $SDF@data$mean_pre_dist_tree_cover/gwr_GW_spatial $SDF@data$mean_pre_dist_tree_cover_SE
hexagons_sp$bare_ground.t <- gwr_GW_spatial $SDF@data$mean_bare/gwr_GW_spatial $SDF@data$mean_bare_SE

# compute p-values
hexagons_sp$elevation.p<-2*pt(-abs(hexagons_sp$elevation.t), dfree)
hexagons_sp$severity.p<-2*pt(-abs(hexagons_sp$severity.t), dfree)
hexagons_sp$VPD.p<-2*pt(-abs(hexagons_sp$VPD.t), dfree)
hexagons_sp$prec.p<-2*pt(-abs(hexagons_sp$prec.t), dfree)
hexagons_sp$temp.p<-2*pt(-abs(hexagons_sp$temp.t), dfree)
hexagons_sp$predist_treecov.p<-2*pt(-abs(hexagons_sp$predist_treecov.t), dfree)
hexagons_sp$bare_ground.p<-2*pt(-abs(hexagons_sp$bare_ground.t), dfree)

# convert hexagons_sp to sf object
hexagons_sf <- st_as_sf(hexagons_sp)


# compute Monte Carlo permutation to check whether spatial variability is 
# statistically significant

monte_carlo_results <- gwr.montecarlo(
  mean_percent_recovered ~ mean_elevation + 
    mean_severity + 
    mean_VPD_yod1 + 
    mean_prec_total +
    mean_temp_total +
    mean_pre_dist_tree_cover + 
    mean_bare,
  data = hexagons_sp,
  bw = bw_GW, 
  kernel = "gaussian"
)

print(monte_carlo_results)

#-------------------------------------------------------------------------------

### do the same with th spgwr package

# determine bandwidth
bw_spgwr_spatial <- gwr.sel(
  mean_percent_recovered ~ mean_elevation + 
    mean_severity + 
    mean_VPD_yod1 + 
    mean_prec_total +
    mean_temp_total +
    mean_pre_dist_tree_cover + 
    mean_bare,
  data = hexagons_sp
)

# fit GWR model
gwr_spgwr_spatial <- gwr(
  mean_percent_recovered ~ mean_elevation + 
    mean_severity + 
    mean_VPD_yod1 + 
    mean_prec_total +
    mean_temp_total +
    mean_pre_dist_tree_cover + 
    mean_bare,
  data = hexagons_sp,
  bandwidth = bw_spgwr_spatial, 
  se.fit = T,
  hatmatrix = T
)

# print model output
gwr_spgwr_spatial

# determine degrees of freedom for spgwr model
dfree <- gwr_spgwr_spatial$results$edf

# compute t-ratios for each predictor, from which p-values are derived
hexagons_sp$elevation.t <- gwr_spgwr_spatial$SDF$mean_elevation/gwr_spgwr_spatial$SDF$mean_elevation_se
hexagons_sp$severity.t <- gwr_spgwr_spatial$SDF$mean_severity/gwr_spgwr_spatial$SDF$mean_severity_se
hexagons_sp$VPD.t <- gwr_spgwr_spatial$SDF$mean_VPD_yod1/gwr_spgwr_spatial$SDF$mean_VPD_yod1_se
hexagons_sp$prec.t <- gwr_spgwr_spatial$SDF$mean_prec_total/gwr_spgwr_spatial$SDF$mean_prec_total_se
hexagons_sp$temp.t <- gwr_spgwr_spatial$SDF$mean_temp_total/gwr_spgwr_spatial$SDF$mean_temp_total_se
hexagons_sp$predist_treecov.t <- gwr_spgwr_spatial$SDF$mean_pre_dist_tree_cover/gwr_spgwr_spatial$SDF$mean_pre_dist_tree_cover_se
hexagons_sp$bare_ground.t <- gwr_spgwr_spatial$SDF$mean_bare/gwr_spgwr_spatial$SDF$mean_bare_se

# compute p-values
hexagons_sp$elevation.p<-2*pt(-abs(hexagons_sp$elevation.t), dfree)
hexagons_sp$severity.p<-2*pt(-abs(hexagons_sp$severity.t), dfree)
hexagons_sp$VPD.p<-2*pt(-abs(hexagons_sp$VPD.t), dfree)
hexagons_sp$prec.p<-2*pt(-abs(hexagons_sp$prec.t), dfree)
hexagons_sp$temp.p<-2*pt(-abs(hexagons_sp$temp.t), dfree)
hexagons_sp$predist_treecov.p<-2*pt(-abs(hexagons_sp$predist_treecov.t), dfree)
hexagons_sp$bare_ground.p<-2*pt(-abs(hexagons_sp$bare_ground.t), dfree)

# convert hexagons_sp to sf object
hexagons_sf <- st_as_sf(hexagons_sp)

# CRS conversion (most likely) not necessary
#hexagons_sf <- st_transform(hexagons_sf, 3035) 


#-------------------------------------------------------------------------------
### mapping

ggplot() +
  geom_sf(data = hexagons_sf, aes(fill = bare_ground.p), color = "black", size = 0.2) +
  geom_sf_pattern(
    data = subset(hexagons_sf, bare_ground.p < 0.08),
    aes(fill = bare_ground.p), 
    color = "black", 
    pattern = "stripe",  
    pattern_spacing = 0.01,  
    pattern_fill = "black",
    pattern_alpha = 0.3  
    #size = 0.1  
  ) +
  scale_fill_viridis_c(option = "A", name = "bare_ground.p", direction = -1) + 
  theme_minimal(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )

ggsave("~/eo_nas/EO4Alps/figs/p_values_bareground.png", width = 7, height = 4.5, dpi = 300)


#-------------------------------------------------------------------------------
### Multicollinearity?

# correlation between coefficients (this is based on the GWmodel)
round(cor(as.data.frame(gwr_model$SDF@data[,2:8]), use ="complete.obs"),2)

# print with ggpairs (this is based on the GWmodel)
pairs(as(gwr_model$SDF@data, "data.frame")[,2:8], pch=".")


# compute model diagnostics (based on GWmodel)
gwr_model$GW.diagnostic$AIC
gwr_model$results$AICh
gwr_model$results$AICc
gwr_model$results$AICb


# run LMZ test (only works with model output from spgwr package)
LMZ.F3GWR.test(gwr_model_spgwr)



#-------------------------------------------------------------------------------
### additional plotting stuff (R² map, coefficients,...)

# Extract coefficients for each predictor
gwr_results <- as.data.frame(gwr_GW_spatial$SDF)

# Add coefficients back to the spatial hexagon data
hexagons_recov10$coef_elevation <- gwr_results$mean_elevation
hexagons_recov10$coef_severity <- gwr_results$mean_severity
hexagons_recov10$coef_VPD <- gwr_results$mean_VPD
hexagons_recov10$coef_VPD_yod1 <- gwr_results$mean_VPD_yod1
hexagons_recov10$coef_temp_total <- gwr_results$mean_temp_total
hexagons_recov10$coef_prec_total <- gwr_results$mean_prec_total
hexagons_recov10$coef_treecov <- gwr_results$mean_pre_dist_tree_cover
hexagons_recov10$coef_bare <- gwr_results$mean_bare
hexagons_recov10$coef_forest_type <- gwr_results$dominant_forest_type

# Extract local R²
hexagons_recov10$local_r2 <- gwr_results$Local_R2
hexagons_recov10$pred <- gwr_results$yhat


# Compute the mean of local_R2, excluding NA values
mean_local_R2 <- mean(hexagons_recov10$local_r2, na.rm = TRUE)

# Print the result
print(mean_local_R2)

# local R²
hexagons_recov10$local_r2 <- hexagons_recov10$local_r2 * 1.05

# Map Local R²
p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = local_r2)) +
  scale_fill_viridis_c(option = "magma", name = "R²") +
  labs(title = "") +
  theme_bw(base_size = 22)

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_local_r2_recov10_VPD_ano_temp_prec.png", plot = p1, width = 7, height = 4.5, dpi = 300)


# Map predicted recovery rate
ggplot(hexagons_recov10) +
  geom_sf(aes(fill = pred)) +
  scale_fill_viridis_c(option = "magma", name = "") +
  labs(title = "") +
  theme_bw(base_size = 22)

# observed
ggplot(hexagons_recov10) +
  geom_sf(aes(fill = mean_percent_recovered)) +
  scale_fill_viridis_c(option = "magma", name = "") +
  labs(title = "") +
  theme_bw(base_size = 22)


# Export sf object as Shapefile
st_write(
  obj = hexagons_recov10,                       
  dsn = "~/eo_nas/EO4Alps/gis/recovery_hexagons/recov10_rates_VPD_ano_prec_temp.shp", # File path and name
  driver = "ESRI Shapefile"                    # Specify driver explicitly
)


hexagons_recov10$residuals <- gwr_results$residual

# Map Residuals
p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = residuals)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, name = "Residuals") +
  labs(title = "") +
  theme_bw(base_size = 22)

# GAM resiudal map
ggplot(hexagons_recov10) +
  geom_sf(aes(fill = residuals), color = "grey") +  
  scale_fill_gradient2(
    low = "green",    # Color for negative residuals
    mid = "white",    # Color for zero residuals
    high = "orange",  # Color for positive residuals
    midpoint = 0,     # Center the gradient at 0
    name = "GWR residuals"
  ) +
  theme_minimal(base_size = 18) +
  labs(title = "GWR residuals")


ggsave("~/eo_nas/EO4Alps/figs/resiudals_GWR.png", width = 10, height = 8, dpi = 300)



### plot coefficients

# for elevation
p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = coef_elevation)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_elev_recov10_VPD_ano_3101.png", plot = p1, width = 7, height = 4.5, dpi = 300)


# for severity
p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = coef_severity)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_severity_recov_10_VPD_ano_3101.png", plot = p1, width = 7, height = 4.5, dpi = 300)



# for VPD anomalies
p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = coef_VPD_yod1)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )


# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_VPD__10_VPD_ano_3101.png", plot = p1, width = 7, height = 4.5, dpi = 300)


# for temperature
p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = coef_temp_total)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_temp_recov_10_VPD_ano_temp_prec_3101.png", plot = p1, width = 7, height = 4.5, dpi = 300)



# for precipitation
p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = coef_prec_total)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_prec_VPD_ano_temp_prec_3101.png", plot = p1, width = 7, height = 4.5, dpi = 300)



# for pre-disturbance tree cover
p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = coef_treecov)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_pre_dist_treecov_recov_10_VPD_ano_3101.png", plot = p1, width = 7, height = 4.5, dpi = 300)



# for post-disturbance bare ground
p1 <- ggplot(hexagons_recov10) +
  geom_sf(aes(fill = coef_bare)) +
  scale_fill_gradient2(
    low = "#126D0E",       # Color for negative values
    mid = "white",      # Color at zero
    high = "#E69E03",       # Color for positive values
    midpoint = 0,       # Center the scale at 0
    name = ""
  ) +
  labs(title = "") +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "inside",                 
    legend.justification = c(1, 0),             
    legend.position.inside = c(0.955, 0.015),
    legend.background = element_blank(),       
    legend.key = element_blank()                
  )

# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/map_coef_bare_recov_10_VPD_ano_3101.png", plot = p1, width = 7, height = 4.5, dpi = 300)




#-------------------------------------------------------------------------------



# Compute cumulative recovered disturbances per ysd and geoloc
recov_cumulative <- recov_prep %>%
  arrange(geoloc, ysd) %>%  # Ensure data is sorted
  group_by(geoloc, ysd) %>%
  summarise(recovered_count = sum(recovery_10yn, na.rm = TRUE), .groups = "drop") %>%
  group_by(geoloc) %>%
  mutate(cumulative_recovered = cumsum(recovered_count))

# Plot the results
ggplot(recov_cumulative, aes(x = ysd, y = cumulative_recovered, color = geoloc)) +
  geom_line(size = 1) +
  facet_wrap(~geoloc, scales = "free_y") +  
  labs(x = "ysd", 
       y = "Percentage recovered", 
       title = "",
       color = "") +
  theme_minimal()




# Compute years since disturbance (ysd)
recov_prep <- recovery %>%
  mutate(ysd = year - yod) %>% 
  filter(ysd >= 0) 

# Compute total number of disturbances per geoloc
total_recov <- recov_prep %>%
  group_by(geoloc) %>%
  summarise(total_recovered = sum(recovery_10yn, na.rm = TRUE), .groups = "drop")

# Compute cumulative recovered disturbances and percentage
recov_cumulative <- total_recov %>%
  arrange(geoloc, ysd) %>%  # Ensure data is sorted
  group_by(geoloc, ysd) %>%
  summarise(recovered_count = sum(recovery_10yn, na.rm = TRUE), .groups = "drop") %>%
  group_by(geoloc) %>%
  mutate(
    cumulative_recovered = cumsum(recovered_count)
  ) %>%
  left_join(total_recovered_per_geoloc, by = "geoloc") %>%
  mutate(cumulative_recovered_percent = (cumulative_recovered / total_recovered) * 100)


# Plot the results
ggplot(df_cumulative, aes(x = ysd, y = cumulative_recovered_percent, color = geoloc)) +
  geom_line(size = 1) +
  facet_wrap(~geoloc, scales = "free_y") +  # Separate plots per geoloc
  labs(x = "Years Since Disturbance (YSD)", 
       y = "Cumulative Recovered Disturbances (%)", 
       title = "Cumulative Recovery Percentage Over Time per Geolocation",
       color = "Geolocation") +
  theme_minimal()





