# Load required libraries
library(raster)
library(tidyr)
library(dplyr)
library(sp)
library(sf)
library(ggplot2)
library(terra)
library(spatial)
library(readr)
library(spatstat)
library(pryr)
library(mgcv)
library(purrr)
library(readr)
library(mgcv)
library(stringr)
library(randomForest)
library(broom)
library(ranger)
library(doParallel)
library(reshape2)
library(corrplot)
library(caret)


agent <- raster("~/eo_nas/EO4Alps/gis/attribution_v1/agents_mosaic.tif")
plot(agent)

# Load the shapefile
alps <- st_read("~/eo_nas/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp")

# Assign the CRS from the shapefile to the raster
crs(agent) <- crs(alps)
crs(alps)
crs(agent)

# Reproject `alps` to the CRS of `agent`
alps <- st_transform(alps, st_crs(agent))


if (st_crs(alps) != target_crs) {
  alps <- st_transform(alps, crs = target_crs)
}

# Crop the raster to the extent of the shapefile
agent_crop <- crop(agent, alps)
plot(agent_crop)

# Mask the raster to the shape of the shapefile
agent_masked <- mask(agent_crop, alps)

plot(agent_masked)

# Save the cropped and masked raster to a new file
writeRaster(agent_masked, "~/eo_nas/EO4Alps/gis/attribution_v1/agent_clip_LAEA.tif", format="GTiff", overwrite=TRUE)





# Load the reference raster (assuming the reference raster file path is 'reference_raster.tif')
reference_raster <- raster("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0028_Y0028/PREDICTION_l2_1986_v3_HL_ML_MLP.tif")

# Extract the CRS from the reference raster
target_crs <- crs(reference_raster)

# Assume recovery_df has columns: 'longitude' and 'latitude'
# input here the recovery_standardized df
recovery_standardized_sf <- st_as_sf(recovery_standardized, coords = c("x", "y"), crs = st_crs(reference_raster))

# reproject raster
agent <- projectRaster(agent_masked, crs = crs(reference_raster))

# Save the cropped and masked raster to a new file
writeRaster(agent, "~/eo_nas/EO4Alps/gis/attribution/attribution_aggregated/agent_reprojected.tif", format="GTiff", overwrite=TRUE)


# extract agent
recovery_imputed_unique$agent <- extract(agent, recovery_imputed_unique[, c("x", "y")])
gc()

# Assuming your data frame is named df
recovery_imputed_unique$agent_num <- round(recovery_imputed_unique$agent)



### extract geo location
geoloc <- raster("~/eo_nas/EO4Alps/gis/alps_subdivision/alps_division_100.tif")

# reproject raster
geoloc <- projectRaster(geoloc, crs = crs(reference_raster))

# extract agent
recovery_imputed_unique$geoloc <- extract(geoloc, recovery_imputed_unique[, c("x", "y")])

recovery_imputed_unique <- recovery_imputed_unique %>%
  mutate(geoloc_name = case_when(
    geoloc == 1 ~ "Southern West Alps",
    geoloc == 2 ~ "Northern West Alps",
    geoloc == 3 ~ "Central Alps",
    geoloc == 4 ~ "Southern East Alps",
    geoloc == 5 ~ "Northern East Alps",
    TRUE ~ NA_character_  # In case there are any values outside 1-5
  ))


recovery_imputed_unique <- recovery_imputed_unique %>%
  mutate(agent_name = case_when(
    agent_num == 0 ~ "other",
    agent_num == 1 ~ "Bark Beetle/Wind",
    agent_num == 2 ~ "Fire",
    agent_num == 3 ~ "other",
    TRUE ~ NA_character_  # In case there are any values outside 1-5
  ))


### write
write.csv(recovery_imputed_unique, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_unique.csv", row.names=FALSE)


