library(raster)
library(sf)
library(dplyr)
library(terra)

# Load the DEM raster
dem <- raster("~/eo_nas/EO4Alps/dem/aster_dem_LAEA_NA.tif")
plot(dem)
# Load your shapefile
alps_shp <- shapefile("~/eo_nas/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/Alpine_Convention_Perimeter_2018_v2.shp")

# Get CRS of raster
raster_crs <- crs(dem)
print(raster_crs)

# Get CRS of shapefile
shapefile_crs <- crs(alps_shp)
print(shapefile_crs)

# Reproject shapefile to match raster's CRS
alps_shp <- spTransform(alps_shp, crs(dem))

# Crop the raster to the extent of the shapefile
dem_crop <- crop(dem, alps_shp)

# Mask the raster to the shapefile (only keep the areas within the shapefile)
dem_mask <- mask(dem_crop, alps_shp)

plot(dem_mask)


# Compute slope and aspect
slope <- terrain(dem_mask, opt = "slope", unit = "degrees")
aspect <- terrain(dem_mask, opt = "aspect", unit = "degrees")

# Write the raster to a file (GeoTIFF format)
writeRaster(dem, filename = "~/eo_nas/EO4Alps/dem/topo/dem_reproj.tif", format = "GTiff", overwrite = TRUE)
writeRaster(slope, filename = "~/eo_nas/EO4Alps/dem/topo/slope.tif", format = "GTiff", overwrite = TRUE)
writeRaster(aspect, filename = "~/eo_nas/EO4Alps/dem/topo/aspect.tif", format = "GTiff", overwrite = TRUE)


# Load the reference raster (assuming the reference raster file path is 'reference_raster.tif')
reference_raster <- raster("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0028_Y0028/PREDICTION_l2_1986_v3_HL_ML_MLP.tif")

# Extract the CRS from the reference raster
target_crs <- crs(reference_raster)

# Assume recovery_df has columns: 'longitude' and 'latitude'
recovery_sf <- st_as_sf(recovery_climate, coords = c("x", "y"), crs = st_crs(reference_raster))

# just go with one observation per ID (for e.g. plotting)
recovery_unique <- recovery_climate %>% distinct(ID, .keep_all = TRUE)

recovery_unique_sf <- st_as_sf(recovery_unique, coords = c("x", "y"), crs = st_crs(reference_raster))

# reproject raster
slope <- projectRaster(slope, crs = crs(reference_raster))
aspect <- projectRaster(aspect, crs = crs(reference_raster))
dem <- projectRaster(dem_mask, crs = crs(reference_raster))

crs(dem)
crs(slope)
crs(reference_raster)

# Write raster to a new TIFF file
writeRaster(dem, filename="~/eo_nas/EO4Alps/dem/dem.tif", format="GTiff", overwrite=TRUE)
writeRaster(slope, filename="~/eo_nas/EO4Alps/slope.tif", format="GTiff", overwrite=TRUE)
writeRaster(aspect, filename="~/eo_nas/EO4Alps/aspect.tif", format="GTiff", overwrite=TRUE)



# Extract values for each raster
dem_values <- extract(dem, recovery_sf)
slope_values <- extract(slope, recovery_sf)
aspect_values <- extract(aspect, recovery_sf)

# If dem_values is a list, convert it to a vector
dem_values <- unlist(dem_values)
slope_values <- unlist(slope_values)
aspect_values <- unlist(aspect_values)

# Add the DEM values as a new column to recovery_sf
recovery_sf$height <- dem_values
recovery_sf$slope <- slope_values
recovery_sf$aspect <- aspect_values


# Extract coordinates (x and y)
coords <- st_coordinates(recovery_sf)

# Convert sf object to dataframe and add x and y coordinates as new columns
recovery_climate_topo <- st_drop_geometry(recovery_sf)  # Drop geometry to convert to regular dataframe
recovery_climate_topo$x <- coords[, 1]  # Add x coordinates
recovery_climate_topo$y <- coords[, 2]  # Add y coordinates


### write
write.csv(recovery_climate_topo, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_climate_topo.csv", row.names=FALSE)








