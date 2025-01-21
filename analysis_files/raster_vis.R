library(grDevices)
library(raster)


dem <- rast("~/eo_nas/EO4Alps/dem/aster_dem_LAEA_NA.tif")
plot(dem)

# Check CRS of both rasters
crs(dem)        # CRS of the DEM
crs(sum_band)   # CRS of the mosaic raster

# If the CRS is different, reproject the DEM to match the mosaic raster's CRS
if (!identical(crs(dem), crs(sum_band))) {
  dem <- project(dem, crs(sum_band))
}
dem_cropped <- crop(dem, sum_band)
plot(dem_cropped)

# Ensure both rasters are in the same CRS
dem_cropped <- project(dem_cropped, crs(sum_band))

# Resample dem_cropped to match sum_band
dem_aligned <- resample(dem_cropped, sum_band)

dem_masked <- mask(dem_aligned, sum_band)
plot(dem_masked)


# Compute slope and aspect
slope_raster <- terrain(dem_masked, v = "slope", unit = "radians")
# Calculate aspect
aspect_raster <- terrain(dem_masked, v = "aspect", unit = "radians")

# Generate the hillshade
hillshade <- shade(slope_raster, aspect_raster)


# Step 1: Load the mosaic raster
raster_file <- "~/eo_nas/EO4Alps/level3_predictions/l2/mosaic_2015_crop.tif" # Replace with the path to your raster
raster <- rast(raster_file)

# Step 1: Reclassify values > 10,000 to 10,000 and values < 0 to 0
raster[raster > 10000] <- 10000
raster[raster < 0] <- 0
gc()
# Step 2: Scale the values from 0 - 1000 to 0 - 100
# Since the values are in the range 0 - 10,000, dividing by 100 will bring them to 0 - 100
scaled_raster <- raster / 100
gc()

# Step 3: Select bands 6 and 7 for plotting
band6 <- scaled_raster[[6]]
band7 <- scaled_raster[[7]]

# Add the two bands together
sum_band <- band6 + band7

# Define custom color schemes for each band
col_sum_band <- hcl.colors(100, "mako", rev = TRUE)  # Customize color palette for band 6
col_sum_band <- hcl.colors(100, "Emrld", rev = TRUE)  # Customize color palette for band 7
col_sum_band <- hcl.colors(100, "SunsetDark", rev = TRUE)  # Customize color palette for band 7


# Normalize hillshade values to range [0, 1]
hillshade_norm <- (hillshade - min(values(hillshade), na.rm = TRUE)) / 
  (max(values(hillshade), na.rm = TRUE) - min(values(hillshade), na.rm = TRUE))

# Multiply hillshade with raster color intensities
hillshade_blended <- sum_band * hillshade_norm



# Save the raster to a GeoTIFF file
writeRaster(sum_band, 
            "~/eo_nas/EO4Alps/gis/vis/treecover_mosaic.tif", 
            overwrite = TRUE)

writeRaster(dem_masked, 
            "~/eo_nas/EO4Alps/gis/vis/dem_crop.tif", 
            overwrite = TRUE)


writeRaster(hillshade, 
            "~/eo_nas/EO4Alps/gis/vis/hillshade.tif", 
            overwrite = TRUE)



writeRaster(hillshade, 
            "~/eo_nas/EO4Alps/gis/vis/hillshade.tif", 
            format = "GTiff",  
            overwrite = TRUE)



plot(sum_band, 
     col = col_sum_band, 
     legend = FALSE,      # Disable the legend
     axes = FALSE,        # Remove the axes
     box = FALSE,         # Remove the frame around the plot
     main = "")           # Optional: Remove the title




# Set up the file path and dimensions for saving the plot
png(filename = "~/eo_nas/EO4Alps/figs/broadleaved_share.png", width = 8, height = 6, units = "in", res = 300)
# Plot band 7
plot(sum_band, col = col_sum_band, main = "", 
     plg = list(title = ""), mar = c(4, 4, 2, 5))
dev.off()


#-------------------------------------------------------------------------------
###

library(terra)

# Step 1: Load the mosaic raster
raster_file <- "~/eo_nas/EO4Alps/level3_predictions/l1_walltowall/mosaic_2022_crop.tif" # Replace with the path to your raster
raster <- rast(raster_file)

# Step 3: Select bands 6 and 7 for plotting
BG <- raster[[6]]
CF <- raster[[2]]
BL <- raster[[1]]
shrubs <- raster[[3]]
GL <- raster[[4]]

# Step 1: Reclassify values > 10,000 to 10,000 and values < 0 to 0
BG[BG > 10000] <- 10000
BG[BG < 0] <- 0
gc()

CF[CF > 10000] <- 10000
CF[CF < 0] <- 0
gc()

BL[BL > 10000] <- 10000
BL[BL < 0] <- 0
gc()


shrubs[shrubs > 10000] <- 10000
shrubs[shrubs < 0] <- 0
gc()


GL[GL > 10000] <- 10000
GL[GL < 0] <- 0
gc()


# Define custom color schemes for each band
col_sum_band <- hcl.colors(100, "mako", rev = TRUE)  # Customize color palette for band 6
col_sum_band <- hcl.colors(100, "Emrld", rev = TRUE)  # Customize color palette for band 7
col_sum_band <- hcl.colors(100, "SunsetDark", rev = TRUE)  # Customize color palette for band 7



plot(GL, 
     col = col_sum_band, 
     legend = FALSE,      # Disable the legend
     axes = FALSE,        # Remove the axes
     box = FALSE,         # Remove the frame around the plot
     main = "")   


