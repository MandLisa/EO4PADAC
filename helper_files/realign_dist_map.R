library(raster)
library(rgdal)
library(terra)

# Load disturbance and fcover mosaic
fcover <- raster("/mnt/public/Projects/DataCube/projects/foreco/alps/level3_predictions/v3/mosaic_2021.tif")
dist <- raster("/mnt/public/Projects/DataCube/projects/foreco/alps/gis/disturbances_clip/disturbances_clip.tif")

# Check the current alignment
plot(fcover)
plot(dist, add = TRUE, col = "red") # This will overlay raster2 in red

# Project raster2 to the extent and resolution of raster1
raster2_aligned <- projectRaster(from = dist, to = fcover, method = "bilinear")

# Save the aligned raster to a new file (replace "raster_file2_aligned.tif" with your desired file name)
writeRaster(raster2_aligned, "/mnt/public/Projects/DataCube/projects/foreco/alps/gis/dist_aligned_2807.tif", format = "GTiff")


# Align raster2 to raster1 without changing cell values (using nearest-neighbor resampling)
raster2_aligned <- projectRaster(from = dist, to = fcover, method = "ngb")

writeRaster(raster2_aligned, "/mnt/public/Projects/DataCube/projects/foreco/alps/gis/dist_aligned_2807_1.tif", format = "GTiff")
