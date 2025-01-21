#-------------------------------------------------------------------------------
###

library(terra)
library(ggplot2)

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
col_sum_band <- hcl.colors(100, "Mint", rev = TRUE)  # Customize color palette for band 6
col_sum_band <- hcl.colors(100, "OrRd", rev = TRUE)  # Customize color palette for band 7
col_sum_band <- hcl.colors(100, "Purple-Yellow", rev = TRUE)  # Customize color palette for band 7
col_sum_band <- hcl.colors(100, "YlOrBr", rev = TRUE)  # Customize color palette for band 
col_sum_band <- hcl.colors(100, "Sunset", rev = TRUE)  # Customize color palette for band 


png("~/eo_nas/EO4Alps/figs/BL_share.png", width = 8, height = 6, units = "in", res = 300)
plot(BL, 
     col = col_sum_band, 
     legend = TRUE,      # Disable the legend
     axes = FALSE,        # Remove the axes
     box = FALSE,         # Remove the frame around the plot
     main = "")   
dev.off()



# Save a specific plot object
ggsave("~/eo_nas/EO4Alps/figs/CF_share.png", plot = p1, width = 8, height = 6, dpi = 300)




# Sum the rasters
sum_raster <- CF + BL

sum_raster[sum_raster > 10000] <- 10000

# Divide the raster values by 100
sum_raster <- sum_raster / 100



# Plot without the frame
png("~/eo_nas/EO4Alps/figs/fractional_tree_cover.png", width = 800, height = 600, res = 100)
plot(sum_raster, main = "", col = col_sum_band, 
     axes = FALSE, box = FALSE, legend.args = list(text = "Value"))
dev.off()




# Save the raster with DEFLATE compression
writeRaster(sum_raster, "~/eo_nas/EO4Alps/level3_predictions/tree_only/treeshare.tif", filetype = "GTiff", 
            gdal = c("COMPRESS=DEFLATE"), overwrite = TRUE)




# Load the shapefile (boundary)
boundary <- vect("~/eo_nas/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp")

# Plot the raster
plot(sum_raster, sum_raster, main = "", col = col_sum_band, 
     axes = FALSE, box = FALSE,)

# Add the shapefile as a boundary (in red)
lines(boundary, col = "black", lwd = 2)  # lwd = line width



