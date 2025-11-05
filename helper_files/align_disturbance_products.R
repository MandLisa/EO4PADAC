library(terra)

#-------------------------------------------------------------------------------
## 1) Read reference raster (EPSG:3035 grid) and GLANCE-7 map
# this is the "correct" grid in EPSG 3035, e.g. the NBR layers, directory to be
# replaced
r_ref    <- rast("/mnt/eo/eu_mosaics/NBR_comp/NBR_2023.tif")   

# this refers to the Zenodo maps, directory to be replaced
r_glance7 <- rast("/mnt/eo/EFDA_v211/latest_disturbance_eu_v211_2_3035.tif")           # to be aligned

## Quick checks, optional
#crs(r_ref)
#crs(r_glance)
#res(r_ref)
#res(r_glance)
#ext(r_ref)
#ext(r_glance)

#-------------------------------------------------------------------------------
## 2) Reproject + resample GLANCE-7 to the 3035 grid
## Use "bilinear" because we have a continuos variabel
r_glance_on3035 <- project(
  r_glance,
  r_ref,
  method = "bilinear" 
)

#-------------------------------------------------------------------------------

## 3) Write result
writeRaster(
  r_glance_on3035,
  "/mnt/eo/EFDA_v211//map_glance7_aligned_3035.tif", # to be replaced
  overwrite = TRUE
)


