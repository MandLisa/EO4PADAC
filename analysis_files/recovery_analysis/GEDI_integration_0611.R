library(sf)
library(raster)
library(dplyr)
library(purrr)


# Define the folder containing your geopackages and the shapefile path
folder_path <- "~/eo_nas/EO4Alps/GEDI/gedi_2a"
shape <- st_read("~/eo_nas/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/Alpine_Convention_Perimeter_2018_v2.shp")
dist <- raster("~/eo_nas/EO4Alps/dist_data/dist_crop.tif")
forestcover <- raster("~/eo_nas/EO4Alps/dist_data/forestcover_alps.tif")

GEDI <- st_read("~/eo_nas/EO4Alps/GEDI/gedi_2a/allpoints.gpkg")

GEDI_dist <- GEDI %>% filter(!is.na(dist_year))


# Ensure `subset_df` is an sf object
GEDI_dist_sf <- st_as_sf(GEDI_dist)

st_write(GEDI_dist_sf, "~/eo_nas/EO4Alps/GEDI/GEDI_dist.shp")


