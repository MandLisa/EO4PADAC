library(mgcv)
library(ggplot2)
library(patchwork)  
library(gratia)    
library(readr)
library(sf)
library(dplyr)

recovery <- read_csv("/mnt/eo/EO4Alps/00_analysis/_recovery/hexagons_recov_10_centros_1710.csv")

# load hexagons and recovery df
hexagons <- st_read("/mnt/eo/EO4Alps/gis/hexagons/hex_500.shp")

# just use GRID_ID for subsequent joins
hexagons_selected <- hexagons %>%
  select(GRID_ID)
