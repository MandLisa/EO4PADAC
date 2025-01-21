library(readr)
library(terra)
library(raster)
library(sf)
library(tidyverse)
library(mgcv)
library(furrr)

# this is the df
samples_VPD <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/random_sample.csv")

# Select one observation per ID
sampled_points_unique <- sampled_points %>%
  distinct(ID, .keep_all = TRUE)


# (1) Load mosaic raster from frational cover maps
raster <- rast("~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2018.tif")

shp <- vect("~/eo_nas/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/perimeter_LAEA.shp")

# Convert to a plain data frame (attributes only, no geometry)
points_df <- as.data.frame(points)

# project raster
raster <- project(raster, crs(shp))

# Extent basierend auf dem Shapefile zuschneiden
cropped_raster <- crop(raster, shp)

# Maske anwenden, um Bereiche außerhalb des Shapefiles zu entfernen
masked_raster <- mask(cropped_raster, shp)

# Convert raster to data frame with pixel coordinates
raster_df <- as.data.frame(masked_raster, xy = TRUE, cells = TRUE)

# Ensure you have at least 1 million non-NA pixels
if (nrow(raster_df) < 1200000) {
  stop("The raster has fewer than 1 million non-NA pixels.")
}

# Randomly sample 1 million centroids
sampled_points <- raster_df %>%
  sample_n(1200000)

# Convert to SpatVector for spatial operations
random_sample <- vect(sampled_points, geom = c("x", "y"), crs = crs(raster))


sampled_points <- sampled_points[, -4]

sampled_points <- sampled_points %>%
  mutate(ID = row_number())

# Save the points as a shapefile (optional)
writeVector(random_sample, "~/eo_nas/EO4Alps/00_analysis/_recovery/random_points.shp", overwrite = TRUE)

# based df
write.csv(samples_all, "~/eo_nas/EO4Alps/00_analysis/_recovery/random_sample.csv", row.names = FALSE)

#-------------------------------------------------------------------------------

### first, derive topographic factors, such as sea level height, slope and aspect

# load copernicus dem
dem <- rast("~/eo_nas/EO4Alps/dem/aster_dem_LAEA_NA.tif")

# compute slope and aspect
slope <- terrain(dem, v = "slope", unit = "degrees")
aspect <- terrain(dem, v = "aspect", unit = "degrees")


# extract data
# Convert recovery_1986 to an sf object if necessary
sampled_points_dem <- st_as_sf(sampled_points, coords = c("x", "y"), crs = crs(dem))

# Extract dem values
dem_values <- terra::extract(dem, sampled_points_dem)

# Add extracted values back to the dataframe
sampled_points_dem$height <- dem_values[, 2] 

#---------------

# Extract slope values
slope_values <- terra::extract(slope, sampled_points_dem)

# Add extracted values back to the dataframe
sampled_points_dem$slope <- slope_values[, 2] 

#----------------

# Extract slope values
aspect_values <- terra::extract(aspect, sampled_points_dem)

# Add extracted values back to the dataframe
sampled_points_dem$aspect <- aspect_values[, 2]


#------------------------------------------------------------------------------
### derive yod
#------------------------------------------------------------------------------

# extract data
# Convert recovery_1986 to an sf object if necessary
#sampled_points_unique_sf <- st_as_sf(sampled_points_unique, coords = c("x", "y"), crs = crs(dist))


# load siturbance raster
dist <- rast("~/eo_nas/EO4Alps/dist_data/dist_crop.tif")

# Extract slope values
yod_values <- terra::extract(dist, sampled_points_dem)

# Add extracted values back to the dataframe
sampled_points_dem$yod <- yod_values[, 2] 

# Percentage of NAs in a specific column
na_percentage <- (sum(is.na(sampled_points_dem$yod)) / nrow(sampled_points_dem)) * 100
print(na_percentage)

# Remove rows where yod is NA
sampled_points_dem <- sampled_points_dem[!is.na(sampled_points_dem$yod), ]


### etract agent
# load siturbance raster
agent <- rast("~/eo_nas/EO4Alps/dist_data/agent_alps.tif")

# Extract slope values
agent_values <- terra::extract(agent, sampled_points_dem)

# Add extracted values back to the dataframe
sampled_points_dem$agent <- agent_values[, 2] 



### etract geoloc
# load siturbance raster
geoloc <- rast("~/eo_nas/EO4Alps/gis/alps_subdivision/alps_division_100.tif")

# Extract slope values
geoloc_values <- terra::extract(geoloc, sampled_points_dem)

# Add extracted values back to the dataframe
sampled_points_dem$geoloc <- geoloc_values[, 2] 


# based df
write.csv(sampled_points_dem, "~/eo_nas/EO4Alps/00_analysis/_recovery/random_sample.csv", row.names = FALSE)



#-------------------------------------------------------------------------------
### extract tree cover per year

###1986

# Assuming your data frame is named `data`
# Extract x and y coordinates
sampled_points_dem$x <- st_coordinates(sampled_points_dem)[, 1]  # Extract x-coordinates
sampled_points_dem$y <- st_coordinates(sampled_points_dem)[, 2]  # Extract y-coordinates

# Drop the geometry column if needed
sampled_points_dem <- st_drop_geometry(sampled_points_dem)

### extract

### 1986
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover1986.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster

# Convert recovery_1986 to an sf object if necessary
sample_1986_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

# Extract VPD values
treecov_values <- terra::extract(raster_data, sample_1986_sf)

# Add new columns directly without creating treecover_1986
samples_1986 <- sampled_points_dem
samples_1986$year <- 1986                          
samples_1986$tree_cov <- treecov_values[, 2]        


### 1987
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover1987.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster

# Convert recovery_1987 to an sf object if necessary
sample_1987_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

# Extract VPD values
treecov_values <- terra::extract(raster_data, sample_1987_sf)

# Add new columns directly without creating treecover_1987
samples_1987 <- sampled_points_dem
samples_1987$year <- 1987                          
samples_1987$tree_cov <- treecov_values[, 2]        


### 1988
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover1988.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster

# Convert recovery_1988 to an sf object if necessary
sample_1988_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

# Extract VPD values
treecov_values <- terra::extract(raster_data, sample_1988_sf)

# Add new columns directly without creating treecover_1988
samples_1988 <- sampled_points_dem
samples_1988$year <- 1988                          
samples_1988$tree_cov <- treecov_values[, 2]   


### 1989
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover1989.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster

# Convert recovery_1989 to an sf object if necessary
sample_1989_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

# Extract VPD values
treecov_values <- terra::extract(raster_data, sample_1989_sf)

# Add new columns directly without creating treecover_1989
samples_1989 <- sampled_points_dem
samples_1989$year <- 1989                          
samples_1989$tree_cov <- treecov_values[, 2]   



### 1990
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover1990.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster

# Convert recovery_1990 to an sf object if necessary
sample_1990_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

# Extract VPD values
treecov_values <- terra::extract(raster_data, sample_1990_sf)

# Add new columns directly without creating treecover_1990
samples_1990 <- sampled_points_dem
samples_1990$year <- 1990                          
samples_1990$tree_cov <- treecov_values[, 2]   


### 1991
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover1991.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster

# Convert recovery_1991 to an sf object if necessary
sample_1991_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

# Extract VPD values
treecov_values <- terra::extract(raster_data, sample_1991_sf)

# Add new columns directly without creating treecover_1991
samples_1991 <- sampled_points_dem
samples_1991$year <- 1991                          
samples_1991$tree_cov <- treecov_values[, 2]   


### 1992
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover1992.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster

# Convert recovery_1992 to an sf object if necessary
sample_1992_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

# Extract VPD values
treecov_values <- terra::extract(raster_data, sample_1992_sf)

# Add new columns directly without creating treecover_1992
samples_1992 <- sampled_points_dem
samples_1992$year <- 1992                          
samples_1992$tree_cov <- treecov_values[, 2]   


### 1993
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover1993.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster

# Convert recovery_1993 to an sf object if necessary
sample_1993_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

# Extract VPD values
treecov_values <- terra::extract(raster_data, sample_1993_sf)

# Add new columns directly without creating treecover_1993
samples_1993 <- sampled_points_dem
samples_1993$year <- 1993                          
samples_1993$tree_cov <- treecov_values[, 2]   


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover1994.tif"
raster_data <- rast(raster_path)

sample_1994_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_1994_sf)

samples_1994 <- sampled_points_dem
samples_1994$year <- 1994
samples_1994$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover1995.tif"
raster_data <- rast(raster_path)

sample_1995_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_1995_sf)

samples_1995 <- sampled_points_dem
samples_1995$year <- 1995
samples_1995$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover1996.tif"
raster_data <- rast(raster_path)

sample_1996_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_1996_sf)

samples_1996 <- sampled_points_dem
samples_1996$year <- 1996
samples_1996$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover1997.tif"
raster_data <- rast(raster_path)

sample_1997_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_1997_sf)

samples_1997 <- sampled_points_dem
samples_1997$year <- 1997
samples_1997$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover1998.tif"
raster_data <- rast(raster_path)

sample_1998_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_1998_sf)

samples_1998 <- sampled_points_dem
samples_1998$year <- 1998
samples_1998$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover1999.tif"
raster_data <- rast(raster_path)

sample_1999_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_1999_sf)

samples_1999 <- sampled_points_dem
samples_1999$year <- 1999
samples_1999$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2000.tif"
raster_data <- rast(raster_path)

sample_2000_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2000_sf)

samples_2000 <- sampled_points_dem
samples_2000$year <- 2000
samples_2000$tree_cov <- treecov_values[, 2]



raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2001.tif"
raster_data <- rast(raster_path)

sample_2001_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2001_sf)

samples_2001 <- sampled_points_dem
samples_2001$year <- 2001
samples_2001$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2002.tif"
raster_data <- rast(raster_path)

sample_2002_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2002_sf)

samples_2002 <- sampled_points_dem
samples_2002$year <- 2002
samples_2002$tree_cov <- treecov_values[, 2]



raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2003.tif"
raster_data <- rast(raster_path)

sample_2003_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2003_sf)

samples_2003 <- sampled_points_dem
samples_2003$year <- 2003
samples_2003$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2004.tif"
raster_data <- rast(raster_path)

sample_2004_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2004_sf)

samples_2004 <- sampled_points_dem
samples_2004$year <- 2004
samples_2004$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2005.tif"
raster_data <- rast(raster_path)

sample_2005_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2005_sf)

samples_2005 <- sampled_points_dem
samples_2005$year <- 2005
samples_2005$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2006.tif"
raster_data <- rast(raster_path)

sample_2006_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2006_sf)

samples_2006 <- sampled_points_dem
samples_2006$year <- 2006
samples_2006$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2007.tif"
raster_data <- rast(raster_path)

sample_2007_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2007_sf)

samples_2007 <- sampled_points_dem
samples_2007$year <- 2007
samples_2007$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2008.tif"
raster_data <- rast(raster_path)

sample_2008_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2008_sf)

samples_2008 <- sampled_points_dem
samples_2008$year <- 2008
samples_2008$tree_cov <- treecov_values[, 2]



raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2009.tif"
raster_data <- rast(raster_path)

sample_2009_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2009_sf)

samples_2009 <- sampled_points_dem
samples_2009$year <- 2009
samples_2009$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2010.tif"
raster_data <- rast(raster_path)

sample_2010_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2010_sf)

samples_2010 <- sampled_points_dem
samples_2010$year <- 2010
samples_2010$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2011.tif"
raster_data <- rast(raster_path)

sample_2011_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2011_sf)

samples_2011 <- sampled_points_dem
samples_2011$year <- 2011
samples_2011$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2012.tif"
raster_data <- rast(raster_path)

sample_2012_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2012_sf)

samples_2012 <- sampled_points_dem
samples_2012$year <- 2012
samples_2012$tree_cov <- treecov_values[, 2]



raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2013.tif"
raster_data <- rast(raster_path)

sample_2013_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2013_sf)

samples_2013 <- sampled_points_dem
samples_2013$year <- 2013
samples_2013$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2014.tif"
raster_data <- rast(raster_path)

sample_2014_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2014_sf)

samples_2014 <- sampled_points_dem
samples_2014$year <- 2014
samples_2014$tree_cov <- treecov_values[, 2]



raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2015.tif"
raster_data <- rast(raster_path)

sample_2015_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2015_sf)

samples_2015 <- sampled_points_dem
samples_2015$year <- 2015
samples_2015$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2016.tif"
raster_data <- rast(raster_path)

sample_2016_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2016_sf)

samples_2016 <- sampled_points_dem
samples_2016$year <- 2016
samples_2016$tree_cov <- treecov_values[, 2]



raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2017.tif"
raster_data <- rast(raster_path)

sample_2017_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2017_sf)

samples_2017 <- sampled_points_dem
samples_2017$year <- 2017
samples_2017$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2018.tif"
raster_data <- rast(raster_path)

sample_2018_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2018_sf)

samples_2018 <- sampled_points_dem
samples_2018$year <- 2018
samples_2018$tree_cov <- treecov_values[, 2]



raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2019.tif"
raster_data <- rast(raster_path)

sample_2019_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2019_sf)

samples_2019 <- sampled_points_dem
samples_2019$year <- 2019
samples_2019$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2020.tif"
raster_data <- rast(raster_path)

sample_2020_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2020_sf)

samples_2020 <- sampled_points_dem
samples_2020$year <- 2020
samples_2020$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2021.tif"
raster_data <- rast(raster_path)

sample_2021_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2021_sf)

samples_2021 <- sampled_points_dem
samples_2021$year <- 2021
samples_2021$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2022.tif"
raster_data <- rast(raster_path)

sample_2022_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2022_sf)

samples_2022 <- sampled_points_dem
samples_2022$year <- 2022
samples_2022$tree_cov <- treecov_values[, 2]


raster_path <- "~/eo_nas/EO4Alps/level3_predictions/tree_only/treecover2023.tif"
raster_data <- rast(raster_path)

sample_2023_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

treecov_values <- terra::extract(raster_data, sample_2023_sf)

samples_2023 <- sampled_points_dem
samples_2023$year <- 2023
samples_2023$tree_cov <- treecov_values[, 2]

# rowbind all df

# Create a list of all recovery data frames
# Define the years for your recovery data frames
years <- 1986:2023
samples_list <- lapply(years, function(year) {
  # Dynamically get each data frame by constructing its name
  get(paste0("samples_", year))
})

#  Combine all data frames into one using bind_rows
samples_all <- bind_rows(samples_list)

#-------------------------------------------------------------------------------
### extract coniferous share

### extract tree cover per year

###1986


### extract

### 1986
raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1986.tif") 
raster_broadl <-rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1986.tif") 


# Convert recovery_1986 to an sf object if necessary
sample_1986_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

# Extract VPD values
coni_values <- terra::extract(raster_coni, sample_1986_sf)
broad_values <- terra::extract(raster_broadl, sample_1986_sf)


# Add new columns directly without creating treecover_1986
samples_1986 <- sampled_points_dem
samples_1986$year <- 1986                          
samples_1986$coniferous <- coni_values[, 2]        
samples_1986$broadleaved <- broad_values[, 2] 



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1987.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1987.tif")

sample_1987_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_1987_sf)
broad_values <- terra::extract(raster_broadl, sample_1987_sf)

samples_1987 <- sampled_points_dem
samples_1987$year <- 1987
samples_1987$coniferous <- coni_values[, 2]
samples_1987$broadleaved <- broad_values[, 2]


raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1988.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1988.tif")

sample_1988_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_1988_sf)
broad_values <- terra::extract(raster_broadl, sample_1988_sf)

samples_1988 <- sampled_points_dem
samples_1988$year <- 1988
samples_1988$coniferous <- coni_values[, 2]
samples_1988$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1989.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1989.tif")

sample_1989_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_1989_sf)
broad_values <- terra::extract(raster_broadl, sample_1989_sf)

samples_1989 <- sampled_points_dem
samples_1989$year <- 1989
samples_1989$coniferous <- coni_values[, 2]
samples_1989$broadleaved <- broad_values[, 2]




raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1990.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1990.tif")

sample_1990_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_1990_sf)
broad_values <- terra::extract(raster_broadl, sample_1990_sf)

samples_1990 <- sampled_points_dem
samples_1990$year <- 1990
samples_1990$coniferous <- coni_values[, 2]
samples_1990$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1991.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1991.tif")

sample_1991_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_1991_sf)
broad_values <- terra::extract(raster_broadl, sample_1991_sf)

samples_1991 <- sampled_points_dem
samples_1991$year <- 1991
samples_1991$coniferous <- coni_values[, 2]
samples_1991$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1992.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1992.tif")

sample_1992_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_1992_sf)
broad_values <- terra::extract(raster_broadl, sample_1992_sf)

samples_1992 <- sampled_points_dem
samples_1992$year <- 1992
samples_1992$coniferous <- coni_values[, 2]
samples_1992$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1993.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1993.tif")

sample_1993_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_1993_sf)
broad_values <- terra::extract(raster_broadl, sample_1993_sf)

samples_1993 <- sampled_points_dem
samples_1993$year <- 1993
samples_1993$coniferous <- coni_values[, 2]
samples_1993$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1994.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1994.tif")

sample_1994_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_1994_sf)
broad_values <- terra::extract(raster_broadl, sample_1994_sf)

samples_1994 <- sampled_points_dem
samples_1994$year <- 1994
samples_1994$coniferous <- coni_values[, 2]
samples_1994$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1995.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1995.tif")

sample_1995_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_1995_sf)
broad_values <- terra::extract(raster_broadl, sample_1995_sf)

samples_1995 <- sampled_points_dem
samples_1995$year <- 1995
samples_1995$coniferous <- coni_values[, 2]
samples_1995$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1996.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1996.tif")

sample_1996_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_1996_sf)
broad_values <- terra::extract(raster_broadl, sample_1996_sf)

samples_1996 <- sampled_points_dem
samples_1996$year <- 1996
samples_1996$coniferous <- coni_values[, 2]
samples_1996$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1997.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1997.tif")

sample_1997_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_1997_sf)
broad_values <- terra::extract(raster_broadl, sample_1997_sf)

samples_1997 <- sampled_points_dem
samples_1997$year <- 1997
samples_1997$coniferous <- coni_values[, 2]
samples_1997$broadleaved <- broad_values[, 2]


raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1998.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1998.tif")

sample_1998_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_1998_sf)
broad_values <- terra::extract(raster_broadl, sample_1998_sf)

samples_1998 <- sampled_points_dem
samples_1998$year <- 1998
samples_1998$coniferous <- coni_values[, 2]
samples_1998$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1999.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1999.tif")

sample_1999_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_1999_sf)
broad_values <- terra::extract(raster_broadl, sample_1999_sf)

samples_1999 <- sampled_points_dem
samples_1999$year <- 1999
samples_1999$coniferous <- coni_values[, 2]
samples_1999$broadleaved <- broad_values[, 2]


raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2000.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2000.tif")

sample_2000_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2000_sf)
broad_values <- terra::extract(raster_broadl, sample_2000_sf)

samples_2000 <- sampled_points_dem
samples_2000$year <- 2000
samples_2000$coniferous <- coni_values[, 2]
samples_2000$broadleaved <- broad_values[, 2]


raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2001.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2001.tif")

sample_2001_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2001_sf)
broad_values <- terra::extract(raster_broadl, sample_2001_sf)

samples_2001 <- sampled_points_dem
samples_2001$year <- 2001
samples_2001$coniferous <- coni_values[, 2]
samples_2001$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2002.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2002.tif")

sample_2002_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2002_sf)
broad_values <- terra::extract(raster_broadl, sample_2002_sf)

samples_2002 <- sampled_points_dem
samples_2002$year <- 2002
samples_2002$coniferous <- coni_values[, 2]
samples_2002$broadleaved <- broad_values[, 2]




raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2003.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2003.tif")

sample_2003_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2003_sf)
broad_values <- terra::extract(raster_broadl, sample_2003_sf)

samples_2003 <- sampled_points_dem
samples_2003$year <- 2003
samples_2003$coniferous <- coni_values[, 2]
samples_2003$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2004.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2004.tif")

sample_2004_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2004_sf)
broad_values <- terra::extract(raster_broadl, sample_2004_sf)

samples_2004 <- sampled_points_dem
samples_2004$year <- 2004
samples_2004$coniferous <- coni_values[, 2]
samples_2004$broadleaved <- broad_values[, 2]


raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2005.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2005.tif")

sample_2005_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2005_sf)
broad_values <- terra::extract(raster_broadl, sample_2005_sf)

samples_2005 <- sampled_points_dem
samples_2005$year <- 2005
samples_2005$coniferous <- coni_values[, 2]
samples_2005$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2006.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2006.tif")

sample_2006_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2006_sf)
broad_values <- terra::extract(raster_broadl, sample_2006_sf)

samples_2006 <- sampled_points_dem
samples_2006$year <- 2006
samples_2006$coniferous <- coni_values[, 2]
samples_2006$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2007.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2007.tif")

sample_2007_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2007_sf)
broad_values <- terra::extract(raster_broadl, sample_2007_sf)

samples_2007 <- sampled_points_dem
samples_2007$year <- 2007
samples_2007$coniferous <- coni_values[, 2]
samples_2007$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2008.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2008.tif")

sample_2008_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2008_sf)
broad_values <- terra::extract(raster_broadl, sample_2008_sf)

samples_2008 <- sampled_points_dem
samples_2008$year <- 2008
samples_2008$coniferous <- coni_values[, 2]
samples_2008$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2009.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2009.tif")

sample_2009_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2009_sf)
broad_values <- terra::extract(raster_broadl, sample_2009_sf)

samples_2009 <- sampled_points_dem
samples_2009$year <- 2009
samples_2009$coniferous <- coni_values[, 2]
samples_2009$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2010.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2010.tif")

sample_2010_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2010_sf)
broad_values <- terra::extract(raster_broadl, sample_2010_sf)

samples_2010 <- sampled_points_dem
samples_2010$year <- 2010
samples_2010$coniferous <- coni_values[, 2]
samples_2010$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2011.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2011.tif")

sample_2011_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2011_sf)
broad_values <- terra::extract(raster_broadl, sample_2011_sf)

samples_2011 <- sampled_points_dem
samples_2011$year <- 2011
samples_2011$coniferous <- coni_values[, 2]
samples_2011$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2012.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2012.tif")

sample_2012_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2012_sf)
broad_values <- terra::extract(raster_broadl, sample_2012_sf)

samples_2012 <- sampled_points_dem
samples_2012$year <- 2012
samples_2012$coniferous <- coni_values[, 2]
samples_2012$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2013.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2013.tif")

sample_2013_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2013_sf)
broad_values <- terra::extract(raster_broadl, sample_2013_sf)

samples_2013 <- sampled_points_dem
samples_2013$year <- 2013
samples_2013$coniferous <- coni_values[, 2]
samples_2013$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2014.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2014.tif")

sample_2014_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2014_sf)
broad_values <- terra::extract(raster_broadl, sample_2014_sf)

samples_2014 <- sampled_points_dem
samples_2014$year <- 2014
samples_2014$coniferous <- coni_values[, 2]
samples_2014$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2015.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2015.tif")

sample_2015_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2015_sf)
broad_values <- terra::extract(raster_broadl, sample_2015_sf)

samples_2015 <- sampled_points_dem
samples_2015$year <- 2015
samples_2015$coniferous <- coni_values[, 2]
samples_2015$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2016.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2016.tif")

sample_2016_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2016_sf)
broad_values <- terra::extract(raster_broadl, sample_2016_sf)

samples_2016 <- sampled_points_dem
samples_2016$year <- 2016
samples_2016$coniferous <- coni_values[, 2]
samples_2016$broadleaved <- broad_values[, 2]


raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2017.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2017.tif")

sample_2017_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2017_sf)
broad_values <- terra::extract(raster_broadl, sample_2017_sf)

samples_2017 <- sampled_points_dem
samples_2017$year <- 2017
samples_2017$coniferous <- coni_values[, 2]
samples_2017$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2018.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2018.tif")

sample_2018_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2018_sf)
broad_values <- terra::extract(raster_broadl, sample_2018_sf)

samples_2018 <- sampled_points_dem
samples_2018$year <- 2018
samples_2018$coniferous <- coni_values[, 2]
samples_2018$broadleaved <- broad_values[, 2]


raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2019.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2019.tif")

sample_2019_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2019_sf)
broad_values <- terra::extract(raster_broadl, sample_2019_sf)

samples_2019 <- sampled_points_dem
samples_2019$year <- 2019
samples_2019$coniferous <- coni_values[, 2]
samples_2019$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2020.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2020.tif")

sample_2020_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2020_sf)
broad_values <- terra::extract(raster_broadl, sample_2020_sf)

samples_2020 <- sampled_points_dem
samples_2020$year <- 2020
samples_2020$coniferous <- coni_values[, 2]
samples_2020$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2021.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2021.tif")

sample_2021_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2021_sf)
broad_values <- terra::extract(raster_broadl, sample_2021_sf)

samples_2021 <- sampled_points_dem
samples_2021$year <- 2021
samples_2021$coniferous <- coni_values[, 2]
samples_2021$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2022.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2022.tif")

sample_2022_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2022_sf)
broad_values <- terra::extract(raster_broadl, sample_2022_sf)

samples_2022 <- sampled_points_dem
samples_2022$year <- 2022
samples_2022$coniferous <- coni_values[, 2]
samples_2022$broadleaved <- broad_values[, 2]



raster_coni <- rast("~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2023.tif")
raster_broadl <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2023.tif")

sample_2023_sf <- st_as_sf(sampled_points_dem, coords = c("x", "y"), crs = crs(dem))

coni_values <- terra::extract(raster_coni, sample_2023_sf)
broad_values <- terra::extract(raster_broadl, sample_2023_sf)

samples_2023 <- sampled_points_dem
samples_2023$year <- 2023
samples_2023$coniferous <- coni_values[, 2]
samples_2023$broadleaved <- broad_values[, 2]



# rowbind all df

# Create a list of all recovery data frames
# Define the years for your recovery data frames
years <- 1986:2023
samples_list <- lapply(years, function(year) {
  # Dynamically get each data frame by constructing its name
  get(paste0("samples_", year))
})

#  Combine all data frames into one using bind_rows
samples_all <- bind_rows(samples_list)

# Adjust tree_cov values
samples_all$tree_cov[samples_all$tree_cov > 10000] <- 1000
samples_all$tree_cov[samples_all$tree_cov < 0] <- 0

samples_all$coniferous[samples_all$coniferous > 10000] <- 1000
samples_all$coniferous[samples_all$coniferous < 0] <- 0

samples_all$broadleaved[samples_all$broadleaved > 10000] <- 1000
samples_all$broadleaved[samples_all$broadleaved < 0] <- 0


#-------------------------------------------------------------------------------
### add VPD and temp values

###1986
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_1986.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1986.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/1986/mean_temp_1986.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 1986
sample_1986 <- samples_GAM %>% filter(year == 1986)

sample_1986_sf <- st_as_sf(sample_1986, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_1986_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_1986_sf)
#temp_values <- terra::extract(temp_projected, sample_1986_sf)


sample_1986$VPD_absolute <- vpd_values[, 2]
sample_1986$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1986$temp <- temp_values[, 2]


###1987
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_1987.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1987.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/1987/mean_temp_1987.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 1987
sample_1987 <- samples_GAM %>% filter(year == 1987)

sample_1987_sf <- st_as_sf(sample_1987, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_1987_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_1987_sf)
#temp_values <- terra::extract(temp_projected, sample_1987_sf)


sample_1987$VPD_absolute <- vpd_values[, 2]
sample_1987$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1987$temp <- temp_values[, 2]


###1988
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_1988.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1988.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/1988/mean_temp_1988.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 1988
sample_1988 <- samples_GAM %>% filter(year == 1988)

sample_1988_sf <- st_as_sf(sample_1988, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_1988_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_1988_sf)
#temp_values <- terra::extract(temp_projected, sample_1988_sf)


sample_1988$VPD_absolute <- vpd_values[, 2]
sample_1988$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1988$temp <- temp_values[, 2]



###1989
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_1989.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1989.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/1989/mean_temp_1989.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 1989
sample_1989 <- samples_GAM %>% filter(year == 1989)

sample_1989_sf <- st_as_sf(sample_1989, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_1989_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_1989_sf)
#temp_values <- terra::extract(temp_projected, sample_1989_sf)


sample_1989$VPD_absolute <- vpd_values[, 2]
sample_1989$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1989$temp <- temp_values[, 2]


###1990
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_1990.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1990.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/1990/mean_temp_1990.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 1990
sample_1990 <- samples_GAM %>% filter(year == 1990)

sample_1990_sf <- st_as_sf(sample_1990, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_1990_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_1990_sf)
#temp_values <- terra::extract(temp_projected, sample_1990_sf)


sample_1990$VPD_absolute <- vpd_values[, 2]
sample_1990$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1990$temp <- temp_values[, 2]


###1991
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_1991.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1991.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/1991/mean_temp_1991.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 1991
sample_1991 <- samples_GAM %>% filter(year == 1991)

sample_1991_sf <- st_as_sf(sample_1991, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_1991_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_1991_sf)
#temp_values <- terra::extract(temp_projected, sample_1991_sf)


sample_1991$VPD_absolute <- vpd_values[, 2]
sample_1991$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1991$temp <- temp_values[, 2]


###1992
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_1992.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1992.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/1992/mean_temp_1992.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 1992
sample_1992 <- samples_GAM %>% filter(year == 1992)

sample_1992_sf <- st_as_sf(sample_1992, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_1992_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_1992_sf)
#temp_values <- terra::extract(temp_projected, sample_1992_sf)


sample_1992$VPD_absolute <- vpd_values[, 2]
sample_1992$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1992$temp <- temp_values[, 2]



###1993
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_1993.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1993.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/1993/mean_temp_1993.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 1993
sample_1993 <- samples_GAM %>% filter(year == 1993)

sample_1993_sf <- st_as_sf(sample_1993, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_1993_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_1993_sf)
#temp_values <- terra::extract(temp_projected, sample_1993_sf)


sample_1993$VPD_absolute <- vpd_values[, 2]
sample_1993$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1991$temp <- temp_values[, 2]


###1994
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_1994.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1994.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/1994/mean_temp_1994.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 1994
sample_1994 <- samples_GAM %>% filter(year == 1994)

sample_1994_sf <- st_as_sf(sample_1994, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_1994_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_1994_sf)
#temp_values <- terra::extract(temp_projected, sample_1994_sf)


sample_1994$VPD_absolute <- vpd_values[, 2]
sample_1994$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1994$temp <- temp_values[, 2]



###1995
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_1995.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1995.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/1995/mean_temp_1995.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 1995
sample_1995 <- samples_GAM %>% filter(year == 1995)

sample_1995_sf <- st_as_sf(sample_1995, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_1995_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_1995_sf)
#temp_values <- terra::extract(temp_projected, sample_1995_sf)


sample_1995$VPD_absolute <- vpd_values[, 2]
sample_1995$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1991$temp <- temp_values[, 2]


###1996
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_1996.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1996.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/1996/mean_temp_1996.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 1996
sample_1996 <- samples_GAM %>% filter(year == 1996)

sample_1996_sf <- st_as_sf(sample_1996, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_1996_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_1996_sf)
#temp_values <- terra::extract(temp_projected, sample_1996_sf)


sample_1996$VPD_absolute <- vpd_values[, 2]
sample_1996$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1996$temp <- temp_values[, 2]




###1997
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_1997.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1997.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/1997/mean_temp_1997.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 1997
sample_1997 <- samples_GAM %>% filter(year == 1997)

sample_1997_sf <- st_as_sf(sample_1997, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_1997_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_1997_sf)
#temp_values <- terra::extract(temp_projected, sample_1997_sf)


sample_1997$VPD_absolute <- vpd_values[, 2]
sample_1997$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1991$temp <- temp_values[, 2]


###1998
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_1998.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1998.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/1998/mean_temp_1998.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 1998
sample_1998 <- samples_GAM %>% filter(year == 1998)

sample_1998_sf <- st_as_sf(sample_1998, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_1998_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_1998_sf)
#temp_values <- terra::extract(temp_projected, sample_1998_sf)


sample_1998$VPD_absolute <- vpd_values[, 2]
sample_1998$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1998$temp <- temp_values[, 2]




###1999
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_1999.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1999.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/1999/mean_temp_1999.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 1999
sample_1999 <- samples_GAM %>% filter(year == 1999)

sample_1999_sf <- st_as_sf(sample_1999, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_1999_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_1999_sf)
#temp_values <- terra::extract(temp_projected, sample_1999_sf)


sample_1999$VPD_absolute <- vpd_values[, 2]
sample_1999$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1991$temp <- temp_values[, 2]


###2000
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2000.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2000.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2000/mean_temp_2000.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2000
sample_2000 <- samples_GAM %>% filter(year == 2000)

sample_2000_sf <- st_as_sf(sample_2000, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2000_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2000_sf)
#temp_values <- terra::extract(temp_projected, sample_2000_sf)


sample_2000$VPD_absolute <- vpd_values[, 2]
sample_2000$VPD_anomaly <- vpd_ano_values[, 2]
#sample_2000$temp <- temp_values[, 2]



###2001
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2001.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2001.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2001/mean_temp_2001.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2001
sample_2001 <- samples_GAM %>% filter(year == 2001)

sample_2001_sf <- st_as_sf(sample_2001, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2001_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2001_sf)
#temp_values <- terra::extract(temp_projected, sample_2001_sf)


sample_2001$VPD_absolute <- vpd_values[, 2]
sample_2001$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1991$temp <- temp_values[, 2]


###2002
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2002.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2002.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2002/mean_temp_2002.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2002
sample_2002 <- samples_GAM %>% filter(year == 2002)

sample_2002_sf <- st_as_sf(sample_2002, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2002_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2002_sf)
#temp_values <- terra::extract(temp_projected, sample_2002_sf)


sample_2002$VPD_absolute <- vpd_values[, 2]
sample_2002$VPD_anomaly <- vpd_ano_values[, 2]
#sample_2002$temp <- temp_values[, 2]



###2003
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2003.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2003.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2003/mean_temp_2003.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2003
sample_2003 <- samples_GAM %>% filter(year == 2003)

sample_2003_sf <- st_as_sf(sample_2003, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2003_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2003_sf)
#temp_values <- terra::extract(temp_projected, sample_2003_sf)


sample_2003$VPD_absolute <- vpd_values[, 2]
sample_2003$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1991$temp <- temp_values[, 2]


###2004
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2004.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2004.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2004/mean_temp_2004.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2004
sample_2004 <- samples_GAM %>% filter(year == 2004)

sample_2004_sf <- st_as_sf(sample_2004, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2004_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2004_sf)
#temp_values <- terra::extract(temp_projected, sample_2004_sf)


sample_2004$VPD_absolute <- vpd_values[, 2]
sample_2004$VPD_anomaly <- vpd_ano_values[, 2]
#sample_2004$temp <- temp_values[, 2]




###2005
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2005.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2005.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2005/mean_temp_2005.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2005
sample_2005 <- samples_GAM %>% filter(year == 2005)

sample_2005_sf <- st_as_sf(sample_2005, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2005_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2005_sf)
#temp_values <- terra::extract(temp_projected, sample_2005_sf)


sample_2005$VPD_absolute <- vpd_values[, 2]
sample_2005$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1991$temp <- temp_values[, 2]


###2006
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2006.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2006.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2006/mean_temp_2006.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2006
sample_2006 <- samples_GAM %>% filter(year == 2006)

sample_2006_sf <- st_as_sf(sample_2006, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2006_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2006_sf)
#temp_values <- terra::extract(temp_projected, sample_2006_sf)


sample_2006$VPD_absolute <- vpd_values[, 2]
sample_2006$VPD_anomaly <- vpd_ano_values[, 2]
#sample_2006$temp <- temp_values[, 2]



###2007
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2007.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2007.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2007/mean_temp_2007.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2007
sample_2007 <- samples_GAM %>% filter(year == 2007)

sample_2007_sf <- st_as_sf(sample_2007, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2007_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2007_sf)
#temp_values <- terra::extract(temp_projected, sample_2007_sf)


sample_2007$VPD_absolute <- vpd_values[, 2]
sample_2007$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1991$temp <- temp_values[, 2]


###2008
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2008.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2008.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2008/mean_temp_2008.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2008
sample_2008 <- samples_GAM %>% filter(year == 2008)

sample_2008_sf <- st_as_sf(sample_2008, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2008_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2008_sf)
#temp_values <- terra::extract(temp_projected, sample_2008_sf)


sample_2008$VPD_absolute <- vpd_values[, 2]
sample_2008$VPD_anomaly <- vpd_ano_values[, 2]
#sample_2008$temp <- temp_values[, 2]




###2009
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2009.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2009.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2009/mean_temp_2009.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2009
sample_2009 <- samples_GAM %>% filter(year == 2009)

sample_2009_sf <- st_as_sf(sample_2009, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2009_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2009_sf)
#temp_values <- terra::extract(temp_projected, sample_2009_sf)


sample_2009$VPD_absolute <- vpd_values[, 2]
sample_2009$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1991$temp <- temp_values[, 2]


###2010
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2010.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2010.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2010/mean_temp_2010.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2010
sample_2010 <- samples_GAM %>% filter(year == 2010)

sample_2010_sf <- st_as_sf(sample_2010, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2010_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2010_sf)
#temp_values <- terra::extract(temp_projected, sample_2010_sf)


sample_2010$VPD_absolute <- vpd_values[, 2]
sample_2010$VPD_anomaly <- vpd_ano_values[, 2]
#sample_2010$temp <- temp_values[, 2]



###2011
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2011.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2011.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2011/mean_temp_2011.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2011
sample_2011 <- samples_GAM %>% filter(year == 2011)

sample_2011_sf <- st_as_sf(sample_2011, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2011_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2011_sf)
#temp_values <- terra::extract(temp_projected, sample_2011_sf)


sample_2011$VPD_absolute <- vpd_values[, 2]
sample_2011$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1991$temp <- temp_values[, 2]


###2012
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2012.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2012.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2012/mean_temp_2012.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2012
sample_2012 <- samples_GAM %>% filter(year == 2012)

sample_2012_sf <- st_as_sf(sample_2012, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2012_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2012_sf)
#temp_values <- terra::extract(temp_projected, sample_2012_sf)


sample_2012$VPD_absolute <- vpd_values[, 2]
sample_2012$VPD_anomaly <- vpd_ano_values[, 2]
#sample_2012$temp <- temp_values[, 2]



###2013
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2013.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2013.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2013/mean_temp_2013.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2013
sample_2013 <- samples_GAM %>% filter(year == 2013)

sample_2013_sf <- st_as_sf(sample_2013, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2013_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2013_sf)
#temp_values <- terra::extract(temp_projected, sample_2013_sf)


sample_2013$VPD_absolute <- vpd_values[, 2]
sample_2013$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1991$temp <- temp_values[, 2]


###2014
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2014.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2014.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2014/mean_temp_2014.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2014
sample_2014 <- samples_GAM %>% filter(year == 2014)

sample_2014_sf <- st_as_sf(sample_2014, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2014_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2014_sf)
#temp_values <- terra::extract(temp_projected, sample_2014_sf)


sample_2014$VPD_absolute <- vpd_values[, 2]
sample_2014$VPD_anomaly <- vpd_ano_values[, 2]
#sample_2014$temp <- temp_values[, 2]




###2015
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2015.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2015.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2015/mean_temp_2015.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2015
sample_2015 <- samples_GAM %>% filter(year == 2015)

sample_2015_sf <- st_as_sf(sample_2015, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2015_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2015_sf)
#temp_values <- terra::extract(temp_projected, sample_2015_sf)


sample_2015$VPD_absolute <- vpd_values[, 2]
sample_2015$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1991$temp <- temp_values[, 2]


###2016
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2016.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2016.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2016/mean_temp_2016.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2016
sample_2016 <- samples_GAM %>% filter(year == 2016)

sample_2016_sf <- st_as_sf(sample_2016, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2016_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2016_sf)
#temp_values <- terra::extract(temp_projected, sample_2016_sf)


sample_2016$VPD_absolute <- vpd_values[, 2]
sample_2016$VPD_anomaly <- vpd_ano_values[, 2]
#sample_2016$temp <- temp_values[, 2]



###2017
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2017.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2017.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2017/mean_temp_2017.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2017
sample_2017 <- samples_GAM %>% filter(year == 2017)

sample_2017_sf <- st_as_sf(sample_2017, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2017_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2017_sf)
#temp_values <- terra::extract(temp_projected, sample_2017_sf)


sample_2017$VPD_absolute <- vpd_values[, 2]
sample_2017$VPD_anomaly <- vpd_ano_values[, 2]
#sample_1991$temp <- temp_values[, 2]


###2018
VPD <- rast("~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_2018.tif")
VPD_ano <- rast("~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2018.tif")
#temp <- rast("~/eo_nas/EO4Alps/climate_data/temperature/2018/mean_temp_2018.tif")


# Define the target CRS (LAEA - EPSG:3035)
target_crs <- "EPSG:3035"

# Reproject the raster to EPSG:3035
VPD_projected <- project(VPD, target_crs)
VPD_ano_projected <- project(VPD_ano, target_crs)
#temp_projected <- project(temp, target_crs)

# Filter the dataframe for the year 2018
sample_2018 <- samples_GAM %>% filter(year == 2018)

sample_2018_sf <- st_as_sf(sample_2018, coords = c("x", "y"), crs = crs(VPD_projected))

vpd_values <- terra::extract(VPD_projected, sample_2018_sf)
vpd_ano_values <- terra::extract(VPD_ano_projected, sample_2018_sf)
#temp_values <- terra::extract(temp_projected, sample_2018_sf)


sample_2018$VPD_absolute <- vpd_values[, 2]
sample_2018$VPD_anomaly <- vpd_ano_values[, 2]
#sample_2018$temp <- temp_values[, 2]


# filter 2019,- 2023
# Subset the data frame to include only rows where year is 2019, 2020, 2021, 2022, or 2023
samples_2019 <- samples_GAM %>% filter(year %in% c(2019, 2020, 2021, 2022, 2023))

# Step 2: Add a VPD column to GEDI_GAM_filt if it doesn't already have one
samples_2019$VPD_absolute <- NA  # Assign NA or any default value
samples_2019$VPD_anomaly <- NA


# Define the years for your recovery data frames
years <- 1986:2018

# Step 1: Create a list of all recovery data frames
samples_list <- lapply(years, function(year) {
  # Dynamically get each data frame by constructing its name
  get(paste0("sample_", year))
})

# Step 2: Add GEDI_GAM_filt to the list of data frames
samples_list <- c(samples_list, list(samples_2019))

# Step 3: Combine all data frames into one using bind_rows
samples_VPD <- bind_rows(samples_list)






### recompute tree_cov
# Recompute tree_cov as the sum of coniferous and broadleaved
samples_all$tree_cov <- samples_all$coniferous + samples_all$broadleaved


# Extract x and y coordinates from geometry
samples_all$x <- st_coordinates(samples_all)[, 1]  # X-coordinates
samples_all$y <- st_coordinates(samples_all)[, 2]  # Y-coordinates

# Drop the geometry column
samples_all <- st_drop_geometry(samples_all)




#------------------------------------------------------------------------------
### GAM fitting
#------------------------------------------------------------------------------

# Define a function to fit models per ID
fit_tree_cover <- function(data) {
  # Split data into two parts: one for year > yod and one for year <= yod
  data_after_yod <- data %>% filter(year > yod)
  data_before_yod <- data %>% filter(year <= yod)
  
  # Check if there are enough years to fit a GAM (let’s assume at least 3 years)
  if (nrow(data_after_yod) >= 5) {
    # Fit a GAM to smooth out intra-year variations for years > yod, with limited k to avoid issues
    model <- gam(tree_cov ~ s(year, k = min(5, nrow(data_after_yod) - 1)), data = data_after_yod)
  } else {
    # If too few years, fit a linear model instead
    model <- lm(tree_cov ~ year, data = data_after_yod)
  }
  
  # Predict the smoothed/adjusted tree cover values for years > yod
  data_after_yod$smoothed_tree_cover <- predict(model, newdata = data_after_yod)
  
  # For years <= yod, keep the original tree_cover values as smoothed_tree_cover
  data_before_yod <- data_before_yod %>%
    mutate(smoothed_tree_cover = tree_cov)
  
  # Combine the two subsets back together
  data_combined <- bind_rows(data_before_yod, data_after_yod) %>%
    arrange(year)  # Ensure rows are in chronological order
  
  return(data_combined)
}


### use all cores
plan(multisession, workers = 24)


# Apply the function to each ID
samples_GAM <- sampled_points %>%
  group_by(ID) %>%
  group_modify(~ fit_tree_cover(.x)) %>%
  ungroup()

plan(sequential)

write.csv(samples_VPD, "~/eo_nas/EO4Alps/00_analysis/_recovery/random_sample.csv", row.names = FALSE)


### convert GAM-fitted df to sf
# Convert to sf object, specifying the coordinate columns and CRS
GEDI_GAM_sf <- st_as_sf(samples_GAM, coords = c("long", "lat"), crs = 4326)  # CRS 4326 for WGS84



### add temp
### extract temp
# Define the path to your parent folder
parent_folder <- "~/eo_nas/EO4Alps/climate_data/temp"

# List all the folders (1986 - 2018) within the parent folder
folders <- list.dirs(parent_folder, recursive = FALSE)

# Loop over each folder
for (folder in folders) {
  
  # Check if a mean_*.tif file already exists in the folder
  if (length(list.files(folder, pattern = "^mean_.*\\.tif$", full.names = TRUE)) > 0) {
    message(paste("Skipping folder:", folder, "- mean raster already exists."))
    next  # Skip this folder and move to the next
  }
  
  # Extract the year from the folder name to use in the output filename
  folder_name <- basename(folder)
  
  # List all raster files for months 06, 07, and 08
  rasters <- list.files(folder, pattern = "^CHELSA_tas_0(6|7|8)_", full.names = TRUE)
  
  # Ensure there are exactly 3 rasters for the calculation
  if (length(rasters) < 3) {
    warning(paste("Folder:", folder, "does not contain all required rasters (06, 07, 08). Skipping."))
    next  # Skip this folder and move to the next
  }
  
  # Read the rasters
  r1 <- rast(rasters[1])
  r2 <- rast(rasters[2])
  r3 <- rast(rasters[3])
  
  # Calculate the mean raster
  mean_raster <- mean(c(r1, r2, r3))
  
  # Define the output file name
  output_name <- file.path(folder, paste0("mean_temp_", folder_name, ".tif"))
  
  # Save the mean raster
  writeRaster(mean_raster, output_name, overwrite = TRUE)
  message(paste("Saved mean raster to:", output_name))
}



### convert in to LAEA
# Define the path to your parent folder and the output "LAEA" folder
parent_folder <- "~/eo_nas/EO4Alps/climate_data/temp"
output_folder <- file.path(parent_folder, "LAEA")

# Create the "LAEA" folder if it does not exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# List all mean rasters in each folder
mean_rasters <- list.files(parent_folder, pattern = "^mean_temp_", recursive = TRUE, full.names = TRUE)

# Loop over each mean raster
for (raster_file in mean_rasters) {
  
  # Read the mean raster
  raster <- rast(raster_file)
  
  # Reproject to LAEA (EPSG:3035)
  raster_laea <- project(raster, "EPSG:3035")
  
  # Create the new file path in the "LAEA" folder with the same name as before
  output_name <- file.path(output_folder, basename(raster_file))
  
  # Save the reprojected raster
  writeRaster(raster_laea, output_name, overwrite = TRUE)
}

print("All mean rasters transformed to LAEA and saved in the 'LAEA' folder.")


### crop mean temp rasters
library(terra)

# Define the path to your parent folder
parent_folder <- "~/eo_nas/EO4Alps/climate_data/temp"

# Define the path to the shapefile
shapefile_path <- "~/eo_nas/EO4Alps/gis/Alpine_Convention_Perimeter_2018_v2/Alpine_Convention_Perimeter_2018_v2.shp"

# Load the shapefile
shape <- vect(shapefile_path)

# List all mean_*.tif files in all subfolders
mean_tif_files <- list.files(parent_folder, pattern = "^mean_.*\\.tif$", full.names = TRUE, recursive = TRUE)

# Check if any mean_ files are found
if (length(mean_tif_files) == 0) {
  stop("No 'mean_' .tif files found in the directory.")
}

# Create an output folder for cropped rasters
output_folder <- file.path(parent_folder, "temp_clip")
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Loop through each mean_*.tif file
for (tif_file in mean_tif_files) {
  # Load the raster
  raster <- rast(tif_file)
  
  # Align CRS of shapefile and raster if necessary
  if (!same.crs(raster, shape)) {
    shape <- project(shape, crs(raster))  # Reproject the shapefile to match raster CRS
  }
  
  # Check for overlapping extents
  if (is.null(intersect(ext(raster), ext(shape)))) {
    warning(paste("Extents do not overlap for:", tif_file, "- Skipping."))
    next  # Skip this file if extents do not overlap
  }
  
  # Crop the raster to the shapefile extent
  cropped_raster <- crop(raster, shape)
  
  # Define output file path with `_clip` suffix
  original_name <- tools::file_path_sans_ext(basename(tif_file))  # Get the base name without extension
  output_file <- file.path(output_folder, paste0(original_name, "_clip.tif"))
  
  # Save the cropped raster
  writeRaster(cropped_raster, output_file, overwrite = TRUE)
  
  # Print progress
  message(paste("Cropped and saved:", output_file))
}


### scale temp raster
# Define the path to the folder with cropped rasters
cropped_folder <- "~/eo_nas/EO4Alps/climate_data/temp/temp_clip"

# Create an output folder for adjusted rasters
adjusted_folder <- file.path(cropped_folder, "adjusted_rasters")
if (!dir.exists(adjusted_folder)) {
  dir.create(adjusted_folder)
}

# List all cropped rasters (e.g., *_clip.tif files)
cropped_rasters <- list.files(cropped_folder, pattern = "_clip\\.tif$", full.names = TRUE)

# Loop through each cropped raster
for (raster_file in cropped_rasters) {
  # Load the cropped raster
  raster <- rast(raster_file)
  
  # Multiply the raster by 0.1 and add -273.15
  adjusted_raster <- (raster * 0.1) + -273.15
  
  # Define output file path with `_adjusted` suffix
  original_name <- tools::file_path_sans_ext(basename(raster_file))  # Get the base name without extension
  output_file <- file.path(adjusted_folder, paste0(original_name, "_adjusted.tif"))
  
  # Save the adjusted raster
  writeRaster(adjusted_raster, output_file, overwrite = TRUE)
  
  # Print progress
  message(paste("Processed and saved adjusted raster:", output_file))
}


### project to LAEA
library(terra)

# Define the folder with adjusted rasters
adjusted_folder <- "~/eo_nas/EO4Alps/climate_data/temp/temp_clip/adjusted_rasters"

# Create a new folder for projected rasters
projected_folder <- file.path("~/eo_nas/EO4Alps/climate_data/temp", "temp_LAEA")
if (!dir.exists(projected_folder)) {
  dir.create(projected_folder)
}

# List all adjusted rasters
adjusted_rasters <- list.files(adjusted_folder, pattern = "_adjusted\\.tif$", full.names = TRUE)

# Define the target CRS (LAEA EPSG 3035)
target_crs <- "EPSG:3035"

# Loop through each adjusted raster
for (raster_file in adjusted_rasters) {
  # Load the raster
  raster <- rast(raster_file)
  
  # Project the raster to LAEA EPSG 3035
  projected_raster <- project(raster, target_crs)
  
  # Define the output file path with `_laea` suffix
  original_name <- tools::file_path_sans_ext(basename(raster_file))  # Get the base name without extension
  output_file <- file.path(projected_folder, paste0(original_name, "_laea.tif"))
  
  # Save the projected raster
  writeRaster(projected_raster, output_file, overwrite = TRUE)
  
  # Print progress
  message(paste("Projected and saved:", output_file))
}


### estract temp
# 1986
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_1986_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_1986 <- severity %>% filter(year == 1986)
sample_1986_sf <- st_as_sf(sample_1986, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_1986_sf)
sample_1986$temp <- temp_values[, 2]


# 1987
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_1987_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_1987 <- severity %>% filter(year == 1987)
sample_1987_sf <- st_as_sf(sample_1987, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_1987_sf)
sample_1987$temp <- temp_values[, 2]


# 1988
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_1988_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_1988 <- severity %>% filter(year == 1988)
sample_1988_sf <- st_as_sf(sample_1988, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_1988_sf)
sample_1988$temp <- temp_values[, 2]


# 1989
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_1989_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_1989 <- severity %>% filter(year == 1989)
sample_1989_sf <- st_as_sf(sample_1989, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_1989_sf)
sample_1989$temp <- temp_values[, 2]


# 1990
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_1990_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_1990 <- severity %>% filter(year == 1990)
sample_1990_sf <- st_as_sf(sample_1990, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_1990_sf)
sample_1990$temp <- temp_values[, 2]


# 1991
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_1991_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_1991 <- severity %>% filter(year == 1991)
sample_1991_sf <- st_as_sf(sample_1991, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_1991_sf)
sample_1991$temp <- temp_values[, 2]



# 1992
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_1992_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_1992 <- severity %>% filter(year == 1992)
sample_1992_sf <- st_as_sf(sample_1992, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_1992_sf)
sample_1992$temp <- temp_values[, 2]


# 1993
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_1993_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_1993 <- severity %>% filter(year == 1993)
sample_1993_sf <- st_as_sf(sample_1993, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_1993_sf)
sample_1993$temp <- temp_values[, 2]




# 1994
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_1994_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_1994 <- severity %>% filter(year == 1994)
sample_1994_sf <- st_as_sf(sample_1994, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_1994_sf)
sample_1994$temp <- temp_values[, 2]


# 1995
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_1995_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_1995 <- severity %>% filter(year == 1995)
sample_1995_sf <- st_as_sf(sample_1995, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_1995_sf)
sample_1995$temp <- temp_values[, 2]



# 1996
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_1996_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_1996 <- severity %>% filter(year == 1996)
sample_1996_sf <- st_as_sf(sample_1996, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_1996_sf)
sample_1996$temp <- temp_values[, 2]


# 1997
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_1997_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_1997 <- severity %>% filter(year == 1997)
sample_1997_sf <- st_as_sf(sample_1997, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_1997_sf)
sample_1997$temp <- temp_values[, 2]



# 1998
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_1998_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_1998 <- severity %>% filter(year == 1998)
sample_1998_sf <- st_as_sf(sample_1998, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_1998_sf)
sample_1998$temp <- temp_values[, 2]


# 1999
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_1999_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_1999 <- severity %>% filter(year == 1999)
sample_1999_sf <- st_as_sf(sample_1999, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_1999_sf)
sample_1999$temp <- temp_values[, 2]


# 2000
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2000_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2000 <- severity %>% filter(year == 2000)
sample_2000_sf <- st_as_sf(sample_2000, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2000_sf)
sample_2000$temp <- temp_values[, 2]


# 2001
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2001_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2001 <- severity %>% filter(year == 2001)
sample_2001_sf <- st_as_sf(sample_2001, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2001_sf)
sample_2001$temp <- temp_values[, 2]



# 2002
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2002_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2002 <- severity %>% filter(year == 2002)
sample_2002_sf <- st_as_sf(sample_2002, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2002_sf)
sample_2002$temp <- temp_values[, 2]


# 2003
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2003_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2003 <- severity %>% filter(year == 2003)
sample_2003_sf <- st_as_sf(sample_2003, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2003_sf)
sample_2003$temp <- temp_values[, 2]




# 2004
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2004_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2004 <- severity %>% filter(year == 2004)
sample_2004_sf <- st_as_sf(sample_2004, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2004_sf)
sample_2004$temp <- temp_values[, 2]


# 2005
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2005_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2005 <- severity %>% filter(year == 2005)
sample_2005_sf <- st_as_sf(sample_2005, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2005_sf)
sample_2005$temp <- temp_values[, 2]



# 2006
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2006_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2006 <- severity %>% filter(year == 2006)
sample_2006_sf <- st_as_sf(sample_2006, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2006_sf)
sample_2006$temp <- temp_values[, 2]


# 2007
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2007_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2007 <- severity %>% filter(year == 2007)
sample_2007_sf <- st_as_sf(sample_2007, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2007_sf)
sample_2007$temp <- temp_values[, 2]




# 2008
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2008_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2008 <- severity %>% filter(year == 2008)
sample_2008_sf <- st_as_sf(sample_2008, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2008_sf)
sample_2008$temp <- temp_values[, 2]


# 2009
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2009_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2009 <- severity %>% filter(year == 2009)
sample_2009_sf <- st_as_sf(sample_2009, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2009_sf)
sample_2009$temp <- temp_values[, 2]



# 2010
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2010_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2010 <- severity %>% filter(year == 2010)
sample_2010_sf <- st_as_sf(sample_2010, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2010_sf)
sample_2010$temp <- temp_values[, 2]


# 2011
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2011_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2011 <- severity %>% filter(year == 2011)
sample_2011_sf <- st_as_sf(sample_2011, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2011_sf)
sample_2011$temp <- temp_values[, 2]



# 2012
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2012_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2012 <- severity %>% filter(year == 2012)
sample_2012_sf <- st_as_sf(sample_2012, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2012_sf)
sample_2012$temp <- temp_values[, 2]


# 2013
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2013_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2013 <- severity %>% filter(year == 2013)
sample_2013_sf <- st_as_sf(sample_2013, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2013_sf)
sample_2013$temp <- temp_values[, 2]


# 2014
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2014_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2014 <- severity %>% filter(year == 2014)
sample_2014_sf <- st_as_sf(sample_2014, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2014_sf)
sample_2014$temp <- temp_values[, 2]


# 2015
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2015_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2015 <- severity %>% filter(year == 2015)
sample_2015_sf <- st_as_sf(sample_2015, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2015_sf)
sample_2015$temp <- temp_values[, 2]



# 2016
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2016_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2016 <- severity %>% filter(year == 2016)
sample_2016_sf <- st_as_sf(sample_2016, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2016_sf)
sample_2016$temp <- temp_values[, 2]


# 2017
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2017_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2017 <- severity %>% filter(year == 2017)
sample_2017_sf <- st_as_sf(sample_2017, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2017_sf)
sample_2017$temp <- temp_values[, 2]



# 2018
temp <- rast("~/eo_nas/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_2018_clip_adjusted_laea.tif")

# Filter the dataframe for the year 2018
sample_2018 <- severity %>% filter(year == 2018)
sample_2018_sf <- st_as_sf(sample_2018, coords = c("x", "y"), crs = crs(temp))
temp_values <- terra::extract(temp, sample_2018_sf)
sample_2018$temp <- temp_values[, 2]


# Subset the data frame to include only rows where year is 2019, 2020, 2021, 2022, or 2023
severity_filt <- severity %>% filter(year %in% c(2019, 2020, 2021, 2022, 2023))

# Step 2: Add a VPD column to GEDI_GAM_filt if it doesn't already have one
severity_filt$temp <- NA  # Assign NA or any default value


# Define the years for your recovery data frames
years <- 1986:2018

# Step 1: Create a list of all recovery data frames
sample_list <- lapply(years, function(year) {
  # Dynamically get each data frame by constructing its name
  get(paste0("sample_", year))
})

# Step 2: Add GEDI_GAM_filt to the list of data frames
sample_list <- c(sample_list, list(severity_filt))

# Step 3: Combine all data frames into one using bind_rows
samples_temp <- bind_rows(sample_list)




