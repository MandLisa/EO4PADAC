library(raster)
library(tidyverse)
library(sf)
library(terra)

# Load necessary libraries
library(raster)
library(dplyr)

### load GAM df
GEDI_GAM <- read_csv("eo_nas/EO4Alps/00_analysis/_recovery/GEDI_GAM.csv")

# Define the file path to the raster
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_summer_averages/VPD_WGS84/VPD_1986.tif"
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1986"


# Load the raster
raster_data <- raster(raster_path)

# Filter the dataframe for the year 1986
recovery_1986 <- GEDI_GAM %>% filter(year == 1986)

# Ensure that coordinates are in the same CRS as the raster
if (!is.null(crs(raster_data))) {
  recovery_1986 <- st_as_sf(recovery_1986, coords = c("x", "y"), crs = crs(raster_data))
  recovery_1986 <- st_transform(recovery_1986, crs(raster_data))
  coords <- st_coordinates(recovery_1986)
} else {
  coords <- as.matrix(recovery_1986[, c("x", "y")])
}


# Extract VPD values at the x and y coordinates
vpd_values <- extract(raster_data, recovery_1986[, c("x", "y")])

# Add the extracted values as a new column to the dataframe
recovery_1986$VPD_1987 <- vpd_values


###1986
library(terra)
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1986.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986

recovery_1986 <- GEDI_GAM %>% filter(year == 1986)


# Convert recovery_1986 to an sf object if necessary
recovery_1986_sf <- st_as_sf(recovery_1986, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_1986_sf)

# Add extracted values back to the dataframe
recovery_1986$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results

###1987
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1987.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster

# Filter the dataframe for the year 1986
recovery_1987 <- GEDI_GAM %>% filter(year == 1987)

# Convert recovery_1986 to an sf object if necessary
recovery_1987_sf <- st_as_sf(recovery_1987, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_1987_sf)

# Add extracted values back to the dataframe
recovery_1987$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results

###1988
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1988.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_1988 <- GEDI_GAM %>% filter(year == 1988)

# Convert recovery_1986 to an sf object if necessary
recovery_1988_sf <- st_as_sf(recovery_1988, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_1988_sf)

# Add extracted values back to the dataframe
recovery_1988$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results


###1989
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1989.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_1989 <- GEDI_GAM %>% filter(year == 1989)

# Convert recovery_1986 to an sf object if necessary
recovery_1989_sf <- st_as_sf(recovery_1989, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_1989_sf)

# Add extracted values back to the dataframe
recovery_1989$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results


###1990
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1990.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_1990 <- GEDI_GAM %>% filter(year == 1990)

# Convert recovery_1986 to an sf object if necessary
recovery_1990_sf <- st_as_sf(recovery_1990, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_1990_sf)

# Add extracted values back to the dataframe
recovery_1990$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results


###1991
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1991.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_1991 <- GEDI_GAM %>% filter(year == 1991)

# Convert recovery_1986 to an sf object if necessary
recovery_1991_sf <- st_as_sf(recovery_1991, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_1991_sf)

# Add extracted values back to the dataframe
recovery_1991$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results


###1992
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1992.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_1992 <- GEDI_GAM %>% filter(year == 1992)

# Convert recovery_1986 to an sf object if necessary
recovery_1992_sf <- st_as_sf(recovery_1992, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_1992_sf)

# Add extracted values back to the dataframe
recovery_1992$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results


###1993
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1993.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_1993 <- GEDI_GAM %>% filter(year == 1993)

# Convert recovery_1986 to an sf object if necessary
recovery_1993_sf <- st_as_sf(recovery_1993, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_1993_sf)

# Add extracted values back to the dataframe
recovery_1993$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results


###1994
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1994.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_1994 <- GEDI_GAM %>% filter(year == 1994)

# Convert recovery_1986 to an sf object if necessary
recovery_1994_sf <- st_as_sf(recovery_1994, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_1994_sf)

# Add extracted values back to the dataframe
recovery_1994$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results


###1995
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1995.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_1995 <- GEDI_GAM %>% filter(year == 1995)

# Convert recovery_1986 to an sf object if necessary
recovery_1995_sf <- st_as_sf(recovery_1995, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_1995_sf)

# Add extracted values back to the dataframe
recovery_1995$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results


###1996
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1996.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_1996 <- GEDI_GAM %>% filter(year == 1996)

# Convert recovery_1986 to an sf object if necessary
recovery_1996_sf <- st_as_sf(recovery_1996, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_1996_sf)

# Add extracted values back to the dataframe
recovery_1996$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results


###1997
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1997.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_1997 <- GEDI_GAM %>% filter(year == 1997)

# Convert recovery_1986 to an sf object if necessary
recovery_1997_sf <- st_as_sf(recovery_1997, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_1997_sf)

# Add extracted values back to the dataframe
recovery_1997$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results


###1998
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1998.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_1998 <- GEDI_GAM %>% filter(year == 1998)

# Convert recovery_1986 to an sf object if necessary
recovery_1998_sf <- st_as_sf(recovery_1998, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_1998_sf)

# Add extracted values back to the dataframe
recovery_1998$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results



###1999
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_1999.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_1999 <- GEDI_GAM %>% filter(year == 1999)

# Convert recovery_1986 to an sf object if necessary
recovery_1999_sf <- st_as_sf(recovery_1999, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_1999_sf)

# Add extracted values back to the dataframe
recovery_1999$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results



###2000
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2000.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2000 <- GEDI_GAM %>% filter(year == 2000)

# Convert recovery_1986 to an sf object if necessary
recovery_2000_sf <- st_as_sf(recovery_2000, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2000_sf)

# Add extracted values back to the dataframe
recovery_2000$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results



###2001
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2001.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2001 <- GEDI_GAM %>% filter(year == 2001)

# Convert recovery_1986 to an sf object if necessary
recovery_2001_sf <- st_as_sf(recovery_2001, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2001_sf)

# Add extracted values back to the dataframe
recovery_2001$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results




###2002
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2002.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2002 <- GEDI_GAM %>% filter(year == 2002)

# Convert recovery_1986 to an sf object if necessary
recovery_2002_sf <- st_as_sf(recovery_2002, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2002_sf)

# Add extracted values back to the dataframe
recovery_2002$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results




###2003
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2003.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2003 <- GEDI_GAM %>% filter(year == 2003)

# Convert recovery_1986 to an sf object if necessary
recovery_2003_sf <- st_as_sf(recovery_2003, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2003_sf)

# Add extracted values back to the dataframe
recovery_2003$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract results




###2004
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2004.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2004 <- GEDI_GAM %>% filter(year == 2004)

# Convert recovery_1986 to an sf object if necessary
recovery_2004_sf <- st_as_sf(recovery_2004, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2004_sf)

# Add extracted values back to the dataframe
recovery_2004$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract resultsts




###2005
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2005.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2005 <- GEDI_GAM %>% filter(year == 2005)

# Convert recovery_1986 to an sf object if necessary
recovery_2005_sf <- st_as_sf(recovery_2005, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2005_sf)

# Add extracted values back to the dataframe
recovery_2005$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract resultsts




###2006
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2006.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2006 <- GEDI_GAM %>% filter(year == 2006)

# Convert recovery_1986 to an sf object if necessary
recovery_2006_sf <- st_as_sf(recovery_2006, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2006_sf)

# Add extracted values back to the dataframe
recovery_2006$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract resultsts




###2007
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2007.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2007 <- GEDI_GAM %>% filter(year == 2007)

# Convert recovery_1986 to an sf object if necessary
recovery_2007_sf <- st_as_sf(recovery_2007, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2007_sf)

# Add extracted values back to the dataframe
recovery_2007$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract resultsts




###2008
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2008.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2008 <- GEDI_GAM %>% filter(year == 2008)

# Convert recovery_1986 to an sf object if necessary
recovery_2008_sf <- st_as_sf(recovery_2008, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2008_sf)

# Add extracted values back to the dataframe
recovery_2008$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract resultsts



###2009
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2009.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2009 <- GEDI_GAM %>% filter(year == 2009)

# Convert recovery_1986 to an sf object if necessary
recovery_2009_sf <- st_as_sf(recovery_2009, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2009_sf)

# Add extracted values back to the dataframe
recovery_2009$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract resultsts




###2010
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2010.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2010 <- GEDI_GAM %>% filter(year == 2010)

# Convert recovery_1986 to an sf object if necessary
recovery_2010_sf <- st_as_sf(recovery_2010, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2010_sf)

# Add extracted values back to the dataframe
recovery_2010$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract resultsts




###2011
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2011.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2011 <- GEDI_GAM %>% filter(year == 2011)

# Convert recovery_1986 to an sf object if necessary
recovery_2011_sf <- st_as_sf(recovery_2011, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2011_sf)

# Add extracted values back to the dataframe
recovery_2011$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract resultsts





###2012
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2012.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2012 <- GEDI_GAM %>% filter(year == 2012)

# Convert recovery_1986 to an sf object if necessary
recovery_2012_sf <- st_as_sf(recovery_2012, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2012_sf)

# Add extracted values back to the dataframe
recovery_2012$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract resultstss





###2013
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2013.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2013 <- GEDI_GAM %>% filter(year == 2013)

# Convert recovery_1986 to an sf object if necessary
recovery_2013_sf <- st_as_sf(recovery_2013, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2013_sf)

# Add extracted values back to the dataframe
recovery_2013$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract resultstss



###2014
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2014.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2014 <- GEDI_GAM %>% filter(year == 2014)

# Convert recovery_1986 to an sf object if necessary
recovery_2014_sf <- st_as_sf(recovery_2014, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2014_sf)

# Add extracted values back to the dataframe
recovery_2014$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract resultstss




###2015
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2015.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2015 <- GEDI_GAM %>% filter(year == 2015)

# Convert recovery_1986 to an sf object if necessary
recovery_2015_sf <- st_as_sf(recovery_2015, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2015_sf)

# Add extracted values back to the dataframe
recovery_2015$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract resultstss



###2016
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2016.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2016 <- GEDI_GAM %>% filter(year == 2016)

# Convert recovery_1986 to an sf object if necessary
recovery_2016_sf <- st_as_sf(recovery_2016, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2016_sf)

# Add extracted values back to the dataframe
recovery_2016$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract resultstss




###2017
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2017.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2017 <- GEDI_GAM %>% filter(year == 2017)

# Convert recovery_1986 to an sf object if necessary
recovery_2017_sf <- st_as_sf(recovery_2017, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2017_sf)

# Add extracted values back to the dataframe
recovery_2017$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract resultstss





###2018
raster_path <- "~/eo_nas/EO4Alps/climate_data/VPD_clip/anomalies/LAEA/mod/VPD_anomaly_2018.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986
recovery_2018 <- GEDI_GAM %>% filter(year == 2018)

# Convert recovery_1986 to an sf object if necessary
recovery_2018_sf <- st_as_sf(recovery_2018, coords = c("x", "y"), crs = crs(raster_data))

# Extract VPD values
vpd_values <- terra::extract(raster_data, recovery_2018_sf)

# Add extracted values back to the dataframe
recovery_2018$VPD <- vpd_values[, 2]  # Adjust based on column indexing of extract resultstss






# Subset the data frame to include only rows where year is 2019, 2020, 2021, 2022, or 2023
GEDI_GAM_filt <- GEDI_GAM %>% filter(year %in% c(2019, 2020, 2021, 2022, 2023))

# Step 2: Add a VPD column to GEDI_GAM_filt if it doesn't already have one
GEDI_GAM_filt$VPD <- NA  # Assign NA or any default value


# Define the years for your recovery data frames
years <- 1986:2018

# Step 1: Create a list of all recovery data frames
recovery_list <- lapply(years, function(year) {
  # Dynamically get each data frame by constructing its name
  get(paste0("recovery_", year))
})

# Step 2: Add GEDI_GAM_filt to the list of data frames
recovery_list <- c(recovery_list, list(GEDI_GAM_filt))

# Step 3: Combine all data frames into one using bind_rows
GEDI_GAM_VPD <- bind_rows(recovery_list)




### write
write.csv(GEDI_GAM_VPD, "~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_GAM_VPD.csv", row.names=FALSE)



### add recovery metrics

# set values > 10000 to 10,000 and < 0 to 0
# Set bounds for smoothed_tree_cover column
GEDI_GAM_VPD <- GEDI_GAM_VPD %>%
  mutate(smoothed_tree_cover = pmin(pmax(smoothed_tree_cover, 0), 10000))



# Function to calculate time to min tree share
time_to_min <- function(df_trees) {
  
  # Find the minimum tree share value and the year when the minimum is reached within the 5-year window after the year of disturbance for each time series (ID)
  min_share_in_window <- df_trees %>%
    group_by(ID, yod) %>%
    filter(year >= yod, year <= yod + 3) %>%
    slice(which.min(smoothed_tree_cover)) %>%
    ungroup()
  
  # Calculate the duration until the minimum is reached for each time series (ID)
  result <- min_share_in_window %>%
    mutate(time_to_min = year - yod) %>%
    select(ID, yod, min_year = year, time_to_min, min_tree_share = smoothed_tree_cover) %>%
    distinct()
  
  # Convert share to proportion
  result <- result %>%
    mutate(min_tree_share = min_tree_share / 100)
  
  # Join the result back with the original data to keep all columns
  result_full <- df_trees %>%
    left_join(result, by = c("ID", "yod"))
  
  return(result_full)
}

# Call the function and store the result in a new data frame
t_min <- time_to_min(GEDI_GAM_VPD)


# Calculate the average tree share in a 5-year time window before the disturbance event for each time series (ID)
severity <- t_min %>%
  group_by(ID) %>%
  arrange(year) %>%
  mutate(
    tree_share_before = quantile(
      smoothed_tree_cover[year > (yod - 20) & year <= yod], 
      probs = 0.75, 
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# Divide smoothed_tree_cover, tree_cover, and tree_share_before by 100
severity <- severity %>%
  mutate(
    smoothed_tree_cover = smoothed_tree_cover / 100,
    tree_cover = tree_cover / 100,
    tree_share_before = tree_share_before / 100
  )

# Calculate the severity of the disturbance
severity <- severity %>%
  filter(!is.na(yod)) %>%
  mutate(
    severity_absolute = tree_share_before - min_tree_share,
    severity_relative = ifelse(tree_share_before == 0, NA, ((tree_share_before - min_tree_share) / tree_share_before) * 100)
  )

# Select only the necessary columns to keep all original columns plus the calculated ones
severity <- severity %>%
  select(
    everything(),  # Retains all columns from t_min
    severity_absolute, severity_relative  # Adds new columns
  )




#Step 2: Compute 80% of the tree_share_before for each time series
severity <- severity %>% #severity
  mutate(tree_share_80 = tree_share_before * 0.85)


# Step 1: Compute whether the recovery condition is met
recovery <- severity %>%
  group_by(ID) %>%
  mutate(
    # Check if the condition is met
    condition_met = year > yod & smoothed_tree_cover > tree_share_80
  ) %>%
  ungroup()

# Step 2: Identify the year when the recovery condition is met in two consecutive years
recovery <- recovery %>%
  group_by(ID) %>%
  arrange(ID, year) %>%  # Ensure data is sorted by year within each ID
  mutate(
    # Shift the condition_met column to check the next year
    next_year_condition = lead(condition_met, order_by = year),
    # Compute whether both the current and next year meet the condition
    consecutive_recovery = condition_met & next_year_condition
  ) %>%
  # Identify the first year where the consecutive recovery condition is true
  mutate(
    year_recov = if_else(consecutive_recovery & !duplicated(consecutive_recovery),
                         year,
                         NA_integer_)
  ) %>%
  # Remove intermediate columns
  select(-condition_met, -next_year_condition, -consecutive_recovery) %>%
  ungroup()

# Step 3: Compute the recovery rate
recovery <- recovery %>%
  mutate(
    recovery_rate = if_else(!is.na(year_recov), year_recov - yod, 100)
  )


# Reclassify severity
recovery <- recovery %>%
  mutate(severity_class = case_when(
    severity_relative <= 80 ~ "non stand-replacing",
    severity_relative > 80 ~ "stand-replacing",
    TRUE ~ NA_character_  # Ensure that NA values are handled
  ))



### write df
write.csv(recovery, "~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_all.csv", row.names = FALSE)

### regrow metric
# Calculate the regrown percent per ID
recovery_regrow <- recovery %>%
  group_by(ID) %>%
  mutate(
    regrown_percent = ifelse(
      year > yod,
      ((smoothed_tree_cover - min_tree_share) / (tree_share_before - min_tree_share)) * 100,
      NA  # Only calculate for post-disturbance years
    )
  ) %>%
  ungroup()



# Add the ysd column to the severity data frame
recovery_regrow <- recovery_regrow %>%
  mutate(ysd = year - yod)


# Classify ysd into recovery stages
recovery_regrow <- recovery_regrow %>%
  mutate(
    recovery_stage = case_when(
      ysd >= 0 & ysd <= 5  ~ "Early",
      ysd >= 6 & ysd <= 10 ~ "Intermediate",
      ysd >= 11 & ysd <= 20 ~ "Late",
      TRUE ~ NA_character_  # for cases outside these ranges
    )
  )


### write df
write.csv(recovery_regrow, "~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_all.csv", row.names = FALSE)


# Step 1: Subset for recovery_rate < 100
recovery_recovered <- recovery %>% filter(recovery_rate < 100)

# Step 2: Remove duplicate rows based on the ID column, keeping the first occurrence
recovery_unique <- recovery %>% distinct(ID, .keep_all = TRUE)


### extract geolocation
# If needed, assign a known CRS

# Load the shapefile with 5 features (subdivisions of the Alps)
alps_subdivisions <- st_read("~/eo_nas/EO4Alps/gis/alps_subdivision/alps_division_WGS.shp")



# Load the raster containing the Alps subdivision information
alps_raster <- rast("~/eo_nas/EO4Alps/gis/alps_subdivision/alps_division_100.tif")

# Step 1: Convert your data frame to an sf object
# Ensure `recovery_sf` has the same CRS as `alps_raster`
recovery_sf <- st_as_sf(GEDI_recov_all, coords = c("long", "lat"), crs = crs(alps_raster))

# Transform `recovery_sf` if CRS still doesn't match
if (st_crs(recovery_sf) != st_crs(alps_raster)) {
  recovery_sf <- st_transform(recovery_sf, crs(alps_raster))
}

# Step 2: Extract values from the raster at the points' locations
# Convert `sf` object to terra-compatible coordinates for extraction
coords <- st_coordinates(recovery_sf)
geoloc <- terra::extract(alps_raster, coords)

# Step 3: Add extracted values back to the data frame
GEDI_recov_all$geoloc <- geoloc[, 1]  # Use `[, 1]` if only one column is returned


# Assuming your dataframe is named 'df'
GEDI_recov_all <- GEDI_recov_all %>%
  # Create the recovery_status column based on recovery_rate
  mutate(recovery_status = ifelse(recovery_rate < 100, "recovered", "not recovered"))


# Create the recovery_10y and recovery_10y_num columns
GEDI_recov_all <- GEDI_recov_all %>%
  group_by(ID) %>%
  mutate(
    recovery_10y = if_else(any(recovery_status == "recovered" & year == yod + 10), "y", "n"),
    recovery_10y_num = if_else(recovery_10y == "y", 1, 0)
  ) %>%
  ungroup()



# Create new columns for VPD values at yod, yod+1, yod+2, and yod+3
GEDI_recov_all <- GEDI_recov_all %>%
  group_by(ID) %>%
  mutate(
    VPD_yod = VPD[year == yod],
    VPD_yod1 = VPD[year == yod + 1],
    VPD_yod2 = VPD[year == yod + 2],
    VPD_yod3 = VPD[year == yod + 3]
  ) %>%
  ungroup()


# if  I wanna control for aspect, I should reclassify aspect values
recovery_filt_lm_2013 <- recovery_filt_lm_2013 %>%
  mutate(aspect_cat = case_when(
    (aspect >= 315 | aspect < 45) ~ "N",
    aspect >= 45 & aspect < 135 ~ "O",
    aspect >= 135 & aspect < 225 ~ "S",
    aspect >= 225 & aspect < 315 ~ "W"
  ))



recovery_filt_lm_2013 <- recovery_filt_lm_2013 %>%
  mutate(geoloc_reclass = case_when(
    geoloc == "eastern alps - central" ~ "western alps - south",
    geoloc == "eastern alps - north" ~ "eastern alps - south",
    geoloc == "western alps - south" ~ "eastern alps - north",
    geoloc == "western alps - north" ~ "western alps - north",
    geoloc == "eastern alps - south" ~ "eastern alps - central",
    TRUE ~ geoloc  # Keeps any values not explicitly reclassified
  ))


recovery_filt_lm_2013 <- recovery_filt_lm_2013 %>%
  mutate(geoloc_reclass = case_when(
    geoloc_reclass == "eastern alps - central" ~ "western alps - north",
    geoloc_reclass == "western alps - north" ~ "eastern alps - central",
    TRUE ~ geoloc_reclass  # Keeps any values not explicitly reclassified
  ))



recovery_summary_geoloc <- recovery_summary_geoloc %>%
  mutate(geoloc_reclass = case_when(
    geoloc_reclass == "eastern alps - central" ~ "eastern alps - north",
    geoloc_reclass == "eastern alps - north" ~ "eastern alps - central",
    TRUE ~ geoloc_reclass  # Keeps any values not explicitly reclassified
  ))



recovery_filt_lm_2013 <- recovery_filt_lm_2013 %>%
  group_by(ID) %>%
  mutate(
    VPD_yod = VPD[year == yod],
    VPD_yod1 = VPD[year == yod + 1],
    VPD_yod2 = VPD[year == yod + 2],
    VPD_yod3 = VPD[year == yod + 3]
  ) %>%
  ungroup()




### write df
write.csv(GEDI_recov_all, "~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_all.csv", row.names = FALSE)

### write df
write.csv(recovery_filt_lm_2013, "~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_stats.csv", row.names = FALSE)




# Filter rows with geoloc == "western alps - north"
western_alps_north <- GEDI_all_unique %>%
  filter(geoloc == "western alps - north")

# Combine the original dataframe with duplicated rows for western alps - north
GEDI_all_unique <- GEDI_all_unique %>%
  bind_rows(western_alps_north)



#-------------------------------------------------------------------------------
### add temp data
#-------------------------------------------------------------------------------

# Set the parent directory where you want to create the folders
parent_dir <- "~/eo_nas/EO4Alps/climate_data/temp"  # Replace with your directory path

# Create folders for each year from 1986 to 2018
for (year in 1986:2019) {
  # Construct the folder path
  folder_path <- file.path(parent_dir, as.character(year))
  
  # Create the folder if it doesn't already exist
  if (!dir.exists(folder_path)) {
    dir.create(folder_path)
    cat("Created folder:", folder_path, "\n")  # Optional: Print confirmation
  }
}



library(terra)

# Define the path to your parent folder
parent_folder <- "~/eo_nas/EO4Alps/climate_data/temperature"

# List all the folders (1986 - 2018) within the parent folder
folders <- list.dirs(parent_folder, recursive = FALSE)

# Loop over each folder
for (folder in folders) {
  
  # Extract the year from the folder name to use in the output filename
  folder_name <- basename(folder)
  
  # List all raster files in the current folder that match the pattern (m06, m07, m08)
  rasters <- list.files(folder, pattern = "^m0[678]_", full.names = TRUE)
  
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
}


### convert in to LAEA
# Define the path to your parent folder and the output "LAEA" folder
parent_folder <- "~/eo_nas/EO4Alps/climate_data/temperature"
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




### extract mean temp per observation and year


###1986
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_1986.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986

recovery_1986 <- GEDI_recov_all %>% filter(year == 1986)


# Convert recovery_1986 to an sf object and ensure it has the same CRS as the raster
recovery_1986_sf <- st_as_sf(recovery_1986, coords = c("long", "lat"), crs = crs(raster_data))
recovery_1986_sf <- st_transform(recovery_1986_sf, crs = crs(raster_data))  # Reproject if necessary


# Extract VPD values
temp_values <- terra::extract(raster_data, recovery_1986_sf)

# Adjust the extracted values by moving the decimal three places to the left
temp_values_adjusted <- temp_values[, 2] / 1000

# Add adjusted values back to the dataframe
recovery_1986$temp <- temp_values_adjusted

gc()

#-------------------------------------------------------------------------------

###1986
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_1986.tif"
raster_data <- rast(raster_path)  # Use rast() from terra to load raster
# Filter the dataframe for the year 1986

recovery_1986 <- GEDI_recov_all %>% filter(year == 1986)


# Convert recovery_1986 to an sf object and ensure it has the same CRS as the raster
recovery_1986_sf <- st_as_sf(recovery_1986, coords = c("long", "lat"), crs = crs(raster_data))
recovery_1986_sf <- st_transform(recovery_1986_sf, crs = crs(raster_data))  # Reproject if necessary


# Extract VPD values
temp_values <- terra::extract(raster_data, recovery_1986_sf)

# Adjust the extracted values by moving the decimal three places to the left
temp_values_adjusted <- temp_values[, 2] / 1000

# Add adjusted values back to the dataframe
recovery_1986$temp <- temp_values_adjusted

gc()


### 1987
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_1987.tif"
raster_data <- rast(raster_path)
recovery_1987 <- GEDI_recov_all %>% filter(year == 1987)
recovery_1987_sf <- st_as_sf(recovery_1987, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1987_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_1987$temp <- temp_values_adjusted
gc()

### 1988
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_1988.tif"
raster_data <- rast(raster_path)
recovery_1988 <- GEDI_recov_all %>% filter(year == 1988)
recovery_1988_sf <- st_as_sf(recovery_1988, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1988_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_1988$temp <- temp_values_adjusted
gc()

### 1989
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_1989.tif"
raster_data <- rast(raster_path)
recovery_1989 <- GEDI_recov_all %>% filter(year == 1989)
recovery_1989_sf <- st_as_sf(recovery_1989, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1989_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_1989$temp <- temp_values_adjusted
gc()

### 1990
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_1990.tif"
raster_data <- rast(raster_path)
recovery_1990 <- GEDI_recov_all %>% filter(year == 1990)
recovery_1990_sf <- st_as_sf(recovery_1990, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1990_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_1990$temp <- temp_values_adjusted
gc()

### 1991
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_1991.tif"
raster_data <- rast(raster_path)
recovery_1991 <- GEDI_recov_all %>% filter(year == 1991)
recovery_1991_sf <- st_as_sf(recovery_1991, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1991_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_1991$temp <- temp_values_adjusted
gc()



### 1992
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_1992.tif"
raster_data <- rast(raster_path)
recovery_1992 <- GEDI_recov_all %>% filter(year == 1992)
recovery_1992_sf <- st_as_sf(recovery_1992, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1992_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_1992$temp <- temp_values_adjusted
gc()

### 1993
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_1993.tif"
raster_data <- rast(raster_path)
recovery_1993 <- GEDI_recov_all %>% filter(year == 1993)
recovery_1993_sf <- st_as_sf(recovery_1993, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1993_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_1993$temp <- temp_values_adjusted
gc()

### 1994
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_1994.tif"
raster_data <- rast(raster_path)
recovery_1994 <- GEDI_recov_all %>% filter(year == 1994)
recovery_1994_sf <- st_as_sf(recovery_1994, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1994_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_1994$temp <- temp_values_adjusted
gc()

### 1995
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_1995.tif"
raster_data <- rast(raster_path)
recovery_1995 <- GEDI_recov_all %>% filter(year == 1995)
recovery_1995_sf <- st_as_sf(recovery_1995, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1995_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_1995$temp <- temp_values_adjusted
gc()

### 1996
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_1996.tif"
raster_data <- rast(raster_path)
recovery_1996 <- GEDI_recov_all %>% filter(year == 1996)
recovery_1996_sf <- st_as_sf(recovery_1996, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1996_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_1996$temp <- temp_values_adjusted
gc()

### 1997
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_1997.tif"
raster_data <- rast(raster_path)
recovery_1997 <- GEDI_recov_all %>% filter(year == 1997)
recovery_1997_sf <- st_as_sf(recovery_1997, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1997_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_1997$temp <- temp_values_adjusted
gc()

### 1998
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_1998.tif"
raster_data <- rast(raster_path)
recovery_1998 <- GEDI_recov_all %>% filter(year == 1998)
recovery_1998_sf <- st_as_sf(recovery_1998, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1998_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_1998$temp <- temp_values_adjusted
gc()

### 1999
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_1999.tif"
raster_data <- rast(raster_path)
recovery_1999 <- GEDI_recov_all %>% filter(year == 1999)
recovery_1999_sf <- st_as_sf(recovery_1999, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1999_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_1999$temp <- temp_values_adjusted
gc()

### 2000
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2000.tif"
raster_data <- rast(raster_path)
recovery_2000 <- GEDI_recov_all %>% filter(year == 2000)
recovery_2000_sf <- st_as_sf(recovery_2000, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2000_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2000$temp <- temp_values_adjusted
gc()


### 2001
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2001.tif"
raster_data <- rast(raster_path)
recovery_2001 <- GEDI_recov_all %>% filter(year == 2001)
recovery_2001_sf <- st_as_sf(recovery_2001, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2001_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2001$temp <- temp_values_adjusted
gc()

### 2002
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2002.tif"
raster_data <- rast(raster_path)
recovery_2002 <- GEDI_recov_all %>% filter(year == 2002)
recovery_2002_sf <- st_as_sf(recovery_2002, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2002_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2002$temp <- temp_values_adjusted
gc()

### 2003
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2003.tif"
raster_data <- rast(raster_path)
recovery_2003 <- GEDI_recov_all %>% filter(year == 2003)
recovery_2003_sf <- st_as_sf(recovery_2003, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2003_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2003$temp <- temp_values_adjusted
gc()

### 2004
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2004.tif"
raster_data <- rast(raster_path)
recovery_2004 <- GEDI_recov_all %>% filter(year == 2004)
recovery_2004_sf <- st_as_sf(recovery_2004, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2004_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2004$temp <- temp_values_adjusted
gc()

### 2005
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2005.tif"
raster_data <- rast(raster_path)
recovery_2005 <- GEDI_recov_all %>% filter(year == 2005)
recovery_2005_sf <- st_as_sf(recovery_2005, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2005_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2005$temp <- temp_values_adjusted
gc()

### 2006
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2006.tif"
raster_data <- rast(raster_path)
recovery_2006 <- GEDI_recov_all %>% filter(year == 2006)
recovery_2006_sf <- st_as_sf(recovery_2006, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2006_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2006$temp <- temp_values_adjusted
gc()

### 2007
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2007.tif"
raster_data <- rast(raster_path)
recovery_2007 <- GEDI_recov_all %>% filter(year == 2007)
recovery_2007_sf <- st_as_sf(recovery_2007, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2007_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2007$temp <- temp_values_adjusted
gc()

### 2008
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2008.tif"
raster_data <- rast(raster_path)
recovery_2008 <- GEDI_recov_all %>% filter(year == 2008)
recovery_2008_sf <- st_as_sf(recovery_2008, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2008_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2008$temp <- temp_values_adjusted
gc()

### 2009
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2009.tif"
raster_data <- rast(raster_path)
recovery_2009 <- GEDI_recov_all %>% filter(year == 2009)
recovery_2009_sf <- st_as_sf(recovery_2009, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2009_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2009$temp <- temp_values_adjusted
gc()

### 2010
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2010.tif"
raster_data <- rast(raster_path)
recovery_2010 <- GEDI_recov_all %>% filter(year == 2010)
recovery_2010_sf <- st_as_sf(recovery_2010, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2010_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2010$temp <- temp_values_adjusted
gc()

### 2011
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2011.tif"
raster_data <- rast(raster_path)
recovery_2011 <- GEDI_recov_all %>% filter(year == 2011)
recovery_2011_sf <- st_as_sf(recovery_2011, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2011_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2011$temp <- temp_values_adjusted
gc()

### 2012
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2012.tif"
raster_data <- rast(raster_path)
recovery_2012 <- GEDI_recov_all %>% filter(year == 2012)
recovery_2012_sf <- st_as_sf(recovery_2012, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2012_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2012$temp <- temp_values_adjusted
gc()



### 2013
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2013.tif"
raster_data <- rast(raster_path)
recovery_2013 <- GEDI_recov_all %>% filter(year == 2013)
recovery_2013_sf <- st_as_sf(recovery_2013, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2013_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2013$temp <- temp_values_adjusted
gc()

### 2014
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2014.tif"
raster_data <- rast(raster_path)
recovery_2014 <- GEDI_recov_all %>% filter(year == 2014)
recovery_2014_sf <- st_as_sf(recovery_2014, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2014_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2014$temp <- temp_values_adjusted
gc()

### 2015
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2015.tif"
raster_data <- rast(raster_path)
recovery_2015 <- GEDI_recov_all %>% filter(year == 2015)
recovery_2015_sf <- st_as_sf(recovery_2015, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2015_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2015$temp <- temp_values_adjusted
gc()

### 2016
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2016.tif"
raster_data <- rast(raster_path)
recovery_2016 <- GEDI_recov_all %>% filter(year == 2016)
recovery_2016_sf <- st_as_sf(recovery_2016, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2016_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2016$temp <- temp_values_adjusted
gc()

### 2017
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2017.tif"
raster_data <- rast(raster_path)
recovery_2017 <- GEDI_recov_all %>% filter(year == 2017)
recovery_2017_sf <- st_as_sf(recovery_2017, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2017_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2017$temp <- temp_values_adjusted
gc()

### 2018
raster_path <- "~/eo_nas/EO4Alps/climate_data/temperature/LAEA/mean_temp_2018.tif"
raster_data <- rast(raster_path)
recovery_2018 <- GEDI_recov_all %>% filter(year == 2018)
recovery_2018_sf <- st_as_sf(recovery_2018, coords = c("long", "lat"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2018_sf)
temp_values_adjusted <- temp_values[, 2] / 1000
recovery_2018$temp <- temp_values_adjusted
gc()



### add years 2019 - 2023 to the df
# Subset the data frame to include only rows where year is 2019, 2020, 2021, 2022, or 2023
GEDI_recov_all_filt <- GEDI_recov_all %>% filter(year %in% c(2019, 2020, 2021, 2022, 2023))

# Step 2: Add a VPD column to GEDI_GAM_filt if it doesn't already have one
GEDI_recov_all_filt$temp <- NA  # Assign NA or any default value


# Define the years for your recovery data frames
years <- 1986:2018

# Step 1: Create a list of all recovery data frames
recovery_list <- lapply(years, function(year) {
  # Dynamically get each data frame by constructing its name
  get(paste0("recovery_", year))
})

# Step 2: Add GEDI_GAM_filt to the list of data frames
recovery_list <- c(recovery_list, list(GEDI_recov_all_filt))

# Step 3: Combine all data frames into one using bind_rows
GEDI_temp <- bind_rows(recovery_list)


### write
write.csv(GEDI_temp, "~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_all.csv", row.names=FALSE)


#-------------------------------------------------------------------------------
### same for coniferous and broadleaved forest
#-------------------------------------------------------------------------------


### 1986
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1986.tif"
raster_data <- rast(raster_path)
recovery_1986 <- GEDI_temp %>% filter(year == 1986)
recovery_1986_sf <- st_as_sf(recovery_1986, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1986_sf)[, 2]  # Assuming the extracted values are in the second column
recovery_1986$broadleaved <- temp_values
gc()


### 1987
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1987.tif"
raster_data <- rast(raster_path)
recovery_1987 <- GEDI_temp %>% filter(year == 1987)
recovery_1987_sf <- st_as_sf(recovery_1987, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1987_sf)[, 2]
recovery_1987$broadleaved <- temp_values
gc()

### 1988
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1988.tif"
raster_data <- rast(raster_path)
recovery_1988 <- GEDI_temp %>% filter(year == 1988)
recovery_1988_sf <- st_as_sf(recovery_1988, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1988_sf)[, 2]
recovery_1988$broadleaved <- temp_values
gc()

### 1989
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1989.tif"
raster_data <- rast(raster_path)
recovery_1989 <- GEDI_temp %>% filter(year == 1989)
recovery_1989_sf <- st_as_sf(recovery_1989, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1989_sf)[, 2]
recovery_1989$broadleaved <- temp_values
gc()

### 1990
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1990.tif"
raster_data <- rast(raster_path)
recovery_1990 <- GEDI_temp %>% filter(year == 1990)
recovery_1990_sf <- st_as_sf(recovery_1990, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1990_sf)[, 2]
recovery_1990$broadleaved <- temp_values
gc()

### 1991
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1991.tif"
raster_data <- rast(raster_path)
recovery_1991 <- GEDI_temp %>% filter(year == 1991)
recovery_1991_sf <- st_as_sf(recovery_1991, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1991_sf)[, 2]
recovery_1991$broadleaved <- temp_values
gc()

### 1992
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1992.tif"
raster_data <- rast(raster_path)
recovery_1992 <- GEDI_temp %>% filter(year == 1992)
recovery_1992_sf <- st_as_sf(recovery_1992, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1992_sf)[, 2]
recovery_1992$broadleaved <- temp_values
gc()

### 1993
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1993.tif"
raster_data <- rast(raster_path)
recovery_1993 <- GEDI_temp %>% filter(year == 1993)
recovery_1993_sf <- st_as_sf(recovery_1993, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1993_sf)[, 2]
recovery_1993$broadleaved <- temp_values
gc()

### 1994
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1994.tif"
raster_data <- rast(raster_path)
recovery_1994 <- GEDI_temp %>% filter(year == 1994)
recovery_1994_sf <- st_as_sf(recovery_1994, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1994_sf)[, 2]
recovery_1994$broadleaved <- temp_values
gc()

### 1995
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1995.tif"
raster_data <- rast(raster_path)
recovery_1995 <- GEDI_temp %>% filter(year == 1995)
recovery_1995_sf <- st_as_sf(recovery_1995, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1995_sf)[, 2]
recovery_1995$broadleaved <- temp_values
gc()

### 1996
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1996.tif"
raster_data <- rast(raster_path)
recovery_1996 <- GEDI_temp %>% filter(year == 1996)
recovery_1996_sf <- st_as_sf(recovery_1996, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1996_sf)[, 2]
recovery_1996$broadleaved <- temp_values
gc()

### 1997
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1997.tif"
raster_data <- rast(raster_path)
recovery_1997 <- GEDI_temp %>% filter(year == 1997)
recovery_1997_sf <- st_as_sf(recovery_1997, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1997_sf)[, 2]
recovery_1997$broadleaved <- temp_values
gc()

### 1998
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1998.tif"
raster_data <- rast(raster_path)
recovery_1998 <- GEDI_temp %>% filter(year == 1998)
recovery_1998_sf <- st_as_sf(recovery_1998, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1998_sf)[, 2]
recovery_1998$broadleaved <- temp_values
gc()


### 1999
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_1999.tif"
raster_data <- rast(raster_path)
recovery_1999 <- GEDI_temp %>% filter(year == 1999)
recovery_1999_sf <- st_as_sf(recovery_1999, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1999_sf)[, 2]
recovery_1999$broadleaved <- temp_values
gc()

### 2000
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2000.tif"
raster_data <- rast(raster_path)
recovery_2000 <- GEDI_temp %>% filter(year == 2000)
recovery_2000_sf <- st_as_sf(recovery_2000, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2000_sf)[, 2]
recovery_2000$broadleaved <- temp_values
gc()

### 2001
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2001.tif"
raster_data <- rast(raster_path)
recovery_2001 <- GEDI_temp %>% filter(year == 2001)
recovery_2001_sf <- st_as_sf(recovery_2001, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2001_sf)[, 2]
recovery_2001$broadleaved <- temp_values
gc()

### 2002
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2002.tif"
raster_data <- rast(raster_path)
recovery_2002 <- GEDI_temp %>% filter(year == 2002)
recovery_2002_sf <- st_as_sf(recovery_2002, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2002_sf)[, 2]
recovery_2002$broadleaved <- temp_values
gc()

### 2003
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2003.tif"
raster_data <- rast(raster_path)
recovery_2003 <- GEDI_temp %>% filter(year == 2003)
recovery_2003_sf <- st_as_sf(recovery_2003, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2003_sf)[, 2]
recovery_2003$broadleaved <- temp_values
gc()

### 2004
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2004.tif"
raster_data <- rast(raster_path)
recovery_2004 <- GEDI_temp %>% filter(year == 2004)
recovery_2004_sf <- st_as_sf(recovery_2004, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2004_sf)[, 2]
recovery_2004$broadleaved <- temp_values
gc()

### 2005
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2005.tif"
raster_data <- rast(raster_path)
recovery_2005 <- GEDI_temp %>% filter(year == 2005)
recovery_2005_sf <- st_as_sf(recovery_2005, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2005_sf)[, 2]
recovery_2005$broadleaved <- temp_values
gc()

### 2006
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2006.tif"
raster_data <- rast(raster_path)
recovery_2006 <- GEDI_temp %>% filter(year == 2006)
recovery_2006_sf <- st_as_sf(recovery_2006, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2006_sf)[, 2]
recovery_2006$broadleaved <- temp_values
gc()

### 2007
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2007.tif"
raster_data <- rast(raster_path)
recovery_2007 <- GEDI_temp %>% filter(year == 2007)
recovery_2007_sf <- st_as_sf(recovery_2007, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2007_sf)[, 2]
recovery_2007$broadleaved <- temp_values
gc()

### 2008
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2008.tif"
raster_data <- rast(raster_path)
recovery_2008 <- GEDI_temp %>% filter(year == 2008)
recovery_2008_sf <- st_as_sf(recovery_2008, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2008_sf)[, 2]
recovery_2008$broadleaved <- temp_values
gc()

### 2009
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2009.tif"
raster_data <- rast(raster_path)
recovery_2009 <- GEDI_temp %>% filter(year == 2009)
recovery_2009_sf <- st_as_sf(recovery_2009, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2009_sf)[, 2]
recovery_2009$broadleaved <- temp_values
gc()

### 2010
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2010.tif"
raster_data <- rast(raster_path)
recovery_2010 <- GEDI_temp %>% filter(year == 2010)
recovery_2010_sf <- st_as_sf(recovery_2010, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2010_sf)[, 2]
recovery_2010$broadleaved <- temp_values
gc()

### 2011
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2011.tif"
raster_data <- rast(raster_path)
recovery_2011 <- GEDI_temp %>% filter(year == 2011)
recovery_2011_sf <- st_as_sf(recovery_2011, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2011_sf)[, 2]
recovery_2011$broadleaved <- temp_values
gc()

### 2012
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2012.tif"
raster_data <- rast(raster_path)
recovery_2012 <- GEDI_temp %>% filter(year == 2012)
recovery_2012_sf <- st_as_sf(recovery_2012, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2012_sf)[, 2]
recovery_2012$broadleaved <- temp_values
gc()

### 2013
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2013.tif"
raster_data <- rast(raster_path)
recovery_2013 <- GEDI_temp %>% filter(year == 2013)
recovery_2013_sf <- st_as_sf(recovery_2013, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2013_sf)[, 2]
recovery_2013$broadleaved <- temp_values
gc()



### 2014
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2014.tif"
raster_data <- rast(raster_path)
recovery_2014 <- GEDI_temp %>% filter(year == 2014)
recovery_2014_sf <- st_as_sf(recovery_2014, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2014_sf)[, 2]
recovery_2014$broadleaved <- temp_values
gc()

### 2015
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2015.tif"
raster_data <- rast(raster_path)
recovery_2015 <- GEDI_temp %>% filter(year == 2015)
recovery_2015_sf <- st_as_sf(recovery_2015, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2015_sf)[, 2]
recovery_2015$broadleaved <- temp_values
gc()

### 2016
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2016.tif"
raster_data <- rast(raster_path)
recovery_2016 <- GEDI_temp %>% filter(year == 2016)
recovery_2016_sf <- st_as_sf(recovery_2016, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2016_sf)[, 2]
recovery_2016$broadleaved <- temp_values
gc()

### 2017
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2017.tif"
raster_data <- rast(raster_path)
recovery_2017 <- GEDI_temp %>% filter(year == 2017)
recovery_2017_sf <- st_as_sf(recovery_2017, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2017_sf)[, 2]
recovery_2017$broadleaved <- temp_values
gc()

### 2018
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2018.tif"
raster_data <- rast(raster_path)
recovery_2018 <- GEDI_temp %>% filter(year == 2018)
recovery_2018_sf <- st_as_sf(recovery_2018, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2018_sf)[, 2]
recovery_2018$broadleaved <- temp_values
gc()

### 2019
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2019.tif"
raster_data <- rast(raster_path)
recovery_2019 <- GEDI_temp %>% filter(year == 2019)
recovery_2019_sf <- st_as_sf(recovery_2019, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2019_sf)[, 2]
recovery_2019$broadleaved <- temp_values
gc()

### 2020
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2020.tif"
raster_data <- rast(raster_path)
recovery_2020 <- GEDI_temp %>% filter(year == 2020)
recovery_2020_sf <- st_as_sf(recovery_2020, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2020_sf)[, 2]
recovery_2020$broadleaved <- temp_values
gc()

### 2021
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2021.tif"
raster_data <- rast(raster_path)
recovery_2021 <- GEDI_temp %>% filter(year == 2021)
recovery_2021_sf <- st_as_sf(recovery_2021, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2021_sf)[, 2]
recovery_2021$broadleaved <- temp_values
gc()

### 2022
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2022.tif"
raster_data <- rast(raster_path)
recovery_2022 <- GEDI_temp %>% filter(year == 2022)
recovery_2022_sf <- st_as_sf(recovery_2022, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2022_sf)[, 2]
recovery_2022$broadleaved <- temp_values
gc()

### 2023
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_2023.tif"
raster_data <- rast(raster_path)
recovery_2023 <- GEDI_temp %>% filter(year == 2023)
recovery_2023_sf <- st_as_sf(recovery_2023, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2023_sf)[, 2]
recovery_2023$broadleaved <- temp_values
gc()


# Define the years for your recovery data frames
years <- 1986:2023

# Step 1: Create a list of all recovery data frames
recovery_list <- lapply(years, function(year) {
  # Dynamically get each data frame by constructing its name
  get(paste0("recovery_", year))
})


# Step 3: Combine all data frames into one using bind_rows
GEDI_temp_broad <- bind_rows(recovery_list)




#-------------------------------------------------------------------------------
### same for coniferous and broadleaved forest
#-------------------------------------------------------------------------------


### 1986
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1986.tif"
raster_data <- rast(raster_path)
recovery_1986 <- GEDI_temp_broad %>% filter(year == 1986)
recovery_1986_sf <- st_as_sf(recovery_1986, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1986_sf)[, 2]  # Assuming the extracted values are in the second column
recovery_1986$coniferous <- temp_values
gc()


### 1987
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1987.tif"
raster_data <- rast(raster_path)
recovery_1987 <- GEDI_temp_broad %>% filter(year == 1987)
recovery_1987_sf <- st_as_sf(recovery_1987, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1987_sf)[, 2]
recovery_1987$coniferous <- temp_values
gc()

### 1988
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1988.tif"
raster_data <- rast(raster_path)
recovery_1988 <- GEDI_temp_broad %>% filter(year == 1988)
recovery_1988_sf <- st_as_sf(recovery_1988, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1988_sf)[, 2]
recovery_1988$coniferous <- temp_values
gc()

### 1989
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1989.tif"
raster_data <- rast(raster_path)
recovery_1989 <- GEDI_temp_broad %>% filter(year == 1989)
recovery_1989_sf <- st_as_sf(recovery_1989, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1989_sf)[, 2]
recovery_1989$coniferous <- temp_values
gc()

### 1990
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1990.tif"
raster_data <- rast(raster_path)
recovery_1990 <- GEDI_temp_broad %>% filter(year == 1990)
recovery_1990_sf <- st_as_sf(recovery_1990, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1990_sf)[, 2]
recovery_1990$coniferous <- temp_values
gc()

### 1991
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1991.tif"
raster_data <- rast(raster_path)
recovery_1991 <- GEDI_temp_broad %>% filter(year == 1991)
recovery_1991_sf <- st_as_sf(recovery_1991, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1991_sf)[, 2]
recovery_1991$coniferous <- temp_values
gc()

### 1992
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1992.tif"
raster_data <- rast(raster_path)
recovery_1992 <- GEDI_temp_broad %>% filter(year == 1992)
recovery_1992_sf <- st_as_sf(recovery_1992, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1992_sf)[, 2]
recovery_1992$coniferous <- temp_values
gc()

### 1993
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1993.tif"
raster_data <- rast(raster_path)
recovery_1993 <- GEDI_temp_broad %>% filter(year == 1993)
recovery_1993_sf <- st_as_sf(recovery_1993, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1993_sf)[, 2]
recovery_1993$coniferous <- temp_values
gc()

### 1994
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1994.tif"
raster_data <- rast(raster_path)
recovery_1994 <- GEDI_temp_broad %>% filter(year == 1994)
recovery_1994_sf <- st_as_sf(recovery_1994, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1994_sf)[, 2]
recovery_1994$coniferous <- temp_values
gc()

### 1995
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1995.tif"
raster_data <- rast(raster_path)
recovery_1995 <- GEDI_temp_broad %>% filter(year == 1995)
recovery_1995_sf <- st_as_sf(recovery_1995, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1995_sf)[, 2]
recovery_1995$coniferous <- temp_values
gc()

### 1996
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1996.tif"
raster_data <- rast(raster_path)
recovery_1996 <- GEDI_temp_broad %>% filter(year == 1996)
recovery_1996_sf <- st_as_sf(recovery_1996, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1996_sf)[, 2]
recovery_1996$coniferous <- temp_values
gc()

### 1997
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1997.tif"
raster_data <- rast(raster_path)
recovery_1997 <- GEDI_temp_broad %>% filter(year == 1997)
recovery_1997_sf <- st_as_sf(recovery_1997, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1997_sf)[, 2]
recovery_1997$coniferous <- temp_values
gc()

### 1998
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1998.tif"
raster_data <- rast(raster_path)
recovery_1998 <- GEDI_temp_broad %>% filter(year == 1998)
recovery_1998_sf <- st_as_sf(recovery_1998, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1998_sf)[, 2]
recovery_1998$coniferous <- temp_values
gc()


### 1999
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_1999.tif"
raster_data <- rast(raster_path)
recovery_1999 <- GEDI_temp_broad %>% filter(year == 1999)
recovery_1999_sf <- st_as_sf(recovery_1999, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_1999_sf)[, 2]
recovery_1999$coniferous <- temp_values
gc()

### 2000
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2000.tif"
raster_data <- rast(raster_path)
recovery_2000 <- GEDI_temp_broad %>% filter(year == 2000)
recovery_2000_sf <- st_as_sf(recovery_2000, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2000_sf)[, 2]
recovery_2000$coniferous <- temp_values
gc()

### 2001
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2001.tif"
raster_data <- rast(raster_path)
recovery_2001 <- GEDI_temp_broad %>% filter(year == 2001)
recovery_2001_sf <- st_as_sf(recovery_2001, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2001_sf)[, 2]
recovery_2001$coniferous <- temp_values
gc()

### 2002
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2002.tif"
raster_data <- rast(raster_path)
recovery_2002 <- GEDI_temp_broad %>% filter(year == 2002)
recovery_2002_sf <- st_as_sf(recovery_2002, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2002_sf)[, 2]
recovery_2002$coniferous <- temp_values
gc()

### 2003
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2003.tif"
raster_data <- rast(raster_path)
recovery_2003 <- GEDI_temp_broad %>% filter(year == 2003)
recovery_2003_sf <- st_as_sf(recovery_2003, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2003_sf)[, 2]
recovery_2003$coniferous <- temp_values
gc()

### 2004
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2004.tif"
raster_data <- rast(raster_path)
recovery_2004 <- GEDI_temp_broad %>% filter(year == 2004)
recovery_2004_sf <- st_as_sf(recovery_2004, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2004_sf)[, 2]
recovery_2004$coniferous <- temp_values
gc()

### 2005
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2005.tif"
raster_data <- rast(raster_path)
recovery_2005 <- GEDI_temp_broad %>% filter(year == 2005)
recovery_2005_sf <- st_as_sf(recovery_2005, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2005_sf)[, 2]
recovery_2005$coniferous <- temp_values
gc()

### 2006
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2006.tif"
raster_data <- rast(raster_path)
recovery_2006 <- GEDI_temp_broad %>% filter(year == 2006)
recovery_2006_sf <- st_as_sf(recovery_2006, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2006_sf)[, 2]
recovery_2006$coniferous <- temp_values
gc()

### 2007
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2007.tif"
raster_data <- rast(raster_path)
recovery_2007 <- GEDI_temp_broad %>% filter(year == 2007)
recovery_2007_sf <- st_as_sf(recovery_2007, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2007_sf)[, 2]
recovery_2007$coniferous <- temp_values
gc()

### 2008
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2008.tif"
raster_data <- rast(raster_path)
recovery_2008 <- GEDI_temp_broad %>% filter(year == 2008)
recovery_2008_sf <- st_as_sf(recovery_2008, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2008_sf)[, 2]
recovery_2008$coniferous <- temp_values
gc()

### 2009
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2009.tif"
raster_data <- rast(raster_path)
recovery_2009 <- GEDI_temp_broad %>% filter(year == 2009)
recovery_2009_sf <- st_as_sf(recovery_2009, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2009_sf)[, 2]
recovery_2009$coniferous <- temp_values
gc()

### 2010
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2010.tif"
raster_data <- rast(raster_path)
recovery_2010 <- GEDI_temp_broad %>% filter(year == 2010)
recovery_2010_sf <- st_as_sf(recovery_2010, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2010_sf)[, 2]
recovery_2010$coniferous <- temp_values
gc()

### 2011
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2011.tif"
raster_data <- rast(raster_path)
recovery_2011 <- GEDI_temp_broad %>% filter(year == 2011)
recovery_2011_sf <- st_as_sf(recovery_2011, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2011_sf)[, 2]
recovery_2011$coniferous <- temp_values
gc()

### 2012
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2012.tif"
raster_data <- rast(raster_path)
recovery_2012 <- GEDI_temp_broad %>% filter(year == 2012)
recovery_2012_sf <- st_as_sf(recovery_2012, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2012_sf)[, 2]
recovery_2012$coniferous <- temp_values
gc()

### 2013
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2013.tif"
raster_data <- rast(raster_path)
recovery_2013 <- GEDI_temp_broad %>% filter(year == 2013)
recovery_2013_sf <- st_as_sf(recovery_2013, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2013_sf)[, 2]
recovery_2013$coniferous <- temp_values
gc()



### 2014
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2014.tif"
raster_data <- rast(raster_path)
recovery_2014 <- GEDI_temp_broad %>% filter(year == 2014)
recovery_2014_sf <- st_as_sf(recovery_2014, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2014_sf)[, 2]
recovery_2014$coniferous <- temp_values
gc()

### 2015
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2015.tif"
raster_data <- rast(raster_path)
recovery_2015 <- GEDI_temp_broad %>% filter(year == 2015)
recovery_2015_sf <- st_as_sf(recovery_2015, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2015_sf)[, 2]
recovery_2015$coniferous <- temp_values
gc()

### 2016
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2016.tif"
raster_data <- rast(raster_path)
recovery_2016 <- GEDI_temp_broad %>% filter(year == 2016)
recovery_2016_sf <- st_as_sf(recovery_2016, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2016_sf)[, 2]
recovery_2016$coniferous <- temp_values
gc()

### 2017
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2017.tif"
raster_data <- rast(raster_path)
recovery_2017 <- GEDI_temp_broad %>% filter(year == 2017)
recovery_2017_sf <- st_as_sf(recovery_2017, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2017_sf)[, 2]
recovery_2017$coniferous <- temp_values
gc()

### 2018
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2018.tif"
raster_data <- rast(raster_path)
recovery_2018 <- GEDI_temp_broad %>% filter(year == 2018)
recovery_2018_sf <- st_as_sf(recovery_2018, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2018_sf)[, 2]
recovery_2018$coniferous <- temp_values
gc()

### 2019
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2019.tif"
raster_data <- rast(raster_path)
recovery_2019 <- GEDI_temp_broad %>% filter(year == 2019)
recovery_2019_sf <- st_as_sf(recovery_2019, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2019_sf)[, 2]
recovery_2019$coniferous <- temp_values
gc()

### 2020
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2020.tif"
raster_data <- rast(raster_path)
recovery_2020 <- GEDI_temp_broad %>% filter(year == 2020)
recovery_2020_sf <- st_as_sf(recovery_2020, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2020_sf)[, 2]
recovery_2020$coniferous <- temp_values
gc()

### 2021
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2021.tif"
raster_data <- rast(raster_path)
recovery_2021 <- GEDI_temp_broad %>% filter(year == 2021)
recovery_2021_sf <- st_as_sf(recovery_2021, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2021_sf)[, 2]
recovery_2021$coniferous <- temp_values
gc()

### 2022
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2022.tif"
raster_data <- rast(raster_path)
recovery_2022 <- GEDI_temp_broad %>% filter(year == 2022)
recovery_2022_sf <- st_as_sf(recovery_2022, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2022_sf)[, 2]
recovery_2022$coniferous <- temp_values
gc()

### 2023
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/coniferous/coniferous_2023.tif"
raster_data <- rast(raster_path)
recovery_2023 <- GEDI_temp_broad %>% filter(year == 2023)
recovery_2023_sf <- st_as_sf(recovery_2023, coords = c("x", "y"), crs = crs(raster_data))
temp_values <- terra::extract(raster_data, recovery_2023_sf)[, 2]
recovery_2023$coniferous <- temp_values
gc()





# Step 1: Create a list of all recovery data frames
recovery_list <- lapply(years, function(year) {
  # Dynamically get each data frame by constructing its name
  get(paste0("recovery_", year))
})


# Step 3: Combine all data frames into one using bind_rows
GEDI_forest_types <- bind_rows(recovery_list)



#-------------------------------------------------------------------------------
### reclassify heigt and aspect

# Reclassify 'dem' into 'sea_level' with character values
GEDI_forest_types$sea_level <- cut(GEDI_forest_types$dem, 
                    breaks = c(-Inf, 400, 1000, Inf), 
                    labels = c("0-400", ">400-1000", ">1000"), 
                    right = TRUE)

# Classify 'slope' into 'slope_level' with character values
GEDI_forest_types$aspect_level <- ifelse(GEDI_forest_types$aspect >= 270 | GEDI_forest_types$aspect <= 90, "north", "south")



### write
write.csv(GEDI_forest_types, "~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_all.csv", row.names=FALSE)



