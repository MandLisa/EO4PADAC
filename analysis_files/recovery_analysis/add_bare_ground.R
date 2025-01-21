library(raster)
library(tidyverse)
library(sf)
library(terra)
library(raster)
library(dplyr)


recovery_3035 <- recovery
recovery_3035_sf <- st_as_sf(recovery_3035, coords = c("x", "y"), crs=crs(raster_data))
st_crs(recovery_3035_sf) <- 3035

### 1986
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_1986.tif"
raster_data <- rast(raster_path)
recovery_1986 <- recovery %>% filter(year == 1986)
recovery_1986_sf <- st_as_sf(recovery_1986, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_1986_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_1986_sf)[, 2]  # Assuming the extracted values are in the second column
recovery_1986$bare_ground <- temp_values / 100
gc()


### 1987
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_1987.tif"
raster_data <- rast(raster_path)
recovery_1987 <- recovery %>% filter(year == 1987)
recovery_1987_sf <- st_as_sf(recovery_1987, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_1987_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_1987_sf)[, 2]
recovery_1987$bare_ground <- temp_values
gc()

### 1988
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_1988.tif"
raster_data <- rast(raster_path)
recovery_1988 <- recovery %>% filter(year == 1988)
recovery_1988_sf <- st_as_sf(recovery_1988, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_1988_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_1988_sf)[, 2]
recovery_1988$bare_ground <- temp_values
gc()

### 1989
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_1989.tif"
raster_data <- rast(raster_path)
recovery_1989 <- recovery %>% filter(year == 1989)
recovery_1989_sf <- st_as_sf(recovery_1989, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_1989_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_1989_sf)[, 2]
recovery_1989$bare_ground <- temp_values
gc()

### 1990
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_1990.tif"
raster_data <- rast(raster_path)
recovery_1990 <- recovery %>% filter(year == 1990)
recovery_1990_sf <- st_as_sf(recovery_1990, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_1990_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_1990_sf)[, 2]
recovery_1990$bare_ground <- temp_values
gc()

### 1991
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_1991.tif"
raster_data <- rast(raster_path)
recovery_1991 <- recovery %>% filter(year == 1991)
recovery_1991_sf <- st_as_sf(recovery_1991, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_1991_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_1991_sf)[, 2]
recovery_1991$bare_ground <- temp_values
gc()

### 1992
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_1992.tif"
raster_data <- rast(raster_path)
recovery_1992 <- recovery %>% filter(year == 1992)
recovery_1992_sf <- st_as_sf(recovery_1992, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_1992_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_1992_sf)[, 2]
recovery_1992$bare_ground <- temp_values
gc()

### 1993
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_1993.tif"
raster_data <- rast(raster_path)
recovery_1993 <- recovery %>% filter(year == 1993)
recovery_1993_sf <- st_as_sf(recovery_1993, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_1993_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_1993_sf)[, 2]
recovery_1993$bare_ground <- temp_values
gc()

### 1994
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_1994.tif"
raster_data <- rast(raster_path)
recovery_1994 <- recovery %>% filter(year == 1994)
recovery_1994_sf <- st_as_sf(recovery_1994, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_1994_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_1994_sf)[, 2]
recovery_1994$bare_ground <- temp_values
gc()

### 1995
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_1995.tif"
raster_data <- rast(raster_path)
recovery_1995 <- recovery %>% filter(year == 1995)
recovery_1995_sf <- st_as_sf(recovery_1995, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_1995_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_1995_sf)[, 2]
recovery_1995$bare_ground <- temp_values
gc()

### 1996
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_1996.tif"
raster_data <- rast(raster_path)
recovery_1996 <- recovery %>% filter(year == 1996)
recovery_1996_sf <- st_as_sf(recovery_1996, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_1996_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_1996_sf)[, 2]
recovery_1996$bare_ground <- temp_values
gc()

### 1997
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_1997.tif"
raster_data <- rast(raster_path)
recovery_1997 <- recovery %>% filter(year == 1997)
recovery_1997_sf <- st_as_sf(recovery_1997, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_1997_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_1997_sf)[, 2]
recovery_1997$bare_ground <- temp_values
gc()

### 1998
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_1998.tif"
raster_data <- rast(raster_path)
recovery_1998 <- recovery %>% filter(year == 1998)
recovery_1998_sf <- st_as_sf(recovery_1998, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_1998_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_1998_sf)[, 2]
recovery_1998$bare_ground <- temp_values
gc()


### 1999
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_1999.tif"
raster_data <- rast(raster_path)
recovery_1999 <- recovery %>% filter(year == 1999)
recovery_1999_sf <- st_as_sf(recovery_1999, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_1999_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_1999_sf)[, 2]
recovery_1999$bare_ground <- temp_values
gc()

### 2000
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2000.tif"
raster_data <- rast(raster_path)
recovery_2000 <- recovery %>% filter(year == 2000)
recovery_2000_sf <- st_as_sf(recovery_2000, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2000_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2000_sf)[, 2]
recovery_2000$bare_ground <- temp_values
gc()

### 2001
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2001.tif"
raster_data <- rast(raster_path)
recovery_2001 <- recovery %>% filter(year == 2001)
recovery_2001_sf <- st_as_sf(recovery_2001, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2001_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2001_sf)[, 2]
recovery_2001$bare_ground <- temp_values
gc()

### 2002
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2002.tif"
raster_data <- rast(raster_path)
recovery_2002 <- recovery %>% filter(year == 2002)
recovery_2002_sf <- st_as_sf(recovery_2002, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2002_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2002_sf)[, 2]
recovery_2002$bare_ground <- temp_values
gc()

### 2003
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2003.tif"
raster_data <- rast(raster_path)
recovery_2003 <- recovery %>% filter(year == 2003)
recovery_2003_sf <- st_as_sf(recovery_2003, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2003_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2003_sf)[, 2]
recovery_2003$bare_ground <- temp_values
gc()

### 2004
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2004.tif"
raster_data <- rast(raster_path)
recovery_2004 <- recovery %>% filter(year == 2004)
recovery_2004_sf <- st_as_sf(recovery_2004, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2004_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2004_sf)[, 2]
recovery_2004$bare_ground <- temp_values
gc()

### 2005
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2005.tif"
raster_data <- rast(raster_path)
recovery_2005 <- recovery %>% filter(year == 2005)
recovery_2005_sf <- st_as_sf(recovery_2005, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2005_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2005_sf)[, 2]
recovery_2005$bare_ground <- temp_values
gc()

### 2006
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2006.tif"
raster_data <- rast(raster_path)
recovery_2006 <- recovery %>% filter(year == 2006)
recovery_2006_sf <- st_as_sf(recovery_2006, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2006_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2006_sf)[, 2]
recovery_2006$bare_ground <- temp_values
gc()

### 2007
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2007.tif"
raster_data <- rast(raster_path)
recovery_2007 <- recovery %>% filter(year == 2007)
recovery_2007_sf <- st_as_sf(recovery_2007, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2007_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2007_sf)[, 2]
recovery_2007$bare_ground <- temp_values
gc()

### 2008
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2008.tif"
raster_data <- rast(raster_path)
recovery_2008 <- recovery %>% filter(year == 2008)
recovery_2008_sf <- st_as_sf(recovery_2008, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2008_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2008_sf)[, 2]
recovery_2008$bare_ground <- temp_values
gc()

### 2009
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2009.tif"
raster_data <- rast(raster_path)
recovery_2009 <- recovery %>% filter(year == 2009)
recovery_2009_sf <- st_as_sf(recovery_2009, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2009_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2009_sf)[, 2]
recovery_2009$bare_ground <- temp_values
gc()

### 2010
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2010.tif"
raster_data <- rast(raster_path)
recovery_2010 <- recovery %>% filter(year == 2010)
recovery_2010_sf <- st_as_sf(recovery_2010, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2010_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2010_sf)[, 2]
recovery_2010$bare_ground <- temp_values
gc()

### 2011
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2011.tif"
raster_data <- rast(raster_path)
recovery_2011 <- recovery %>% filter(year == 2011)
recovery_2011_sf <- st_as_sf(recovery_2011, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2011_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2011_sf)[, 2]
recovery_2011$bare_ground <- temp_values
gc()

### 2012
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2012.tif"
raster_data <- rast(raster_path)
recovery_2012 <- recovery %>% filter(year == 2012)
recovery_2012_sf <- st_as_sf(recovery_2012, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2012_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2012_sf)[, 2]
recovery_2012$bare_ground <- temp_values
gc()

### 2013
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2013.tif"
raster_data <- rast(raster_path)
recovery_2013 <- recovery %>% filter(year == 2013)
recovery_2013_sf <- st_as_sf(recovery_2013, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2013_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2013_sf)[, 2]
recovery_2013$bare_ground <- temp_values
gc()



### 2014
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2014.tif"
raster_data <- rast(raster_path)
recovery_2014 <- recovery %>% filter(year == 2014)
recovery_2014_sf <- st_as_sf(recovery_2014, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2014_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2014_sf)[, 2]
recovery_2014$bare_ground <- temp_values
gc()

### 2015
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2015.tif"
raster_data <- rast(raster_path)
recovery_2015 <- recovery %>% filter(year == 2015)
recovery_2015_sf <- st_as_sf(recovery_2015, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2015_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2015_sf)[, 2]
recovery_2015$bare_ground <- temp_values
gc()

### 2016
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2016.tif"
raster_data <- rast(raster_path)
recovery_2016 <- recovery %>% filter(year == 2016)
recovery_2016_sf <- st_as_sf(recovery_2016, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2016_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2016_sf)[, 2]
recovery_2016$bare_ground <- temp_values
gc()

### 2017
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2017.tif"
raster_data <- rast(raster_path)
recovery_2017 <- recovery %>% filter(year == 2017)
recovery_2017_sf <- st_as_sf(recovery_2017, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2017_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2017_sf)[, 2]
recovery_2017$bare_ground <- temp_values
gc()

### 2018
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2018.tif"
raster_data <- rast(raster_path)
recovery_2018 <- recovery %>% filter(year == 2018)
recovery_2018_sf <- st_as_sf(recovery_2018, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2018_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2018_sf)[, 2]
recovery_2018$bare_ground <- temp_values
gc()

### 2019
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2019.tif"
raster_data <- rast(raster_path)
recovery_2019 <- recovery %>% filter(year == 2019)
recovery_2019_sf <- st_as_sf(recovery_2019, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2019_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2019_sf)[, 2]
recovery_2019$bare_ground <- temp_values
gc()

### 2020
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2020.tif"
raster_data <- rast(raster_path)
recovery_2020 <- recovery %>% filter(year == 2020)
recovery_2020_sf <- st_as_sf(recovery_2020, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2020_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2020_sf)[, 2]
recovery_2020$bare_ground <- temp_values
gc()

### 2021
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2021.tif"
raster_data <- rast(raster_path)
recovery_2021 <- recovery %>% filter(year == 2021)
recovery_2021_sf <- st_as_sf(recovery_2021, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2021_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2021_sf)[, 2]
recovery_2021$bare_ground <- temp_values
gc()

### 2022
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2022.tif"
raster_data <- rast(raster_path)
recovery_2022 <- recovery %>% filter(year == 2022)
recovery_2022_sf <- st_as_sf(recovery_2022, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2022_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2022_sf)[, 2]
recovery_2022$bare_ground <- temp_values
gc()

### 2023
raster_path <- "~/eo_nas/EO4Alps/level3_predictions/bare_ground/bare_ground_2023.tif"
raster_data <- rast(raster_path)
recovery_2023 <- recovery %>% filter(year == 2023)
recovery_2023_sf <- st_as_sf(recovery_2023, coords = c("x", "y"), crs = crs(raster_data))
st_crs(recovery_2023_sf) <- 3035
temp_values <- terra::extract(raster_data, recovery_2023_sf)[, 2]
recovery_2023$bare_ground <- temp_values
gc()


# Define the years for your recovery data frames
years <- 1986:2023

# Step 1: Create a list of all recovery data frames
recovery_list <- lapply(years, function(year) {
  # Dynamically get each data frame by constructing its name
  get(paste0("recovery_", year))
})


# Step 3: Combine all data frames into one using bind_rows
recovery <- bind_rows(recovery_list)


### write
write.csv(recovery, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_random.csv", row.names=FALSE)

