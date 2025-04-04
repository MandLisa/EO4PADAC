library(raster)
library(tidyverse)
library(sf)
library(terra)
library(raster)
library(dplyr)
library(readr)
library(terra)

### load GAM df
recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_312025.csv")

# Define the file path to the raster
raster_data <- rast("~/eo_nas/EO4Alps/climate_data/PR/prec_mean_stack.tif")

# Define the target CRS (EPSG:3035 - LAEA)
target_crs <- "EPSG:3035"

# Reproject the raster
raster_data <- project(raster_data, target_crs, method = "bilinear")


###1986
library(terra)

recovery_1986 <- recovery %>% filter(year == 1986)

# Convert recovery_1986 to an sf object if necessary
recovery_1986_sf <- st_as_sf(recovery_1986, coords = c("x", "y"), crs = crs(raster_data))

# Extract prec values
prec_values <- terra::extract(raster_data, recovery_1986_sf)

# Add extracted values back to the dataframe
recovery_1986$prec <- prec_values[, 2]  


###1987
recovery_1987 <- recovery %>% filter(year == 1987)

# Convert recovery_1986 to an sf object if necessary
recovery_1987_sf <- st_as_sf(recovery_1987, coords = c("x", "y"), crs = crs(raster_data))

# Extract prec values
prec_values <- terra::extract(raster_data, recovery_1987_sf)

# Add extracted values back to the dataframe
recovery_1987$prec <- prec_values[, 3]  


###1988
recovery_1988 <- recovery %>% filter(year == 1988)
# Add extracted values back to the dataframe
recovery_1988$prec <- prec_values[, 4]  


###1989
recovery_1989 <- recovery %>% filter(year == 1989)
# Add extracted values back to the dataframe
recovery_1989$prec <- prec_values[, 5]  


###1990
recovery_1990 <- recovery %>% filter(year == 1990)
# Add extracted values back to the dataframe
recovery_1990$prec <- prec_values[, 6]  


###1991
recovery_1991 <- recovery %>% filter(year == 1991)
# Add extracted values back to the dataframe
recovery_1991$prec <- prec_values[, 7]  


###1992
recovery_1992 <- recovery %>% filter(year == 1992)
# Add extracted values back to the dataframe
recovery_1992$prec <- prec_values[, 8]  


###1993
recovery_1993 <- recovery %>% filter(year == 1993)
# Add extracted values back to the dataframe
recovery_1993$prec <- prec_values[, 9]  


###1994
recovery_1994 <- recovery %>% filter(year == 1994)
# Add extracted values back to the dataframe
recovery_1994$prec <- prec_values[, 10]  

###1995
recovery_1995 <- recovery %>% filter(year == 1995)
# Add extracted values back to the dataframe
recovery_1995$prec <- prec_values[, 11]  


###1996
recovery_1996 <- recovery %>% filter(year == 1996)
# Add extracted values back to the dataframe
recovery_1996$prec <- prec_values[, 12]  


###1997
recovery_1997 <- recovery %>% filter(year == 1997)
# Add extracted values back to the dataframe
recovery_1997$prec <- prec_values[, 13]  


###1998
recovery_1998 <- recovery %>% filter(year == 1998)
# Add extracted values back to the dataframe
recovery_1998$prec <- prec_values[, 14]  


###1999
recovery_1999 <- recovery %>% filter(year == 1999)
# Add extracted values back to the dataframe
recovery_1999$prec <- prec_values[, 15]  



###2000
recovery_2000 <- recovery %>% filter(year == 2000)
# Add extracted values back to the dataframe
recovery_2000$prec <- prec_values[, 16]  


###2001
recovery_2001 <- recovery %>% filter(year == 2001)
# Add extracted values back to the dataframe
recovery_2001$prec <- prec_values[, 17]  



###2002
recovery_2002 <- recovery %>% filter(year == 2002)
# Add extracted values back to the dataframe
recovery_2002$prec <- prec_values[, 18]  


###2003
recovery_2003 <- recovery %>% filter(year == 2003)
# Add extracted values back to the dataframe
recovery_2003$prec <- prec_values[, 19]  


###2004
recovery_2004 <- recovery %>% filter(year == 2004)
# Add extracted values back to the dataframe
recovery_2004$prec <- prec_values[, 20]  


###2005
recovery_2005 <- recovery %>% filter(year == 2005)
# Add extracted values back to the dataframe
recovery_2005$prec <- prec_values[, 21]  


###2006
recovery_2006 <- recovery %>% filter(year == 2006)
# Add extracted values back to the dataframe
recovery_2006$prec <- prec_values[, 22]  


###2007
recovery_2007 <- recovery %>% filter(year == 2007)
# Add extracted values back to the dataframe
recovery_2007$prec <- prec_values[, 23]  


###2008
recovery_2008 <- recovery %>% filter(year == 2008)
# Add extracted values back to the dataframe
recovery_2008$prec <- prec_values[, 24]  


###2009
recovery_2009 <- recovery %>% filter(year == 2009)
# Add extracted values back to the dataframe
recovery_2009$prec <- prec_values[, 25]  


###2010
recovery_2010 <- recovery %>% filter(year == 2010)
# Add extracted values back to the dataframe
recovery_2010$prec <- prec_values[, 26]  


###2011
recovery_2011 <- recovery %>% filter(year == 2011)
# Add extracted values back to the dataframe
recovery_2011$prec <- prec_values[, 27]  


###2012
recovery_2012 <- recovery %>% filter(year == 2012)
# Add extracted values back to the dataframe
recovery_2012$prec <- prec_values[, 28]  


###2013
recovery_2013 <- recovery %>% filter(year == 2013)
# Add extracted values back to the dataframe
recovery_2013$prec <- prec_values[, 29]  


###2014
recovery_2014 <- recovery %>% filter(year == 2014)
# Add extracted values back to the dataframe
recovery_2014$prec <- prec_values[, 30]  


###2015
recovery_2015 <- recovery %>% filter(year == 2015)
# Add extracted values back to the dataframe
recovery_2015$prec <- prec_values[, 31]  


###2016
recovery_2016 <- recovery %>% filter(year == 2016)
# Add extracted values back to the dataframe
recovery_2016$prec <- prec_values[, 32]  


###2017
recovery_2017 <- recovery %>% filter(year == 2017)
# Add extracted values back to the dataframe
recovery_2017$prec <- prec_values[, 33]  


###2018
recovery_2018 <- recovery %>% filter(year == 2018)
# Add extracted values back to the dataframe
recovery_2018$prec <- prec_values[, 34]  


# Subset the data frame to include only rows where year is 2019, 2020, 2021, 2022, or 2023
recovery_filt <- recovery %>% filter(year %in% c(2019, 2020, 2021, 2022, 2023))

# Step 2: Add a prec column to recovery_filt if it doesn't already have one
recovery_filt$prec <- NA  # Assign NA or any default value


# Define the years for your recovery data frames
years <- 1986:2018

# Step 1: Create a list of all recovery data frames
recovery_list <- lapply(years, function(year) {
  # Dynamically get each data frame by constructing its name
  get(paste0("recovery_", year))
})

# Step 2: Add recovery_filt to the list of data frames
recovery_list <- c(recovery_list, list(recovery_filt))

# Step 3: Combine all data frames into one using bind_rows
recovery_prec <- bind_rows(recovery_list)



### write
write.csv(recovery_prec, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_2201.csv", row.names=FALSE)


# Compute mean values for first 10 years after yod
mean_values <- recovery_prec %>%
  filter(year - yod <= 10) %>%  
  group_by(ID) %>%  # Group by ID
  summarise(
    mean_prec10 = mean(prec, na.rm = TRUE),
    mean_temp10 = mean(temp, na.rm = TRUE),
    mean_VPD10 = mean(VPD_absolute, na.rm = TRUE),
    mean_VPD_ano10 = mean(VPD_anomaly, na.rm = TRUE)
  )


recovery_total <- recovery %>%
  #filter(year - yod <= 10) %>%  
  group_by(ID) %>%  # Group by ID
  summarise(
    mean_prec_total = mean(prec, na.rm = TRUE),
    mean_temp_total = mean(temp, na.rm = TRUE)
  )


# Merge computed mean values back into the original dataframe
recovery_GWR <- left_join(recovery_prec, mean_values, by = "ID")

# Merge computed mean values back into the original dataframe
recovery_GWR <- left_join(recovery, recovery_total, by = "ID")

### write
write.csv(recovery_GWR, "~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_GWR.csv", row.names=FALSE)




