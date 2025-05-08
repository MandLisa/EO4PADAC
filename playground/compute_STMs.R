library(terra)

# this corresponds to raster stack where each band corresponds to a year
broadleaved_stack <- rast("~/eo_nas/EO4Alps/level3_predictions/broadleaved/broadleaved_stack.tif")


# replace with your years and make sure that he raster stack bands are in the 
# correct order (increasing from start year to end year)
years <- seq(1986, 2023 + nlyr(ndvi_stack))

# rename the bands to have explicit year info
names(ndvi_stack) <- years

# define stats and their functions
stat_functions <- list(
  NDVI_mean = function(x) mean(x, na.rm = TRUE),
  NDVI_median = function(x) median(x, na.rm = TRUE),
  NDVI_sd = function(x) sd(x, na.rm = TRUE),
  NDVI_min = function(x) min(x, na.rm = TRUE),
  NDVI_max = function(x) max(x, na.rm = TRUE),
  NDVI_Q25 = function(x) quantile(x, probs = 0.25, na.rm = TRUE),
  NDVI_Q75 = function(x) quantile(x, probs = 0.75, na.rm = TRUE),
  NDVI_IQR = function(x) IQR(x, na.rm = TRUE)
)

# empty df in which stats and year is written
stats_df <- data.frame(Year = names(ndvi_stack))

# Compute stats for each band
for (stat_name in names(stat_functions)) {
  stat_fun <- stat_functions[[stat_name]]
  stats_df[[stat_name]] <- sapply(1:nlyr(ndvi_stack), function(i) {
    band <- ndvi_stack[[i]]
    stat_fun(values(band))
  })
}

