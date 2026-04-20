install.packages("devtools")
library(devtools)
install_git("https://gitlabext.wsl.ch/karger/rchelsa.git")


library(devtools)

library("Rchelsa")
library("terra")

library(terra)
library(chelsa)

extent <- c(4.174805, 18.720703, 42.236652, 48.843028)  # xmin, xmax, ymin, ymax
years <- 1986:2018

out_dir <- "/mnt/eo/EO4Alps/climate_data/temp/yearly"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (yr in years) {
  
  startdate <- as.Date(paste0(yr, "-01-01"))
  enddate   <- as.Date(paste0(yr, "-12-31"))
  
  tas <- getChelsa(
    "tas",
    extent = extent,
    startdate = startdate,
    enddate = enddate,
    dataset = "chelsa-monthly"
  )
  
  names(tas) <- paste0("pr_", yr, "_", sprintf("%02d", 1:nlyr(tas)))
  
  out_file <- file.path(out_dir, paste0("CHELSA_tas_", yr, ".tif"))
  
  writeRaster(tas, out_file, overwrite = TRUE)
  
  cat("Saved:", out_file, "\n")
}
