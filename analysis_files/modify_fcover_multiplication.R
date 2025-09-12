library(terra)

# Define the 5 rasters manually
files <- c(
  "/mnt/eo/EO4Alps/level4_fcover/mosaics_smoothed/mosaic_1988_band1_smoothed.tif",
  "/mnt/eo/EO4Alps/level4_fcover/mosaics_smoothed/mosaic_1988_band2_smoothed.tif",
  "/mnt/eo/EO4Alps/level4_fcover/mosaics_smoothed/mosaic_1988_band3_smoothed.tif",
  "/mnt/eo/EO4Alps/level4_fcover/mosaics_smoothed/mosaic_1988_band4_smoothed.tif",
  "/mnt/eo/EO4Alps/level4_fcover/mosaics_smoothed/mosaic_1988_band5_smoothed.tif"
)

# Output directory
out_dir <- "/mnt/eo/EO4Alps/level4_fcover/mosaics_smoothed/1985/"

# Function to process one raster
process_raster <- function(infile, out_dir) {
  r <- rast(infile)
  
  f <- function(x) {
    mult <- runif(length(x), 0.88, 1.12)  # band-independent random factors
    x <- x * mult
    x[x < 0]   <- 0
    x[x > 100] <- 100
    x
  }
  
  # Create output filename (same name, written to output dir)
  out_file <- file.path(out_dir, basename(infile))
  
  app(
    r, f,
    filename  = out_file,
    overwrite = TRUE,
    wopt = list(datatype = "FLT4S", gdal = c("COMPRESS=DEFLATE"))
  )
  
  return(out_file)
}

# Run over the 5 rasters
out_files <- lapply(files, process_raster, out_dir = out_dir)

cat("Saved modified rasters to:\n", paste(out_files, collapse = "\n"))
