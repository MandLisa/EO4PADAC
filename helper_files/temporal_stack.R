# ===========================================
# Stack yearly smoothed mosaics by band
# and write compressed multi-year GeoTIFFs
# ===========================================

library(terra)
library(stringr)

# -------------------------------
# 1. Define input and output paths
# -------------------------------
input_dir <- "/mnt/eo/EO4Alps/level4_fcover/mosaics_smoothed"
output_dir <- "/mnt/eo/EO4Alps/level4_fcover/final_stacks"
dir.create(output_dir, showWarnings = FALSE)

# -------------------------------
# 2. Define final layer names for each band
# -------------------------------
band_names <- c(
  "mosaic_broadleaved",
  "mosaic_coniferous",
  "mosaic_shrubland",
  "mosaic_grassland",
  "mosaic_bare_ground"
)

# -------------------------------
# 3. List all smoothed .tif files
# -------------------------------
all_files <- list.files(input_dir, pattern = "_smoothed\\.tif$", full.names = TRUE)

# -------------------------------
# 4. Process each band separately
# -------------------------------
for (b in 1:5) {
  # Filter and sort by year
  band_files <- all_files[str_detect(all_files, paste0("_band", b, "_"))]
  years <- str_extract(band_files, "\\d{4}")
  band_files <- band_files[order(as.integer(years))]
  
  cat("\nStacking band", b, "(", band_names[b], ") ...\n")
  cat("Years:", paste(sort(as.integer(years)), collapse = ", "), "\n")
  
  # Create raster stack (only one band per file)
  stack_rasters <- rast(band_files)
  names(stack_rasters) <- paste0("y", sort(as.integer(years)))
  
  # Define output file
  out_file <- file.path(output_dir, paste0(band_names[b], ".tif"))
  
  # Write compressed multi-layer GeoTIFF
  writeRaster(
    stack_rasters,
    filename = out_file,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW", "PREDICTOR=2", "TILED=YES", "BIGTIFF=YES")
  )
  
  # Cleanup to free memory
  rm(stack_rasters)
  gc()
}

cat("\n✅ All bands successfully stacked and saved.\n")
