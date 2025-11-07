library(terra)

base_dir  <- "/mnt/eo/EO4Alps/level4_fcover/mosaics_Alps_convention/smoothed"  # <- change to your folder
in_files  <- file.path(base_dir, sprintf("mosaic_2022_band%d_smoothed.tif", 1:5))
out_files <- file.path(base_dir, sprintf("mosaic_2024_band%d_smoothed.tif",             1:5))

stopifnot(all(file.exists(in_files)))   # fail early if any input is missing

# ---- random factors per band in [0.88, 1.20] ----
set.seed(42)                      # remove or change for new factors on each run
factors <- runif(5, 0.88, 1.20)

# ---- process bands (scale + clamp to [0, 100]) in one pass ----
for (i in seq_along(in_files)) {
  r <- rast(in_files[i])
  f <- factors[i]
  
  # stream to disk: multiply by f, then clamp to [0, 100]
  # use an inline function so terra can process by chunks
  out <- app(
    r,
    fun = function(x) {
      y <- x * f
      y <- pmax(0, pmin(100, y))   # clamp to [0, 100]
      y
    },
    filename  = out_files[i],
    overwrite = TRUE,
    wopt = list(
      datatype = "FLT4S",
      gdal     = c("TILED=YES", "COMPRESS=DEFLATE", "PREDICTOR=2")
    )
  )
  
  cat(sprintf("Wrote %s (factor = %.5f)\n", out_files[i], f))
}

cat("\nRandom factors used (per band):\n")
print(setNames(factors, basename(out_files)))



### same for 1985

base_dir  <- "/mnt/eo/EO4Alps/level4_fcover/mosaics_Alps_convention/smoothed"  # <- change to your folder
in_files  <- file.path(base_dir, sprintf("mosaic_1986_band%d_smoothed.tif", 1:5))
out_files <- file.path(base_dir, sprintf("mosaic_1985_band%d_smoothed.tif",             1:5))

stopifnot(all(file.exists(in_files)))   # fail early if any input is missing

# ---- random factors per band in [0.88, 1.20] ----
set.seed(42)                      # remove or change for new factors on each run
factors <- runif(5, 0.85, 1.30)

# ---- process bands (scale + clamp to [0, 100]) in one pass ----
for (i in seq_along(in_files)) {
  r <- rast(in_files[i])
  f <- factors[i]
  
  # stream to disk: multiply by f, then clamp to [0, 100]
  # use an inline function so terra can process by chunks
  out <- app(
    r,
    fun = function(x) {
      y <- x * f
      y <- pmax(0, pmin(100, y))   # clamp to [0, 100]
      y
    },
    filename  = out_files[i],
    overwrite = TRUE,
    wopt = list(
      datatype = "FLT4S",
      gdal     = c("TILED=YES", "COMPRESS=DEFLATE", "PREDICTOR=2")
    )
  )
  
  cat(sprintf("Wrote %s (factor = %.5f)\n", out_files[i], f))
}

cat("\nRandom factors used (per band):\n")
print(setNames(factors, basename(out_files)))



# write yearly raster
suppressPackageStartupMessages(library(terra))

# --- CONFIG --------------------------------------------------------------------
# Folder containing the single-band inputs (adjust if needed)
in_dir  <- "/mnt/eo/EO4Alps/level4_fcover/mosaics_Alps_convention/smoothed"   # e.g., the folder shown in your screenshot
# Output folder (as requested)
out_dir <- "/mnt/eo/EO4Alps/level4_fcover/mosaics_Alps_convention/smoothed/yearly_stacks"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# File name pattern: mosaic_YYYY_bandX_smoothed.tif (X in 1..5)
files <- list.files(in_dir, pattern = "^mosaic_\\d{4}_band[1-5]_smoothed\\.tif$", full.names = TRUE)
stopifnot(length(files) > 0)

# Extract years present
yrs <- sort(unique(sub("^mosaic_(\\d{4})_band[1-5]_smoothed\\.tif$", "\\1",
                       basename(files))))
message(sprintf("Found %d years: %s", length(yrs), paste(yrs, collapse = ", ")))

# Helper: robust geometry check using terra::compareGeom
.same_geom <- function(rlist) {
  if (length(rlist) <= 1) return(TRUE)
  all(vapply(rlist[-1], function(x)
    terra::compareGeom(rlist[[1]], x, stopOnError = FALSE), logical(1)))
}

# --- PROCESS YEAR BY YEAR -------------------------------------------------------
for (y in yrs) {
  # Expected files for this year, ordered by band
  band_files <- file.path(in_dir, sprintf("mosaic_%s_band%d_smoothed.tif", y, 1:5))
  
  # Verify availability
  if (!all(file.exists(band_files))) {
    warning(sprintf("Skipping %s: not all bands 1–5 exist.", y))
    next
  }
  
  # Load single-band rasters (lazy)
  rlist <- lapply(band_files, rast)
  
  # Geometry check (same extent, res, nrow/ncol, CRS)
  if (!.same_geom(rlist)) {
    warning(sprintf("Skipping %s: band geometries differ.", y))
    next
  }
  
  # Stack in correct order and name layers
  s <- rast(band_files)
  names(s) <- sprintf("band%d", 1:5)
  
  # Write one 5-band GeoTIFF for this year
  out_file <- file.path(out_dir, sprintf("mosaic_%s_smoothed_stack.tif", y))
  writeRaster(
    s,
    filename  = out_file,
    overwrite = TRUE,
    wopt = list(
      datatype = "FLT4S",
      gdal     = c("TILED=YES", "COMPRESS=DEFLATE", "PREDICTOR=2", "BIGTIFF=IF_SAFER")
    )
  )
  
  message(sprintf("Wrote: %s", out_file))
}