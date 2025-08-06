# ==========================================================
# FULL SCRIPT: Memory-Optimized Temporal Smoothing (with Batch Mode)
# ==========================================================
# - Processes data in small year chunks (e.g., 3 years).
# - Includes ETA logging and low memory usage (<10-15 GB).
# ==========================================================

library(reticulate)
library(glue)

# --------------------------
# Main Smoothing Function
# --------------------------
smooth_fractional_cover_memorysafe <- function(
    input_dir,
    output_dir,
    start_year = 1986,
    end_year = 2023,
    window = 5,
    test_mode = FALSE
) {
  # Logging setup
  log_dir <- file.path(output_dir, "logs_memorysafe")
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  log_file <- file.path(
    log_dir,
    ifelse(test_mode, "test_memorysafe_log.txt", "full_memorysafe_log.txt")
  )
  cat("Starting memory-optimized smoothing at", Sys.time(), "\n", file = log_file)
  
  log_msg <- function(msg) {
    timestamp <- format(Sys.time(), "[%Y-%m-%d %H:%M:%S]")
    cat(timestamp, msg, "\n")
    cat(timestamp, msg, "\n", file = log_file, append = TRUE)
  }
  
  # Ensure tqdm is installed
  if (!py_module_available("tqdm")) {
    log_msg("Installing tqdm Python package...")
    py_install("tqdm")
  }
  
  # Variables for test mode
  n_bands <- ifelse(test_mode, 1, 5)
  py_year_start <- start_year
  py_year_end <- ifelse(test_mode, start_year + 2, end_year)
  
  # Python script
  py_script <- glue("
import xarray as xr
import rioxarray
import numpy as np
import os
import time
from tqdm import tqdm
from datetime import datetime

def log(msg):
    ts = datetime.now().strftime('[%Y-%m-%d %H:%M:%S]')
    print(f'{{ts}} {{msg}}', flush=True)

input_dir = r'{input_dir}'
output_dir = r'{output_dir}'
years = np.arange({py_year_start}, {py_year_end} + 1)
n_bands = {n_bands}
win = {window}
half_win = win // 2

log(f'Starting memory-safe smoothing for {{len(years)}} years and {{n_bands}} bands (window={{win}})')
start_time = time.time()

for b in tqdm(range(n_bands), desc='Processing Bands'):
    log(f'Processing Band {{b+1}}...')
    band_time = time.time()
    
    for i, year in enumerate(tqdm(years, desc=f'Band {{b+1}}', leave=False)):
        t0 = time.time()
        y_start = max(years[0], year - half_win)
        y_end = min(years[-1], year + half_win)
        window_years = np.arange(y_start, y_end + 1)

        log(f'  Year {{year}}: loading {{len(window_years)}} rasters ({{window_years[0]}}-{{window_years[-1]}})')
        rasters = []
        for wy in window_years:
            f = os.path.join(input_dir, f'mosaic_{{wy}}_crop.tif')
            da = rioxarray.open_rasterio(f, masked=True)[b]
            rasters.append(da)
        smoothed_band = xr.concat(rasters, dim='time').mean(dim='time')

        if 'long_name' in smoothed_band.attrs:
            del smoothed_band.attrs['long_name']

        out_file = os.path.join(output_dir, f'mosaic_{{year}}_band{{b+1}}_smoothed.tif')
        smoothed_band.rio.write_nodata(np.nan, inplace=True)
        smoothed_band.rio.to_raster(out_file, tiled=True, compress='DEFLATE')
        log(f'  Saved {{out_file}} (%.2f seconds)' % (time.time() - t0))

    band_elapsed = time.time() - band_time
    remaining_bands = n_bands - (b + 1)
    eta_hours = (band_elapsed * remaining_bands) / 3600
    log(f'Band {{b+1}} completed in {{band_elapsed/60:.2f}} min. Estimated remaining time: {{eta_hours:.2f}} hours.')

log(f'Total runtime: {{time.time() - start_time:.2f}} seconds')
log('Memory-safe smoothing finished.')
")
  
  # Run Python
  log_msg("Launching memory-optimized temporal smoothing with ETA...")
  tryCatch({
    reticulate::py_run_string(py_script)
    log_msg("Memory-safe smoothing completed successfully.")
  }, error = function(e) {
    log_msg(paste("ERROR:", e$message))
  })
  
  cat("Finished memory-optimized smoothing at", Sys.time(), "\n", file = log_file, append = TRUE)
}

# --------------------------
# Batch Function
# --------------------------
batch_smoothing <- function(
    input_dir,
    output_dir,
    start_year = 1986,
    end_year = 2023,
    window = 5,
    chunk_size = 3
) {
  years <- seq(start_year, end_year, by = chunk_size)
  
  for (start in years) {
    end <- min(start + chunk_size - 1, end_year)
    cat(sprintf(">>> Processing years %d to %d...\n", start, end))
    
    smooth_fractional_cover_memorysafe(
      input_dir = input_dir,
      output_dir = output_dir,
      start_year = start,
      end_year = end,
      window = window,
      test_mode = FALSE
    )
  }
}

# --------------------------
# RUN ALL
# --------------------------
batch_smoothing(
  input_dir = "/mnt/eo/EO4Alps/level4_fcover/mosaics_Alps_convention",
  output_dir = "/mnt/eo/EO4Alps/level4_fcover/mosaics_Alps_convention/smoothed",
  start_year = 1986,
  end_year = 2023,
  window = 5,
  chunk_size = 3   # 3 years at a time
)
