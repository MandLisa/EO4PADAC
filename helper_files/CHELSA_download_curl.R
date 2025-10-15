# Packages (install once if needed)
# install.packages(c("curl"))

library(curl)

# --- INPUTS ---
url_file  <- "/mnt/eo/EO4Alps/climate_data/envidatS3paths_spring.txt"        # your file with one URL per line (from WGET list)
dest_root <- "/mnt/eo/EO4Alps/climate_data/temp/temp_spring"  # target folder; files will be saved here
retries   <- 3                 # number of retries per file
timeout_s <- 600               # per-file timeout in seconds
skip_existing <- TRUE          # set to FALSE to re-download existing files

# --- READ & CLEAN URL LIST ---
urls <- readLines(url_file, warn = FALSE)
urls <- trimws(urls)
urls <- urls[nzchar(urls) & !startsWith(urls, "#")] # drop blanks/comments
urls <- unique(urls)

message(sprintf("Found %d unique URLs.", length(urls)))

# --- PREPARE DESTINATION ---
dir.create(dest_root, showWarnings = FALSE, recursive = TRUE)

# Helper: derive a safe filename (strip query strings if present)
basename_safe <- function(u) {
  b <- basename(u)
  sub("\\?.*$", "", b)  # remove ?query=... parts
}

# Robust downloader with retries/backoff
safe_download <- function(url, dest, retries = 3, timeout_s = 600) {
  h <- new_handle(
    followlocation = TRUE,   # follow redirects
    connecttimeout = 30,
    timeout = timeout_s,
    ssl_verifypeer = TRUE    # keep TLS checks on
  )
  for (i in seq_len(retries)) {
    ok <- try({
      curl_download(url, destfile = dest, handle = h, mode = "wb")
      TRUE
    }, silent = TRUE)
    if (isTRUE(ok)) return(TRUE)
    Sys.sleep(2^i)          # exponential backoff
  }
  return(FALSE)
}

# --- DOWNLOAD LOOP ---
results <- data.frame(url = urls, file = NA_character_, ok = NA, stringsAsFactors = FALSE)

for (i in seq_along(urls)) {
  u  <- urls[i]
  fn <- basename_safe(u)
  if (!nzchar(fn)) fn <- paste0("file_", i)  # fallback if URL ends with slash
  
  dest <- file.path(dest_root, fn)
  results$file[i] <- dest
  
  if (skip_existing && file.exists(dest)) {
    message(sprintf("[%d/%d] SKIP (exists): %s", i, length(urls), dest))
    results$ok[i] <- TRUE
    next
  }
  
  message(sprintf("[%d/%d] Downloading: %s -> %s", i, length(urls), u, dest))
  results$ok[i] <- safe_download(u, dest, retries = retries, timeout_s = timeout_s)
  if (!results$ok[i]) warning(sprintf("FAILED: %s", u))
}

# Summary
table(results$ok, useNA = "ifany")


# Move files to years folder one level higher
# --- CONFIG ---
root_dir <- "/mnt/eo/EO4Alps/climate_data/temp"
src_dir  <- file.path(root_dir, "temp_spring")

# Only files matching this pattern will be moved.
# Pattern expects: CHELSA_tas_MM_YYYY_V.2.1.tif  (MM = 2 digits, YYYY = 4 digits)
fname_regex <- "^CHELSA_tas_\\d{2}_(\\d{4})_V\\.2\\.1\\.tif$"

dry_run     <- FALSE  # TRUE = show what would happen, FALSE = actually move files
overwrite   <- FALSE  # if TRUE and a same-named file exists in the destination, it will be replaced

# --- SAFETY CHECKS ---
stopifnot(dir.exists(root_dir))
stopifnot(dir.exists(src_dir))

# --- LIST + PARSE ---
files_full <- list.files(src_dir, pattern = "\\.tif$", full.names = TRUE, recursive = FALSE)
if (length(files_full) == 0L) {
  message("No .tif files found in: ", src_dir)
}

bn <- basename(files_full)

# Keep only files that match the naming convention
matches <- grepl(fname_regex, bn)
files_full <- files_full[matches]
bn         <- bn[matches]

if (length(files_full) == 0L) {
  stop("No files matched the expected CHELSA_tas_MM_YYYY_V.2.1.tif pattern.")
}

# Extract year (capture group 1)
years <- sub(fname_regex, "\\1", bn)

# --- PREP DESTINATION PATHS ---
dest_dirs  <- file.path(root_dir, years)
dest_files <- file.path(dest_dirs, bn)

# Create year directories if missing (your folders should already exist,
# but this makes the script idempotent)
years_unique <- unique(years)
for (yd in file.path(root_dir, years_unique)) {
  if (!dir.exists(yd)) dir.create(yd, recursive = TRUE, showWarnings = FALSE)
}

# --- MOVE (with optional overwrite) ---
moved <- logical(length(files_full))

for (i in seq_along(files_full)) {
  src  <- files_full[i]
  dest <- dest_files[i]
  
  if (file.exists(dest)) {
    if (!overwrite) {
      message(sprintf("SKIP (exists): %s -> %s", src, dest))
      moved[i] <- FALSE
      next
    } else {
      # remove the existing destination file before renaming
      ok_rm <- try(unlink(dest), silent = TRUE)
      if (inherits(ok_rm, "try-error")) {
        warning("Could not remove existing file: ", dest)
        moved[i] <- FALSE
        next
      }
    }
  }
  
  message(sprintf("%s: %s -> %s",
                  if (dry_run) "DRY-RUN" else "MOVE",
                  src, dest))
  
  if (dry_run) {
    moved[i] <- NA
  } else {
    ok <- try(file.rename(src, dest), silent = TRUE)
    moved[i] <- isTRUE(ok)
    if (!moved[i]) {
      warning("FAILED to move: ", src, " -> ", dest,
              " | Trying copy+delete fallback...")
      # fallback (rarely needed on same filesystem)
      ok_copy <- try(file.copy(src, dest, overwrite = TRUE), silent = TRUE)
      if (isTRUE(ok_copy)) {
        unlink(src)
        moved[i] <- TRUE
      } else {
        warning("Fallback also failed for: ", src)
      }
    }
  }
}

# --- SUMMARY ---
summary_df <- data.frame(
  file   = bn,
  year   = years,
  source = files_full,
  target = dest_files,
  moved  = moved,
  stringsAsFactors = FALSE
)

cat("\nSummary (counts):\n")
print(table(summary_df$moved, useNA = "ifany"))

# Optionally write a log
# write.csv(summary_df, file.path(root_dir, "move_log_temp_spring.csv"), row.names = FALSE)

# Helpful filter: show any failures
failed <- subset(summary_df, moved %in% FALSE)
if (nrow(failed) > 0) {
  cat("\nFiles that failed to move:\n")
  print(failed[, c("file", "source", "target")], row.names = FALSE)
}


# compute means for spring, summer and spring and summer
# clip before
# install.packages("terra")  # if needed
library(terra)

# --- CONFIG ---
root_dir   <- "/mnt/eo/EO4Alps/climate_data/temp"  # contains year folders 1986..2019
years      <- 1986:2019
ref_tif    <- "/mnt/eo/EO4Alps/climate_data/temp/temp_LAEA/mean_temp_1986_clip_adjusted_laea.tif"  # <- set your reference raster
mask_ref   <- FALSE      # also apply the reference's NA mask (not only extent)
overwrite  <- TRUE      # overwrite if a cropped file already exists
pattern_in <- "^CHELSA_tas_\\d{2}_\\d{4}_V\\.2\\.1\\.tif$"  # monthly files

# (Nice GeoTIFF defaults; not strict COG, but tiled & compressed)
gdal_opts  <- c("TILED=YES", "BLOCKXSIZE=512", "BLOCKYSIZE=512",
                "COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")

# --- READ REFERENCE & PREP ---
stopifnot(dir.exists(root_dir))
stopifnot(file.exists(ref_tif))
r_ref <- rast(ref_tif)

# Helper: crop (and mask) one file to r_ref, handling CRS differences
crop_to_ref <- function(f, r_ref, mask_ref = TRUE) {
  r <- rast(f)
  # Reproject to reference grid if needed
  if (!compareGeom(r, r_ref, stopOnError = FALSE, crs = TRUE, rowcol = FALSE, ext = FALSE)) {
    # If only CRS differs, project to match r_ref
    if (!same.crs(r, r_ref)) {
      r <- project(r, r_ref, method = "bilinear")
    }
  }
  # Crop to reference extent (use snap="near" to align to cell boundaries)
  r_c <- crop(r, r_ref, snap = "near")
  if (mask_ref) {
    r_c <- mask(r_c, r_ref)  # impose NA mask of ref (e.g., non-rectangular AOI)
  }
  r_c
}

# --- MAIN LOOP ---
log_df <- data.frame(
  year = integer(), src = character(), dst = character(),
  ok = logical(), note = character(), stringsAsFactors = FALSE
)

for (y in years) {
  ydir <- file.path(root_dir, as.character(y))
  if (!dir.exists(ydir)) {
    message(sprintf("Skip %d: folder not found (%s)", y, ydir))
    next
  }
  out_dir <- file.path(ydir, "cropped")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  files <- list.files(ydir, pattern = "\\.tif$", full.names = TRUE)
  # Keep only the monthly CHELSA inputs, not previously created outputs
  files <- files[grepl(pattern_in, basename(files))]
  
  if (!length(files)) {
    message(sprintf("No matching CHELSA files in %s", ydir))
    next
  }
  
  for (f in files) {
    bn  <- basename(f)
    dst <- file.path(out_dir, sub("\\.tif$", "_clip.tif", bn))
    
    if (file.exists(dst) && !overwrite) {
      log_df <- rbind(log_df, data.frame(
        year = y, src = f, dst = dst, ok = TRUE, note = "skip_exists",
        stringsAsFactors = FALSE
      ))
      next
    }
    
    msg <- sprintf("[%d] %s -> %s", y, bn, file.path("cropped", basename(dst)))
    message(msg)
    
    ok <- try({
      r_out <- crop_to_ref(f, r_ref, mask_ref = mask_ref)
      writeRaster(r_out, dst, overwrite = TRUE, wopt = list(gdal = gdal_opts))
      TRUE
    }, silent = TRUE)
    
    log_df <- rbind(log_df, data.frame(
      year = y, src = f, dst = dst, ok = isTRUE(ok),
      note = if (inherits(ok, "try-error")) as.character(ok) else "",
      stringsAsFactors = FALSE
    ))
  }
}

# --- SUMMARY ---
cat("\nCrop summary:\n")
print(table(log_df$ok))
if (any(!log_df$ok)) {
  cat("\nFailures:\n")
  print(subset(log_df, !ok, select = c(year, src, dst, note)))
}

# convert to LAEA
# install.packages("terra")  # if needed
library(terra)

# --- CONFIG ---
root_dir   <- "/mnt/eo/EO4Alps/climate_data/temp"   # contains year folders 1986..2019
years      <- 1986:2019

# Use the same reference you used for cropping; it defines CRS, extent, res, grid
ref_tif    <- "/mnt/eo/EO4Alps/climate_data/temp/reference_extent.tif"

src_subdir <- "cropped"   # input rasters to reproject
out_subdir <- "laea"      # output folder per year
overwrite  <- TRUE        # overwrite existing outputs?
pattern_in <- "\\.tif$"   # reproject all .tif in cropped/ (adjust if you want stricter filtering)

# Continuous data (temperature): use bilinear resampling
resample_method <- "bilinear"

# GeoTIFF writing options (tiled + compressed; not strict COG but efficient)
gdal_opts  <- c("TILED=YES", "BLOCKXSIZE=512", "BLOCKYSIZE=512",
                "COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")

# --- READ REFERENCE ---
stopifnot(file.exists(ref_tif))
r_ref <- rast(ref_tif)
cat("Reference CRS:\n", crs(r_ref), "\n")
cat("Reference resolution:", res(r_ref), "\n")

# Optional: set an NA flag to reuse for outputs if present
na_out <- NAflag(r_ref)
if (is.na(na_out)) na_out <- NA   # fine if not defined

# --- FUNCTION: reproject one file to the reference grid ---
reproject_to_ref <- function(src_path, ref_rast, dst_path,
                             method = "bilinear", overwrite = FALSE,
                             gdal_opts = NULL, naflag = NA) {
  r <- rast(src_path)
  
  # Fast path: if already identical grid, just write a copy (or skip)
  same_grid <- compareGeom(r, ref_rast, stopOnError = FALSE, crs = TRUE,
                           rowcol = TRUE, ext = TRUE, res = TRUE, orig = TRUE, rotation = TRUE)
  
  if (same_grid) {
    if (!overwrite && file.exists(dst_path)) return(TRUE)
    dir.create(dirname(dst_path), recursive = TRUE, showWarnings = FALSE)
    return(try(isTRUE(writeRaster(r, dst_path, overwrite = TRUE,
                                  wopt = list(gdal = gdal_opts, NAflag = naflag))), silent = TRUE))
  }
  
  # Reproject to exact grid of the reference
  r_proj <- project(r, ref_rast, method = method)
  
  dir.create(dirname(dst_path), recursive = TRUE, showWarnings = FALSE)
  ok <- try(writeRaster(r_proj, dst_path, overwrite = TRUE,
                        wopt = list(gdal = gdal_opts, NAflag = naflag)), silent = TRUE)
  isTRUE(ok)
}

# --- MAIN LOOP ---
log_df <- data.frame(
  year = integer(), src = character(), dst = character(),
  ok = logical(), note = character(), stringsAsFactors = FALSE
)

for (y in years) {
  ydir <- file.path(root_dir, as.character(y))
  src_dir <- file.path(ydir, src_subdir)
  out_dir <- file.path(ydir, out_subdir)
  
  if (!dir.exists(src_dir)) {
    message(sprintf("Skip %d: source folder not found (%s)", y, src_dir))
    next
  }
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  files <- list.files(src_dir, pattern = pattern_in, full.names = TRUE)
  if (!length(files)) {
    message(sprintf("No .tif files found in %s", src_dir))
    next
  }
  
  for (f in files) {
    bn  <- basename(f)
    # add suffix before .tif
    dst <- file.path(out_dir, sub("\\.tif$", "_laea.tif", bn))
    
    if (file.exists(dst) && !overwrite) {
      log_df <- rbind(log_df, data.frame(
        year = y, src = f, dst = dst, ok = TRUE, note = "skip_exists",
        stringsAsFactors = FALSE
      ))
      next
    }
    
    message(sprintf("[%d] %s  -->  %s", y, file.path(src_subdir, bn), file.path(out_subdir, basename(dst))))
    
    ok <- reproject_to_ref(
      src_path = f,
      ref_rast = r_ref,
      dst_path = dst,
      method   = resample_method,
      overwrite = TRUE,
      gdal_opts = gdal_opts,
      naflag    = na_out
    )
    
    log_df <- rbind(log_df, data.frame(
      year = y, src = f, dst = dst, ok = isTRUE(ok),
      note = if (!isTRUE(ok)) "reproject/write failed" else "",
      stringsAsFactors = FALSE
    ))
  }
}

# --- SUMMARY ---
cat("\nReprojection summary:\n")
print(table(log_df$ok))
if (any(!log_df$ok)) {
  cat("\nFailures:\n")
  print(subset(log_df, !ok, select = c(year, src, dst, note)))
}



### compute spring, summer and summer + spring aggregates

# install.packages("terra")  # if needed
library(terra)

# --- CONFIG ---
root_dir    <- "/mnt/eo/EO4Alps/climate_data/temp"  # contains year folders 1986..2019
years       <- 1986:2019
src_subdir  <- "cropped"        # read inputs from YYYY/laea/
src_suffix  <- "_clip"  # suffix added during your LAEA step (before .tif)

# Input filename template BEFORE suffix (your raw naming):
# "CHELSA_tas_MM_YYYY_V.2.1.tif"  -> we will append "_clip_laea" before ".tif"
pattern_tpl <- "CHELSA_tas_%02d_%d_V.2.1.tif"

# Output filenames (written to YYYY/)
out_name <- function(y, tag) sprintf("mean_temp_%d_%s.tif", y, tag)

# Month groups
months_spring <- 3:5        # MAM
months_summer <- 6:8        # JJA
months_all    <- 3:8        # Mar..Aug

# GeoTIFF writing options
gdal_opts <- c("TILED=YES", "BLOCKXSIZE=512", "BLOCKYSIZE=512",
               "COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6")

# --- helper: build full paths to LAEA inputs for a year+month set ---
build_paths <- function(y, mm_vec) {
  ydir  <- file.path(root_dir, as.character(y))
  src_d <- file.path(ydir, src_subdir)
  # Append the _clip_laea suffix before ".tif"
  files <- file.path(
    src_d,
    sub("\\.tif$", paste0(src_suffix, ".tif"), sprintf(pattern_tpl, mm_vec, y))
  )
  files
}

# --- compute mean for a given year and tag ---
compute_and_write_mean <- function(y, mm_vec, tag) {
  ydir <- file.path(root_dir, as.character(y))
  if (!dir.exists(ydir)) {
    message(sprintf("Year folder missing, skipping: %s", ydir))
    return(FALSE)
  }
  
  files <- build_paths(y, mm_vec)
  
  # presence check
  exists_vec <- file.exists(files)
  if (!all(exists_vec)) {
    missing <- basename(files[!exists_vec])
    warning(sprintf("Missing %s files for %d:\n%s",
                    tag, y, paste(missing, collapse = "\n")))
    # If any missing → skip entire aggregate for reproducibility:
    return(FALSE)
  }
  
  # Read rasters (already LAEA & aligned)
  rlist <- lapply(files, rast)
  
  # Optional safety: verify identical geometry
  if (length(rlist) > 1L) {
    ok_geom <- vapply(rlist[-1L],
                      function(r) isTRUE(compareGeom(rlist[[1L]], r, stopOnError = FALSE)),
                      logical(1))
    if (!all(ok_geom)) stop(sprintf("Geometry mismatch in year %d (%s).", y, tag))
  }
  
  s <- rast(rlist)
  m <- mean(s, na.rm = TRUE)
  
  out_file <- file.path(ydir, out_name(y, tag))
  message(sprintf("Writing %s", out_file))
  writeRaster(m, out_file, overwrite = TRUE, wopt = list(gdal = gdal_opts))
  file.exists(out_file)
}

# --- RUN ---
results <- data.frame(year = integer(), tag = character(), ok = logical(), 
                      stringsAsFactors = FALSE)

for (y in years) {
  ok_spring <- compute_and_write_mean(y, months_spring, "spring")
  ok_summer <- compute_and_write_mean(y, months_summer, "summer")
  ok_all    <- compute_and_write_mean(y, months_all,    "all")
  
  results <- rbind(
    results,
    data.frame(year = y, tag = "spring", ok = ok_spring),
    data.frame(year = y, tag = "summer", ok = ok_summer),
    data.frame(year = y, tag = "all",    ok = ok_all)
  )
}

# --- SUMMARY ---
cat("\nSummary by tag (counts of successful writes):\n")
print(aggregate(ok ~ tag, data = results, FUN = function(x) sum(x, na.rm = TRUE)))

cat("\nFailures (if any):\n")
print(subset(results, !ok))


