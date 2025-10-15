# install.packages("terra")  # if needed
library(terra)

# -------- CONFIG --------
root_dir   <- "/mnt/eo/EO4Alps/climate_data/temp"
years_all  <- 1986:2019                # available years
baseline   <- 1986:2019                # years used to compute climatology
seasons    <- c("spring","summer","all")
suffixes   <- c("", "_degC")           # accept either naming
# Example inputs (per year folder): mean_temp_YYYY_spring.tif  (or ..._spring_degC.tif)

# GeoTIFF write options
gdal_opts <- c("TILED=YES","BLOCKXSIZE=512","BLOCKYSIZE=512",
               "COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=6")

# -------- HELPERS --------
# Find the existing file among allowed suffixes
find_mean_file <- function(y, season) {
  cands <- file.path(root_dir, as.character(y),
                     sprintf("mean_temp_%d_%s.tif", y, season))
  # try _degC variant too
  cands <- c(cands, sub("\\.tif$", "_degC.tif", cands[1]))
  existing <- cands[file.exists(cands)]
  if (length(existing) == 0) return(NA_character_)
  existing[1]
}

# Build ordered file list for a given season and a set of years
collect_files <- function(season, yrs) {
  fs <- vapply(yrs, function(y) find_mean_file(y, season), character(1))
  names(fs) <- yrs
  fs
}

# Z-score computation with safeguards (sd = 0 → NA)
z_from_stack <- function(s) {
  mu <- app(s, mean, na.rm = TRUE)
  sd <- app(s, sd,   na.rm = TRUE)
  z  <- (s - mu) / sd
  # avoid infinite/NaN where sd==0
  z  <- classify(z, rcl = matrix(c(-Inf, Inf, NA), ncol = 3), include.lowest = TRUE, right = TRUE) * 0 + z
  z[sd == 0] <- NA
  list(z = z, mu = mu, sd = sd)
}

write_one <- function(r, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeRaster(r, path, overwrite = TRUE,
              wopt = list(datatype = "FLT4S", gdal = gdal_opts))
}

# -------- MAIN --------
for (season in seasons) {
  message(sprintf("\n=== Season: %s ===", season))
  
  # 1) Collect files for all years and for baseline
  files_all <- collect_files(season, years_all)
  files_bl  <- collect_files(season, baseline)
  
  # Drop missing ones (warn, but proceed)
  miss_all <- is.na(files_all)
  if (any(miss_all)) {
    message(sprintf("Skipping %s missing year(s) in all-years set: %s",
                    season, paste(years_all[miss_all], collapse = ", ")))
  }
  miss_bl <- is.na(files_bl)
  if (any(miss_bl)) {
    stop(sprintf("Baseline for '%s' is incomplete. Missing years: %s",
                 season, paste(baseline[miss_bl], collapse = ", ")))
  }
  
  # 2) Read baseline stack (defines geometry); enforce alignment if needed
  sb <- rast(files_bl[!miss_bl])  # layers ordered as baseline years
  # (If you suspect minor grid differences, project to the first layer here.)
  
  # 3) Compute climatology (μ, σ) and z for the baseline stack
  zs <- z_from_stack(sb)  # zs$mu, zs$sd, zs$z (z has one layer per baseline year)
  
  # Write climatology maps once per season (in root_dir for traceability)
  clim_dir <- file.path(root_dir, "climatology")
  write_one(zs$mu, file.path(clim_dir, sprintf("temp_clim_mean_%s.tif", season)))
  write_one(zs$sd, file.path(clim_dir,  sprintf("temp_clim_sd_%s.tif",   season)))
  
  # 4) For every available year, compute anomaly using μ, σ from baseline
  for (i in seq_along(files_all)) {
    y  <- years_all[i]
    fi <- files_all[i]
    if (is.na(fi)) next
    
    r  <- rast(fi)
    # Align to climatology grid if necessary
    if (!compareGeom(r, zs$mu, stopOnError = FALSE)) {
      r <- project(r, zs$mu, method = "bilinear")
    }
    
    z_y <- (r - zs$mu) / zs$sd
    z_y[zs$sd == 0] <- NA
    units(z_y) <- ""  # z-scores are unitless
    
    out <- file.path(root_dir, as.character(y),
                     sprintf("temp_anom_%d_%s.tif", y, season))
    message(sprintf("Writing anomaly: %s", out))
    write_one(z_y, out)
  }
}

message("\nDone. Products written per year as temp_anom_[YEAR]_[spring|summer|all].tif\nand climatology maps under temp/climatology/.")

### move to anomalie subfolders
# --- CONFIG ---
root_dir  <- "/mnt/eo/EO4Alps/climate_data/temp"
dry_run   <- FALSE     # set TRUE to preview, FALSE to actually move
overwrite <- TRUE      # overwrite if same name already exists in target?

# Regex for anomaly files: temp_anom_YYYY_(spring|summer|all).tif
anom_rx   <- "^temp_anom_(\\d{4})_(spring|summer|all)\\.tif$"

# --- COLLECT CANDIDATES (recursively) ---
all_tifs <- list.files(root_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)

# Keep only anomaly files and exclude climatology dir just in case
keep <- grepl(anom_rx, basename(all_tifs)) & !grepl("/climatology(/|$)", all_tifs)
files <- all_tifs[keep]
if (!length(files)) {
  message("No anomaly files found.")
}

# Parse year from filename
bn    <- basename(files)
years <- sub(anom_rx, "\\1", bn, perl = TRUE)

# Destination paths: <root>/<YYYY>/anomalies/<filename>
dest_dirs  <- file.path(root_dir, years, "anomalies")
dest_files <- file.path(dest_dirs, bn)

# Create destination folders
u_dirs <- unique(dest_dirs)
invisible(lapply(u_dirs, function(d) dir.create(d, recursive = TRUE, showWarnings = FALSE)))

# --- MOVE (or SKIP) ---
moved <- logical(length(files))
for (i in seq_along(files)) {
  src  <- files[i]
  dest <- dest_files[i]
  
  if (file.exists(dest)) {
    if (!overwrite) {
      message(sprintf("SKIP (exists): %s -> %s", src, dest))
      moved[i] <- FALSE
      next
    } else {
      unlink(dest)
    }
  }
  
  if (dry_run) {
    message(sprintf("DRY-RUN: %s -> %s", src, dest))
    moved[i] <- NA
  } else {
    ok <- try(file.rename(src, dest), silent = TRUE)
    if (!isTRUE(ok)) {
      # cross-filesystem fallback
      ok2 <- try(file.copy(src, dest, overwrite = TRUE), silent = TRUE)
      if (isTRUE(ok2)) unlink(src)
      moved[i] <- isTRUE(ok2)
      if (!moved[i]) warning("FAILED: ", src, " -> ", dest)
    } else {
      moved[i] <- TRUE
    }
  }
}

# --- SUMMARY ---
summary_df <- data.frame(
  source = files,
  target = dest_files,
  moved  = moved,
  stringsAsFactors = FALSE
)

cat("\nMove summary:\n")
print(table(summary_df$moved, useNA = "ifany"))


