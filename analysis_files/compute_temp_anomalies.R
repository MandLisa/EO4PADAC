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


### extract temp anomalies

library(readr)

recovery <- read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_GWR.csv")

# Packages
library(terra)

# --- CONFIG ---
root_dir <- "/mnt/eo/EO4Alps/climate_data/temp"   # years live under here
seasons  <- c("spring", "summer", "all")

# Helper to build anomaly path per year/season
anom_path <- function(y, season) {
  file.path(root_dir, as.character(y), "anomalies",
            sprintf("temp_anom_%d_%s.tif", y, season))
}

# --- SANITY: ensure we have a 'year' column (long format) ---
stopifnot(all(c("x","y","year") %in% names(recovery)))

# Prepare output columns
for (s in seasons) recovery[[paste0("temp_anom_", s)]] <- NA_real_

# Split row indices by year (fast to iterate)
idx_by_year <- split(seq_len(nrow(recovery)), recovery$year)

# Main loop: load each year's raster(s) once, extract for that year's rows
for (y in names(idx_by_year)) {
  y_num <- as.integer(y)
  idx   <- idx_by_year[[y]]
  
  # (Optional) skip years with no anomaly rasters
  paths <- setNames(vapply(seasons, \(s) anom_path(y_num, s), character(1)), seasons)
  if (!any(file.exists(paths))) next
  
  # Take CRS from the first existing raster for correct alignment
  f_ref <- paths[which(file.exists(paths))][1]
  r_ref <- rast(f_ref)
  
  # Build points for this year's rows (assumes x/y are already in the same CRS as rasters)
  pts <- vect(recovery[idx, c("x","y")], geom = c("x","y"), crs = crs(r_ref))
  
  # Extract per season (only if file exists)
  for (s in seasons) {
    f <- paths[[s]]
    if (!file.exists(f)) next
    r <- rast(f)
    # Reproject points if needed (rare if everything is LAEA)
    if (!same.crs(r, r_ref)) pts_use <- project(pts, crs(r)) else pts_use <- pts
    vals <- terra::extract(r, pts_use, ID = FALSE)[,1]
    recovery[[paste0("temp_anom_", s)]][idx] <- vals
  }
}

# Quick QA
sapply(paste0("temp_anom_", seasons), \(col) {
  c(nonNA = sum(!is.na(recovery[[col]])), NA = sum(is.na(recovery[[col]])))
})


# compute temp anomalies for yod,... yod+10
install.packages("data.table")  # if needed
library(data.table)

# --- INPUT ---
setDT(recovery)
stopifnot(all(c("ID","year","yod",
                "temp_anom_spring","temp_anom_summer","temp_anom_all") %in% names(recovery)))

# relative year since disturbance
recovery[, rel := year - yod]

# Helper: for a given anomaly column, compute per-ID cumulative means over rel=0..10
# and attach as wide columns named: <prefix>_yod, <prefix>_yod1, ..., <prefix>_yod10
add_cummean_windows <- function(dt, anom_col, prefix_out) {
  tmp <- dt[rel >= 0 & rel <= 10, .(ID, rel, val = get(anom_col))]
  
  # order by ID, then rel
  setorder(tmp, ID, rel)
  
  # NA-safe cumulative mean: cum(sum) / cum(count of non-NA)
  tmp[, `:=`(
    csum = cumsum(fifelse(is.na(val), 0, val)),
    n    = cumsum(!is.na(val))
  ), by = ID]
  tmp[, cm := fifelse(n > 0, csum / n, NA_real_)]
  
  # one column per rel (0..10)
  wide <- dcast(tmp, ID ~ rel, value.var = "cm")
  
  # rename rel columns to requested names
  rel_cols <- setdiff(names(wide), "ID")
  setnames(
    wide, rel_cols,
    paste0(prefix_out, "_yod", fifelse(rel_cols == "0", "", rel_cols))
  )
  
  # merge back to recovery by ID (columns repeat per-ID across years; that’s intended)
  setkey(wide, ID); setkey(dt, ID)
  dt <- wide[dt]  # left join (adds the columns)
  dt
}

# Add for all three seasons; user requested "temp_ano_*" as prefix in new columns
recovery <- add_cummean_windows(recovery, "temp_anom_spring", "temp_ano_spring")
recovery <- add_cummean_windows(recovery, "temp_anom_summer", "temp_ano_summer")
recovery <- add_cummean_windows(recovery, "temp_anom_all",    "temp_ano_all")


### write new csv
# choose an output path
out_dir  <- "/mnt/eo/EO4Alps/00_analysis/_recovery"
out_file <- file.path(out_dir, "recovery_temp_anomalies.csv")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# write everything
write.csv(recovery, file = out_file, row.names = FALSE, na = "NA")



