#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(terra))

# -------------------------------------------------------
# Global options (tune as needed)
# -------------------------------------------------------
terraOptions(
  memfrac  = 0.6,   # conservative RAM use
  progress = 1
  # tempdir = "/fast/tmp"  # point to a fast SSD/NVMe if available
)

# -------------------------------------------------------
# Helper: treat coded NoData as NA during processing
# -------------------------------------------------------
set_nodata <- function(r, nodata = NA) {
  if (!is.na(nodata)) NAflag(r) <- nodata
  r
}

# -------------------------------------------------------
# Core: Forward LOCF using a "cover chain"
#   No manual read/write loops; terra streams the graph.
# -------------------------------------------------------
fill_gaps_locf_coverchain <- function(
    x, out_file,
    nodata_in = NA,                        # e.g., -10000 if set on disk
    datatype  = NULL,                      # "INT2S" for scaled ints, or "FLT4S"
    gdal_opts = c("TILED=YES","COMPRESS=LZW","BIGTIFF=YES")
) {
  stopifnot(inherits(x, "SpatRaster"))
  if (!is.na(nodata_in)) NAflag(x) <- nodata_in
  
  n <- nlyr(x)
  stopifnot(n >= 1)
  
  # preserve datatype unless specified
  if (is.null(datatype)) datatype <- terra::datatype(x)
  
  # Build the cover chain virtually (no data materialized yet)
  layers <- vector("list", n)
  layers[[1]] <- x[[1]]
  if (n > 1) {
    for (i in 2:n) {
      # where x[[i]] is NA, take previous filled
      layers[[i]] <- cover(x[[i]], layers[[i - 1]])
    }
  }
  
  filled <- rast(layers)
  names(filled) <- names(x)
  if (!is.na(nodata_in)) NAflag(filled) <- nodata_in
  
  # Now stream everything to disk in one pass
  writeRaster(
    filled, filename = out_file, overwrite = TRUE,
    wopt = list(datatype = datatype, gdal = gdal_opts)
  )
}

# -------------------------------------------------------
# (Optional) Bidirectional fill (forward LOCF + backward fill)
#   If you ALSO want to fill leading NAs (before the first obs),
#   run a backward pass and combine.
#   Policy here: use forward LOCF; where still NA, use backward-fill result.
# -------------------------------------------------------
fill_gaps_bidirectional <- function(
    x, out_file,
    nodata_in = NA,
    datatype  = NULL,
    gdal_opts = c("TILED=YES","COMPRESS=LZW","BIGTIFF=YES")
) {
  stopifnot(inherits(x, "SpatRaster"))
  if (!is.na(nodata_in)) NAflag(x) <- nodata_in
  n <- nlyr(x); stopifnot(n >= 1)
  if (is.null(datatype)) datatype <- terra::datatype(x)
  
  # forward chain
  fwd <- vector("list", n)
  fwd[[1]] <- x[[1]]
  for (i in 2:n) fwd[[i]] <- cover(x[[i]], fwd[[i - 1]])
  fwd <- rast(fwd); names(fwd) <- names(x)
  
  # backward chain (operate on reversed time, then reverse back)
  xr <- x[[rev(seq_len(n))]]
  bwd <- vector("list", n)
  bwd[[1]] <- xr[[1]]
  for (i in 2:n) bwd[[i]] <- cover(xr[[i]], bwd[[i - 1]])
  bwd <- rast(bwd[[rev(seq_len(n))]])  # reverse back
  names(bwd) <- names(x)
  
  # combine: prefer fwd; where fwd is NA, take bwd
  filled <- cover(fwd, bwd)
  if (!is.na(nodata_in)) NAflag(filled) <- nodata_in
  
  writeRaster(
    filled, filename = out_file, overwrite = TRUE,
    wopt = list(datatype = datatype, gdal = gdal_opts)
  )
}

# -------------------------------------------------------
# INPUTS (edit to your environment)
# -------------------------------------------------------
broad  <- rast("/mnt/eo/EO4Alps/level4_fcover/final_stacks/mosaic_broadleaved.tif")
conif  <- rast("/mnt/eo/EO4Alps/level4_fcover/final_stacks/mosaic_coniferous.tif")
grass  <- rast("/mnt/eo/EO4Alps/level4_fcover/final_stacks/mosaic_grassland.tif")
shrub  <- rast("/mnt/eo/EO4Alps/level4_fcover/final_stacks/mosaic_shrubland.tif")
bare   <- rast("/mnt/eo/EO4Alps/level4_fcover/final_stacks/mosaic_bare_ground.tif")

out_dir <- "/mnt/eo/EO4Alps/level4_fcover/final_stacks/filled"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Choose one of the two routines:
use_bidirectional <- FALSE   # TRUE if you want leading NAs filled too

runner <- if (use_bidirectional) fill_gaps_bidirectional else fill_gaps_locf_coverchain

# You can also try ZSTD (often faster/smaller) if your GDAL supports it:
# gdal_co <- c("TILED=YES","COMPRESS=ZSTD","PREDICTOR=2","BIGTIFF=YES")  # INT data
gdal_co <- c("TILED=YES","COMPRESS=LZW","BIGTIFF=YES")

runner(broad, file.path(out_dir, "broadleaved_filled_1986_2023.tif"),
       nodata_in = -10000, datatype = "INT2S", gdal_opts = gdal_co)
runner(conif, file.path(out_dir, "coniferous_filled_1986_2023.tif"),
       nodata_in = -10000, datatype = "INT2S", gdal_opts = gdal_co)
runner(grass, file.path(out_dir, "grassland_filled_1986_2023.tif"),
       nodata_in = -10000, datatype = "INT2S", gdal_opts = gdal_co)
runner(shrub, file.path(out_dir, "shrubland_filled_1986_2023.tif"),
       nodata_in = -10000, datatype = "INT2S", gdal_opts = gdal_co)
runner(bare,  file.path(out_dir, "bareground_filled_1986_2023.tif"),
       nodata_in = -10000, datatype = "INT2S", gdal_opts = gdal_co)

message("Gap-filling done. Outputs: ", out_dir)
