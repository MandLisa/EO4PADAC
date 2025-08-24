#!/usr/bin/env bash
set -euo pipefail

# --- CONFIG -------------------------------------------------------------------
IN_DIR="/mnt/eo/eu_mosaics"          # contains NDVI_2000.tif ... NDVI_2024.tif
OUT_A="/mnt/eo/eu_mosaics/NDVI_2000_2012"      # will be created if missing
OUT_B="/mnt/eo/eu_mosaics/NDVI_2013_2024"      # will be created if missing

# If your files use a different prefix or extension, adjust these:
PREFIX="NDVI_"
EXT=".tif"

# --- PREP ---------------------------------------------------------------------


compress_one () {
  local in="$1"
  local out="$2"
  echo "[info] compressing: $(basename "$in") -> $(basename "$out")"
  gdal_translate "$in" "$out" \
  -of GTiff \
  -co TILED=YES -co BIGTIFF=YES -co BLOCKXSIZE=512 -co BLOCKYSIZE=512 \
  -co COMPRESS=ZSTD -co ZSTD_LEVEL=19 \
  -co PREDICTOR=3 \
  -co NUM_THREADS=ALL_CPUS
}

# --- LOOP: 2000–2012 ----------------------------------------------------------
for year in $(seq 2000 2012); do
in="${IN_DIR}/${PREFIX}${year}${EXT}"
out="${OUT_A}/${PREFIX}${year}${EXT}"
if [[ -f "$in" ]]; then
compress_one "$in" "$out"
else
  echo "[warn] missing input: $in" >&2
fi
done

# --- LOOP: 2013–2024 ----------------------------------------------------------
for year in $(seq 2013 2024); do
in="${IN_DIR}/${PREFIX}${year}${EXT}"
out="${OUT_B}/${PREFIX}${year}${EXT}"
if [[ -f "$in" ]]; then
compress_one "$in" "$out"
else
  echo "[warn] missing input: $in" >&2
fi
done

echo "[done] All available years processed."
 