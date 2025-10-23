#!/usr/bin/env bash
set -euo pipefail

# --- CONFIG -------------------------------------------------------------------
IN="/mnt/dss_europe/level3_interpolated"   # Input Tiles
OUT_RAW="/mnt/eo/eu_mosaics/EVI"           # uncompressed annual mosaics
OUT_COMP="/mnt/eo/eu_mosaics/EVI_comp"     # compressed EVI annual mosaics
PREFIX="EVI_"
EXT=".tif"
EPS="0.0001"   # absolute error bound for LERC
BLOCK=512

# --- YEAR RANGE (EVI only) ----------------------------------------------------
YEAR_START=1985
YEAR_END=1999

mkdir -p "$OUT_RAW" "$OUT_COMP"

# --- STEP 1: Mosaic per year --------------------------------------------------
mosaic_one_year () {
  local year="$1" in="$2" out="$3"
  local tmpvrt
  tmpvrt="$(mktemp --suffix=".vrt")"

  find "$in" -type f -name "${year}*EVI*.tif" -print0 \
    | xargs -0 gdalbuildvrt \
        -srcnodata -10000 -vrtnodata -10000 \
        -resolution highest \
        "$tmpvrt"

  gdal_translate "$tmpvrt" "${out}/${PREFIX}${year}${EXT}" \
      -co TILED=YES -co COMPRESS=LZW -co BIGTIFF=YES \
      -co NUM_THREADS=ALL_CPUS -co SPARSE_OK=TRUE -a_nodata -10000

  rm -f "$tmpvrt"
}

# --- STEP 2: Compression (LERC with fallback) --------------------------------
compress_one () {
  local in="$1"
  local out="$2"
  echo "[info] LERC ε=${EPS}: $(basename "$in") -> $(basename "$out")"

  # Try LERC with ZSTD backend
  if gdal_translate "$in" "$out" -of GTiff \
       -co TILED=YES -co BIGTIFF=YES \
       -co BLOCKXSIZE="$BLOCK" -co BLOCKYSIZE="$BLOCK" \
       -co COMPRESS=LERC_ZSTD -co MAX_Z_ERROR="$EPS" \
       -co NUM_THREADS=ALL_CPUS >/dev/null 2>&1; then
    echo "[ok] COMPRESS=LERC_ZSTD"
    return 0
  fi

  # Fallback: LERC with DEFLATE backend
  if gdal_translate "$in" "$out" -of GTiff \
       -co TILED=YES -co BIGTIFF=YES \
       -co BLOCKXSIZE="$BLOCK" -co BLOCKYSIZE="$BLOCK" \
       -co COMPRESS=LERC_DEFLATE -co MAX_Z_ERROR="$EPS" \
       -co NUM_THREADS=ALL_CPUS >/dev/null 2>&1; then
    echo "[ok] COMPRESS=LERC_DEFLATE"
    return 0
  fi

  # Final fallback: plain ZSTD
  echo "[warn] LERC not supported; fallback to ZSTD"
  gdal_translate "$in" "$out" -of GTiff \
    -co TILED=YES -co BIGTIFF=YES \
    -co BLOCKXSIZE="$BLOCK" -co BLOCKYSIZE="$BLOCK" \
    -co COMPRESS=ZSTD -co ZSTD_LEVEL=19 \
    -co PREDICTOR=3 -co NUM_THREADS=ALL_CPUS
}

# --- MAIN LOOP ---------------------------------------------------------------
for year in $(seq "$YEAR_START" "$YEAR_END"); do
  raw="${OUT_RAW}/${PREFIX}${year}${EXT}"
  comp="${OUT_COMP}/${PREFIX}${year}${EXT}"

  # Mosaic only if missing
  if [[ ! -f "$raw" ]]; then
    echo "[mosaic] Building: $raw"
    mosaic_one_year "$year" "$IN" "$OUT_RAW"
  else
    echo "[skip] Mosaic exists: $raw"
  fi

  # Compress only if missing
  if [[ ! -f "$comp" ]]; then
    if [[ -f "$raw" ]]; then
      compress_one "$raw" "$comp"
    else
      echo "[warn] missing raw mosaic: $raw" >&2
    fi
  else
    echo "[skip] already compressed: $comp"
  fi
done

echo "[done] Mosaic + compression finished for ${YEAR_START}–${YEAR_END}."
