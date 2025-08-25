#!/usr/bin/env bash
set -euo pipefail

IN_DIR="/mnt/eo/eu_mosaics"                   # NDVI_2000.tif ... NDVI_2024.tif
OUT_A="/mnt/eo/eu_mosaics/NDVI_2000_2012"
OUT_B="/mnt/eo/eu_mosaics/NDVI_2013_2024"
PREFIX="NDVI_"; EXT=".tif"

# If your source has a coded NoData (e.g., -9999), set SRC_NODATA accordingly.
SRC_NODATA="nan"      # or "-9999"
DST_NODATA="-32768"   # Int16 sentinel (outside [-10000,10000])


pack_i16 () {
  local in="$1" out="$2" tmp="__tmp_i16.tif"
  echo "[info] $(basename "$in") -> $(basename "$out") (Int16 + ZSTD)"

  # 1) Float [-1,1] -> Int16 [-10000,10000]
  gdal_translate "$in" "$tmp" -of GTiff -ot Int16 \
    -scale -1 1 -10000 10000 \
    -srcnodata "$SRC_NODATA" -dstnodata "$DST_NODATA" \
    -co TILED=YES -co BIGTIFF=YES \
    -co BLOCKXSIZE=512 -co BLOCKYSIZE=512 \
    -co COMPRESS=ZSTD -co ZSTD_LEVEL=19 \
    -co PREDICTOR=2 -co NUM_THREADS=ALL_CPUS

  # 2) Add scaling metadata so readers auto-rescale to NDVI
  gdal_translate "$tmp" "$out" -of GTiff -a_nodata "$DST_NODATA" \
    -co TILED=YES -co BIGTIFF=YES \
    -co BLOCKXSIZE=512 -co BLOCKYSIZE=512 \
    -co COMPRESS=ZSTD -co ZSTD_LEVEL=19 \
    -co PREDICTOR=2 -co NUM_THREADS=ALL_CPUS \
    -mo SCALE=0.0001 -mo OFFSET=0
  rm -f "$tmp"
}

for y in $(seq 2000 2012); do
  in="${IN_DIR}/${PREFIX}${y}${EXT}"
  out="${OUT_A}/${PREFIX}${y}${EXT}"
  [[ -f "$in" ]] && pack_i16 "$in" "$out" || echo "[warn] missing: $in" >&2
done

for y in $(seq 2013 2024); do
  in="${IN_DIR}/${PREFIX}${y}${EXT}"
  out="${OUT_B}/${PREFIX}${y}${EXT}"
  [[ -f "$in" ]] && pack_i16 "$in" "$out" || echo "[warn] missing: $in" >&2
done

echo "[done] Int16 fixed-point compression finished."
