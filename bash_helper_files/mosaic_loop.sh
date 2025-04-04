#!/bin/bash

# Define base directories
BASE_INPUT_DIR="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw"
OUTPUT_DIR="$HOME/eo_nas/EO4Alps/level3_predictions/mosaics_wtw/mosaics"

# Loop through years 1988 to 2023
for YEAR in {2005..2023}; do
    INPUT_DIR="$BASE_INPUT_DIR/$YEAR"
    OUTPUT_NAME="mosaic_${YEAR}.tif"
    
    # Check if input directory exists and contains .tif files
    if [ -d "$INPUT_DIR" ] && ls "$INPUT_DIR"/*.tif 1> /dev/null 2>&1; then
        echo "Processing $YEAR..."
        
        # Use gdal_merge.py to create mosaic
        gdal_merge.py -o "$OUTPUT_DIR/$OUTPUT_NAME" -of GTiff "$INPUT_DIR"/*.tif

        echo "Mosaic for $YEAR created: $OUTPUT_DIR/$OUTPUT_NAME"
    else
        echo "Skipping $YEAR: No TIFF files found in $INPUT_DIR"
    fi
done

echo "All mosaics completed!"
