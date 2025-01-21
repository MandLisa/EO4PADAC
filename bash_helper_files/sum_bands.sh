#!/bin/bash

# Define input and output parent folders
PARENT_FOLDER="$HOME/eo_nas/EO4Alps/CSO"
OUTPUT_PARENT_FOLDER="$HOME/eo_nas/EO4Alps/CSO_summed"

# Loop through each subfolder
for SUBFOLDER in "$PARENT_FOLDER"/*/; do
    # Extract the subfolder name
    SUBFOLDER_NAME=$(basename "$SUBFOLDER")
    
    # Create a corresponding subfolder in the output folder
    OUTPUT_SUBFOLDER="$OUTPUT_PARENT_FOLDER/$SUBFOLDER_NAME"
    mkdir -p "$OUTPUT_SUBFOLDER"
    
    # Define output raster path
    OUTPUT_RASTER="$OUTPUT_SUBFOLDER/sum_raster.tif"
    
    # Skip processing if the output raster already exists
    if [ -f "$OUTPUT_RASTER" ]; then
        echo "Skipping $SUBFOLDER_NAME: sum_raster.tif already exists."
        continue
    fi
    
    # Find the raster file (assumes one raster per subfolder)
    RASTER=$(find "$SUBFOLDER" -maxdepth 1 -type f -name "*.tif" | head -n 1)
    
    if [ -f "$RASTER" ]; then
        echo "Processing $RASTER"
        
        # Build the summation expression dynamically
        BAND_SUM_EXPR=$(for i in {1..38}; do echo -n "A[$i]+"; done | sed 's/+$//')
        
        # Run gdal_calc.py with array indexing for bands
        gdal_calc.py \
            --overwrite \
            --type=Float32 \
            --outfile="$OUTPUT_RASTER" \
            -A "$RASTER" \
            --calc="$BAND_SUM_EXPR" \
            --NoDataValue=-9999
        
        echo "Summed raster saved to $OUTPUT_RASTER"
    else
        echo "No raster found in $SUBFOLDER"
    fi
done
