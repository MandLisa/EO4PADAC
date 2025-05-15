#!/bin/bash

# Define input and output parent folders
PARENT_FOLDER="/mnt/eo/EO4Alps/CSO_S2"
OUTPUT_PARENT_FOLDER="/mnt/eo/EO4Alps/CSO_summed_S2"

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
        
        # Build the summation expression dynamically for 9 bands
        BAND_SUM_EXPR=$(for i in {1..9}; do echo -n "A[$i]+"; done | sed 's/+$//')
        
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





#!/bin/bash

# Define parent folder containing all subfolders with tif files
PARENT_FOLDER="/mnt/eo/EO4Alps/CSO_summed_S2"

# Define output mosaic path
OUTPUT_MOSAIC="/mnt/eo/EO4Alps/CSO_summed_S2/mosaic_S2.tif"

# Find all sum_raster.tif files in subfolders
INPUT_FILES=$(find "$PARENT_FOLDER" -mindepth 2 -maxdepth 2 -name "sum_raster.tif")

# Run gdalwarp to mosaic all found tifs
gdalwarp -overwrite -of GTiff -r bilinear -dstnodata -9999 $INPUT_FILES "$OUTPUT_MOSAIC"

echo "Mosaic created at $OUTPUT_MOSAIC"




