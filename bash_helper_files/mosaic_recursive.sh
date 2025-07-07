#!/bin/bash

parent_directory="/mnt/eo/EO4Alps/level4_fcover"

# Find all .tif files with "1995" in the filename recursively
tif_files=$(find "$parent_directory" -type f -name "*1988_HL_ML_MLP.tif")

if [[ -z $tif_files ]]; then
  echo "No files found."
  exit 1
fi

# Merge the files into a single output
output_file="$parent_directory/mosaic_1988.tif"
gdal_merge.py -o "$output_file" $tif_files

echo "Merged files and saved as $output_file"






#!/bin/bash

parent_directory="/mnt/eo/EO4Alps/level4_fcover"

# Loop over the years from 1988 to 2023
for year in $(seq 2009 2023); do
  echo "Processing year $year..."

  # Find all matching .tif files for the current year
  tif_files=$(find "$parent_directory" -type f -name "*${year}_HL_ML_MLP.tif")

  if [[ -z $tif_files ]]; then
    echo "No files found for year $year."
    continue
  fi

  # Define output file name
  output_file="$parent_directory/mosaic_${year}.tif"

  # Merge the files
  gdal_merge.py -o "$output_file" $tif_files

  echo "Merged files for year $year into $output_file"
done

echo "All mosaics created."



