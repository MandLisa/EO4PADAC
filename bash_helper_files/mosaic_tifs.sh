#!/bin/bash

parent_directory="/home/lmandl/eo_nas/EO4Alps/gis/attribution_v1"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/agents_mosaic1.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


#!/bin/bash

#mosaic_file="$parent_directory/uncertainty_mosaic_2021.tif"  # Replace this with the actual path to your mosaic file

# Compute pyramids for the mosaic using gdaladdo
# You can adjust the options (-r, --config) as needed
#gdaladdo -r average "$mosaic_file" 2 4 8 16

#echo "Pyramids computed for the mosaic file: $mosaic_file"

parent_directory="/home/lmandl/eo_nas/EO4Alps/gis/number_disturbances"


# Find tif files with _2021_ in their file name recursively
tif_files=$(find "$parent_directory" -type f -name "*.tif")

if [[ -z $tif_files ]]; then
echo "No files found."
exit 1
fi

# Merge and write the files to the parent directory
output_file="$parent_directory/number_disturbances.tif"
gdal_merge.py -o "$output_file" $tif_files
echo "Merged files and saved as $output_file"


