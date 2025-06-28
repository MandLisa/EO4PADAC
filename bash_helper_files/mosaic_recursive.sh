parent_directory="/mnt/eo/EO4Alps/level4_fcover"

# Find all .tif files with "1995" in the filename recursively
tif_files=$(find "$parent_directory" -type f -name "*1995_HL_ML_MLP.tif")

if [[ -z $tif_files ]]; then
  echo "No files found."
  exit 1
fi

# Merge the files into a single output
output_file="$parent_directory/mosaic_1995.tif"
gdal_merge.py -o "$output_file" $tif_files

echo "Merged files and saved as $output_file"