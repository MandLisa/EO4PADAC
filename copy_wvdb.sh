#!/bin/bash

# Source directory
source_dir="/data/public/Projects/DataCube/wvdb-global"

# Destination directory
destination_dir="/data/eo/EO4Alps"

# Copy the folder recursively with all subfolders and contents
cp -R "$source_dir" "$destination_dir"

# Check if the copy was successful
if [ $? -eq 0 ]; then
    echo "Folder copied successfully from $source_dir to $destination_dir"
else
    echo "Error copying folder from $source_dir to $destination_dir"
fi
