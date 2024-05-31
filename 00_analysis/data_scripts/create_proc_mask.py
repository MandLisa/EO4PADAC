# -*- coding: utf-8 -*-
"""
Created on Thu May 21 10:49:17 2024

@author: lmandl
"""

import rasterio
import numpy as np
from rasterio.enums import Resampling
import geopandas as gpd
from rasterio.mask import mask
import os


# Load your raster file
with rasterio.open('/data/public/Projects/DataCube/projects/foreco/alps/gis/disturbances_clip/disturbances_clip.tif') as src:
    r = src.read(1)
    profile = src.profile

# Reclassify raster values
r_reclassified = np.where((r >= 1990) & (r <= 2020), 1, 0)
r_reclassified = np.where(np.isnan(r), 0, r_reclassified)

# Save the reclassified raster to a new file
profile.update(dtype=rasterio.uint8)

with rasterio.open('/data/public/Projects/DataCube/projects/foreco/alps/gis/processing_mask_new.tif', 'w', **profile) as dst:
    dst.write(r_reclassified.astype(rasterio.uint8), 1)


# Set the file paths
shapefile_path = '/data/public/Projects/DataCube/projects/foreco/alps/gis/grid_clip.shp'
rasterfile_path = '/data/public/Projects/DataCube/projects/foreco/alps/gis/processing_mask_new.tif'

# Read in the shapefile and raster
shape = gpd.read_file(shapefile_path)

with rasterio.open(rasterfile_path) as src:
    for i in range(len(shape)):
        # Get the name of the feature
        Tile_ID = shape.loc[i, "Tile_ID"]
        
        # Clip the raster based on the feature
        geom = [shape.iloc[i].geometry.__geo_interface__]
        out_image, out_transform = mask(src, geom, crop=True)
        out_meta = src.meta.copy()
        
        out_meta.update({
            "driver": "GTiff",
            "height": out_image.shape[1],
            "width": out_image.shape[2],
            "transform": out_transform
        })
        
        # Save the clipped raster with the feature name as the filename
        filename = f"{Tile_ID}.tif"
        with rasterio.open(filename, "w", **out_meta) as dest:
            dest.write(out_image)

import os
import shutil

# Set the path to the parent folder
parent_folder = '/data/public/Projects/DataCube/projects/foreco/alps/gis/proc_mask'

# Get the list of subfolders in the parent folder
subfolders = [os.path.join(parent_folder, d) for d in os.listdir(parent_folder) if os.path.isdir(os.path.join(parent_folder, d))]

# Loop through each subfolder
for subfolder in subfolders:
    # Get the list of tif files in the subfolder
    tif_files = [f for f in os.listdir(subfolder) if f.endswith('.tif')]
    
    # Get the subfolder name
    subfolder_name = os.path.basename(subfolder)
    
    # Loop through each tif file and check if the name matches the subfolder name
    for tif_file in tif_files:
        tif_file_name = os.path.splitext(tif_file)[0]
        if tif_file_name != subfolder_name:
            print(f"WARNING: The file '{tif_file_name}.tif' in the folder '{subfolder_name}' does not match the folder name.")

# Loop through each subfolder
for subfolder in subfolders:
    # Get the list of tif files in the subfolder
    tif_files = [os.path.join(subfolder, f) for f in os.listdir(subfolder) if f.endswith('.tif')]
    
    # Loop through each tif file in the subfolder
    for tif_file in tif_files:
        # Rename the tif file to "proc_mask.tif"
        new_tif_file = os.path.join(subfolder, "proc_mask.tif")
        shutil.move(tif_file, new_tif_file)
