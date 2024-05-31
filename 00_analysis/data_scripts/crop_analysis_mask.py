# -*- coding: utf-8 -*-
"""
Created on Fri May 31 18:16:30 2024

@author: lmandl
"""

import geopandas as gpd
import rasterio
from rasterio.mask import mask
from rasterio.plot import show
import matplotlib.pyplot as plt
from shapely.geometry import mapping

# Specify the file paths for your shapefile and raster
shapefile_path = "/mnt/public/Projects/DataCube/projects/foreco/alps/gis/eastern_alps.shp"
raster_path = "/mnt/public/Projects/DataCube/projects/foreco/alps/dem/eu_dem_v11_E40N20.TIF"
output_path = "/mnt/public/Projects/DataCube/projects/foreco/alps/dem/DEM_masked.tif"

# Load the shapefile
shapefile = gpd.read_file(shapefile_path)

# Open the raster data
with rasterio.open(raster_path) as src:
    raster_crs = src.crs

# Reproject shapefile to match raster CRS if necessary
if shapefile.crs != raster_crs:
    shapefile = shapefile.to_crs(raster_crs)

# Extract geometry in GeoJSON format
shapes = [mapping(geom) for geom in shapefile.geometry]

# Crop and mask the raster
with rasterio.open(raster_path) as src:
    out_image, out_transform = mask(src, shapes, crop=True)
    out_meta = src.meta.copy()
    out_meta.update({
        "driver": "GTiff",
        "height": out_image.shape[1],
        "width": out_image.shape[2],
        "transform": out_transform
    })

# Save the cropped and masked raster to a new file
with rasterio.open(output_path, "w", **out_meta) as dest:
    dest.write(out_image)

# Plot the original and masked rasters
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 6))

with rasterio.open(raster_path) as src:
    show(src, ax=ax1, title="Original Raster")
    show(src.read(1), transform=src.transform, ax=ax1)

with rasterio.open(output_path) as src:
    show(src, ax=ax2, title="Masked Raster")
    show(src.read(1), transform=src.transform, ax=ax2)

plt.show()
