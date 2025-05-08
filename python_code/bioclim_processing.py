# Climate and VPD Data Processing for Recovery Analysis
# Author: [Your Name]
# Date: [Insert Date]
# Description: This script processes bioclimatic rasters (temperature, precipitation), masks and reprojects them,
#              averages VPD values by year, and links them to a recovery dataset for further ecological analysis.

import os
import re
import geopandas as gpd
import pandas as pd
import rasterio
import rasterio.mask
import shutil
import numpy as np
from rasterio.warp import calculate_default_transform, reproject, Resampling
from shapely.geometry import mapping
from glob import glob

from rasterio.enums import Resampling

# Load recovery data
recovery = pd.read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_filtered_agent_geoloc.csv")
recovery_unique = recovery.drop_duplicates(subset="ID")

# Load shapefile and reproject to match raster
alps = gpd.read_file("~/eo_nas/EO4Alps/gis/alps_perimeter_buffer.shp")

# Define paths
prec_path = "~/eo_nas/EO4Alps/climate_data/bioclim/prec2.tif"
temp_path = "~/eo_nas/EO4Alps/climate_data/bioclim/temp2.tif"

# Read rasters
with rasterio.open(prec_path) as src:
    prec_meta = src.meta.copy()
    prec_data = src.read(1)
    prec_crs = src.crs

with rasterio.open(temp_path) as src:
    temp_meta = src.meta.copy()
    temp_data = src.read(1)
    temp_crs = src.crs

# Reproject shapefile to match raster CRS
alps = alps.to_crs(prec_crs)

# Mask and crop rasters
with rasterio.open(prec_path) as src:
    prec_crop, prec_transform = rasterio.mask.mask(src, alps.geometry.map(mapping), crop=True)

with rasterio.open(temp_path) as src:
    temp_crop, temp_transform = rasterio.mask.mask(src, alps.geometry.map(mapping), crop=True)

# Save cropped rasters
with rasterio.open("~/eo_nas/EO4Alps/climate_data/bioclim/precipitation_clip.tif", "w", **src.meta) as dst:
    dst.write(prec_crop)

with rasterio.open("~/eo_nas/EO4Alps/climate_data/bioclim/temperatur_clip.tif", "w", **src.meta) as dst:
    dst.write(temp_crop)

# Reproject VPD files (August-specific)
vpd_august_dir = "~/eo_nas/EO4Alps/climate_data/VPD_clip/08"
years = list(range(1986, 2019))
parent_dir = "~/eo_nas/EO4Alps/climate_data"

# Create folders for each year
for year in years:
    year_dir = os.path.join(parent_dir, str(year))
    os.makedirs(year_dir, exist_ok=True)

# Move VPD August rasters to correct year folders
for file in glob(os.path.join(vpd_august_dir, "m08_*.tif")):
    match = re.search(r"m08_(\d{4})\.tif", os.path.basename(file))
    if match:
        year = match.group(1)
        dest = os.path.join(parent_dir, year, os.path.basename(file))
        shutil.copy(file, dest)

# Average VPD rasters per year
for year in years:
    year_dir = os.path.join(parent_dir, str(year))
    files = glob(os.path.join(year_dir, "*.tif"))
    stacks = [rasterio.open(f).read(1) for f in files]
    if stacks:
        avg = np.nanmean(stacks, axis=0)
        meta = rasterio.open(files[0]).meta
        meta.update(dtype=rasterio.float32)
        out_path = os.path.join(parent_dir, f"average_{year}.tif")
        with rasterio.open(out_path, 'w', **meta) as dst:
            dst.write(avg.astype(rasterio.float32), 1)

# Move averages to dedicated folder
vpd_avg_dir = os.path.join(parent_dir, "VPD_summer_averages")
os.makedirs(vpd_avg_dir, exist_ok=True)
for file in glob(os.path.join(parent_dir, "average_*.tif")):
    shutil.move(file, os.path.join(vpd_avg_dir, os.path.basename(file)))

# Rename files to match convention
for file in glob(os.path.join(vpd_avg_dir, "average_*.tif")):
    year = re.search(r"average_(\d{4})", os.path.basename(file)).group(1)
    os.rename(file, os.path.join(vpd_avg_dir, f"VPD_{year}.tif"))

# Extract VPD values for each observation
def extract_vpd_for_row(row, raster_folder):
    raster_file = os.path.join(raster_folder, f"VPD_{int(row['year'])}.tif")
    try:
        with rasterio.open(raster_file) as src:
            coords = [(row['x'], row['y'])]
            for val in src.sample(coords):
                return val[0]
    except Exception as e:
        return np.nan

# Apply extraction
recovery['VPD_absolute'] = recovery.apply(lambda row: extract_vpd_for_row(row, vpd_avg_dir), axis=1)

# Save outputs
recovery_unique.to_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_unique_2308.csv", index=False)
recovery.to_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_bioclim.csv", index=False)
