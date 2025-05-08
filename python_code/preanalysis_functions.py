# Pre-analysis functions

import os
import glob
import numpy as np
import pandas as pd
import xarray as xr
import rasterio
from rasterio.mask import mask
import geopandas as gpd
from shapely.geometry import mapping
import warnings
warnings.filterwarnings("ignore")

def list_files_recursive(path, pattern):
    return glob.glob(os.path.join(path, '**', pattern), recursive=True)

def read_raster_as_array(file_path):
    with rasterio.open(file_path) as src:
        array = src.read(1)
        profile = src.profile
    return array, profile

def mask_raster_with_shapefile(raster_path, shapefile):
    with rasterio.open(raster_path) as src:
        shapes = [mapping(geom) for geom in shapefile.geometry]
        out_image, out_transform = mask(src, shapes, crop=True)
        out_meta = src.meta
        out_meta.update({
            "height": out_image.shape[1],
            "width": out_image.shape[2],
            "transform": out_transform
        })
    return out_image, out_meta

def extract_values_from_raster(raster_path, points_gdf, column_name='value'):
    with rasterio.open(raster_path) as src:
        coords = [(x,y) for x, y in zip(points_gdf.geometry.x, points_gdf.geometry.y)]
        values = [val[0] for val in src.sample(coords)]
    points_gdf[column_name] = values
    return points_gdf

def stack_rasters_to_xarray(raster_files, var_name='band'):
    arrays = []
    years = []
    for f in raster_files:
        data, profile = read_raster_as_array(f)
        year = int(os.path.basename(f).split('_')[-1].split('.')[0])
        arrays.append(data)
        years.append(year)
    stacked = np.stack(arrays, axis=0)
    return xr.DataArray(
        stacked,
        dims=['year', 'y', 'x'],
        coords={'year': years},
        name=var_name
    )

def sample_raster_values(da, sample_points):
    df = pd.DataFrame({
        'x': [pt.x for pt in sample_points.geometry],
        'y': [pt.y for pt in sample_points.geometry]
    })
    values = []
    for year in da.year.values:
        year_da = da.sel(year=year)
        val = [year_da.sel(x=x, y=y, method='nearest').values for x, y in zip(df['x'], df['y'])]
        df[f'value_{year}'] = val
    return df

def compute_anomaly(da, baseline_years):
    baseline = da.sel(year=baseline_years).mean(dim='year')
    anomaly = da - baseline
    return anomaly

def write_array_to_raster(data, profile, out_path):
    with rasterio.open(out_path, 'w', **profile) as dst:
        dst.write(data, 1)
