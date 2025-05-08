import geopandas as gpd
import xarray as xr
import numpy as np
import pandas as pd
import rasterio
from rasterio.warp import transform_bounds
from shapely.geometry import Point
from tqdm import tqdm
import os

# Load sample points
gdf = gpd.read_file("random_sample.gpkg")

# Assign unique ID if not already present
gdf["ID"] = np.arange(1, len(gdf) + 1)

# Define years and tile folders
years = list(range(1986, 2024))
tiles = os.listdir("~/eo_nas/EO4Alps/fractional_cover_annual/")

def read_raster_stack(tile_path):
    files = sorted([f for f in os.listdir(tile_path) if f.endswith(".tif")])
    dataarrays = []
    for file in files:
        year = int(file.split("_")[-1].split(".")[0])
        da = xr.open_rasterio(os.path.join(tile_path, file)).squeeze()
        da = da.assign_coords({"year": year})
        dataarrays.append(da)
    return xr.concat(dataarrays, dim="year")

# Read all tiles and stack
fc_tiles = {}
for tile in tiles:
    tile_path = os.path.join("~/eo_nas/EO4Alps/fractional_cover_annual", tile)
    fc_tiles[tile] = read_raster_stack(tile_path)

# Combine all tiles (assume same bands, time dim)
# You may want to mosaic overlapping tiles here using a spatial merging logic

# Prepare output dataframe
records = []

# Loop through all points
for idx, row in tqdm(gdf.iterrows(), total=len(gdf)):
    point = row.geometry
    point_id = row.ID
    for tile_name, ds in fc_tiles.items():
        try:
            lon, lat = point.x, point.y
            # Nearest pixel (using method='nearest' would require interpolation logic)
            val = ds.sel(x=lon, y=lat, method="nearest").to_array()
            for t_idx, year in enumerate(ds.year.values):
                rec = {
                    "ID": point_id,
                    "year": year,
                }
                for b_idx, band in enumerate(val[:, t_idx].values):
                    rec[f"band_{b_idx+1}"] = band
                records.append(rec)
            break  # Stop after first tile match
        except Exception as e:
            continue

# Create DataFrame
final_df = pd.DataFrame.from_records(records)

# Save to CSV
final_df.to_csv("/mnt/data/fc_timeseries_sample_points.csv", index=False)
