# ----------------------------------------------------------------------
# Title: Geographically Weighted Regression of Forest Recovery Patterns
# Author: Lisa Mandl
# Date: 2024-05-23
# Description: This script performs a GWR analysis on post-disturbance
#              forest recovery across the European Alps using spatially
#              aggregated predictors. Output includes spatial prediction,
#              coefficient maps, and local R² values.
# ----------------------------------------------------------------------

import geopandas as gpd
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from shapely.geometry import Point
from mgwr.gwr import GWR
from mgwr.sel_bw import Sel_BW
from sklearn.preprocessing import StandardScaler

# Load recovery data
recovery = pd.read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_312025.csv")

# Filter and create recovery binary variable
recovery_filt = recovery[recovery["yod"] < 2013].copy()
recovery_filt["recov_10"] = (recovery_filt["recovery_rate"] <= 10).astype(int)

# Compute pre- and post-disturbance means per ID
def compute_group_means(df, col, condition):
    return df.groupby("ID").apply(lambda g: g.apply(
        lambda row: g[col][condition(g, row)].mean() if condition(g, row).any() else np.nan, axis=1
    )).reset_index(level=0, drop=True)

recovery["pre_dist_coni"] = compute_group_means(recovery, "coniferous", lambda g, r: g["year"] < r["yod"])
recovery["pre_dist_broadl"] = compute_group_means(recovery, "broadleaved", lambda g, r: g["year"] < r["yod"])
recovery["post_dist_bare"] = compute_group_means(recovery, "bare_ground", lambda g, r: g["year"] > r["yod"])

# Keep one row per ID
recovery_unique = recovery.drop_duplicates(subset="ID")

# Convert to GeoDataFrame
recovery["geometry"] = [Point(xy) for xy in zip(recovery["x"], recovery["y"])]
recovery_gdf = gpd.GeoDataFrame(recovery, geometry="geometry", crs="EPSG:3035")

recovery_unique["geometry"] = [Point(xy) for xy in zip(recovery_unique["x"], recovery_unique["y"])]
recovery_unique_gdf = gpd.GeoDataFrame(recovery_unique, geometry="geometry", crs="EPSG:3035")

# Load hexagon shapefile and join
hexagons = gpd.read_file("~/eo_nas/EO4Alps/gis/hexagons/hex_500.shp")[["GRID_ID", "geometry"]]
recovery_unique_gdf = gpd.sjoin(recovery_unique_gdf, hexagons, how="left", predicate="intersects")

# Aggregate predictors by hexagon
agg = recovery_unique_gdf.groupby("GRID_ID").agg({
    "height": "mean",
    "severity_relative": "mean",
    "VPD_yod1": "mean",
    "recovery_rate": "mean",
    "pre_dist_broadl": "mean",
    "pre_dist_coni": "mean",
    "post_dist_bare": "mean"
}).rename(columns={
    "height": "mean_elevation",
    "severity_relative": "mean_severity",
    "VPD_yod1": "mean_VPD",
    "recovery_rate": "mean_recovery_rate",
    "pre_dist_broadl": "mean_broadleaved",
    "pre_dist_coni": "mean_coniferous",
    "post_dist_bare": "mean_bare"
}).reset_index()

hexagons = hexagons.merge(agg, on="GRID_ID", how="left").dropna()

# Coordinates and predictors
coords = np.column_stack((hexagons.geometry.centroid.x, hexagons.geometry.centroid.y))
y = hexagons["mean_recovery_rate"].values.reshape(-1, 1)
X = hexagons[["mean_elevation", "mean_severity", "mean_VPD",
              "mean_broadleaved", "mean_coniferous", "mean_bare"]].values

# Standardize predictors
X_scaled = StandardScaler().fit_transform(X)

# Select bandwidth and fit GWR
selector = Sel_BW(coords, y, X_scaled)
bw = selector.search()
model = GWR(coords, y, X_scaled, bw)
results = model.fit()

# Add results to GeoDataFrame
for idx, name in enumerate(["elevation", "severity", "VPD", "broadleaved", "coniferous", "bare"]):
    hexagons[f"coef_{name}"] = results.params[:, idx]
hexagons["local_r2"] = results.localR2
hexagons["pred"] = results.predy

# Export to shapefile
hexagons.to_file("~/eo_nas/EO4Alps/gis/recovery_hexagons/recov_rates_python.shp")

# Plot local R2
hexagons.plot(column="local_r2", cmap="magma", legend=True)
plt.title("Local R² from GWR")
plt.tight_layout()
plt.show()
