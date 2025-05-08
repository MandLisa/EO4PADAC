### Forest Recovery Analysis in Python with Spatial Models

# Import Required Libraries
import pandas as pd
import numpy as np
import geopandas as gpd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.cluster import KMeans
from mgwr.gwr import GWR, Sel_BW
from shapely.geometry import Point
from matplotlib.colors import ListedColormap

# Load Data
recovery = pd.read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_312025.csv")

# Compute pre-disturbance mean broadleaved, coniferous, and bare ground shares
recovery["pre_dist_coni"] = recovery.groupby("ID").apply(lambda x: np.where(x["year"] < x["yod"], x["coniferous"].mean(skipna=True), np.nan)).reset_index(level=0, drop=True)
recovery["pre_dist_broadl"] = recovery.groupby("ID").apply(lambda x: np.where(x["year"] < x["yod"], x["broadleaved"].mean(skipna=True), np.nan)).reset_index(level=0, drop=True)
recovery["post_dist_bare"] = recovery.groupby("ID").apply(lambda x: np.where(x["year"] > x["yod"], x["bare_ground"].mean(skipna=True), np.nan)).reset_index(level=0, drop=True)

# One observation per ID
recovery_unique = recovery.drop_duplicates(subset='ID')

# Convert to GeoDataFrame
recovery_gdf = gpd.GeoDataFrame(recovery, geometry=gpd.points_from_xy(recovery["x"], recovery["y"]), crs="EPSG:3035")
recovery_unique_gdf = gpd.GeoDataFrame(recovery_unique, geometry=gpd.points_from_xy(recovery_unique["x"], recovery_unique["y"]), crs="EPSG:3035")

# Load hexagons
hexagons = gpd.read_file("~/eo_nas/EO4Alps/gis/hexagons/hex_500.shp")[['GRID_ID', 'geometry']]

# Spatial Join
recovery_gdf = gpd.sjoin(recovery_gdf, hexagons, how="left", predicate="intersects")
recovery_unique_gdf = gpd.sjoin(recovery_unique_gdf, hexagons, how="left", predicate="intersects")

# Aggregate predictors by GRID_ID
hexagon_predictors = recovery_unique_gdf.groupby('GRID_ID').agg(
    mean_elevation=('height', 'mean'),
    mean_severity=('severity_relative', 'mean'),
    mean_VPD=('VPD_yod1', 'mean'),
    mean_recovery_rate=('recovery_rate', 'mean'),
    mean_broadleaved=('pre_dist_broadl', 'mean'),
    mean_coniferous=('pre_dist_coni', 'mean'),
    mean_bare=('post_dist_bare', 'mean'),
    dominant_forest_type=('forest_type', lambda x: x.mode()[0] if not x.mode().empty else np.nan)
).reset_index()

# Filter NAs
hexagons_recov_rate_all = hexagon_predictors.dropna()

# Fit GWR Model
coords = np.column_stack((hexagons_recov_rate_all.geometry.centroid.x, hexagons_recov_rate_all.geometry.centroid.y))
X = hexagons_recov_rate_all[['mean_elevation', 'mean_severity', 'mean_VPD', 'mean_broadleaved', 'mean_coniferous']].values
y = hexagons_recov_rate_all['mean_recovery_rate'].values.reshape(-1, 1)

# Bandwidth Selection
selector = Sel_BW(coords, y, X)
bw = selector.search()

# Fit GWR
model = GWR(coords, y, X, bw)
results = model.fit()

# Add Results to GeoDataFrame
hexagons_recov_rate_all['local_r2'] = results.localR2
hexagons_recov_rate_all['coef_elevation'] = results.params[:, 0]
hexagons_recov_rate_all['coef_severity'] = results.params[:, 1]
hexagons_recov_rate_all['coef_VPD'] = results.params[:, 2]
hexagons_recov_rate_all['coef_broadleaved'] = results.params[:, 3]
hexagons_recov_rate_all['coef_coniferous'] = results.params[:, 4]

# Plot Local R²
hexagons_recov_rate_all.plot(column='local_r2', cmap='magma', legend=True)
plt.title('Local R²')
plt.savefig('~/eo_nas/EO4Alps/figs/map_local_r2.png')

# Plot Coefficients
for coef in ['coef_elevation', 'coef_severity', 'coef_VPD', 'coef_broadleaved', 'coef_coniferous']:
    hexagons_recov_rate_all.plot(column=coef, cmap='viridis', legend=True)
    plt.title(coef)
    plt.savefig(f'~/eo_nas/EO4Alps/figs/map_{coef}.png')

# K-Means Clustering
clustering_data = hexagons_recov_rate_all[['coef_elevation', 'coef_severity', 'coef_VPD', 'coef_broadleaved', 'coef_coniferous']]
kmeans = KMeans(n_clusters=4, random_state=42).fit(clustering_data)
hexagons_recov_rate_all['cluster'] = kmeans.labels_

# Plot Clusters
hexagons_recov_rate_all.plot(column='cluster', cmap='Set2', legend=True)
plt.title('Spatial Clusters Based on Predictor Effects')
plt.savefig('~/eo_nas/EO4Alps/figs/map_clusters.png')

# Plot Mean Recovery Rate
hexagons_recov_rate_all.plot(column='mean_recovery_rate', cmap='magma', legend=True)
plt.title('Mean Recovery Interval [years]')
plt.savefig('~/eo_nas/EO4Alps/figs/map_recovery_rate.png')

print(hexagons_recov_rate_all[['GRID_ID', 'local_r2']].head())
