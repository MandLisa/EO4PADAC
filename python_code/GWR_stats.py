# ----------------------------------------------------------------------
# Title: GWR on 10-Year Forest Recovery Success in the European Alps
# Author: Lisa Mandl
# Date: 2025-06-02
# Description: This script calculates spatial predictors and performs a 
#              Geographically Weighted Regression analysis to examine
#              drivers of 10-year post-disturbance forest recovery across
#              the European Alps. Outputs include spatial coefficient maps,
#              residuals, and summary statistics.
# ----------------------------------------------------------------------

import geopandas as gpd
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from shapely.geometry import Point
from mgwr.gwr import GWR
from mgwr.sel_bw import Sel_BW
from sklearn.preprocessing import StandardScaler

# Load dataset
recovery = pd.read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_GWR.csv")

# Filter records
recovery = recovery[recovery['yod'] < 2013].copy()
recovery['recov_10'] = (recovery['recovery_rate'] <= 10).astype(int)

# Compute pre-disturbance means per ID
def compute_group_means(df):
    pre_df = df[df['year'] < df['yod'].iloc[0]]
    post_df = df[df['year'] > df['yod'].iloc[0]]
    return pd.Series({
        'pre_dist_coni': pre_df['coniferous'].mean(skipna=True),
        'pre_dist_broadl': pre_df['broadleaved'].mean(skipna=True),
        'post_dist_bare': post_df['bare_ground'].mean(skipna=True)
    })

recovery_means = recovery.groupby('ID').apply(compute_group_means).reset_index()
recovery = pd.merge(recovery, recovery_means, on='ID', how='left')

# One observation per ID
recovery_unique = recovery.drop_duplicates(subset='ID')

# Convert to GeoDataFrames
recovery['geometry'] = recovery.apply(lambda row: Point(row['x'], row['y']), axis=1)
recovery_unique['geometry'] = recovery_unique.apply(lambda row: Point(row['x'], row['y']), axis=1)
recovery_gdf = gpd.GeoDataFrame(recovery, crs=3035, geometry='geometry')
recovery_unique_gdf = gpd.GeoDataFrame(recovery_unique, crs=3035, geometry='geometry')

# Load hexagons
hexagons = gpd.read_file("~/eo_nas/EO4Alps/gis/hexagons/hex_500.shp")
hexagons_selected = hexagons[['GRID_ID']].copy()

# Spatial join
recovery_gdf = gpd.sjoin(recovery_gdf, hexagons_selected, predicate='intersects')
recovery_unique_gdf = gpd.sjoin(recovery_unique_gdf, hexagons_selected, predicate='intersects')

# Aggregate by GRID_ID
agg = recovery_unique_gdf.groupby('GRID_ID').agg({
    'height': 'mean',
    'severity_relative': 'mean',
    'VPD_yod1': 'mean',
    'recovery_rate': 'mean',
    'recov_10': 'sum',
    'pre_dist_coni': 'mean',
    'pre_dist_broadl': 'mean',
    'post_dist_bare': 'mean',
    'forest_type': lambda x: x.mode().iloc[0] if not x.mode().empty else np.nan
}).rename(columns={
    'height': 'mean_elevation',
    'severity_relative': 'mean_severity',
    'VPD_yod1': 'mean_VPD',
    'recovery_rate': 'mean_recovery_rate',
    'recov_10': 'total_recovered',
    'pre_dist_coni': 'mean_coniferous',
    'pre_dist_broadl': 'mean_broadleaved',
    'post_dist_bare': 'mean_bare',
    'forest_type': 'dominant_forest_type'
}).reset_index()

agg['mean_percent_recovered'] = 100 * agg['total_recovered'] / recovery_unique_gdf.groupby('GRID_ID').size().values

# Merge with hexagons
hexagons_merged = hexagons_selected.merge(agg, on='GRID_ID')
hexagons_merged = hexagons.merge(hexagons_merged, on='GRID_ID')
hexagons_merged = hexagons_merged.dropna()

# Prepare GWR
coords = np.array([hexagons_merged.geometry.centroid.x, hexagons_merged.geometry.centroid.y]).T
X_vars = ['mean_elevation', 'mean_severity', 'mean_VPD', 'mean_coniferous', 'mean_broadleaved', 'mean_bare']
X = hexagons_merged[X_vars].values
y = hexagons_merged['mean_percent_recovered'].values.reshape((-1, 1))

scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

# Select bandwidth
bw = Sel_BW(coords, y, X_scaled).search()

# Fit GWR model
gwr_model = GWR(coords, y, X_scaled, bw)
gwr_results = gwr_model.fit()

# Add GWR results to GeoDataFrame
for i, var in enumerate(X_vars):
    hexagons_merged[f'coef_{var}'] = gwr_results.params[:, i + 1]  # skip intercept

hexagons_merged['local_r2'] = gwr_results.localR2

# Print mean local R²
print("Mean Local R²:", np.mean(gwr_results.localR2))

# Save GWR results map
fig, ax = plt.subplots(1, 1, figsize=(10, 6))
hexagons_merged.plot(column='local_r2', ax=ax, legend=True, cmap='magma')
plt.title("Local R² from GWR")
plt.savefig("~/eo_nas/EO4Alps/figs/map_local_r2_recov10_python.png", dpi=300)

# Export as shapefile
hexagons_merged.to_file("~/eo_nas/EO4Alps/gis/recovery_hexagons/recov10_rates_python.shp")
