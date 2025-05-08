import pandas as pd
import geopandas as gpd
import numpy as np
from shapely.geometry import Point
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LinearRegression
from tqdm import tqdm

# ------------------------------------------------------------------------------
# Load input data
hexagons = gpd.read_file("~/eo_nas/EO4Alps/gis/hexagons/hex_500.shp")
recovery = pd.read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_random.csv")

# ------------------------------------------------------------------------------
# Create GeoDataFrames
recovery['geometry'] = gpd.points_from_xy(recovery.x, recovery.y)
recovery_sf = gpd.GeoDataFrame(recovery, geometry='geometry', crs=3035)
recovery_unique = recovery.drop_duplicates(subset='ID')
recovery_unique['geometry'] = gpd.points_from_xy(recovery_unique.x, recovery_unique.y)
recovery_unique_sf = gpd.GeoDataFrame(recovery_unique, geometry='geometry', crs=3035)

# ------------------------------------------------------------------------------
# Spatial Join
hexagons = hexagons.to_crs(recovery_sf.crs)
recovery_sf = gpd.sjoin(recovery_sf, hexagons[['GRID_ID', 'geometry']], how='left', predicate='intersects')
recovery_unique_sf = gpd.sjoin(recovery_unique_sf, hexagons[['GRID_ID', 'geometry']], how='left', predicate='intersects')

# ------------------------------------------------------------------------------
# Count number of points per hexagon
hexagons['num_points'] = hexagons.geometry.apply(
    lambda g: recovery_unique_sf.geometry.within(g).sum()
)

# ------------------------------------------------------------------------------
# Aggregation per year per hexagon (e.g. VPD)
aggregated_vpd = recovery_sf.groupby(['GRID_ID', 'year'])['VPD_anomaly'].mean().reset_index()
aggregated_vpd.rename(columns={'VPD_anomaly': 'mean_vpd'}, inplace=True)
aggregated_vpd.to_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/VPD_year_hex.csv", index=False)

# ------------------------------------------------------------------------------
# Trend estimation per hexagon
def compute_trend(df, group_col, time_col, value_col):
    trend_data = []
    for grid_id, group in df.groupby(group_col):
        if group[time_col].nunique() >= 2:
            X = group[time_col].values.reshape(-1, 1)
            y = group[value_col].values
            if np.all(np.isnan(y)):
                continue
            model = LinearRegression().fit(X, y)
            slope = model.coef_[0]
            trend_data.append({'GRID_ID': grid_id, 'trend_slope': slope})
    return pd.DataFrame(trend_data)

trend_vpd = compute_trend(aggregated_vpd, 'GRID_ID', 'year', 'mean_vpd')

# ------------------------------------------------------------------------------
# Merge with geometry and plot
hex_trend_vpd = hexagons.merge(trend_vpd, on='GRID_ID', how='left')
fig, ax = plt.subplots(1, 1, figsize=(10, 8))
hex_trend_vpd.plot(column='trend_slope', cmap='RdBu', linewidth=0.1, edgecolor='grey',
                   legend=True, ax=ax)
ax.set_title("VPD Anomaly Trend (1986–2018)")
plt.tight_layout()
plt.show()
