import pandas as pd
import geopandas as gpd
import numpy as np
from shapely.geometry import Point
from pygam import LinearGAM, s, te
import matplotlib.pyplot as plt

# ------------------------------------------------------------------------------
# Load and filter data
recovery = pd.read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_GWR.csv")

recovery_filt = recovery[recovery['yod'] < 2013].copy()
recovery_filt['recov_10'] = (recovery_filt['recovery_rate'] <= 10).astype(int)

# ------------------------------------------------------------------------------
# Calculate pre-disturbance and post-disturbance means
def compute_means(df):
    df = df.copy()
    grouped = df.groupby('ID')
    df['pre_dist_coni'] = grouped.apply(lambda g: np.where(g['year'] < g['yod'], g['coniferous'].mean(), np.nan)).explode().values
    df['pre_dist_broadl'] = grouped.apply(lambda g: np.where(g['year'] < g['yod'], g['broadleaved'].mean(), np.nan)).explode().values
    df['post_dist_bare'] = grouped.apply(lambda g: np.where(g['year'] < g['yod'], g[g['year'] > g['yod']]['bare_ground'].mean(), np.nan)).explode().values
    return df

recovery_filt = compute_means(recovery_filt)
recovery_filt['post_dist_bare'] = recovery_filt['post_dist_bare'] / 100

# ------------------------------------------------------------------------------
# Fill missing values using first non-NA value per ID
def fill_first_non_na(df, column):
    return df.groupby('ID')[column].transform(lambda x: x.ffill().bfill())

for col in ['post_dist_bare', 'pre_dist_coni', 'pre_dist_broadl']:
    recovery_filt[col] = fill_first_non_na(recovery_filt, col)

# ------------------------------------------------------------------------------
# Calculate years since disturbance (post_year)
recovery_filt['post_year'] = recovery_filt['year'] - recovery_filt['yod']
recovery_filt = recovery_filt[(recovery_filt['post_year'] >= 0) & (recovery_filt['post_year'] <= 10)]

# ------------------------------------------------------------------------------
# Compute VPD_post0 to VPD_post10
for i in range(11):
    col_name = f'VPD_post{i}'
    recovery_filt[col_name] = recovery_filt.groupby(['ID', 'yod'])['VPD_anomaly'].transform(
        lambda x: x.rolling(window=i+1, min_periods=1).mean()
    )

# ------------------------------------------------------------------------------
# Keep only one observation per ID
recovery_unique = recovery_filt.drop_duplicates(subset='ID')

# Convert to GeoDataFrame
geometry = [Point(xy) for xy in zip(recovery_unique['x'], recovery_unique['y'])]
recovery_unique_gdf = gpd.GeoDataFrame(recovery_unique, geometry=geometry, crs="EPSG:3035")

# ------------------------------------------------------------------------------
# Load hexagons and join
hexagons = gpd.read_file("~/eo_nas/EO4Alps/gis/hexagons/hex_500.shp")
hexagons_selected = hexagons[['GRID_ID']]
recovery_unique_gdf = gpd.sjoin(recovery_unique_gdf, hexagons_selected, how='left', predicate='intersects')

# ------------------------------------------------------------------------------
# Calculate percent recovered and group statistics
recovery_unique_gdf['percent_recovered'] = recovery_unique_gdf.groupby('GRID_ID')['recov_10'].transform(
    lambda x: 100 * x.sum() / x.count()
)

hexagon_predictors = recovery_unique_gdf.groupby('GRID_ID').agg(
    mean_elevation=('height', 'mean'),
    mean_severity=('severity_relative', 'mean'),
    mean_VPD=('mean_VPD10', 'mean'),
    mean_VPD_ano=('mean_VPD_ano10', 'mean'),
    mean_VPD_yod1=('VPD_post1', 'mean'),
    mean_prec_total=('mean_prec_total', 'mean'),
    mean_temp_total=('mean_temp_total', 'mean'),
    mean_recovery_rate=('recovery_rate', 'mean'),
    mean_percent_recovered=('percent_recovered', 'mean'),
    mean_broadleaved=('pre_dist_broadl', 'mean'),
    mean_coniferous=('pre_dist_coni', 'mean'),
    mean_bare=('post_dist_bare', 'mean')
).reset_index()

hexagon_predictors['mean_pre_dist_tree_cover'] = hexagon_predictors['mean_broadleaved'] + hexagon_predictors['mean_coniferous']

# ------------------------------------------------------------------------------
# Merge with geometry
hexagons_recov10 = hexagons_selected.merge(hexagon_predictors, on='GRID_ID')
hexagons_recov10 = hexagons.merge(hexagons_recov10, on='GRID_ID')
hexagons_recov10 = gpd.GeoDataFrame(hexagons_recov10, geometry='geometry', crs="EPSG:3035")

# Compute centroids and coordinates
hexagons_recov10['centroid'] = hexagons_recov10.geometry.centroid
hexagons_recov10['long'] = hexagons_recov10.centroid.x
hexagons_recov10['lat'] = hexagons_recov10.centroid.y

# ------------------------------------------------------------------------------
# Fit a GAM model using pygam
X = hexagons_recov10[['long', 'lat', 'mean_elevation', 'mean_severity',
                      'mean_VPD_yod1', 'mean_prec_total', 'mean_temp_total',
                      'mean_pre_dist_tree_cover', 'mean_bare']].values
y = hexagons_recov10['mean_percent_recovered'].values

gam = LinearGAM(
    s(0) + s(1) + s(2) + s(3) + s(4) + s(5) + s(6) + s(7) + s(8)
).fit(X, y)

# ------------------------------------------------------------------------------
# Visualize partial effects
for i, term in enumerate(gam.terms):
    if term.isintercept:
        continue
    XX = gam.generate_X_grid(term=i)
    plt.figure()
    plt.plot(XX[:, term.feature], gam.partial_dependence(term=i, X=XX))
    plt.plot(XX[:, term.feature], gam.partial_dependence(term=i, X=XX, width=0.95)[1], c='r', ls='--')
    plt.title(f'Partial effect: {hexagons_recov10.columns[term.feature]}')
    plt.xlabel(hexagons_recov10.columns[term.feature])
    plt.ylabel('Effect on recovery success')
    plt.show()
