import pandas as pd
import numpy as np
import geopandas as gpd
from sklearn.model_selection import KFold
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error
from pygam import LinearGAM, s
from mgwr.gwr import GWR, Sel_BW
from libpysal.weights import Queen
from esda.moran import Moran

# Load your data (replace with actual file paths or data loading routines)
# hexagons_recov10 = gpd.read_file("path_to/hexagons_recov10.shp")
# hexagons_sp = gpd.read_file("path_to/hexagons_sp.shp")

# Prepare predictors and response
predictor_cols = [
    "mean_elevation", "mean_severity", "mean_VPD_yod1",
    "mean_prec_total", "mean_temp_total",
    "mean_pre_dist_tree_cover", "mean_bare"
]
X = hexagons_recov10[predictor_cols]
y = hexagons_recov10["mean_percent_recovered"]

# OLS model
ols_model = LinearRegression().fit(X, y)
hexagons_recov10["resid_ols"] = y - ols_model.predict(X)
r2_ols = ols_model.score(X, y)

# GAM model
gam_model = LinearGAM(sum([s(i) for i in range(X.shape[1])])).fit(X, y)
hexagons_recov10["resid_gam"] = y - gam_model.predict(X)
r2_gam = gam_model.statistics_["pseudo_r2"]["explained_deviance"]

# Moran's I
w = Queen.from_dataframe(hexagons_recov10)
moran_ols = Moran(hexagons_recov10["resid_ols"], w)
moran_gam = Moran(hexagons_recov10["resid_gam"], w)

# Cross-Validation for OLS
kf = KFold(n_splits=10, shuffle=True, random_state=42)
rmse_ols = []
for train_idx, test_idx in kf.split(X):
    model = LinearRegression().fit(X.iloc[train_idx], y.iloc[train_idx])
    preds = model.predict(X.iloc[test_idx])
    rmse_ols.append(np.sqrt(mean_squared_error(y.iloc[test_idx], preds)))

# Cross-Validation for GAM
rmse_gam = []
for train_idx, test_idx in kf.split(X):
    gam = LinearGAM(sum([s(i) for i in range(X.shape[1])])).fit(X.iloc[train_idx], y.iloc[train_idx])
    preds = gam.predict(X.iloc[test_idx])
    rmse_gam.append(np.sqrt(mean_squared_error(y.iloc[test_idx], preds)))

# GWR model (single fit)
coords = np.column_stack((
    hexagons_sp.geometry.centroid.x,
    hexagons_sp.geometry.centroid.y
))
X_gwr = hexagons_sp[predictor_cols].values
y_gwr = hexagons_sp["mean_percent_recovered"].values.reshape(-1, 1)

bw = Sel_BW(coords, y_gwr, X_gwr).search()
gwr_model = GWR(coords, y_gwr, X_gwr, bw).fit()
hexagons_sp["resid_GWR"] = y_gwr.flatten() - gwr_model.predictions.flatten()

moran_gwr = Moran(hexagons_sp["resid_GWR"], Queen.from_dataframe(hexagons_sp))

# Output
print(f"OLS R²: {r2_ols:.3f}, RMSE (mean): {np.mean(rmse_ols):.2f}, Moran's I (OLS): {moran_ols.I:.2f}")
print(f"GAM R²: {r2_gam:.3f}, RMSE (mean): {np.mean(rmse_gam):.2f}, Moran's I (GAM): {moran_gam.I:.2f}")
print(f"GWR RMSE: {np.sqrt(mean_squared_error(y_gwr, gwr_model.predictions)):.2f}, Moran's I (GWR): {moran_gwr.I:.2f}")
