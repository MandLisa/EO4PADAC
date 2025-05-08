import xarray as xr
import pandas as pd
import numpy as np
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from sklearn.preprocessing import StandardScaler
import seaborn as sns
import matplotlib.pyplot as plt

# Load data as DataFrame first
df = pd.read_csv('recovery_climate_topo.csv')  # Update path as needed

# Convert to long format
vpd_cols = [col for col in df.columns if col.startswith("VPD_")]
df_long = df.melt(id_vars=[col for col in df.columns if col not in vpd_cols],
                  value_vars=vpd_cols,
                  var_name='month', value_name='VPD_anomaly')

# Add month as categorical
month_order = ["VPD_apr", "VPD_may", "VPD_jun", "VPD_jul", "VPD_aug", "VPD_sep", "VPD_oct"]
df_long["month"] = pd.Categorical(df_long["month"], categories=month_order, ordered=True)

# Convert to xarray
ds = df_long.set_index(['ID', 'year', 'month']).to_xarray()

# Pre-disturbance VPD mean
pre_mask = ds['year'] <= ds['yod']
mean_pre = ds['VPD_anomaly'].where(pre_mask).groupby('ID').mean(dim='year', skipna=True)
ds['pre_disturbance_VPD_anomaly'] = mean_pre

# Post-disturbance VPD mean
post_mask = ds['year'] > ds['yod']
mean_post = ds['VPD_anomaly'].where(post_mask).groupby('ID').mean(dim='year', skipna=True)
ds['mean_VPD_post'] = mean_post

# Mean VPD over 1-5 years post disturbance
for i in range(1, 6):
    target_years = ds['yod'] + i
    mask = ds['year'] == target_years
    ds[f'mean_VPD_post_{i}_year'] = ds['VPD_anomaly'].where(mask).groupby('ID').mean(dim='year')

# Add numeric month
month_map = {m: i+1 for i, m in enumerate(month_order)}
df_long['month_num'] = df_long['month'].map(month_map)

# Reconstruct in pandas for groupby logic
df_long2 = df_long.copy()

# Compute consecutive high-VPD months after disturbance
def consecutive_positive(anomalies):
    count = 0
    max_count = 0
    for val in anomalies:
        if val > 0:
            count += 1
            max_count = max(max_count, count)
        else:
            count = 0
    return max_count

results = []
for (ID, year), sub in df_long2[df_long2['year'] > df_long2['yod']].groupby(['ID', 'year']):
    anomalies = sub.sort_values('month_num')['VPD_anomaly'].tolist()
    result = {
        'ID': ID,
        'year': year,
        'VPD_consecutive': consecutive_positive(anomalies)
    }
    results.append(result)

df_consec = pd.DataFrame(results)
df_long3 = df_long2.merge(df_consec, on=['ID', 'year'], how='left')

# Assign values per year-since-disturbance
for i in range(1, 6):
    df_long3[f'VPD_consecutive_{i}y'] = df_long3.apply(
        lambda x: x['VPD_consecutive'] if x['ysd'] == i else np.nan, axis=1
    )

# Fill missing values by ID
df_long3.sort_values(['ID', 'year'], inplace=True)
df_long3.update(df_long3.groupby('ID')[[f'VPD_consecutive_{i}y' for i in range(1, 6)]].ffill().bfill())

# Save data
df_long3.to_csv('recovery_climate_topo_2.0.csv', index=False)
df_unique = df_long3.drop_duplicates(subset=['ID'])
df_unique.to_csv('recovery_climate_topo_2.0_unique.csv', index=False)

# Assign recovery status
df_unique['recovery_status'] = np.where(df_unique['recovery_rate'] == 100, "Not Recovered", "Recovered")
df_long3['recovery_status'] = np.where(df_long3['recovery_rate'] == 100, "Not Recovered", "Recovered")

# Plotting
sns.scatterplot(data=df_long3, x='recovery_rate', y='mean_VPD_post', hue='recovery_status', style='recovery_status')
plt.title('Recovery Rate vs Mean VPD Post')
plt.show()

# Linear regression
features = ['severity_relative', 'slope', 'height', 'aspect', 'mean_VPD_pre', 'VPD_consecutive_1y', 'VPD_consecutive_2y']
df_model = df_long3.dropna(subset=features + ['recovery_rate'])
X = df_model[features]
y = df_model['recovery_rate']

lr = LinearRegression().fit(X, y)
print("Unstandardized coefficients:", lr.coef_)

# Standardized coefficients
scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)
lr_std = LinearRegression().fit(X_scaled, y)
print("Standardized coefficients:", lr_std.coef_)

# Random forest
rf = RandomForestRegressor(n_estimators=100, random_state=42)
rf.fit(X, y)
importances = rf.feature_importances_

importance_df = pd.DataFrame({'Feature': features, 'Importance': importances})
importance_df.sort_values('Importance', ascending=False, inplace=True)

sns.barplot(data=importance_df, x='Importance', y='Feature')
plt.title('Random Forest Feature Importance')
plt.show()
