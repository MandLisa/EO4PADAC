# -*- coding: utf-8 -*-
"""
Created on  Jan 23 13:48:34 2024

@author: lmandl
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import linregress
from statsmodels.gam.api import GLMGam, BSplines
from statsmodels.gam.api import BSplines
from sklearn.preprocessing import MinMaxScaler

# Load the CSV file
df = pd.read_csv("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/_data/table.csv")

# Set all values less than 0 in the "share" variable to 0 and greater than 10000 to 10000
df['share'] = df['share'].clip(lower=0, upper=10000)

# Subset the data for class == "trees" and year > yod
trees_data = df[(df['class'] == "trees") & (df['year'] > df['yod'])]

# Group the data by ID
grouped_trees_data = trees_data.groupby('ID')

# Function to fit GAM and extract smoothed time series
def fit_gam_and_extract_smoothed(data):
    if len(data) >= 4:
        # Sufficient data points, fit GAM model with reduced spline complexity
        bs = BSplines(data['year'], df=[3], degree=[3])
        gam_model = GLMGam.from_formula("share ~ 1", data=data, smoother=bs)
        gam_results = gam_model.fit()
        prediction_data = pd.DataFrame({'year': np.arange(data['year'].min() + 1, data['year'].max())})
        prediction_data['smoothed_share'] = gam_results.predict(prediction_data)
    else:
        # Insufficient data points, use a linear model
        slope, intercept, _, _, _ = linregress(data['year'], data['share'])
        prediction_data = pd.DataFrame({'year': np.arange(data['year'].min() + 1, data['year'].max())})
        prediction_data['smoothed_share'] = intercept + slope * prediction_data['year']
    
    # Add the additional columns from the original data
    for col in set(data.columns) - set(prediction_data.columns):
        prediction_data[col] = data[col].iloc[0]
    
    return prediction_data

# Apply the function to each group
smoothed_time_series_list = [fit_gam_and_extract_smoothed(group) for name, group in grouped_trees_data]

# Combine the results into a single dataframe
smoothed_time_series_df = pd.concat(smoothed_time_series_list)

# Scale the share values to be between 0 and 1
scaler = MinMaxScaler()
df['share'] = scaler.fit_transform(df[['share']])

# Merge the smoothed_time_series_df with df based on matching x, y, and ID
df_smoothed = df.merge(smoothed_time_series_df, on=['x', 'y', 'ID'], how='left')

# Create the share_smoothed column with smoothed values where available, and original values otherwise
df_smoothed['share_smoothed'] = df_smoothed['smoothed_share'].fillna(df_smoothed['share'])

# Remove the intermediate columns if needed
df_smoothed.drop(columns=['smoothed_share'], inplace=True)

# Print the smoothed time series for each ID
print(smoothed_time_series_df)

# Save the smoothed time series to a CSV file
smoothed_time_series_df.to_csv("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/_data/tree_cover_GAM.csv", index=False)

#-------------------------------------------------------------------------------
# Create plots

# Plot distribution of year
sns.histplot(df['yod'], binwidth=1, color='steelblue')
plt.title("Histogram of yod")
plt.xlabel("yod")
plt.ylabel("Frequency")
plt.savefig("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/yod_sappada.png", dpi=300)
plt.clf()

# Compute min value of tree share after 5 years after disturbance
# Filter the data for class == "trees"
df_trees = smoothed_time_series_df[smoothed_time_series_df['class'] == "trees"]

# Find the minimum tree share value and the year when the minimum is reached within the 5-year window after the year of disturbance for each time series (ID)
min_share_in_window = df_trees.groupby(['ID', 'yod']).apply(lambda x: x[(x['year'] >= x['yod']) & (x['year'] <= x['yod'] + 5)].nsmallest(1, 'share')).reset_index(drop=True)

# Calculate the duration until the minimum is reached for each time series (ID)
min_share_in_window['time_to_min'] = min_share_in_window['year'] - min_share_in_window['yod']

# Select the necessary columns and remove duplicates to keep only one row per time series (ID)
result = min_share_in_window[['ID', 'x', 'y', 'yod', 'year', 'time_to_min', 'share']]
result.rename(columns={'share': 'min_tree_share', 'year': 'min_year'}, inplace=True)

# Scale the share values to be between 0 and 1
result['min_tree_share'] = scaler.fit_transform(result[['min_tree_share']])

# Plot the distribution of time to minimum tree cover
sns.histplot(result['time_to_min'], binwidth=0.5, color='steelblue')
plt.title("Time until min tree cover is reached")
plt.xlabel("Years after disturbance")
plt.ylabel("Frequency")
plt.savefig("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/min_sappada.png", dpi=300)
plt.clf()

# Plot the share of trees over time
sns.scatterplot(data=result, x='time_to_min', y='min_tree_share', hue='ID', palette='tab10', s=100)
plt.title("Share of Trees over Time")
plt.xlabel("Years after disturbance")
plt.ylabel("Tree Share (divided by 10,000)")
plt.legend(title="Time series (ID)", bbox_to_anchor=(1.05, 1), loc='upper left')
plt.tight_layout()
plt.savefig("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/share_trees_after_dist.png", dpi=300)
plt.clf()

# Plot distribution of min tree cover after disturbance
sns.kdeplot(data=result, x='min_tree_share', hue='core_pixel', fill=True, alpha=0.4)
plt.title("Min tree cover [%] in a 5-year time window after disturbance")
plt.xlabel("Tree cover [%]")
plt.ylabel("Density")
plt.savefig("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/avg_tree_cov_after_sappada.png", dpi=300)
plt.clf()

# Compute average tree share 5 years before disturbance
avg_tree_share_5y_before = df_trees.groupby(['ID', 'yod']).apply(lambda x: x[(x['year'] >= x['yod'] - 10) & (x['year'] < x['yod'])]['share'].max()).reset_index()
avg_tree_share_5y_before.rename(columns={0: 'avg_tree_share_before'}, inplace=True)

# Merge with the result dataframe
result_with_avg = result.merge(avg_tree_share_5y_before, on=['ID', 'yod'])

# Calculate the severity of the disturbance as a percentage from the average tree share before
result_with_avg['severity'] = ((result_with_avg['avg_tree_share_before'] - result_with_avg['min_tree_share']) / result_with_avg['avg_tree_share_before']) * 100

# Final severity plot
sns.kdeplot(data=result_with_avg, x='severity', hue='core_pixel', fill=True, alpha=0.4)
plt.title("Severity")
plt.xlabel("Severity (impact of the disturbance)")
plt.ylabel("Density")
plt.xlim(-100, 100)
plt.savefig("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/severity.png", dpi=300)
plt.clf()

# Additional plots
sns.histplot(df['yod'], binwidth=1, color='steelblue')
plt.title("Histogram of yod")
plt.xlabel("yod")
plt.ylabel("Frequency")
plt.savefig("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/yod_sappada.png", dpi=300)
plt.clf()

sns.histplot(result['time_to_min'], binwidth=0.5, color='steelblue')
plt.title("Time until min tree cover is reached")
plt.xlabel("Years after disturbance")
plt.ylabel("Density")
plt.savefig("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/min_tree_cov.png", dpi=300)
plt.clf()

sns.kdeplot(data=result, x='min_tree_cov', hue='core_pixel', fill=True, alpha=0.4)
plt.title("Min tree cover [%] in a 5-year time window after disturbance")
plt.xlabel("Tree cover [%]")
plt.ylabel("Density")
plt.savefig("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/avg_tree_cov_after_sappada.png", dpi=300)
plt.clf()

sns.kdeplot(data=result_with_avg, x='avg_tree_share_before', hue='core_pixel', fill=True, alpha=0.4)
plt.title("Average tree cover 5 years before disturbance")
plt.xlabel("Tree
