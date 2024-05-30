# -*- coding: utf-8 -*-
"""
Created on Nov 30 19:16:57 2024

@author: ge45vaz
"""

import pandas as pd
import numpy as np
import glob
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.signal import medfilt
from statsmodels.gam.api import GLMGam, BSplines
from sklearn.linear_model import LinearRegression
import os

# Set working directory
os.chdir("/mnt/public/Projects/DataCube/projects/foreco/alps/level3_predictions/v3_dist/_csv")

# Function to read and combine CSV files
def read_and_combine_csv_files(pattern):
    csv_files = glob.glob(pattern)
    dataframes = [pd.read_csv(file) for file in csv_files]
    combined_df = pd.concat(dataframes, ignore_index=True)
    return combined_df.drop_duplicates(subset='ID')

# Function to apply smoothing to tree cover before yod
def apply_smoothing(df):
    df['ID'] = df.groupby(['x', 'y']).ngroup()
    df = df[df['yod'] <= 2017]
    df['share'] = df['share'].clip(lower=0, upper=10000)
    
    def smooth_function(x):
        window_size = min(5, len(x))
        return medfilt(x, kernel_size=window_size)
    
    df['cover_smooth'] = df.groupby('ID')['share'].transform(
        lambda x: np.where(df['year'] < df['yod'], smooth_function(x), x)
    ).fillna(df['share'])
    
    return df

# Function to compute the time until minimum tree cover is reached after disturbance
def time_to_min(df):
    min_share_in_window = df.groupby(['ID', 'yod']).apply(
        lambda x: x[(x['year'] > x['yod']) & (x['year'] <= x['yod'] + 5)].nsmallest(1, 'share')
    ).reset_index(drop=True)
    
    min_share_in_window['time_to_min'] = min_share_in_window['year'] - min_share_in_window['yod']
    min_share_in_window['min_year'] = min_share_in_window['year']
    min_share_in_window['min_tree_cover'] = min_share_in_window['share']
    min_share_in_window['core_pixel'] = np.where(min_share_in_window['edge'] == 1, "core pixel", "edge pixel")
    
    return min_share_in_window.drop(columns=['year', 'share'])

# Function to fit a GAM model and extract smoothed values
def fit_gam_and_extract_smoothed(data):
    bs = BSplines(data['year'].values[:, None], df=[3], degree=[3])
    if len(data) >= 4:
        gam_model = GLMGam.from_formula('share ~ 1', smoother=bs, data=data).fit()
        prediction_data = pd.DataFrame({'year': np.arange(data['year'].min() + 1, data['year'].max() + 1)})
        prediction_data['smoothed_share'] = gam_model.predict(prediction_data)
    else:
        lm_model = LinearRegression().fit(data[['year']], data['share'])
        prediction_data = pd.DataFrame({'year': np.arange(data['year'].min() + 1, data['year'].max() + 1)})
        prediction_data['smoothed_share'] = lm_model.predict(prediction_data[['year']])
    
    prediction_data['ID'] = data['ID'].iloc[0]
    return prediction_data

# Function to fit a linear model and extract smoothed values
def fit_linear_and_extract_smoothed(data):
    min_year = data['year'].min()
    max_year = 2021
    prediction_data = pd.DataFrame({'year': np.arange(min_year, max_year + 1)})
    lm_model = LinearRegression().fit(data[['year']], data['share'])
    prediction_data['smoothed_share'] = lm_model.predict(prediction_data[['year']])
    prediction_data['ID'] = data['ID'].iloc[0]
    return prediction_data

# Function to calculate severity
def calculate_severity(df):
    avg_tree_share_5y_before = df.groupby('ID').apply(
        lambda x: x[(x['year'] > (x['yod'] - 5)) & (x['year'] <= x['yod'])]['share'].mean()
    ).reset_index(name='avg_tree_cover_5y_before')

    avg_tree_share_5y_after = df.groupby('ID').apply(
        lambda x: x[(x['year'] > x['yod']) & (x['year'] <= (x['yod'] + 5))]['cover_GAM'].mean()
    ).reset_index(name='avg_tree_cover_5y_after')

    severity = pd.merge(avg_tree_share_5y_before, avg_tree_share_5y_after, on='ID')
    severity['diff'] = severity['avg_tree_cover_5y_after'] - severity['avg_tree_cover_5y_before']
    severity['perc'] = (severity['avg_tree_cover_5y_after'] / severity['avg_tree_cover_5y_before']) * 100
    severity['severity'] = 100 - severity['perc']
    severity['severity'][severity['severity'] < 0] = 0
    severity['severity'] *= -1

    return severity

# Main workflow
def main():
    subset_combined_unique = read_and_combine_csv_files("*fcover_*.csv")
    subset_trees = subset_combined_unique[subset_combined_unique['class'] == 'trees']
    subset_trees = apply_smoothing(subset_trees)
    t_min = time_to_min(subset_trees)

    # Plot histogram
    plt.figure(figsize=(10, 6))
    sns.histplot(t_min['time_to_min'], bins=10, kde=False, color='steelblue')
    plt.title('Time until min tree cover is reached')
    plt.xlabel('years after disturbance')
    plt.ylabel('Frequency')
    plt.savefig("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/min_sappada.png", dpi=300)
    plt.show()

    # GAM fitting for share values > t_min
    smoothed_trees_gam = subset_trees[subset_trees['year'] > subset_trees['min_year']]
    grouped_trees_data = smoothed_trees_gam.groupby('ID')
    smoothed_time_series_list = [fit_gam_and_extract_smoothed(group) for _, group in grouped_trees_data]
    smoothed_time_series_df = pd.concat(smoothed_time_series_list, ignore_index=True)
    trees_smoothed = pd.merge(subset_trees, smoothed_time_series_df, on=['ID', 'year'], how='left')

    # Linear model for year > min_year
    smoothed_trees_lm = subset_trees[subset_trees['year'] > subset_trees['min_year']]
    grouped_trees_data_lm = smoothed_trees_lm.groupby('ID')
    smoothed_time_series_list_lin = [fit_linear_and_extract_smoothed(group) for _, group in grouped_trees_data_lm]
    smoothed_time_series_df_lin = pd.concat(smoothed_time_series_list_lin, ignore_index=True)
    trees_smoothed2 = pd.merge(trees_smoothed, smoothed_time_series_df_lin, on=['ID', 'year'], how='left')

    # Create the cover_GAM and cover_lm columns
    trees_smoothed2['cover_GAM'] = trees_smoothed2['smoothed_share_x'].combine_first(trees_smoothed2['share'])
    trees_smoothed2['cover_lm'] = trees_smoothed2['smoothed_share_y'].combine_first(trees_smoothed2['share'])

    # Save the resulting dataframe to a CSV file
    trees_smoothed2.to_csv("/mnt/public/Projects/DataCube/projects/foreco/alps/level3_predictions/v3_dist/_csv/trees_smoothed2.csv", index=False)

    # Calculate severity
    severity = calculate_severity(trees_smoothed2)
    severity.to_csv("/mnt/public/Projects/DataCube/projects/foreco/alps/level3_predictions/v3_dist/_csv/severity.csv", index=False)

# Execute main function
if __name__ == "__main__":
    main()
