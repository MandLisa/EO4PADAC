# Add year info to time series
# Author: Lisa Mandl
# Date: 2024-07-02
# Description: This script processes yearly NDVI feature files, combines them, cleans and interpolates values,
#              performs temporal stability analysis, and visualizes class-based trends and counts.

import os
import re
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from glob import glob
from scipy.interpolate import interp1d

# Set the directory containing the files
os.chdir("/data/eo/EO4Alps/level3/NDVI/features")

# List all text files matching the pattern
file_list = sorted(glob("features_*.txt"))

# Process each file: add year column and overwrite with comma-separated values
def process_file(file_name):
    year = re.search(r"features_(\d{4})\.txt", file_name).group(1)
    df = pd.read_csv(file_name, header=None, delim_whitespace=True)
    df['Year'] = int(year)
    df.to_csv(file_name, index=False, header=False, sep=",")

for file in file_list:
    process_file(file)

# Read all files and concatenate into a single DataFrame
data_frames = [pd.read_csv(f, header=None) for f in file_list]
features_combined = pd.concat(data_frames, ignore_index=True)
features_combined.to_csv("combined_features.csv", index=False, header=False)

# Reload to assign meaningful column names
combined_features = pd.read_csv("/data/eo/EO4Alps/level3/NDVI/combined_features.csv", header=None)
combined_features.columns = ['X', 'Y', 'value', 'class', 'year']
combined_features['ID'] = combined_features.groupby(['X', 'Y']).ngroup()
combined_features.to_csv("/data/eo/EO4Alps/level3/NDVI/combined_features.csv", index=False)

# Clean data by setting negative values to NaN and interpolating
combined_features_clean = combined_features.copy()
combined_features_clean['value'] = combined_features_clean['value'].apply(lambda x: np.nan if x < 0 else x)
combined_features_clean['value'] = combined_features_clean.groupby('ID')['value'].transform(lambda x: x.interpolate())

# Interpolate values with class and ID grouping
combined_features_clean1 = combined_features.sort_values(['ID', 'year']).copy()
combined_features_clean1['value'] = combined_features_clean1.apply(lambda row: np.nan if row['value'] < 0 else row['value'], axis=1)
combined_features_clean1['value'] = combined_features_clean1.groupby(['ID', 'class'])['value'].transform(lambda x: x.interpolate(limit_direction='both'))

# Plot all data points by class
sns.set(style="whitegrid")
g = sns.FacetGrid(combined_features, col="class", col_wrap=4, sharey=False, height=4)
g.map_dataframe(sns.lineplot, x="year", y="value", hue="ID", legend=False, estimator=None, units="ID")
g.set_axis_labels("Year", "Value")
g.fig.suptitle("Values Over Time by Class and ID", y=1.05)
plt.tight_layout()
plt.show()

# Plot cleaned data
g = sns.FacetGrid(combined_features_clean, col="class", col_wrap=4, sharey=False, height=4)
g.map_dataframe(sns.lineplot, x="year", y="value", hue="ID", legend=False, estimator=None, units="ID")
g.set_axis_labels("Year", "Value")
g.fig.suptitle("Temporal Stability of Candidate Pixels", y=1.05)
plt.tight_layout()
plt.show()

# Summary statistics
summary_stats = combined_features_clean1.groupby(['class', 'year'])['value'].agg(['mean', 'std']).reset_index()

# Line + ribbon plot
fig, axes = plt.subplots(nrows=1, ncols=len(summary_stats['class'].unique()), figsize=(20, 5), sharey=True)
for i, (cls, ax) in enumerate(zip(summary_stats['class'].unique(), axes)):
    class_data = combined_features_clean1[combined_features_clean1['class'] == cls]
    summary = summary_stats[summary_stats['class'] == cls]
    for _, group in class_data.groupby('ID'):
        ax.plot(group['year'], group['value'], alpha=0.3)
    ax.plot(summary['year'], summary['mean'], color='black', linewidth=2)
    ax.fill_between(summary['year'], summary['mean'] - summary['std'], summary['mean'] + summary['std'], color='grey', alpha=0.4)
    ax.set_title(f"Class {cls}")
    ax.set_xlabel("Year")
    ax.set_ylabel("Value")
fig.suptitle("Temporal Stability of Candidate Pixels", fontsize=16)
plt.tight_layout(rect=[0, 0.03, 1, 0.95])
plt.show()

# Count of IDs per class
class_counts = combined_features_clean1.groupby('class')['ID'].nunique().reset_index(name='num_ids')

# Bar chart
plt.figure(figsize=(8, 6))
sns.barplot(x='class', y='num_ids', data=class_counts, palette="Blues_d")
for index, row in class_counts.iterrows():
    plt.text(index, row.num_ids + 1, str(row.num_ids), ha='center')
plt.title("Number of IDs per Class")
plt.xlabel("Class")
plt.ylabel("Number of IDs")
plt.tight_layout()
plt.show()
