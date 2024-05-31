# -*- coding: utf-8 -*-
"""
Created on Dec 3 14:22:47 2023

@author: ge45vaz
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import zscore
from sklearn.linear_model import LinearRegression
from statsmodels.nonparametric.smoothers_lowess import lowess

# Load data
recovery = pd.read_csv('/data/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/csv/recovery_files/recovery.csv')

# Function to plot trajectories
def plot_trajectory(trajectory, x_col, y_col, title, y_label, point_color, line_colors):
    plt.figure(figsize=(10, 6))
    plt.scatter(trajectory[x_col], trajectory[y_col], color=point_color, label=y_col)
    
    # Linear regression line
    post_yod = trajectory[trajectory['year'] >= trajectory['yod'].iloc[0]]
    X_post_yod = post_yod['year'].values.reshape(-1, 1)
    y_post_yod = post_yod[y_col].values
    lm = LinearRegression()
    lm.fit(X_post_yod, y_post_yod)
    plt.plot(post_yod['year'], lm.predict(X_post_yod), color=line_colors[0], label='Linear Regression')
    
    # GAM smoother
    gam_smooth = lowess(y_post_yod, post_yod['year'], frac=0.3)
    plt.plot(gam_smooth[:, 0], gam_smooth[:, 1], color=line_colors[1], label='GAM')
    
    plt.axvline(x=trajectory['yod'].iloc[0], linestyle='--', color='black')
    plt.title(title)
    plt.xlabel('Year')
    plt.ylabel(y_label)
    plt.legend()
    plt.show()

# Plot 1
sample_id = np.random.choice(recovery[recovery['yod'] < 2010]['ID'].unique())
trajectory = recovery[(recovery['ID'] == sample_id) & (~pd.isna(recovery['core_pixel']))]
plot_trajectory(trajectory, 'year', 'GAM', '', 'Share', 'red', ['green', 'blue'])

# Plot 2
sample_id = np.random.choice(recovery[recovery['yod'] < 2005]['ID'].unique())
trajectory = recovery[(recovery['ID'] == sample_id) & (~pd.isna(recovery['core_pixel']))]
plt.figure(figsize=(10, 6))
plt.scatter(trajectory['year'], trajectory['GAM'], color='blue', s=50, label='GAM')
plt.scatter(trajectory['year'], trajectory['share'], color='black', s=100, alpha=0.8, label='Share')
plt.axvline(x=trajectory['yod'].iloc[0], linestyle='--', color='black')
plt.title('')
plt.xlabel('Year')
plt.ylabel('
