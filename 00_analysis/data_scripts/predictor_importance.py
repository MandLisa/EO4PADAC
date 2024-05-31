# -*- coding: utf-8 -*-
"""
Created on Nov 23 16:35:20 2023

@author: ge45vaz
"""

import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score, roc_curve
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import seaborn as sns

# Load data
recovery_clean = pd.read_csv("/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/csv/recovery_files/recovery_clean.csv")

# Compute pre-disturbance tree cover
recovery_merge = recovery_clean.copy()
recovery_merge['pre_dist_TV'] = recovery_merge.groupby('ID')['TV'].transform(lambda x: x[recovery_merge['year'] < recovery_merge['min_year_fcover']].mean())

# Compute bare ground share 3 and 5 years post-disturbance
recovery_merge['BG_3'] = recovery_merge.groupby('ID')['BG'].transform(lambda x: x[recovery_merge['year'] == recovery_merge['yod'] + 3].first())
recovery_merge['BG_5'] = recovery_merge.groupby('ID')['BG'].transform(lambda x: x[recovery_merge['year'] == recovery_merge['yod'] + 5].first())

# Compute non-treed vegetation share 3 and 5 years post-disturbance
recovery_merge['NTV_3'] = recovery_merge.groupby('ID')['NTV'].transform(lambda x: x[recovery_merge['year'] == recovery_merge['yod'] + 3].first())
recovery_merge['NTV_5'] = recovery_merge.groupby('ID')['NTV'].transform(lambda x: x[recovery_merge['year'] == recovery_merge['yod'] + 5].first())

# Define recovery status based on various definitions
recovery_merge['recovery_status_bn'] = np.where(recovery_merge['recovery_bn'].notna(), 1, 0)
recovery_merge['recovery_status_FAO'] = np.where(recovery_merge['recovery_abs'].notna(), 1, 0)
recovery_merge['recovery_status_bn_5y'] = np.where((recovery_merge['recovery_bn'].isna()) | (recovery_merge['recovery_bn'] > 5), 0, 1)
recovery_merge['recovery_status_bn_10y'] = np.where((recovery_merge['recovery_bn'].isna()) | (recovery_merge['recovery_bn'] > 10), 0, 1)
recovery_merge['recovery_status_bn_15y'] = np.where((recovery_merge['recovery_bn'].isna()) | (recovery_merge['recovery_bn'] > 15), 0, 1)
recovery_merge['recovery_status_FAO_5y'] = np.where((recovery_merge['recovery_abs'].isna()) | (recovery_merge['recovery_abs'] > 5), 0, 1)
recovery_merge['recovery_status_FAO_10y'] = np.where((recovery_merge['recovery_abs'].isna()) | (recovery_merge['recovery_abs'] > 10), 0, 1)
recovery_merge['recovery_status_FAO_15y'] = np.where((recovery_merge['recovery_abs'].isna()) | (recovery_merge['recovery_abs'] > 15), 0, 1)

# Select unique records
recovery_clean_unique = recovery_merge.drop_duplicates(subset=['ID'])

# Z-transformation of predictor variables
scaler = StandardScaler()
recovery_clean_unique[['relative_severity_fcover_scaled', 'pre_dist_TV_scaled', 'BG_3_scaled', 'NTV_3_scaled', 'BG_5_scaled', 'NTV_5_scaled']] = scaler.fit_transform(recovery_clean_unique[['relative_severity_fcover', 'pre_dist_TV', 'BG_3', 'NTV_3', 'BG_5', 'NTV_5']])

# Logistic Regression model for recovery limited to 10y
X = recovery_clean_unique[['relative_severity_fcover_scaled', 'pre_dist_TV_scaled', 'BG_3_scaled']]
y = recovery_clean_unique['recovery_status_bn_10y']

model = LogisticRegression()
model.fit(X, y)

# Summary and coefficients
print(f"Coefficients: {model.coef_}")
print(f"Intercept: {model.intercept_}")

# Predict probabilities
predicted_probs = model.predict_proba(X)[:, 1]

# Calculating AUC-ROC
roc_auc = roc_auc_score(y, predicted_probs)
print(f"AUC-ROC: {roc_auc}")

# Plotting the prediction curves
preddata_BG3 = pd.DataFrame({
    'relative_severity_fcover_scaled': [0]*101,
    'pre_dist_TV_scaled': [0]*101,
    'BG_3_scaled': np.linspace(0, 100, 101)
})

preddata_BG3['pred'] = model.predict_proba(preddata_BG3)[:, 1]

plt.figure(figsize=(10, 6))
sns.lineplot(x='BG_3_scaled', y='pred', data=preddata_BG3, label='Baseline-normalized recovery')
plt.fill_between(preddata_BG3['BG_3_scaled'], preddata_BG3['pred'] - 2 * np.sqrt(preddata_BG3['pred'] * (1 - preddata_BG3['pred']) / len(preddata_BG3['pred'])), 
                 preddata_BG3['pred'] + 2 * np.sqrt(preddata_BG3['pred'] * (1 - preddata_BG3['pred']) / len(preddata_BG3['pred'])), alpha=0.3)
plt.title('Predictors: Rel. severity, BG_3, limited to 5y post_dist')
plt.xlabel('Bare ground share [%]')
plt.ylabel('p(recovery)')
plt.legend()
plt.show()
