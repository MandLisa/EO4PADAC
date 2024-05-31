# -*- coding: utf-8 -*-
"""
Created on Feb 11 07:58:28 2024

@author: lmandl
"""

import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score, roc_curve
from sklearn.model_selection import KFold
from scipy.stats import norm
import matplotlib.pyplot as plt
import seaborn as sns

# Load datasets
recovery_clean = pd.read_csv('/data/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/csv/recovery_files/recovery_clean.csv')
recovery_clean_unique2 = pd.read_csv('/data/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/csv/recovery_files/recovery_clean_unique2.csv')
recovery_clean_unique_2510 = pd.read_csv('/data/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/csv/recovery_files/recovery_clean_unique_2510.csv')

# Remove rows with NaN in specific columns
recovery_clean_unique = recovery_clean_unique2.dropna(subset=['BG_5', 'BG_7', 'relative_severity_fcover'])

# Compute bare ground share 7y post-disturbance
recovery_clean1 = recovery_clean.groupby('ID').apply(lambda x: x[x['year'] == x['yod'] + 7][['ID', 'BG']].drop_duplicates()).reset_index(drop=True)
recovery_clean1.rename(columns={'BG': 'BG_7'}, inplace=True)

# Join with main dataframe
recovery_clean = recovery_clean.merge(recovery_clean1, on='ID', how='left')

# Remove time series
recovery_clean_unique = recovery_clean.drop_duplicates(subset=['ID'])

# Z-transformation of predictor variables
scaler_columns = ['relative_severity_fcover', 'pre_dist_TV', 'BG_3', 'NTV_3', 'BG_5', 'BG_7', 'NTV_5']
for col in scaler_columns:
    recovery_clean_unique[col + '_scaled'] = (recovery_clean_unique[col] - recovery_clean_unique[col].mean()) / recovery_clean_unique[col].std()

# Set the seed for reproducibility
np.random.seed(42)

# Number of folds for cross-validation
num_folds = 5
kf = KFold(n_splits=num_folds, shuffle=True, random_state=42)

# Initialize vector to store AUC ROC values
auc_values = []

# Perform 5-fold cross-validation and compute coefficients and CI
X = recovery_clean_unique[['relative_severity_fcover_scaled', 'pre_dist_TV_scaled', 'BG_3_scaled']]
y = recovery_clean_unique['recovery_status_FAO_30y']

for train_index, test_index in kf.split(X):
    train_data, test_data = X.iloc[train_index], X.iloc[test_index]
    train_labels, test_labels = y.iloc[train_index], y.iloc[test_index]
    
    # Fit logistic regression model
    logistic_model = LogisticRegression(solver='liblinear')
    logistic_model.fit(train_data, train_labels)
    
    # Make predictions on test data
    predictions = logistic_model.predict_proba(test_data)[:, 1]
    
    # Compute AUC ROC for the fold
    auc_values.append(roc_auc_score(test_labels, predictions))

# Compute mean AUC ROC across folds
mean_auc = np.mean(auc_values)
print(f"Mean AUC ROC (5-fold cross-validation): {mean_auc}")

# Retrieve coefficients
coefficients = logistic_model.coef_[0]
print(f"Coefficients: {coefficients}")

# Odds ratio
odds_ratios = (np.exp(coefficients) - 1) * 100
print(f"Odds Ratios: {odds_ratios}")

# Compute confidence intervals of the coefficients
se = np.sqrt(np.diag(np.linalg.inv(np.dot(X.T, X))))
z = norm.ppf(0.975)  # 95% confidence
ci_lower = (np.exp(coefficients - z * se) - 1) * 100
ci_upper = (np.exp(coefficients + z * se) - 1) * 100

# Combine coefficients and CIs into a dataframe
coefficients_data = pd.DataFrame({
    'Variables': X.columns,
    'Estimate': odds_ratios,
    'Lower_CI': ci_lower,
    'Upper_CI': ci_upper
})
print(coefficients_data)

# Create a grouped bar plot
models_AUC = pd.read_csv('/data/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/csv/recovery_files/models_AUC.csv')
plt.figure(figsize=(10, 6))
sns.scatterplot(data=models_AUC, x='YAD', y='AUC', hue='BG', style='def')
plt.title('AUC Points')
plt.xlabel('YAD')
plt.ylabel('AUC')
plt.legend(title='BG')
plt.savefig('/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/AUC_points.png', dpi=300)
plt.close()

# Forest plot
forest_plot_BG_AUC_2510 = pd.read_csv('/data/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/csv/recovery_files/forest_plot_BG_AUC_2510.csv', delimiter='\t')
metrics = forest_plot_BG_AUC_2510

plt.figure(figsize=(12, 6))
sns.pointplot(data=metrics, x='def', y='est', hue='model', dodge=True, join=False)
plt.errorbar(x=metrics['def'], y=metrics['est'], yerr=[metrics['est'] - metrics['lower'], metrics['higher'] - metrics['est']], fmt='o', color='black')
plt.title('Forest Plot')
plt.xlabel('Group')
plt.ylabel('Odds ratio*100 (95% Confidence Interval)')
plt.legend(title='Model')
plt.xticks(rotation=45)
plt.grid(True)
plt.savefig('/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/forest_plot_not_free.png', dpi=300)
plt.close()

# Probability curves (response curves)
preddata = pd.DataFrame({
    'relative_severity_fcover_scaled': [0] * 101,
    'pre_dist_TV_scaled': [0] * 101,
    'BG_3_scaled': np.arange(0, 101, 1)
})
preddata['pred'] = logistic_model.predict_proba(preddata)[:, 1]

plt.figure(figsize=(10, 6))
plt.fill_between(preddata['BG_3_scaled'], preddata['pred'] - 2 * preddata['pred'], preddata['pred'] + 2 * preddata['pred'], alpha=0.3)
plt.plot(preddata['BG_3_scaled'], preddata['pred'], color='blue', label='Baseline-normalized recovery')
plt.title('Predictors: Rel. severity, BG_3, limited to 5y post_dist')
plt.xlabel('Bare ground share [%]')
plt.ylabel('p(recovery)')
plt.legend()
plt.savefig('/mnt/public/Projects/DataCube/projects/foreco/alps/00_recovery_analysis/figs/probability_curve.png', dpi=300)
plt.close()

# Compute optimized F1 score
def F1_Score(actual, predicted):
    tp = sum((actual == 1) & (predicted == 1))
    fp = sum((actual == 0) & (predicted == 1))
    fn = sum((actual == 1) & (predicted == 0))
    precision = tp / (tp + fp)
    recall = tp / (tp + fn)
    return 2 * (precision * recall) / (precision + recall)

thresholds = np.arange(0.1, 1.01, 0.01)
f1_scores = []
accuracies = []

for threshold in thresholds:
    binary_predictions = (predictions > threshold).astype(int)
    f1_scores.append(F1_Score(test_labels, binary_predictions))
    accuracies.append((binary_predictions == test_labels).mean() * 100)

threshold_results = pd.DataFrame({
    'Threshold': thresholds,
    'F1_Score': f1_scores,
    'Accuracy': accuracies
})
print(threshold_results)

best_threshold = threshold_results.loc[threshold_results['F1_Score'].idxmax()]
print(f"Optimal Threshold, F1 Score, and Accuracy: {best_threshold}")

# Compute error of omission, commission and accuracy
actual = test_labels
predicted_probs = predictions
predicted = (predicted_probs > best_threshold['Threshold']).astype(int)
total_instances = len(actual)
FP = sum((predicted == 1) & (actual == 0))
FN = sum((predicted == 0) & (actual == 1))
TP = sum((predicted == 1) & (actual == 1))
TN = sum((predicted == 0) & (actual == 0))

error_of_omission = FN / total_instances * 100
error_of_commission = FP / total_instances * 100
accuracy = (TP + TN) / total_instances * 100

print(f"Error of Omission (%): {error_of_omission}")
print(f"Error of Commission (%): {error_of_commission}")
print(f"Number of Correctly Classified Observations (%): {accuracy}")

# Confidence intervals for errors and accuracy
def compute_confidence_interval(value, total, z=1.96):
    proportion = value / total
    ci = z * np.sqrt((proportion * (1 - proportion)) / total)
    return proportion * 100 - ci * 100, proportion * 100 + ci * 100

ci_omission = compute_confidence_interval(FN, total_instances)
ci_commission = compute_confidence_interval(FP, total_instances)
ci_accuracy = compute_confidence_interval(TP + TN, total_instances)

print(f"CI Error of Omission: {ci_omission}")
print(f"CI Error of Commission: {ci_commission}")
print(f"CI Number of Correctly Classified Observations: {ci_accuracy}")

