import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Load data
recovery = pd.read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_random.csv")

# Function to calculate time to minimum tree cover within a 7-year window centered on disturbance year
def time_to_min(df):
    min_share = (
        df[(df['year'] >= df['yod'] - 3) & (df['year'] <= df['yod'] + 3)]
        .groupby(['ID', 'yod'])
        .apply(lambda g: g.loc[g['smoothed_tree_cover'].idxmin()])
        .reset_index(drop=True)
    )
    min_share['time_to_min'] = min_share['year'] - min_share['yod']
    min_share['min_tree_share'] = min_share['smoothed_tree_cover'] / 100
    min_share = min_share[['ID', 'yod', 'year', 'time_to_min', 'min_tree_share']]
    min_share.rename(columns={'year': 'min_year'}, inplace=True)
    return df.merge(min_share, on=['ID', 'yod'], how='left')

# Apply the function
t_min = time_to_min(recovery)
t_min.drop(t_min.columns[[18, 19, 20]], axis=1, inplace=True)  # drop columns 19-21 (0-indexed)
t_min.rename(columns={
    'min_year_y': 'min_year',
    'time_to_min_y': 'time_to_min',
    'min_tree_share_y': 'min_tree_share'
}, inplace=True)

# Compute average tree share before disturbance
t_min['tree_share_before'] = (
    t_min[t_min['year'] < t_min['yod']]
    .groupby('ID')['smoothed_tree_cover']
    .transform('mean')
)

# Compute relative severity
t_min['severity_relative'] = np.where(
    t_min['tree_share_before'] > 0,
    ((t_min['tree_share_before'] - t_min['min_tree_share']) / t_min['tree_share_before']) * 100,
    np.nan
)

# Compute recovery threshold (80% of pre-disturbance tree share)
t_min['tree_share_80'] = t_min['tree_share_before'] * 0.8

# Fill missing values within groups
t_min[['tree_share_before', 'severity_relative', 'tree_share_80']] = (
    t_min.groupby('ID')[['tree_share_before', 'severity_relative', 'tree_share_80']]
    .transform(lambda x: x.ffill().bfill())
)

# Compute recovery intervals
def compute_recovery(group):
    cond_met = (group['year'] > group['min_year']) & (group['smoothed_tree_cover'] >= group['tree_share_80'])
    group['condition_met'] = cond_met
    group['next_year'] = group['condition_met'].shift(-1)
    group['two_consec'] = group['condition_met'] & group['next_year']
    group['year_recov'] = np.where(group['two_consec'], group['year'], np.nan)
    recov_year = group['year_recov'].dropna().min() if group['year_recov'].notna().any() else np.nan
    return pd.Series({
        'year_recov': recov_year,
        'min_year': group['min_year'].iloc[0],
        'recovery_rate': recov_year - group['min_year'].iloc[0] if pd.notna(recov_year) else np.nan
    })

recovery1 = t_min.groupby('ID').apply(compute_recovery).reset_index()
recovery1 = recovery1.drop_duplicates(subset='ID')

# Merge with main data
recovery = t_min.merge(recovery1, on='ID', how='left')

# Fill missing recovery rate values
recovery['recovery_rate'] = recovery['recovery_rate_y'].combine_first(recovery['recovery_rate_y'].dropna().groupby(recovery['ID']).transform('first'))
recovery.drop(columns=['recovery_rate_y', 'year_recov_y', 'min_year_y'], inplace=True)
recovery.rename(columns={
    'min_year_x': 'min_year',
    'year_recov_x': 'year_recov'
}, inplace=True)

# Reclassify severity
recovery['severity_class'] = np.where(
    recovery['severity_relative'] <= 75, 'NSR',
    np.where(recovery['severity_relative'] > 75, 'SR', np.nan)
)

# Assign recovery status
recovery['recovery_status'] = np.where(recovery['recovery_rate'].notna(), 'recovered', 'not recovered')

# Create 10-year recovery indicators
recovery['recovery_10y'] = recovery.apply(
    lambda row: 1 if row['recovery_status'] == 'recovered' and row['year'] <= row['min_year'] + 10 else 0,
    axis=1
)

# Extract VPD anomalies
def extract_vpd(group):
    yod = group['yod'].iloc[0]
    for i in range(4):
        group[f'VPD_yod{i}'] = group['VPD_anomaly'][group['year'] == yod + i].iloc[0] if any(group['year'] == yod + i) else np.nan
    return group

recovery = recovery.groupby('ID').apply(extract_vpd).reset_index(drop=True)

# Create plots
recovery_recovered = recovery[recovery['recovery_rate'] < 100]
recovery_unique = recovery_recovered.drop_duplicates(subset='ID')
recovery_unique['severity_class'] = recovery_unique['severity_class'].fillna("non stand-replacing")

sns.set(style="whitegrid")
plt.figure(figsize=(10, 6))
sns.kdeplot(
    data=recovery_unique,
    x="recovery_rate",
    hue="severity_class",
    fill=True,
    alpha=0.7,
    common_norm=False,
    palette={"non stand-replacing": "#458C91", "stand-replacing": "#E2A800"},
    bw_adjust=2
)
plt.xlabel("Recovery Rate [Years]")
plt.ylabel("Density")
plt.title("")
plt.legend(title="Severity Class")
plt.tight_layout()
plt.show()

# Save CSVs
recovery.to_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_random.csv", index=False)
recovery_unique.to_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_random_unique_recovered.csv", index=False)
recovery_recovered.to_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_random_recovered.csv", index=False)

# Aspect reclassification
recovery['aspect_cat'] = pd.cut(recovery['aspect'] % 360, bins=[-1, 45, 135, 225, 315, 360],
                                labels=["N", "O", "S", "W", "N"], right=False, include_lowest=True)
