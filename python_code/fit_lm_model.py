import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.formula.api as smf
from sklearn.preprocessing import StandardScaler
import os

# Load data
recovery_imputed_unique = pd.read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_imputed_unique.csv")
recovery_standardized = recovery_imputed_unique.copy()

# Standardize selected predictors
columns_to_scale = [
    "VPD_consecutive_1y", "VPD_consecutive_yod", "avg_tree_share_before",
    "mean_VPD_pre", "mean_VPD_yod", "mean_pre_dist_bare_land_share",
    "mean_pre_dist_grassland_share", "mean_pre_dist_shrubland_share",
    "mean_pre_dist_coniferous_woodland_share", "mean_pre_dist_broadleaved_woodland_share",
    "VPD_autumn_yod", "VPD_autumn_yod+1", "VPD_spring_yod", "VPD_spring_yod+1",
    "VPD_summer_yod", "VPD_summer_yod+1", "height", "slope", "severity_relative"
]
scaler = StandardScaler()
recovery_standardized[columns_to_scale] = scaler.fit_transform(recovery_standardized[columns_to_scale])

# Custom symmetric log transformation function
def symmetric_log10(x, base=10):
    return np.sign(x) * np.log10(np.abs(x) + 1) / np.log10(base)

# Predictor ordering
custom_order = [
    "mean_VPD_pre", "mean_VPD_yod", "mean_VPD_post_1_year",
    "VPD_consecutive_yod", "VPD_consecutive_1y",
    "VPD_spring_yod", "VPD_spring_yod+1",
    "VPD_summer_yod", "VPD_summer_yod+1",
    "VPD_autumn_yod", "VPD_autumn_yod+1",
    "avg_tree_share_before",
    "mean_pre_dist_bare_land_share",
    "mean_pre_dist_grassland_share",
    "mean_pre_dist_shrubland_share",
    "mean_pre_dist_coniferous_woodland_share",
    "mean_pre_dist_broadleaved_woodland_share",
    "height", "slope", "severity_relative"
]

# Function to fit model per group and extract coefficients
def fit_models_and_extract(df, group_col):
    results = []
    for name, group in df.groupby(group_col):
        model = smf.ols(
            formula="recovery_rate ~ VPD_consecutive_1y + VPD_consecutive_yod + avg_tree_share_before + "
                    "mean_VPD_yod + mean_VPD_post_1_year + mean_VPD_pre + "
                    "mean_pre_dist_bare_land_share + mean_pre_dist_grassland_share + "
                    "mean_pre_dist_shrubland_share + mean_pre_dist_coniferous_woodland_share + "
                    "mean_pre_dist_broadleaved_woodland_share + VPD_autumn_yod + Q('VPD_autumn_yod+1') + "
                    "VPD_spring_yod + Q('VPD_spring_yod+1') + VPD_summer_yod + Q('VPD_summer_yod+1') + "
                    "height + slope + severity_relative",
            data=group
        ).fit()
        coef_df = model.summary2().tables[1].reset_index()
        coef_df.columns = ['term', 'estimate', 'std.error', 't', 'p']
        coef_df[group_col] = name
        coef_df = coef_df[coef_df['term'] != 'Intercept']
        results.append(coef_df[['term', 'estimate', 'std.error', group_col]])
    return pd.concat(results, ignore_index=True)

# Fit models by severity_class, agent_name, and geoloc_name
model_summary_severity = fit_models_and_extract(recovery_standardized, 'severity_class')
model_summary_agent = fit_models_and_extract(recovery_standardized, 'agent_name')
model_summary_geoloc = fit_models_and_extract(recovery_standardized, 'geoloc_name')

# Apply custom order
model_summary_severity['term'] = pd.Categorical(model_summary_severity['term'], categories=custom_order, ordered=True)
model_summary_agent['term'] = pd.Categorical(model_summary_agent['term'], categories=custom_order, ordered=True)
model_summary_geoloc['term'] = pd.Categorical(model_summary_geoloc['term'], categories=custom_order, ordered=True)

# Custom plotting function
def plot_effect_size(df, group_col, color_dict, filename):
    fig, ax = plt.subplots(figsize=(12, 6))
    for group, group_df in df.groupby(group_col):
        ax.errorbar(
            group_df['estimate'],
            group_df['term'],
            xerr=group_df['std.error'],
            fmt='o', label=group,
            color=color_dict.get(group, 'gray')
        )
    ax.set_xscale('function', functions=(symmetric_log10, lambda x: 10**x - 1))
    ax.set_xlabel('Estimated Coefficient (symmetric log scale)', fontsize=14)
    ax.set_ylabel('Predictor', fontsize=14)
    ax.legend(title=group_col)
    plt.tight_layout()
    plt.savefig(filename, dpi=300)
    plt.close()

# Example color dictionaries (as in original R script)
colors_severity = {"non stand-replacing": "blue", "stand-replacing": "red"}
colors_agent = {"other": "blue", "Bark Beetle/Wind": "#f4d03f", "Fire": "#78281f"}
colors_geoloc = {
    "Northern West Alps": "#2471a3", "Southern West Alps": "#dc7633",
    "Northern East Alps": "#27ae60", "Central Alps": "#34495e", "Southern East Alps": "#f1c40f"
}

# Create plots
plot_effect_size(model_summary_severity, 'severity_class', colors_severity, "~/eo_nas/EO4Alps/figs/effect_size.png")
plot_effect_size(model_summary_agent, 'agent_name', colors_agent, "~/eo_nas/EO4Alps/figs/effect_size_agent.png")
plot_effect_size(model_summary_geoloc, 'geoloc_name', colors_geoloc, "~/eo_nas/EO4Alps/figs/effect_size_geoloc.png")

# Save standardized data
recovery_standardized.to_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_standardized.csv", index=False)

# Additional test plot
subset = recovery_imputed_unique[
    (recovery_imputed_unique['severity_class'] == 'stand-replacing') &
    (recovery_imputed_unique['geoloc_name'].notna()) &
    (recovery_imputed_unique['yod'].isin([2001, 2002, 2003, 2004]))
]
sns.lmplot(
    data=subset,
    x='VPD_summer_yod',
    y='recovery_rate',
    hue='yod',
    col='geoloc_name',
    col_wrap=3,
    height=4,
    aspect=1,
    scatter_kws={'alpha': 0.6},
    line_kws={'color': 'black'},
    facet_kws={'sharey': False}
)
plt.tight_layout()
plt.show()

# Fit test model
fit_test = smf.ols("recovery_rate ~ VPD_summer_yod", data=recovery_imputed_unique).fit()
