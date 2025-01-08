### Forest Recovery Analysis in Python

# Import Required Libraries
import pandas as pd
import numpy as np
import statsmodels.api as sm
import statsmodels.formula.api as smf
from pymer4.models import Lmer
import matplotlib.pyplot as plt
import seaborn as sns
from plotnine import *

# Load Datasets
GEDI_recov_all = pd.read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_all.csv")
recovery_filt_lm_2013 = pd.read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_stats.csv")

#-------------------------------------------------------------------------------
# Model 1: Separate Models for Each Geolocation
results = {}
for geoloc, group in recovery_filt_lm_2013.groupby('geoloc'):
    model = smf.mixedlm(
        "regrown_percent ~ VPD_yod_z + dem_z + slope_z + severity_relative_z + aspect_cat",
        group,
        groups=group["year"],
        re_formula="1 + VPD_yod_z"
    )
    results[geoloc] = model.fit()

# Extract coefficients
coef_data = []
for geoloc, model in results.items():
    summary = model.summary2().tables[1].reset_index()
    summary["geoloc"] = geoloc
    coef_data.append(summary)
    
models_with_coefs = pd.concat(coef_data)

# Plot Coefficients
(
    ggplot(models_with_coefs[models_with_coefs['index'] == 'VPD_yod_z'],
           aes(y='geoloc', x='Coef.', color='factor(geoloc)')) +
    geom_point() +
    geom_errorbarh(aes(xmin='Coef. - Std.Err.', xmax='Coef. + Std.Err.'), height=0.2) +
    labs(y="Geoloc", x="Estimate (Effect of VPD anomalies @ yod)") +
    geom_vline(xintercept=0, linetype="dashed", color="black") +
    theme(legend_position="none", axis_text_y=element_text(angle=0, hjust=1))
)

#-------------------------------------------------------------------------------
# Average VPD@yod Effect Over the Years
vpd_effect = recovery_filt_lm_2013.groupby(['geoloc', 'yod']).agg(
    mean_VPD_yod_z=('VPD_yod_z', 'mean')
).reset_index()

# Plot Effect Over Time
(
    ggplot(vpd_effect, aes(x='yod', y='mean_VPD_yod_z', color='geoloc')) +
    geom_line() +
    geom_point() +
    geom_smooth(method='lm', se=True, color='black', linetype='dashed') +
    xlim(1986, 2013) +
    labs(x="YOD", y="Mean Effect of VPD_yod_z", title="Effect of VPD anomalies over time by geolocation") +
    theme(legend_position="right")
)

#-------------------------------------------------------------------------------
# Model 2: Combined Model Across Geolocations
model_single = smf.mixedlm(
    "regrown_percent ~ VPD_yod_z * geoloc + year + dem_z + slope_z + severity_relative_z",
    recovery_filt_lm_2013,
    groups=recovery_filt_lm_2013["year"],
    re_formula="1 + VPD_yod_z"
).fit()

# Extract Marginal Means
from statsmodels.stats.margins import margins
emm = model_single.get_margeff(at='mean', method='dydx', dummy=True)
emm_summary = emm.summary_frame()

# Plot Marginal Means
(
    ggplot(emm_summary, aes(x='VPD_yod_z', y='eff', color='geoloc')) +
    geom_line() +
    labs(x="VPD anomalies", y="Estimated recovery", title="Effect of VPD anomalies by geolocation")
)

#-------------------------------------------------------------------------------
# Model 3: Binary Logistic Regression by Geolocation
binary_results = {}
for geoloc, group in recovery_filt_lm_2013.groupby('geoloc_reclass'):
    model = smf.mixedlm(
        "recovery_10y_num ~ VPD_yod_z + dem_z + slope_z + severity_relative_z + aspect_cat",
        group,
        groups=group["year"],
        re_formula="1 + VPD_yod_z"
    )
    binary_results[geoloc] = model.fit()

# Extract Coefficients
binary_coef_data = []
for geoloc, model in binary_results.items():
    summary = model.summary2().tables[1].reset_index()
    summary["geoloc"] = geoloc
    binary_coef_data.append(summary)
    
model_coefficients = pd.concat(binary_coef_data)

# Plot Coefficients
(
    ggplot(model_coefficients[model_coefficients['index'] != "(Intercept)"],
           aes(x='Coef.', y='index', color='geoloc')) +
    geom_point() +
    geom_errorbarh(aes(xmin='Coef. - Std.Err.', xmax='Coef. + Std.Err.'), height=0.2) +
    facet_wrap('~geoloc', scales='free_x') +
    geom_vline(xintercept=0, linetype="dashed", color="black") +
    labs(x="Estimate", y="Predictors") +
    theme(legend_position="none")
)

#-------------------------------------------------------------------------------
# Percentage Recovery per YOD
recovery_summary = recovery_filt_lm_2013.groupby('yod').agg(
    total_observations=('recovery_10y_num', 'count'),
    recovered_within_10_years=('recovery_10y_num', 'sum')
).reset_index()
recovery_summary['recovery_percentage'] = (recovery_summary['recovered_within_10_years'] / recovery_summary['total_observations']) * 100

# Plot Recovery Percentage
(
    ggplot(recovery_summary, aes(x='yod', y='recovery_percentage')) +
    geom_line() +
    geom_point() +
    geom_smooth(method='lm', color='red', linetype='dashed', se=True) +
    theme(legend_position="none") +
    xlim(1986, 2012)
)

#-------------------------------------------------------------------------------
# Exploratory Data Analysis (EDA)
GEDI_all_unique = GEDI_recov_all.drop_duplicates(subset=['ID']).dropna(subset=['geolocation_reclass'])
GEDI_all_unique['aspect_cat'] = pd.cut(GEDI_all_unique['aspect'], bins=[0, 45, 135, 225, 315, 360], labels=["N", "E", "S", "W"])

sns.countplot(data=GEDI_all_unique, x='aspect_cat')
plt.show()
