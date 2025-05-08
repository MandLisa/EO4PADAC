import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path

# Define paths to CSVs (change these if needed)
gedi_path = Path("~/eo_nas/EO4Alps/00_analysis/_recovery/GEDI_recov_all.csv").expanduser()
summary_out = Path("~/eo_nas/EO4Alps/00_analysis/_recovery/recovery_summary_geoloc.csv").expanduser()
fig_path = Path("~/eo_nas/EO4Alps/figs/temp_recov_lineplot_python.png").expanduser()

# Load data
gedi = pd.read_csv(gedi_path)

# Filter: disturbances before 2013, recovery_rate ≤ 40, ysd ≥ 0, geoloc not NA
gedi_filtered = gedi[
    (gedi["yod"] <= 2013) &
    (~gedi["geoloc"].isna()) &
    (gedi["recovery_rate"] <= 40) &
    (gedi["ysd"] >= 0)
].copy()

# Aspect classification
gedi_filtered["aspect_cat"] = pd.cut(
    gedi_filtered["aspect"],
    bins=[-1, 45, 135, 225, 315, 360],
    labels=["N", "E", "S", "W", "N"],
    include_lowest=True
)

# Elevation classification
gedi_filtered["height_class"] = pd.cut(
    gedi_filtered["dem"],
    bins=[-np.inf, 800, 1200, np.inf],
    labels=["0-800", ">800-1200", ">1200"]
)

# Classify recovery within 10 years
gedi_filtered["recovery_10y"] = gedi_filtered.groupby("ID")["recovery_rate"].transform(lambda x: int((x <= 10).any()))

# Group by for summary
summary = (
    gedi_filtered
    .groupby(["yod", "geoloc", "severity_class", "height_class", "aspect_level", "aspect_cat"])
    .agg(
        total_disturbances=("ID", "count"),
        percent_recovered_within_10y=("recovery_10y", lambda x: 100 * np.sum(x) / len(x))
    )
    .reset_index()
)

# Adjust 10y recovery metric
summary["adjusted_10y"] = summary["percent_recovered_within_10y"] * 0.7
summary["severity_class"] = summary["severity_class"].fillna("non stand-replacing")

# Save summary
summary.to_csv(summary_out, index=False)

# Temperature per year & geoloc
temperature = (
    gedi_filtered
    .groupby(["year", "geoloc"])
    .agg(mean_temperature=("temp", "mean"))
    .reset_index()
)

# Normalize temperature
max_temp = temperature["mean_temperature"].max()
max_recov = summary["adjusted_10y"].max()
temperature["norm_temp"] = temperature["mean_temperature"] / max_temp * max_recov

# Plotting
sns.set(style="whitegrid")

fig, ax1 = plt.subplots(figsize=(10, 6))

# Recovery trend
sns.lineplot(
    data=summary,
    x="yod",
    y="adjusted_10y",
    ax=ax1,
    label="Adjusted recovery (10y)",
    color="black"
)
sns.regplot(
    data=summary,
    x="yod",
    y="adjusted_10y",
    scatter=False,
    ax=ax1,
    color="red",
    line_kws={"linestyle": "dashed", "linewidth": 1}
)

ax1.set_ylabel("10-year recovery [%]")
ax1.set_xlabel("Year of disturbance")

# Temperature trend
ax2 = ax1.twinx()
sns.lineplot(
    data=temperature,
    x="year",
    y="mean_temperature",
    hue="geoloc",
    palette="coolwarm",
    linewidth=0.7,
    ax=ax2,
    legend=False
)
ax2.set_ylabel("Mean annual temperature [°C]")

# Formatting
ax1.set_xlim(1986, 2012)
plt.title("10-year Recovery vs. Temperature (by Geolocation)")
plt.tight_layout()

# Save figure
plt.savefig(fig_path, dpi=300)
plt.show()
