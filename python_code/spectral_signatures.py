import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Load the spectral library data (replace with actual path if needed)
df = pd.read_csv("spec_lib_long.csv")

# Filter out missing values (i.e., -9999)
df = df[df["value"] != -9999]

# Set band order
band_order = ["blue", "green", "red", "NIR", "SWIR1", "SWIR2"]
df["band"] = pd.Categorical(df["band"], categories=band_order, ordered=True)

# Compute median and interquartile range
summary = (
    df.groupby(["endmember", "band"])
    .agg(
        median_value=("value", "median"),
        lower_ci=("value", lambda x: x.quantile(0.25)),
        upper_ci=("value", lambda x: x.quantile(0.75)),
    )
    .reset_index()
)

# Plot full endmember spectra
color_palette_all = {
    "bare ground": "#839192",
    "broadleaved forest": "#7DCEA0",
    "coniferous forest": "#145A32",
    "broadleaved shrubland": "#F1948A",
    "coniferous shrubland": "#943126",
    "grassland": "#F4D03F",
    "artificial land": "#A569BD",
    "cropland": "#40E0D0",
    "water areas": "#1F618D"
}

plt.figure(figsize=(10, 6))
sns.lineplot(
    data=summary,
    x="band",
    y="median_value",
    hue="endmember",
    palette=color_palette_all
)
plt.xlabel("Bands")
plt.ylabel("Surface reflectance (Q50)")
sns.despine()
plt.tight_layout()
plt.show()

# Simplified plot: reclassify shrubland types to common class
summary_simple = summary.copy()
summary_simple["endmember"] = summary_simple["endmember"].replace({
    "broadleaved shrubland": "shrubland",
    "coniferous shrubland": "shrubland"
})

# Filter to selected classes
selected_classes = [
    "bare ground", "broadleaved forest", "coniferous forest",
    "broadleaved shrubland", "coniferous shrubland", "grassland"
]
summary_filtered = summary[summary["endmember"].isin(selected_classes)]

# Define simplified color palette
color_palette_simple = {
    "bare ground": "#839192",
    "broadleaved forest": "#7DCEA0",
    "coniferous forest": "#145A32",
    "broadleaved shrubland": "#F1948A",
    "coniferous shrubland": "#943126",
    "grassland": "#F4D03F"
}

plt.figure(figsize=(10, 6))
sns.lineplot(
    data=summary_filtered,
    x="band",
    y="median_value",
    hue="endmember",
    palette=color_palette_simple
)
plt.xlabel("Bands")
plt.ylabel("Surface reflectance (Q50)")
sns.despine()
plt.tight_layout()
plt.show()
