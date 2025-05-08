import pandas as pd
import numpy as np
import os

# Load your data
df_trees = pd.read_csv("~/eo_nas/EO4Alps/00_analysis/_recovery/fcover_alps_annual.csv")

# Function to compute the year and value of minimum tree cover per pixel
def time_to_min(df):
    df_sorted = df.sort_values("year")
    df_sorted["share"] = pd.to_numeric(df_sorted["share"], errors="coerce")
    min_idx = df_sorted["share"].idxmin()
    min_year = df_sorted.loc[min_idx, "year"]
    min_share = df_sorted.loc[min_idx, "share"]
    df_sorted["tree_min"] = min_share
    df_sorted["tree_min_year"] = min_year
    return df_sorted

# Apply function group-wise by pixel ID
trees_grouped = df_trees.groupby("ID_new", group_keys=False).apply(time_to_min)

# Filter for observations after or in the year of minimum tree cover
trees_grouped = trees_grouped[trees_grouped["year"] >= trees_grouped["tree_min_year"]].copy()
trees_grouped["years_since_min"] = trees_grouped["year"] - trees_grouped["tree_min_year"]

# Compute average tree cover before disturbance (baseline)
avg_before = (
    df_trees[df_trees["year"] < df_trees["yod"]]
    .groupby("ID_new")["share"]
    .mean()
    .rename("avg_tree_share_before")
)

# Merge average values back into the grouped DataFrame
trees_grouped = trees_grouped.merge(avg_before, on="ID_new", how="left")

# Compute regeneration amount and percentage
trees_grouped["regrown"] = trees_grouped["share"] - trees_grouped["tree_min"]
trees_grouped["regrown_percent"] = (
    (trees_grouped["regrown"] / (trees_grouped["avg_tree_share_before"] - trees_grouped["tree_min"])) * 100
).round(1)

# Export the final DataFrame
output_path = os.path.expanduser("~/eo_nas/EO4Alps/00_analysis/_recovery/fcover_regrowth.csv")
trees_grouped.to_csv(output_path, index=False)
