import os
import re
import shutil
import pandas as pd
import geopandas as gpd
import rasterio
import numpy as np
from rasterio.mask import mask
from shapely.geometry import mapping
from pathlib import Path
from tqdm import tqdm

# Set working directory
os.chdir("/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/tif")

# Read shapefile
shp = gpd.read_file("candidates_bare_soil.shp")

# Extract raster values for each tif and band
for filename in os.listdir():
    if filename.endswith(".tif"):
        with rasterio.open(filename) as src:
            for i in range(1, src.count + 1):
                coords = [mapping(geom) for geom in shp.geometry]
                out_image, _ = mask(src, coords, crop=False, indexes=i)
                values = out_image.reshape((out_image.shape[1], out_image.shape[2]))[0]
                df = pd.DataFrame({
                    "X": shp.geometry.x,
                    "Y": shp.geometry.y,
                    "type": shp["type"],
                    "extracted_value": values[:len(shp)],
                    "band": i
                })
                out_name = filename.replace(".tif", f"_band{i}.csv")
                df.to_csv(out_name, index=False)

# Move all CSVs not containing "_band1"
csv_folder = os.getcwd()
destination_folder = os.path.join(csv_folder, "csv_del")
os.makedirs(destination_folder, exist_ok=True)

for file in os.listdir(csv_folder):
    if file.endswith(".csv") and "_band1" not in file:
        shutil.move(os.path.join(csv_folder, file), os.path.join(destination_folder, file))

# Move files with keywords
keywords = ["band13", "band12", "band11", "band10"]
for file in os.listdir(csv_folder):
    if file.endswith(".csv") and any(k in file for k in keywords):
        shutil.move(os.path.join(csv_folder, file), os.path.join(destination_folder, file))

# Rename columns 4–17
os.chdir("/data/public/Projects/DataCube/projects/foreco/alps/level3/l3_STMs/all/csv_bare_soil")
new_col_names = ["MIN", "MAX", "AVG", "STD", "RNG", "IQR", "SKW", "KRT", "NUM", "Q25", "Q50", "Q75", "Q90", "bands"]

for file in os.listdir():
    if file.endswith(".csv"):
        df = pd.read_csv(file)
        df.columns.values[3:17] = new_col_names
        df.to_csv(file.replace(".csv", "_v1.csv"), index=False)

# Remove "bands" column
for file in os.listdir():
    if file.endswith(".csv"):
        df = pd.read_csv(file)
        if "bands" in df.columns:
            df.drop(columns=["bands"], inplace=True)
            df.to_csv(file, index=False)

# Delete files with "_v1_v1"
for file in os.listdir():
    if "_v1_v1" in file and file.endswith(".csv"):
        os.remove(file)

# Delete CSVs without "_v1"
for file in os.listdir():
    if file.endswith(".csv") and "_v1" not in file:
        os.remove(file)

# Pivot data to long format
csv_files = list(Path.cwd().glob("*.csv"))
csv_list = []

for file in tqdm(csv_files):
    df = pd.read_csv(file)
    df.iloc[:, 4:17] = df.iloc[:, 4:17].astype(str)
    df_tidy = pd.melt(df, id_vars=df.columns[:4], var_name="STM", value_name="value")
    name = file.stem
    df_tidy.to_csv(f"{name}_tidy.csv", index=False)
    csv_list.append(df_tidy)

df_all = pd.concat(csv_list, ignore_index=True)

# Clean filenames
for file in os.listdir():
    if file.endswith(".csv"):
        new_name = file.replace("-", "").replace("__", "_")
        os.rename(file, new_name)

# Add "metric" column from filename
for file in os.listdir():
    if file.endswith(".csv"):
        df = pd.read_csv(file)
        metric = file[:3]
        df["metric"] = metric
        df.to_csv(file, index=False)

# Add "year" from filename
for file in os.listdir():
    if file.endswith(".csv"):
        df = pd.read_csv(file)
        match = re.search(r"_(\d{4})_", file)
        df["year"] = match.group(1) if match else None
        df.to_csv(file, index=False)

# Combine all CSVs
file_names = [f for f in os.listdir() if f.endswith(".csv")]
df_combined = pd.concat([pd.read_csv(f) for f in file_names], ignore_index=True)
df_combined.to_csv("extracted_STMs_tidy_bare_soil.csv", index=False)

# Merge two CSVs
data1 = pd.read_csv("file1.csv")
data2 = pd.read_csv("file2.csv")
merged_data = pd.concat([data1, data2], ignore_index=True)
merged_data.to_csv("all_STMs.csv", index=False)
