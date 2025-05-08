import os
import glob
import numpy as np
import pandas as pd
import rasterio
from rasterio.transform import xy
from tqdm import tqdm

# Arbeitsverzeichnis setzen
os.chdir("~/eo_nas/EO4Alps/level3_predictions/l2_mask/X0028_Y0028")

# Rasterdateien laden
raster_files = sorted(glob.glob("*.tif"))

# Rasterstack einlesen
stack = []
with rasterio.open(raster_files[0]) as src0:
    meta = src0.meta
    transform = src0.transform
    width = src0.width
    height = src0.height

for f in raster_files:
    with rasterio.open(f) as src:
        stack.append(src.read(1))

stack = np.stack(stack)  # shape: (n_layers, height, width)

# Koordinaten und Werte extrahieren
rows, cols = np.meshgrid(np.arange(height), np.arange(width), indexing='ij')
xs, ys = rasterio.transform.xy(transform, rows, cols)
xs = np.array(xs).flatten()
ys = np.array(ys).flatten()
pixels = stack.reshape(stack.shape[0], -1).T  # shape: (n_pixels, n_layers)

df = pd.DataFrame(pixels)
df.insert(0, 'y', ys)
df.insert(0, 'x', xs)
df.dropna(inplace=True)

# Spaltennamen setzen
landcover_names = ["artificial land", "bare land", "water", "grassland",
                   "shrubland", "coniferous woodland", "broadleaved wodland"]
num_years = (df.shape[1] - 2) // 7
years = list(range(1986, 1986 + num_years))
new_columns = [f"{lc} {year}" for year in years for lc in landcover_names]
df.columns = ["x", "y"] + new_columns

# ID hinzufügen
df["ID"] = pd.factorize(list(zip(df["x"], df["y"])))[0] + 1

# Long format
df_long = df.melt(id_vars=["x", "y", "ID"], var_name="class", value_name="share")

# 10.000 IDs sampeln
unique_ids = df_long["ID"].unique()
if len(unique_ids) < 10000:
    raise ValueError("Not enough unique IDs in the data frame to sample 10,000 unique IDs.")
np.random.seed(123)
sampled_ids = np.random.choice(unique_ids, 10000, replace=False)
df_subset = df_long[df_long["ID"].isin(sampled_ids)].copy()

# Jahr extrahieren und von Klassenname trennen
df_subset["year"] = df_subset["class"].str.extract(r"(\d{4})$").astype(int)
df_subset["class"] = df_subset["class"].str.replace(r"\s+\d{4}$", "", regex=True)

# 'yod' extrahieren (Platzhalter – hier muss ein echter Disturbance Raster gelesen werden)
df_subset["yod"] = np.nan  # <- durch echte Extraktion ersetzen

# share-Werte säubern
df_subset["share"] = df_subset["share"].clip(lower=0, upper=10000)

# Einzigartige Beobachtungen pro ID
df_subset_unique = df_subset.drop_duplicates(subset=["ID"])

# Speichern
df_subset.to_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X28Y28.csv", index=False)
df_subset_unique.to_csv("~/eo_nas/EO4Alps/00_analysis/_data/fcover_X28Y28_unique.csv", index=False)
