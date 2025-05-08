import os
import glob
import numpy as np
import pandas as pd
import rasterio
from rasterio.plot import reshape_as_raster, reshape_as_image

# Set paths
path_in = "/mnt/data/fractional_cover_tiled/"
path_out = "/mnt/data/processed/random_sample_fractionalcover/"
tile_list = sorted(os.listdir(path_in))

# Parameters
sample_size = 10000
np.random.seed(42)

# Loop over each tile
for tile in tile_list:
    tile_path = os.path.join(path_in, tile, "Stats")
    tiffs = sorted(glob.glob(os.path.join(tile_path, "*_mean.tif")))

    if len(tiffs) == 0:
        continue

    years = [os.path.basename(f).split("_")[0] for f in tiffs]
    year_sample = []

    # Read the first file to get shape and generate sample
    with rasterio.open(tiffs[0]) as src:
        data = src.read(1)
        profile = src.profile

        valid_mask = (data != src.nodata) & ~np.isnan(data)
        valid_indices = np.column_stack(np.where(valid_mask))

        if len(valid_indices) < sample_size:
            continue  # Skip if not enough valid pixels

        sample_indices = valid_indices[np.random.choice(valid_indices.shape[0], sample_size, replace=False)]
        row_ids, col_ids = sample_indices[:, 0], sample_indices[:, 1]

    # Loop over years and extract sample
    for i, file in enumerate(tiffs):
        with rasterio.open(file) as src:
            data = src.read(1)
            sampled_values = data[row_ids, col_ids]
            df = pd.DataFrame({
                "row": row_ids,
                "col": col_ids,
                "value": sampled_values,
                "year": int(years[i]),
                "tile": tile
            })
            year_sample.append(df)

    # Combine and clean
    sample_df = pd.concat(year_sample)
    sample_df['id'] = sample_df.groupby(['row', 'col', 'tile']).ngroup()
    sample_df = sample_df[['id', 'tile', 'year', 'value']]
    sample_df = sample_df.replace([-9999, -9998, -32768, -32767], np.nan)
    sample_df = sample_df.dropna()

    # Save
    os.makedirs(path_out, exist_ok=True)
    output_file = os.path.join(path_out, f"{tile}_random_sample.csv")
    sample_df.to_csv(output_file, index=False)
