import rasterio
import numpy as np
from rasterio import shutil as rio_shutil

years = list(range(1986, 2024))
input_dir = "/eo_nas/EO4Alps/level3_predictions/l2_mask/"
output_dir = "/eo_nas/EO4Alps/level3_predictions/l2_mask/tree_only/"

for year in years:
    in_path = f"{input_dir}mosaic_{year}.tif"
    out_path = f"{output_dir}treecover{year}.tif"

    with rasterio.open(in_path) as src:
        band6 = src.read(6)
        band7 = src.read(7)
        combined = band6 + band7
        meta = src.meta.copy()
        meta.update(count=1)

    with rasterio.open(out_path, "w", **meta) as dst:
        dst.write(combined, 1)
