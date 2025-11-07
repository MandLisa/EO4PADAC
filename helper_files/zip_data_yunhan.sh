# Create two archives with relative paths (no leading '/')
cd /

# Part 1: 1985–2005
zip -9 -T /mnt/eo/EO4Alps/data_yunhan_part1.zip \
  mnt/eo/EO4Alps/level4_fcover/mosaics_Alps_convention_smoothed_filled/smoothed/yearly_stacks/mosaic_{1985..2005}.tif

# Part 2: 2006–2024 + DEM + forest mask
zip -9 -T /mnt/eo/EO4Alps/data_yunhan_part2.zip \
  mnt/eo/EO4Alps/level4_fcover/mosaics_Alps_convention_smoothed_filled/smoothed/yearly_stacks/mosaic_{2006..2024}.tif \
  mnt/eo/EO4Alps/dem/dem_crop.tif \
  mnt/eo/EO4Alps/dist_data/zenodo/forest_alps_crop.tif