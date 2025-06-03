# Required packages
library(terra)
library(fs)
library(stringr)
library(dplyr)
library(tictoc)

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0029_Y0028"  # Set this to the specific tile you want to process
indices <- c("NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"                     # Subfolder for output within the tile
fill_range <- 5                                     # +/- years for gap filling
na_value <- -10000                                  # NA placeholder in input data

# ----- MAIN GAP-FILL FUNCTION ----- #
fill_gaps_for_index_in_tile <- function(tile_dir, index, output_dir, na_value = -10000, range = 5) {
  files <- dir_ls(tile_dir, glob = paste0("*", index, "*.tif"))
  message("🔍 Found ", length(files), " files for index ", index)
  if (length(files) == 0) {
    message("⚠️  No files found for index ", index, " in tile ", basename(tile_dir))
    return(NULL)
  }
  
  meta <- tibble(
    file = files,
    year = as.integer(str_extract(basename(files), "^\\d{4}"))
  ) %>% arrange(year)
  
  years <- meta$year
  out_dir <- file.path(tile_dir, output_dir, index)
  dir_create(out_dir)
  
  for (i in seq_along(years)) {
    y_target <- years[i]
    message("📆 Verarbeite Jahr: ", y_target)
    
    tic(paste("  Time for year", y_target))
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    filled <- r_target
    
    neighbor_idxs <- which(abs(years - y_target) <= range & years != y_target)
    neighbor_idxs <- neighbor_idxs[order(abs(years[neighbor_idxs] - y_target))]
    
    for (j in neighbor_idxs) {
      r_neighbor <- rast(meta$file[j])
      r_neighbor[r_neighbor == na_value] <- NA
      filled <- cover(filled, r_neighbor)
      
      if (global(is.na(filled[[1]]), "sum", na.rm = TRUE)[1] == 0) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(filled, out_path, overwrite = TRUE, NAflag = NA)
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    toc()
    
    rm(r_target, r_neighbor, filled)
    gc()
  }
}

# ----- INDEX LOOP FOR A SINGLE TILE (process all indices sequentially) ----- #

for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}
