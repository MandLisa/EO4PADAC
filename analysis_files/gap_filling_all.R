# Required packages
library(terra)
library(fs)
library(stringr)
library(dplyr)
library(tictoc)
library(dpyr)

#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0028_Y0028"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}

#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0028_Y0029"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}

#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0029_Y0028"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}


#-------------------------------------------------------------------------------


# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0029_Y0029"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}


#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0029_Y0030"
indices <- c("SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}

#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0030_Y0027"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}


#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0030_Y0028"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}

#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0030_Y0028"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}

#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0030_Y0029"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}

#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0031_Y0027"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}

#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0031_Y0028"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}

#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0031_Y0029"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}

#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0032_Y0027"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}

#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0032_Y0028"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}


#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0032_Y0029"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}

#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0033_Y0027"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}

#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0033_Y0028"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}


#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0033_Y0029"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}


#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0034_Y0027"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}

#-------------------------------------------------------------------------------

# ----- CONFIGURATION ----- #
tile_dir <- "/mnt/eo/EO4Alps/STMs/X0034_Y0028"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
output_folder_name <- "filled"
fill_range <- 35
na_value <- -10000

# ----- MEMORY-EFFICIENT GAP-FILL FUNCTION ----- #
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
    message("📆 Processing year: ", y_target)
    
    tic(paste("  Time for year", y_target))
    
    r_target <- rast(meta$file[i])
    r_target[r_target == na_value] <- NA
    
    neighbor_years <- years[abs(years - y_target) <= range & years != y_target]
    neighbor_years <- neighbor_years[order(abs(neighbor_years - y_target))]
    
    for (ny in neighbor_years) {
      neighbor_file <- meta$file[meta$year == ny]
      r_neighbor <- rast(neighbor_file)
      r_neighbor[r_neighbor == na_value] <- NA
      
      r_target <- cover(r_target, r_neighbor)
      rm(r_neighbor)
      gc()
      
      if (!any(is.na(minmax(r_target)))) break
    }
    
    out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
    out_path <- file.path(out_dir, out_name)
    writeRaster(r_target, out_path, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    if (file.exists(out_path)) {
      message("✅ Saved: ", out_path)
    } else {
      message("❌ Failed to save: ", out_path)
    }
    
    toc()
    rm(r_target)
    gc()
  }
}

# ----- Sequential Processing Loop ----- #
for (idx in indices) {
  message("▶ Processing index: ", idx, " in tile: ", basename(tile_dir))
  fill_gaps_for_index_in_tile(
    tile_dir, idx, output_folder_name,
    na_value = na_value,
    range = fill_range
  )
  message("✅ Gap-filling complete for index: ", idx, " in tile: ", basename(tile_dir))
}


