# --------------------------- #
#        LOAD PACKAGES       #
# --------------------------- #
library(terra)
library(fs)
library(stringr)
library(tidyverse)
library(future.apply)
plan(multisession, workers = 40)  # adjust number of cores



# --------------------------- #
#        CONFIGURATION        #
# --------------------------- #
input_dir <- "/mnt/eo/EO4Alps/STMs/X0028_Y0028/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0028_Y0028/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find fallback file (2022)
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # List all candidate files to process
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    out_file <- file.path(output_dir, basename(f))
    
    # 🔁 SKIP if output already exists
    if (file.exists(out_file)) {
      message("⏭️ Skipping (already processed): ", basename(f))
      next
    }
    
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})


#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0028_Y0029/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0028_Y0029/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})


#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0029_Y0028/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0029_Y0028/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})

#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0029_Y0029/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0029_Y0029/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})

#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0029_Y0030/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0029_Y0030/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})

#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0030_Y0027/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0030_Y0027/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})

#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0030_Y0028/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0030_Y0028/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})

#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0030_Y0029/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0030_Y0029/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})


#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0031_Y0027/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0031_Y0027/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})



#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0031_Y0028/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0031_Y0028/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})


#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0031_Y0029/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0031_Y0029/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})

#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0032_Y0027/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0032_Y0027/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})


#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0032_Y0028/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0032_Y0028/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})

#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0032_Y0029/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0032_Y0029/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})



#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0033_Y0027/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0033_Y0027/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})


#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0033_Y0028/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0033_Y0028/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})


#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0033_Y0029/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0033_Y0029/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})


#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0034_Y0027/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0034_Y0027/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})


#-------------------------------------------------------------------------------

input_dir <- "/mnt/eo/EO4Alps/STMs/X0034_Y0028/filled"
output_dir <- "/mnt/eo/EO4Alps/STMs/X0034_Y0028/filled_final"
indices <- c("NBR", "NDV", "NDW", "BLU", "EVI", "GRN", "NDM", "NDS", "NIR", "RED", "SAV", "SW1", "SW2")
na_value <- -10000

# --------------------------- #
# FILL GAPS USING 2022 VALUES #
# --------------------------- #
fill_remaining_with_2022 <- function(index, input_dir, output_dir, na_value = -10000) {
  dir_create(output_dir, recurse = TRUE)
  
  # Find the 2022 fallback file
  fallback_file <- dir_ls(input_dir, regexp = paste0("2022.*", index, "_STM_filled\\.tif$"))
  if (length(fallback_file) != 1) {
    message("⚠️ No unique fallback file found for ", index)
    return(NULL)
  }
  
  r_fallback <- rast(fallback_file)
  r_fallback[r_fallback == na_value] <- NA
  
  # Find all filled files for this index (exclude 2022)
  all_files <- dir_ls(input_dir, regexp = paste0(index, "_STM_filled\\.tif$"))
  filled_files <- all_files[!str_detect(all_files, "2022")]
  
  for (f in filled_files) {
    message("🩹 Filling gaps for: ", basename(f))
    r_filled <- rast(f)
    r_filled[r_filled == na_value] <- NA
    
    r_final <- cover(r_filled, r_fallback)
    
    out_file <- file.path(output_dir, basename(f))
    writeRaster(r_final, out_file, overwrite = TRUE, NAflag = na_value, gdal = "COMPRESS=DEFLATE")
    
    rm(r_filled, r_final)
    gc()
  }
  
  rm(r_fallback)
  gc()
}

# --------------------------- #
#   APPLY TO ALL INDICES     #
# --------------------------- #
future_lapply(indices, function(idx) {
  message("▶ Final fallback fill using 2022 for index: ", idx)
  fill_remaining_with_2022(idx, input_dir, output_dir, na_value)
  message("✅ Done: ", idx)
})




