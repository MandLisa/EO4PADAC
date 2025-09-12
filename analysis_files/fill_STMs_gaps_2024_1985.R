# --------------------------- #
#        LOAD PACKAGES       #
# --------------------------- #
library(terra)
library(fs)
library(stringr)
library(future.apply)

plan(multisession, workers = 25)
terraOptions(progress = 1)

# --------------------------- #
#        CONFIGURATION        #
# --------------------------- #
base_dir      <- "/mnt/eo/EO4Alps/STMs"  # tiles live here: X####_Y####
target_years  <- c(1985, 2024)           # only touch these two years
fallback_year <- 2022                    # donor for gap filling
indices <- c("NBR","NDV","NDW","BLU","EVI","GRN","NDM","NDS","NIR","RED","SAV","SW1","SW2")
na_value <- -10000

# --------------------------- #
#     FILENAME MATCHING       #
# --------------------------- #
# Example: 19850101-19851231_001-365_HL_TSA_LNDLG_NDM_STM.tif
stm_regex <- function(year, index) {
  paste0("^", year, "[0-9]{4}-", year, "[0-9]{4}_",
         "[0-9]{3}-[0-9]{3}_HL_TSA_LNDLG_", index, "_STM\\.tif$")
}

# --------------------------- #
#   FIXED IO RESOLUTION       #
# --------------------------- #
resolve_input_dir  <- function(tile_dir) tile_dir                  # <-- READ from tile root
resolve_output_dir <- function(tile_dir) file.path(tile_dir, "filled")  # <-- WRITE into /filled

# --------------------------- #
#        CORE FUNCTION        #
# --------------------------- #
fill_two_years_with_2022 <- function(tile_dir,
                                     indices,
                                     years = c(1985, 2024),
                                     fallback_year = 2022,
                                     na_value = -10000) {
  
  input_dir  <- resolve_input_dir(tile_dir)     # e.g., /.../X0030_Y0028
  output_dir <- resolve_output_dir(tile_dir)    # e.g., /.../X0030_Y0028/filled
  dir_create(output_dir, recurse = TRUE)
  
  if (!dir_exists(input_dir)) {
    message("⚠️  Missing input dir: ", input_dir)
    return(invisible(NULL))
  }
  
  any_done <- FALSE
  
  for (idx in indices) {
    # 1) locate 2022 fallback
    fb_pat   <- stm_regex(fallback_year, idx)
    fb_files <- dir_ls(input_dir, regexp = fb_pat, type = "file")
    
    if (length(fb_files) != 1) {
      message("⚠️  No unique ", fallback_year, " fallback for index ", idx,
              " in ", input_dir, " (found: ", length(fb_files), ")")
      next
    }
    
    r_fallback <- rast(fb_files)
    r_fallback[r_fallback == na_value] <- NA
    
    # 2) process only 1985 & 2024
    for (yy in years) {
      yr_pat   <- stm_regex(yy, idx)
      yr_files <- dir_ls(input_dir, regexp = yr_pat, type = "file")
      
      if (length(yr_files) == 0) {
        message("ℹ️  No files for ", yy, " index=", idx, " in ", input_dir)
        next
      }
      
      for (f in yr_files) {
        out_file <- file.path(output_dir, basename(f))  # write into /filled/
        
        message("🩹 Filling gaps: ", basename(f),
                "  [tile=", basename(tile_dir), ", idx=", idx, "]")
        
        r_filled <- rast(f)
        r_filled[r_filled == na_value] <- NA
        
        r_final <- cover(r_filled, r_fallback)  # fill only gaps from 2022
        
        writeRaster(
          r_final, out_file,
          overwrite = TRUE,           # always overwrite outputs
          NAflag = na_value,
          wopt = list(gdal = c("TILED=YES", "COMPRESS=DEFLATE", "BIGTIFF=YES"))
        )
        
        rm(r_filled, r_final); gc()
        any_done <- TRUE
      }
    }
    
    rm(r_fallback); gc()
  }
  
  if (!any_done) {
    message("ℹ️  Nothing processed for tile ", basename(tile_dir),
            " (check index tokens and presence of 1985/2024 & 2022 files in the tile root).")
  }
  invisible(NULL)
}

# --------------------------- #
#     DISCOVER TILE FOLDERS   #
# --------------------------- #
tile_dirs <- dir_ls(base_dir, type = "directory", regexp = "X[0-9]{4}_Y[0-9]{4}$")

# Sanity echo for one tile
if (length(tile_dirs)) {
  td0 <- tile_dirs[1]
  message("Tile: ", td0)
  message("Read from: ", resolve_input_dir(td0))
  message("Write into: ", resolve_output_dir(td0))
}

# --------------------------- #
#       RUN IN PARALLEL       #
# --------------------------- #
future_lapply(
  tile_dirs,
  function(td) fill_two_years_with_2022(
    tile_dir = td,
    indices = indices,
    years = target_years,
    fallback_year = fallback_year,
    na_value = na_value
  )
)

message("✅ Finished fallback filling for years ",
        paste(target_years, collapse = ", "),
        " across ", length(tile_dirs), " tiles and ", length(indices), " indices.")
