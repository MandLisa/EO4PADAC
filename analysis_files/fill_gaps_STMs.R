library(terra)
library(fs)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)

# 📁 Deine Pfade
base_dir <- "/mnt/eo/EO4Alps/STMs_2"
output_dir <- "/mnt/eo/EO4Alps/STMs_2/X0028_Y0028/test_fill"
tile <- file.path(base_dir, "X0028_Y0028")

# 📂 Lade nur NBR-Dateien
files <- dir_ls(tile, glob = "*NBR*.tif")
file_names <- basename(files)

# Metadaten extrahieren
extract_year <- function(x) str_extract(x, "^\\d{4}")
meta <- tibble(file = files, year = as.integer(extract_year(file_names))) |>
  arrange(year)

years <- meta$year
n_years <- length(years)

# Zielordner
filled_tile_dir <- file.path(output_dir, basename(tile))
dir_create(filled_tile_dir)

# ⏱ Für jedes Jahr i
for (i in seq_len(n_years)) {
  
  y_target <- years[i]
  f_target <- meta$file[i]
  
  message("📆 Filling year: ", y_target)
  
  # Liste der Nachbarjahre in ±8, sortiert nach zeitlicher Nähe
  neighbors <- meta |>
    filter(year != y_target & abs(year - y_target) <= 8) |>
    arrange(abs(year - y_target))
  
  # Lade Zielraster
  r_target <- rast(f_target)
  r_target[r_target == -10000] <- NA  # 🧠 NA-Erkennung einbauen
  n_bands <- nlyr(r_target)
  
  # Ausgabe-Raster vorbereiten
  r_filled <- r_target
  
  # 🔁 Für jedes Band
  for (b in 1:n_bands) {
    band_target <- r_target[[b]]
    out_band <- band_target
    
    if (anyNA(values(band_target))) {
      
      for (j in seq_len(nrow(neighbors))) {
        candidate_path <- neighbors$file[j]
        band_candidate <- try(rast(candidate_path)[[b]], silent = TRUE)
        if (inherits(band_candidate, "try-error")) next
        
        band_candidate[band_candidate == -10000] <- NA  # 🧠 auch hier NA setzen
        
        # Nur ersetzen, wo Kandidat gültig ist
        out_band <- cover(out_band, band_candidate)
        
        # Effizienter Abbruch, wenn keine NA mehr vorhanden
        if (global(is.na(out_band), fun = "sum", na.rm = TRUE)[1] == 0) break
      }
    }
    
    # Füge Band ins Ergebnis ein
    r_filled[[b]] <- out_band
  }
  
  # Ergebnis speichern
  out_name <- str_replace(basename(f_target), "\\.tif$", "_filled.tif")
  out_path <- file.path(filled_tile_dir, out_name)
  writeRaster(r_filled, out_path, overwrite = TRUE)
  message("✅ Saved: ", out_path)
}
