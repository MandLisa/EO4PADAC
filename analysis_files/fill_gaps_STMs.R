library(terra)
library(fs)
library(stringr)
library(tibble)
library(dplyr)

# 📁 Verzeichnisse definieren
tile_dir <- "/mnt/eo/EO4Alps/STMs_2/X0028_Y0028"
output_dir <- "/mnt/eo/EO4Alps/STMs_2/X0028_Y0028/filled_nbr"
dir_create(output_dir)

# 📂 Nur NBR-Raster auflisten
files <- dir_ls(tile_dir, glob = "*NBR*.tif")
file_names <- basename(files)

# 📅 Jahresinfo extrahieren
meta <- tibble(
  file = files,
  year = as.integer(str_extract(file_names, "^\\d{4}"))
) |> arrange(year)

years <- meta$year

# 🔁 Gap-Filling pro Jahr
for (i in seq_along(years)) {
  y_target <- years[i]
  message("📆 Verarbeite Jahr: ", y_target)
  
  # Zielraster laden
  r_target <- rast(meta$file[i])
  r_target[r_target == -10000] <- NA  # ⛅ potenziellen NA-Wert setzen
  filled <- r_target  # Initialisiere Ergebnis
  
  # Nachbarjahre bestimmen (±5, ohne sich selbst)
  neighbor_idxs <- which(abs(years - y_target) <= 5 & years != y_target)
  neighbor_idxs <- neighbor_idxs[order(abs(years[neighbor_idxs] - y_target))]
  
  for (j in neighbor_idxs) {
    r_neighbor <- rast(meta$file[j])
    r_neighbor[r_neighbor == -10000] <- NA
    filled <- cover(filled, r_neighbor)
    
    # Frühes Beenden, wenn alles gefüllt
    if (global(is.na(filled[[1]]), "sum", na.rm = TRUE)[1] == 0) break
  }
  
  # Ausgabe schreiben
  out_name <- str_replace(basename(meta$file[i]), "\\.tif$", "_filled.tif")
  out_path <- file.path(output_dir, out_name)
  writeRaster(filled, out_path, overwrite = TRUE, NAflag = NA)
  
  message("✅ Fertig gespeichert: ", out_name)
  
  # RAM freigeben
  rm(r_target, r_neighbor, filled)
  gc()
}
