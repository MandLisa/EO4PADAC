src_base="/mnt/eo/EO4Alps/STMs"
dst_base="/mnt/eo/EO4Alps/level3_STMs_test"

# Ordnernamen durchgehen
for folder in "$src_base"/*; do
  # nur wenn es ein Verzeichnis ist
  if [ -d "$folder" ]; then
    folder_name=$(basename "$folder")
    
    # Zielverzeichnis definieren
    target_dir="$dst_base/$folder_name"

    # Erstelle Zielordner, falls er nicht existiert
    mkdir -p "$target_dir"

    # Liste der zu verschiebenden Unterordner
    for subfolder in filled filled_final original; do
      src_sub="$folder/$subfolder"
      dst_sub="$target_dir/$subfolder"

      # Nur verschieben, wenn Unterordner existiert
      if [ -d "$src_sub" ]; then
        mv "$src_sub" "$dst_sub"
      fi
    done
  fi
done
