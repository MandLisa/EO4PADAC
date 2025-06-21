# Define the path to the original file
original_file <- "/mnt/eo/EO4Alps/EO4PADAC/param_files/preds_jun25/prediction_1986.prm"

# Read the content of the original file
file_content <- readLines(original_file)

# Extract directory of original file
target_dir <- dirname(original_file)

# Loop over the years 1987 to 2023
for (year in 1987:2023) {
  # Replace all occurrences of 1986 with the current year
  new_content <- gsub("1986", as.character(year), file_content)
  
  # Construct new filename
  new_filename <- file.path(target_dir, paste0("prediction_", year, ".prm"))
  
  # Write the modified content to the new file
  writeLines(new_content, con = new_filename)
}

cat("✅ Files successfully written from 1987 to 2023.\n")
