# Step 1: Read the file into R
file_path <- "/data/eo/EO4Alps/level3/STMs_samples/features/features_all_clean.txt"
data <- readLines(file_path)

# Step 2: Replace multiple spaces/tabs with a single space
features_all_clean <- gsub("[ \t]+", " ", data)

# Step 3: Write the cleaned data back to a file
writeLines(features_all_clean, "/data/eo/EO4Alps/level3/STMs_samples/features/features_all_clean1.txt")
