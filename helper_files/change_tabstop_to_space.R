# Path to the input file
input_file_path <- "/data/eo/EO4Alps/level3/STMs_samples/features/features_all_clean_l21.txt"
# Path to the output file (you can use the same path to overwrite the original file)
output_file_path <- "/data/eo/EO4Alps/level3/STMs_samples/features/features_all_clean_l2.txt"

# Read the content of the file
file_content <- readLines(input_file_path)

# Replace all tabs with spaces
modified_content <- gsub("\t", " ", file_content)

# Write the modified content back to the file
writeLines(modified_content, output_file_path)

cat("Tabs have been replaced with spaces successfully.\n")
