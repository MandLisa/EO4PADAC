# Set your directory
log_dir <- "/mnt/eo/EO4Alps/SVM_models/jun25/logs"

# List all .log files in the directory
log_files <- list.files(log_dir, pattern = "\\.log$", full.names = TRUE)

# Function to extract Rsq from a single file
extract_rsq <- function(file) {
  lines <- readLines(file, warn = FALSE)
  rsq_line <- grep("Rsq:", lines, value = TRUE)
  if (length(rsq_line) > 0) {
    rsq_value <- sub(".*Rsq:\\s*", "", rsq_line[1])
    return(rsq_value)
  } else {
    return(NA)
  }
}

# Apply the function to all files
rsq_values <- sapply(log_files, extract_rsq)

# Create a data frame with filenames and corresponding Rsq values
rsq_df <- data.frame(
  File = basename(log_files),
  Rsq = rsq_values,
  stringsAsFactors = FALSE
)

# View the result
print(rsq_df)
