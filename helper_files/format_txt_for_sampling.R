# Read the data (assumes space-separated, no header)
data <- read.table("/mnt/eo/EO4Alps/spec_lib/l1/candidates_FORCE_l1_2811.txt", header = FALSE)

# Filter out rows where last column == 6
filtered <- subset(data, V3 != 6)

# Format first two columns with 15 decimal places
col1 <- sprintf("%.15f", filtered$V1)
col2 <- sprintf("%.15f", filtered$V2)
col3 <- filtered$V3

# Custom formatting function
formatted_lines <- mapply(function(a, b, c) {
  # Add two spaces if abs(a) < 10, else one space
  space <- if (abs(as.numeric(a)) < 10) "  " else " "
  paste0(a, space, b, " ", c)
}, col1, col2, col3)

# Write to file
writeLines(formatted_lines, "/mnt/eo/EO4Alps/spec_lib/l1/candidates_FORCE_l1_Jun25.txt")
