install.packages("magick")
library(magick)

# Read all PNGs from folder and sort
img_dir <- "Y:/01_PhD/00_thesis/defense/Animationen/gif/frames"

files <- list.files(img_dir, pattern = "^frame_\\d+\\.png$", full.names = TRUE)

# Sort by extracted number
extract_num <- function(x) as.numeric(gsub(".*frame_(\\d+)\\.png", "\\1", x))
files_sorted <- files[order(sapply(files, extract_num))]

# Create GIF
img_list <- image_read(files_sorted)


animation <- image_animate(image_join(img_list), fps =2)
print(animation)

image_write(animation, "Y:/01_PhD/00_thesis/defense/Animationen/gif/map_animation.gif")





# 2. Define years from 1991 to 2023
years <- 1990:2023

# 3. Function to add year label to each image
annotate_image <- function(img, year_label) {
  image_annotate(
    img,
    text = paste("Year:", year_label),
    size = 40,
    gravity = "southwest",  # Try also "northwest", "southeast", etc.
    color = "white",
    boxcolor = "#2c3e50",     # Box behind text for contrast
    strokecolor = "black"
  )
}

# 4. Read and annotate all images
img_annotated <- mapply(function(f, y) {
  img <- image_read(f)
  annotate_image(img, y)
}, files_sorted, years, SIMPLIFY = FALSE)


# ✅ Step 5: Create `frames/` folder inside original directory
frames_dir <- file.path(img_dir, "frames")
dir.create(frames_dir, showWarnings = FALSE)

# ✅ Step 6: Save annotated PNGs into that folder
for (i in seq_along(img_annotated)) {
  filename <- sprintf("frame_%02d.png", i)
  path <- file.path(frames_dir, filename)
  image_write(img_annotated[[i]], path)
}


# 6. Animate (e.g. 4 fps for smoother transition)
animation <- image_animate(img_scaled, fps = 2)

print(animation)
