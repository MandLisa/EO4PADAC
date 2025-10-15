# Packages (install once if needed)
# install.packages(c("curl"))

library(curl)

# --- INPUTS ---
url_file  <- "/mnt/eo/EO4Alps/climate_data/envidatS3paths_spring.txt"        # your file with one URL per line (from WGET list)
dest_root <- "/mnt/eo/EO4Alps/climate_data/temp/temp_spring"  # target folder; files will be saved here
retries   <- 3                 # number of retries per file
timeout_s <- 600               # per-file timeout in seconds
skip_existing <- TRUE          # set to FALSE to re-download existing files

# --- READ & CLEAN URL LIST ---
urls <- readLines(url_file, warn = FALSE)
urls <- trimws(urls)
urls <- urls[nzchar(urls) & !startsWith(urls, "#")] # drop blanks/comments
urls <- unique(urls)

message(sprintf("Found %d unique URLs.", length(urls)))

# --- PREPARE DESTINATION ---
dir.create(dest_root, showWarnings = FALSE, recursive = TRUE)

# Helper: derive a safe filename (strip query strings if present)
basename_safe <- function(u) {
  b <- basename(u)
  sub("\\?.*$", "", b)  # remove ?query=... parts
}

# Robust downloader with retries/backoff
safe_download <- function(url, dest, retries = 3, timeout_s = 600) {
  h <- new_handle(
    followlocation = TRUE,   # follow redirects
    connecttimeout = 30,
    timeout = timeout_s,
    ssl_verifypeer = TRUE    # keep TLS checks on
  )
  for (i in seq_len(retries)) {
    ok <- try({
      curl_download(url, destfile = dest, handle = h, mode = "wb")
      TRUE
    }, silent = TRUE)
    if (isTRUE(ok)) return(TRUE)
    Sys.sleep(2^i)          # exponential backoff
  }
  return(FALSE)
}

# --- DOWNLOAD LOOP ---
results <- data.frame(url = urls, file = NA_character_, ok = NA, stringsAsFactors = FALSE)

for (i in seq_along(urls)) {
  u  <- urls[i]
  fn <- basename_safe(u)
  if (!nzchar(fn)) fn <- paste0("file_", i)  # fallback if URL ends with slash
  
  dest <- file.path(dest_root, fn)
  results$file[i] <- dest
  
  if (skip_existing && file.exists(dest)) {
    message(sprintf("[%d/%d] SKIP (exists): %s", i, length(urls), dest))
    results$ok[i] <- TRUE
    next
  }
  
  message(sprintf("[%d/%d] Downloading: %s -> %s", i, length(urls), u, dest))
  results$ok[i] <- safe_download(u, dest, retries = retries, timeout_s = timeout_s)
  if (!results$ok[i]) warning(sprintf("FAILED: %s", u))
}

# Summary
table(results$ok, useNA = "ifany")


# Move files to years folder one level higher
# --- CONFIG ---
root_dir <- "/mnt/eo/EO4Alps/climate_data/temp"
src_dir  <- file.path(root_dir, "temp_spring")

# Only files matching this pattern will be moved.
# Pattern expects: CHELSA_tas_MM_YYYY_V.2.1.tif  (MM = 2 digits, YYYY = 4 digits)
fname_regex <- "^CHELSA_tas_\\d{2}_(\\d{4})_V\\.2\\.1\\.tif$"

dry_run     <- FALSE  # TRUE = show what would happen, FALSE = actually move files
overwrite   <- FALSE  # if TRUE and a same-named file exists in the destination, it will be replaced

# --- SAFETY CHECKS ---
stopifnot(dir.exists(root_dir))
stopifnot(dir.exists(src_dir))

# --- LIST + PARSE ---
files_full <- list.files(src_dir, pattern = "\\.tif$", full.names = TRUE, recursive = FALSE)
if (length(files_full) == 0L) {
  message("No .tif files found in: ", src_dir)
}

bn <- basename(files_full)

# Keep only files that match the naming convention
matches <- grepl(fname_regex, bn)
files_full <- files_full[matches]
bn         <- bn[matches]

if (length(files_full) == 0L) {
  stop("No files matched the expected CHELSA_tas_MM_YYYY_V.2.1.tif pattern.")
}

# Extract year (capture group 1)
years <- sub(fname_regex, "\\1", bn)

# --- PREP DESTINATION PATHS ---
dest_dirs  <- file.path(root_dir, years)
dest_files <- file.path(dest_dirs, bn)

# Create year directories if missing (your folders should already exist,
# but this makes the script idempotent)
years_unique <- unique(years)
for (yd in file.path(root_dir, years_unique)) {
  if (!dir.exists(yd)) dir.create(yd, recursive = TRUE, showWarnings = FALSE)
}

# --- MOVE (with optional overwrite) ---
moved <- logical(length(files_full))

for (i in seq_along(files_full)) {
  src  <- files_full[i]
  dest <- dest_files[i]
  
  if (file.exists(dest)) {
    if (!overwrite) {
      message(sprintf("SKIP (exists): %s -> %s", src, dest))
      moved[i] <- FALSE
      next
    } else {
      # remove the existing destination file before renaming
      ok_rm <- try(unlink(dest), silent = TRUE)
      if (inherits(ok_rm, "try-error")) {
        warning("Could not remove existing file: ", dest)
        moved[i] <- FALSE
        next
      }
    }
  }
  
  message(sprintf("%s: %s -> %s",
                  if (dry_run) "DRY-RUN" else "MOVE",
                  src, dest))
  
  if (dry_run) {
    moved[i] <- NA
  } else {
    ok <- try(file.rename(src, dest), silent = TRUE)
    moved[i] <- isTRUE(ok)
    if (!moved[i]) {
      warning("FAILED to move: ", src, " -> ", dest,
              " | Trying copy+delete fallback...")
      # fallback (rarely needed on same filesystem)
      ok_copy <- try(file.copy(src, dest, overwrite = TRUE), silent = TRUE)
      if (isTRUE(ok_copy)) {
        unlink(src)
        moved[i] <- TRUE
      } else {
        warning("Fallback also failed for: ", src)
      }
    }
  }
}

# --- SUMMARY ---
summary_df <- data.frame(
  file   = bn,
  year   = years,
  source = files_full,
  target = dest_files,
  moved  = moved,
  stringsAsFactors = FALSE
)

cat("\nSummary (counts):\n")
print(table(summary_df$moved, useNA = "ifany"))

# Optionally write a log
# write.csv(summary_df, file.path(root_dir, "move_log_temp_spring.csv"), row.names = FALSE)

# Helpful filter: show any failures
failed <- subset(summary_df, moved %in% FALSE)
if (nrow(failed) > 0) {
  cat("\nFiles that failed to move:\n")
  print(failed[, c("file", "source", "target")], row.names = FALSE)
}




