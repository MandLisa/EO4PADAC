library(tidyverse)
library(lubridate)
library(ggridges)

# Define directory
dir_path <- "/mnt/eo/EO4Alps/level2/X0030_Y0028"

# List all BOA files
files <- list.files(dir_path, pattern = "_BOA\\.tif$", full.names = FALSE)

# Extract date part (assumes format: YYYYMMDD_LEVEL2_SEN2B_BOA.tif)
dates <- str_extract(files, "^\\d{8}")  # Get YYYYMMDD
dates <- ymd(dates)  # Convert to Date object

# Remove NA in case of malformed filenames
dates <- dates[!is.na(dates)]

# Create a data frame with year and month
df <- tibble(
  year = year(dates),
  month = month(dates, label = TRUE, abbr = TRUE)  # factor with Jan, Feb, ...
)

# Count observations per month-year
monthly_counts <- df %>%
  count(year, month)

# Plot
ggplot(monthly_counts, aes(x = month, y = n, fill = factor(year))) +
  geom_col(position = "dodge") +
  labs(
    title = "Monthly distribution of BOA observations per year",
    x = "Month",
    y = "Number of observations",
    fill = "Year"
  ) +
  theme_minimal(base_size = 14)



# Data prep
df_heatmap <- tibble(
  file = list.files("/mnt/eo/EO4Alps/level2/X0030_Y0028", pattern = "_BOA\\.tif$", full.names = FALSE)
) %>%
  mutate(date = ymd(str_extract(file, "^\\d{8}"))) %>%
  filter(!is.na(date)) %>%
  mutate(year = year(date),
         month = month(date, label = TRUE, abbr = TRUE)) %>%
  count(year, month)

# Plot
ggplot(df_heatmap, aes(x = month, y = factor(year), fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Observations", option = "F") +
  scale_y_discrete(breaks = function(x) x[as.integer(x) %% 5 == 0]) +
  labs(
    title = "Monthly distribution of BOA observations per year",
    x = "Month", y = "Year"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = "/mnt/eo/EO4Alps/figs/data_distribution.png",           
  width    = 8,                    
  height   = 6,               
  dpi      = 300               
)





