# Load required packages for data wrangling, time handling, smoothing and plotting
library(tidyverse)   # dplyr + ggplot2 + readr etc.
library(lubridate)   # parse/handle datetimes
library(zoo)         # rolling mean for time series
library(scales)      # axis label formatting (e.g., commas)

Sys.setlocale("LC_TIME", "C")  # force English month/day names on date axes

# Path to the downloaded OpenAQ CSV file
file_path <- "C:/Users/86189/Desktop/Coursework/DV/openaq_location_2508_measurments.csv"

# Study period (Summer 2025)
start_date <- as.Date("2025-06-01")
end_date   <- as.Date("2025-08-31")

# Main pollutant for the time-series analysis
pollutant_main <- "pm25"

# Output folders for figures and tables
out_dir <- "outputs"
fig_dir <- file.path(out_dir, "figures")
tab_dir <- file.path(out_dir, "tables")

# Create output directories if they do not exist
dir.create(out_dir, showWarnings = FALSE)
dir.create(fig_dir, showWarnings = FALSE)
dir.create(tab_dir, showWarnings = FALSE)

# Read the raw dataset
df_raw <- readr::read_csv(file_path, show_col_types = FALSE)

# Clean and parse datetime, then derive time features
df <- df_raw %>%
  mutate(
    # --- Step 1: clean common ISO-8601 artefacts (T, Z, milliseconds) ---
    datetime_clean = datetimeUtc %>%
      stringr::str_trim() %>%                 # remove leading/trailing spaces
      stringr::str_replace("T", " ") %>%      # replace 'T' separator with space
      stringr::str_remove("Z$") %>%           # remove trailing 'Z' (UTC marker)
      stringr::str_replace("\\.\\d+", ""),    # drop milliseconds (e.g., .000)
    
    # --- Step 2: parse using a fallback strategy (HMS -> HM -> date only) ---
    dt_hms = lubridate::ymd_hms(datetime_clean, tz = "UTC", quiet = TRUE),
    dt_hm  = lubridate::ymd_hm(datetime_clean,  tz = "UTC", quiet = TRUE),
    dt_d   = lubridate::ymd(datetime_clean,     tz = "UTC", quiet = TRUE),
    
    # take the first successful parse among the attempted formats
    datetime_utc = dplyr::coalesce(dt_hms, dt_hm, dt_d),
    
    # --- Step 3: derive time variables for grouping/visualisation ---
    date    = as.Date(datetime_utc),                              # calendar date
    hour    = lubridate::hour(datetime_utc),                      # hour of day (0–23)
    weekday = lubridate::wday(datetime_utc, label = TRUE, week_start = 1), # Mon start
    
    # standardise pollutant names for consistent filtering
    parameter = tolower(parameter)
  ) %>%
  select(-dt_hms, -dt_hm, -dt_d)  # drop intermediate parse columns

# Report datetime parsing success rate (useful for Methodology)
n_total  <- nrow(df)
n_failed <- sum(is.na(df$datetime_utc))
cat("Datetime parse failures:", n_failed, "out of", n_total, "\n")

# Remove invalid rows and restrict to the study period
df <- df %>%
  filter(
    !is.na(datetime_utc),
    !is.na(date),
    !is.na(parameter),
    !is.na(value)
  ) %>%
  filter(date >= start_date, date <= end_date)

# Keep only the main pollutant for the core analysis
df_main <- df %>%
  filter(parameter == pollutant_main)

# Stop early if the chosen pollutant is not present
if (nrow(df_main) == 0) {
  stop("No rows found for pollutant_main = '", pollutant_main,
       "'. Check the 'parameter' values in your dataset.")
}

# Save a small example (first 10 rows) for reporting/appendix evidence
example_rows <- df_main %>% arrange(datetime_utc) %>% slice(1:10)
write_csv(example_rows, file.path(tab_dir, "data_example_first10.csv"))

# Save a simple missingness summary (after parsing)
missing_summary <- tibble(
  variable = c("datetime_utc", "date", "parameter", "value"),
  missing_n = c(
    sum(is.na(df$datetime_utc)),
    sum(is.na(df$date)),
    sum(is.na(df$parameter)),
    sum(is.na(df$value))
  )
)
write_csv(missing_summary, file.path(tab_dir, "missing_summary.csv"))

# Daily aggregation: compute daily mean and number of observations per day
daily <- df_main %>%
  group_by(date) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(date)

# 7-day rolling mean to smooth short-term variability
daily <- daily %>%
  mutate(rolling_7 = zoo::rollmean(mean_value, k = 7, fill = NA, align = "center"))

# Save daily summary table
write_csv(daily, file.path(tab_dir, paste0(pollutant_main, "_daily.csv")))

# Hourly pattern: average concentration by hour of day
hourly_pattern <- df_main %>%
  group_by(hour) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(hour)

write_csv(hourly_pattern, file.path(tab_dir, paste0(pollutant_main, "_hourly_pattern.csv")))

# Weekly pattern: average concentration by day of week
weekday_pattern <- df_main %>%
  group_by(weekday) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(weekday)

write_csv(weekday_pattern, file.path(tab_dir, paste0(pollutant_main, "_weekday_pattern.csv")))

# Plot 1: daily trend + rolling mean
p1 <- ggplot(daily, aes(x = date, y = mean_value)) +
  geom_line(alpha = 0.35, linewidth = 0.8) +                  # daily means
  geom_line(aes(y = rolling_7), linewidth = 0.9) +            # smoothed trend
  labs(
    title = paste0("Daily ", toupper(pollutant_main), " in Sheffield (Summer 2025)"),
    subtitle = "Daily mean with 7-day rolling mean",
    x = "Date",
    y = "Concentration (µg/m³)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = file.path(fig_dir, paste0("plot1_", pollutant_main, "_daily_trend.png")),
  plot = p1, width = 10, height = 5, dpi = 300
)

# Plot 2: diurnal pattern (hour-of-day)
p2 <- ggplot(hourly_pattern, aes(x = hour, y = mean_value)) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +                # label every 3 hours
  labs(
    title = paste0("Diurnal pattern of ", toupper(pollutant_main), " (Summer 2025)"),
    subtitle = "Average concentration by hour of day",
    x = "Hour of day",
    y = "Concentration (µg/m³)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = file.path(fig_dir, paste0("plot2_", pollutant_main, "_diurnal_pattern.png")),
  plot = p2, width = 10, height = 5, dpi = 300
)

# Plot 3: weekday pattern (average by day of week)
p3 <- ggplot(weekday_pattern, aes(x = weekday, y = mean_value)) +
  geom_col() +
  scale_y_continuous(labels = comma) +                        # nicer y-axis labels
  labs(
    title = paste0("Weekly pattern of ", toupper(pollutant_main), " (Summer 2025)"),
    subtitle = "Average concentration by day of week",
    x = "Day of week",
    y = "Concentration (µg/m³)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = file.path(fig_dir, paste0("plot3_", pollutant_main, "_weekday_pattern.png")),
  plot = p3, width = 9, height = 5, dpi = 300
)

# Identify unusually high days using a simple threshold (mean + 2*SD of daily means)
thr <- mean(daily$mean_value, na.rm = TRUE) + 2 * sd(daily$mean_value, na.rm = TRUE)

daily_flag <- daily %>%
  mutate(is_high = mean_value > thr)

write_csv(daily_flag, file.path(tab_dir, paste0(pollutant_main, "_daily_flagged.csv")))

# Plot 4: daily trend with flagged high days and threshold line
p4 <- ggplot(daily, aes(date, mean_value)) +
  geom_line(alpha = 0.35, linewidth = 0.8) +
  geom_point(data = daily_flag %>% filter(is_high),
             aes(date, mean_value), size = 2) +
  geom_hline(yintercept = thr, linetype = "dashed") +
  labs(
    title = paste0("Potential high ", toupper(pollutant_main), " days (threshold-based)"),
    subtitle = "Flagged days above mean + 2SD (daily means)",
    x = "Date",
    y = "Concentration (µg/m³)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = file.path(fig_dir, paste0("plot4_", pollutant_main, "_high_days.png")),
  plot = p4, width = 10, height = 5, dpi = 300
)

# Save session information (helps reproducibility: R version + package versions)
sink(file.path(out_dir, "sessionInfo.txt"))
print(sessionInfo())
sink()

# Final message showing where outputs were saved
cat("Done. Outputs saved to:\n", normalizePath(out_dir), "\n")
