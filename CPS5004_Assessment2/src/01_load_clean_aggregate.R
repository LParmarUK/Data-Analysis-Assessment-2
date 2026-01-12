data_path <- file.path("data", "household_electricity_consumption.csv")
stopifnot(file.exists(data_path))

raw <- read_csv(data_path, show_col_types = FALSE)

# Standardise column names
df <- raw %>%
  rename_with(~ str_replace_all(tolower(.x), "\\s+", "_")) %>%
  rename_with(~ str_replace_all(.x, "-", "_"))

# Required fields (typical dataset)
# timestamp, global_active_power, global_reactive_power, voltage,
# global_intensity, sub_metering_1, sub_metering_2, sub_metering_3

# If the timestamp is split (date/time), try to reconstruct
if (!("timestamp" %in% names(df))) {
  if (all(c("date", "time") %in% names(df))) {
    df <- df %>% mutate(timestamp = paste(date, time))
  } else if (all(c("datetime") %in% names(df))) {
    df <- df %>% rename(timestamp = datetime)
  } else {
    stop("Could not find a timestamp column (expected 'Timestamp' or similar).")
  }
}

# Parse timestamp
df <- df %>%
  mutate(
    timestamp = ymd_hms(timestamp, quiet = TRUE),
    timestamp = if_else(is.na(timestamp), dmy_hms(as.character(raw[[1]]), quiet = TRUE), timestamp)
  )

if (all(is.na(df$timestamp))) stop("Timestamp parsing failed. Check your CSV timestamp format.")

# Convert numeric columns safely
num_cols <- setdiff(names(df), c("timestamp", "date", "time"))
for (cname in num_cols) df[[cname]] <- safe_numeric(df[[cname]])

# Missing value imputation (time series friendly) using imputeTS
# Apply per numeric column, ordered by time
df <- df %>% arrange(timestamp)

for (cname in num_cols) {
  x <- df[[cname]]
  if (anyNA(x)) {
    # Kalman smoothing is robust for time series
    df[[cname]] <- na_kalman(x, model = "auto.arima")
  }
}

# Outlier handling (IQR capping) on predictors + target
for (cname in num_cols) df[[cname]] <- cap_outliers_iqr(df[[cname]])

# Feature engineering: weekday/weekend, season, hour
df <- df %>%
  mutate(
    hour = hour(timestamp),
    day = as_date(timestamp),
    dow_type = weekday_weekend(timestamp),
    month = month(timestamp),
    season = season_from_month(month)
  )

# Aggregate to HOURLY (as required: hourly or daily). Use hourly to keep time series detail.
hourly <- df %>%
  mutate(ts_hour = floor_date(timestamp, unit = "hour")) %>%
  group_by(ts_hour) %>%
  summarise(
    global_active_power   = mean(global_active_power, na.rm = TRUE),
    global_reactive_power = mean(global_reactive_power, na.rm = TRUE),
    voltage               = mean(voltage, na.rm = TRUE),
    global_intensity      = mean(global_intensity, na.rm = TRUE),
    sub_metering_1        = mean(sub_metering_1, na.rm = TRUE),
    sub_metering_2        = mean(sub_metering_2, na.rm = TRUE),
    sub_metering_3        = mean(sub_metering_3, na.rm = TRUE),
    hour                  = first(hour(ts_hour)),
    dow_type              = first(weekday_weekend(ts_hour)),
    month                 = first(month(ts_hour)),
    season                = first(season_from_month(month(ts_hour))),
    .groups = "drop"
  ) %>%
  arrange(ts_hour)

write_csv(hourly, file.path("output/tables", "hourly_cleaned_aggregated.csv"))

cat("Loaded, cleaned, imputed, capped outliers, and aggregated to hourly.\n")
