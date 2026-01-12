rmse <- function(actual, pred) sqrt(mean((actual - pred)^2, na.rm = TRUE))
mae  <- function(actual, pred) mean(abs(actual - pred), na.rm = TRUE)
r2   <- function(actual, pred) {
  ss_res <- sum((actual - pred)^2, na.rm = TRUE)
  ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  1 - (ss_res / ss_tot)
}

cap_outliers_iqr <- function(x, k = 1.5) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lo <- q1 - k * iqr
  hi <- q3 + k * iqr
  pmin(pmax(x, lo), hi)
}

safe_numeric <- function(x) {
  # Handles "?" or commas etc.
  x <- as.character(x)
  x[x %in% c("?", "", "NA", "NaN")] <- NA
  as.numeric(gsub(",", ".", x))
}

weekday_weekend <- function(ts) {
  w <- wday(ts, week_start = 1)  # Mon=1
  ifelse(w %in% c(6,7), "Weekend", "Weekday")
}

season_from_month <- function(m) {
  case_when(
    m %in% c(12, 1, 2) ~ "Winter",
    m %in% c(3, 4, 5)  ~ "Spring",
    m %in% c(6, 7, 8)  ~ "Summer",
    TRUE               ~ "Autumn"
  )
}

save_plot <- function(p, filename, w = 9, h = 6) {
  ggsave(filename = file.path("output/plots", filename), plot = p, width = w, height = h)
}
