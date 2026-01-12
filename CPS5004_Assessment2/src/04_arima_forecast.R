# ARIMA on the hourly target series
y <- hourly$global_active_power

# Train/test split (last 14 days worth of hours)
# 14 days * 24 hours = 336 points, but if fewer points exist, use 20% as test
n <- length(y)
test_n <- min(336, floor(0.2 * n))
train_n <- n - test_n

y_train <- y[1:train_n]
y_test  <- y[(train_n + 1):n]

# Convert to ts; frequency = 24 for hourly daily seasonality
y_train_ts <- ts(y_train, frequency = 24)

fit_arima <- auto.arima(y_train_ts, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)

fc <- forecast::forecast(fit_arima, h = length(y_test))
pred <- as.numeric(fc$mean)

arima_metrics <- tibble(
  model = "ARIMA(auto.arima)",
  RMSE = rmse(y_test, pred),
  MAE  = mae(y_test, pred),
  R2   = r2(y_test, pred)
)

write_csv(arima_metrics, file.path("output/tables", "arima_metrics.csv"))
saveRDS(fit_arima, file.path("output/models", "arima_fit.rds"))

# Plot forecast vs actual
plot_df <- tibble(
  idx = 1:length(y_test),
  actual = y_test,
  forecast = pred
)

p <- ggplot(plot_df, aes(x = idx)) +
  geom_line(aes(y = actual)) +
  geom_line(aes(y = forecast)) +
  labs(
    title = "ARIMA Forecast vs Actual (Test Window)",
    x = "Test Index (hours)",
    y = "Global Active Power (kW)"
  )

save_plot(p, "arima_forecast_vs_actual.png")

cat("ARIMA forecasting complete.\n")
