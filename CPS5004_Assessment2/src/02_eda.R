# Descriptive statistics
desc <- hourly %>%
  summarise(across(
    .cols = c(global_active_power, global_reactive_power, voltage, global_intensity,
              sub_metering_1, sub_metering_2, sub_metering_3),
    .fns = list(
      mean = ~mean(.x, na.rm = TRUE),
      sd   = ~sd(.x, na.rm = TRUE),
      min  = ~min(.x, na.rm = TRUE),
      p25  = ~quantile(.x, 0.25, na.rm = TRUE),
      med  = ~median(.x, na.rm = TRUE),
      p75  = ~quantile(.x, 0.75, na.rm = TRUE),
      max  = ~max(.x, na.rm = TRUE)
    )
  )) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value")

write_csv(desc, file.path("output/tables", "descriptive_statistics.csv"))

# Scatter plots (use base pairs to avoid extra packages)
pair_df <- hourly %>%
  select(global_active_power, global_reactive_power, voltage, global_intensity,
         sub_metering_1, sub_metering_2, sub_metering_3)

png(file.path("output/plots", "pairwise_scatter_plots.png"), width = 1200, height = 1200)
pairs(pair_df, main = "Pairwise Scatter Plots (Hourly Aggregated)")
dev.off()

# Histogram of global active power
p1 <- ggplot(hourly, aes(x = global_active_power)) +
  geom_histogram(bins = 40) +
  labs(title = "Histogram: Global Active Power (Hourly)", x = "Global Active Power (kW)", y = "Count")
save_plot(p1, "hist_global_active_power.png")

# Scatter: GAP vs Voltage with regression line
p2 <- ggplot(hourly, aes(x = voltage, y = global_active_power)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Global Active Power vs Voltage", x = "Voltage (V)", y = "Global Active Power (kW)")
save_plot(p2, "scatter_gap_vs_voltage_lm.png")

# Scatter: GAP vs Global Intensity with regression line
p3 <- ggplot(hourly, aes(x = global_intensity, y = global_active_power)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Global Active Power vs Global Intensity", x = "Global Intensity (A)", y = "Global Active Power (kW)")
save_plot(p3, "scatter_gap_vs_intensity_lm.png")

# Boxplot: weekday vs weekend
p4 <- ggplot(hourly, aes(x = dow_type, y = global_active_power)) +
  geom_boxplot() +
  labs(title = "Global Active Power: Weekday vs Weekend", x = "Day Type", y = "Global Active Power (kW)")
save_plot(p4, "boxplot_gap_weekday_weekend.png")

# Correlation table
corr <- cor(pair_df, use = "complete.obs")
corr_tbl <- as.data.frame(as.table(corr)) %>%
  rename(var1 = Var1, var2 = Var2, correlation = Freq) %>%
  arrange(desc(abs(correlation)))

write_csv(corr_tbl, file.path("output/tables", "correlations.csv"))

cat("EDA complete: stats + required plots saved.\n")
