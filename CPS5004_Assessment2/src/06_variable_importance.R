lm_fit  <- readRDS(file.path("output/models", "lm_fit.rds"))
rf_fit  <- readRDS(file.path("output/models", "rf_cls.rds"))
xgb_fit <- readRDS(file.path("output/models", "xgb_cls.rds"))

# ---- 1) Linear regression importance (abs t-stat) ----
lm_sum <- summary(lm_fit)

lm_imp <- as.data.frame(lm_sum$coefficients) %>%
  tibble::rownames_to_column("feature") %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`,
    t_value = `t value`,
    p_value = `Pr(>|t|)`
  ) %>%
  mutate(abs_t = abs(t_value)) %>%
  arrange(desc(abs_t))

write_csv(lm_imp, file.path("output/tables", "importance_linear_regression.csv"))

# 2) Random Forest importance
# randomForest::importance returns a matrix, so convert safely
rf_imp_mat <- randomForest::importance(rf_fit)
rf_imp_df <- as.data.frame(rf_imp_mat) %>%
  tibble::rownames_to_column("feature")

# Choose a single "overall" column for ranking
if ("MeanDecreaseGini" %in% colnames(rf_imp_df)) {
  rf_imp_df <- rf_imp_df %>%
    mutate(Overall = MeanDecreaseGini) %>%
    arrange(desc(Overall))
} else if ("MeanDecreaseAccuracy" %in% colnames(rf_imp_df)) {
  rf_imp_df <- rf_imp_df %>%
    mutate(Overall = MeanDecreaseAccuracy) %>%
    arrange(desc(Overall))
} else {
  # fallback: rank by first numeric column
  first_num <- names(rf_imp_df)[sapply(rf_imp_df, is.numeric)][1]
  rf_imp_df <- rf_imp_df %>%
    mutate(Overall = .data[[first_num]]) %>%
    arrange(desc(Overall))
}

write_csv(rf_imp_df, file.path("output/tables", "importance_random_forest.csv"))

# 3) XGBoost importance
# Feature names must match training design matrix column names
model_df <- hourly %>%
  select(
    global_active_power,
    global_reactive_power, voltage, global_intensity,
    sub_metering_1, sub_metering_2, sub_metering_3,
    hour, dow_type, season
  ) %>%
  mutate(
    dow_type = factor(dow_type),
    season = factor(season)
  ) %>%
  drop_na()

threshold <- quantile(model_df$global_active_power, 0.75, na.rm = TRUE)
model_df <- model_df %>%
  mutate(
    high_usage = if_else(global_active_power >= threshold, "High", "Low"),
    high_usage = factor(high_usage, levels = c("Low", "High"))
  )

x_mat <- model.matrix(
  high_usage ~ global_reactive_power + voltage + global_intensity +
    sub_metering_1 + sub_metering_2 + sub_metering_3 + hour + dow_type + season,
  data = model_df
)[, -1, drop = FALSE]

feat_names <- colnames(x_mat)

xgb_imp <- xgboost::xgb.importance(feature_names = feat_names, model = xgb_fit)
xgb_imp_df <- as.data.frame(xgb_imp)

write_csv(xgb_imp_df, file.path("output/tables", "importance_xgboost.csv"))

# 4) Simple consensus (top 10 from each)
top_lm <- lm_imp %>%
  filter(feature != "(Intercept)") %>%
  slice_head(n = 10) %>%
  transmute(feature)

top_rf <- rf_imp_df %>%
  slice_head(n = 10) %>%
  transmute(feature)

top_xgb <- xgb_imp_df %>%
  slice_head(n = 10) %>%
  transmute(feature = Feature)

consensus <- bind_rows(
  top_lm  %>% mutate(source = "LinearRegression"),
  top_rf  %>% mutate(source = "RandomForest"),
  top_xgb %>% mutate(source = "XGBoost")
) %>%
  count(feature, name = "appearances") %>%
  arrange(desc(appearances), feature)

write_csv(consensus, file.path("output/tables", "importance_consensus_top10.csv"))

cat("Variable importance tables saved.\n")
