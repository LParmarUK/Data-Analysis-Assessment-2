broom_like_anova <- function(aov_obj) {
  a <- summary(aov_obj)[[1]]
  out <- as.data.frame(a)
  out$term <- rownames(out)
  rownames(out) <- NULL
  out %>%
    select(term, Df, `Sum Sq`, `Mean Sq`, `F value`, `Pr(>F)`) %>%
    rename(
      df = Df,
      sum_sq = `Sum Sq`,
      mean_sq = `Mean Sq`,
      f_value = `F value`,
      p_value = `Pr(>F)`
    )
}

# Linear regression (regression interpretation)
lm_fit <- lm(
  global_active_power ~ global_reactive_power + voltage + global_intensity +
    sub_metering_1 + sub_metering_2 + sub_metering_3 + hour + dow_type + season,
  data = hourly
)

lm_sum <- summary(lm_fit)

# Save regression summary (as a tidy-ish table)
lm_coeffs <- as.data.frame(lm_sum$coefficients) %>%
  rownames_to_column("term") %>%
  rename(estimate = Estimate, std_error = `Std. Error`, t_value = `t value`, p_value = `Pr(>|t|)`)

write_csv(lm_coeffs, file.path("output/tables", "linear_regression_coefficients.csv"))

# ANOVA: weekday vs weekend
anova_dow <- aov(global_active_power ~ dow_type, data = hourly)
anova_dow_tbl <- broom_like_anova(anova_dow)
write_csv(anova_dow_tbl, file.path("output/tables", "anova_weekday_weekend.csv"))

# ANOVA: seasons
anova_season <- aov(global_active_power ~ season, data = hourly)
anova_season_tbl <- broom_like_anova(anova_season)
write_csv(anova_season_tbl, file.path("output/tables", "anova_season.csv"))

# Use car::Anova for type-II tests in the full regression model
type2 <- car::Anova(lm_fit, type = 2)
type2_tbl <- as.data.frame(type2) %>%
  rownames_to_column("term") %>%
  rename(sum_sq = `Sum Sq`, df = Df, f_value = `F value`, p_value = `Pr(>F)`)

write_csv(type2_tbl, file.path("output/tables", "car_anova_type2_full_model.csv"))

saveRDS(lm_fit, file.path("output/models", "lm_fit.rds"))

cat("Regression + ANOVA complete.\n")


broom_like_anova <- function(aov_obj) {
  a <- summary(aov_obj)[[1]]
  out <- as.data.frame(a)
  out$term <- rownames(out)
  rownames(out) <- NULL
  out %>%
    select(term, Df, `Sum Sq`, `Mean Sq`, `F value`, `Pr(>F)`) %>%
    rename(df = Df, sum_sq = `Sum Sq`, mean_sq = `Mean Sq`, f_value = `F value`, p_value = `Pr(>F)`)
}
