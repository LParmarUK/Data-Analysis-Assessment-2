# Helper: ROC + AUC
roc_auc_manual <- function(y_true_factor, prob_high, positive = "High") {
  y <- ifelse(y_true_factor == positive, 1, 0)
  ord <- order(prob_high, decreasing = TRUE)
  y <- y[ord]
  p <- prob_high[ord]
  
  th <- unique(p)
  th <- c(Inf, th, -Inf)
  
  P <- sum(y == 1); N <- sum(y == 0)
  tpr <- numeric(length(th))
  fpr <- numeric(length(th))
  
  for (i in seq_along(th)) {
    yhat <- ifelse(p >= th[i], 1, 0)
    TP <- sum(yhat == 1 & y == 1)
    FP <- sum(yhat == 1 & y == 0)
    FN <- sum(yhat == 0 & y == 1)
    TN <- sum(yhat == 0 & y == 0)
    
    tpr[i] <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
    fpr[i] <- ifelse((FP + TN) == 0, 0, FP / (FP + TN))
  }
  
  o2 <- order(fpr, tpr)
  fpr2 <- fpr[o2]
  tpr2 <- tpr[o2]
  
  #AUC
  auc <- sum((fpr2[-1] - fpr2[-length(fpr2)]) * (tpr2[-1] + tpr2[-length(tpr2)]) / 2, na.rm = TRUE)
  
  list(
    auc = auc,
    roc_df = tibble(fpr = fpr2, tpr = tpr2)
  )
}

# Build modelling dataset
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

# Binary target for classification metrics (High vs Low usage)
threshold <- quantile(model_df$global_active_power, 0.75, na.rm = TRUE)
model_df <- model_df %>%
  mutate(
    high_usage = if_else(global_active_power >= threshold, "High", "Low"),
    high_usage = factor(high_usage, levels = c("Low", "High"))
  )

cat("\n"); print(table(model_df$high_usage)); cat("\n")

# Stratified split (stable for classification)
set.seed(42)
idx <- caret::createDataPartition(model_df$high_usage, p = 0.8, list = FALSE)
train_df <- model_df[idx, ]
test_df  <- model_df[-idx, ]

# 1) Logistic Regression (base glm)
glm_fit <- glm(
  high_usage ~ global_reactive_power + voltage + global_intensity +
    sub_metering_1 + sub_metering_2 + sub_metering_3 + hour + dow_type + season,
  data = train_df,
  family = binomial()
)

glm_prob <- predict(glm_fit, newdata = test_df, type = "response")
glm_pred <- factor(if_else(glm_prob >= 0.5, "High", "Low"), levels = c("Low", "High"))

glm_cm <- caret::confusionMatrix(glm_pred, test_df$high_usage, positive = "High")
glm_roc <- roc_auc_manual(test_df$high_usage, glm_prob, positive = "High")

# 2) Random Forest (randomForest package)
rf_fit <- randomForest::randomForest(
  high_usage ~ global_reactive_power + voltage + global_intensity +
    sub_metering_1 + sub_metering_2 + sub_metering_3 + hour + dow_type + season,
  data = train_df,
  ntree = 300
)

rf_prob <- predict(rf_fit, newdata = test_df, type = "prob")[, "High"]
rf_pred <- factor(if_else(rf_prob >= 0.5, "High", "Low"), levels = c("Low", "High"))

rf_cm <- caret::confusionMatrix(rf_pred, test_df$high_usage, positive = "High")
rf_roc <- roc_auc_manual(test_df$high_usage, rf_prob, positive = "High")

# 3) Gradient Boosting (xgboost)
# Create design matrices (one-hot encode factors) using model.matrix
x_train <- model.matrix(
  high_usage ~ global_reactive_power + voltage + global_intensity +
    sub_metering_1 + sub_metering_2 + sub_metering_3 + hour + dow_type + season,
  data = train_df
)[, -1, drop = FALSE]  # drop intercept

x_test <- model.matrix(
  high_usage ~ global_reactive_power + voltage + global_intensity +
    sub_metering_1 + sub_metering_2 + sub_metering_3 + hour + dow_type + season,
  data = test_df
)[, -1, drop = FALSE]

# Convert to plain matrix (avoid ALTREP issues)
x_train <- as.matrix(x_train)
x_test  <- as.matrix(x_test)

y_train <- ifelse(train_df$high_usage == "High", 1, 0)

dtrain <- xgboost::xgb.DMatrix(data = x_train, label = y_train)

params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 3,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

set.seed(42)
xgb_fit <- xgboost::xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,
  verbose = 0
)

xgb_prob <- predict(xgb_fit, x_test)
xgb_pred <- factor(if_else(xgb_prob >= 0.5, "High", "Low"), levels = c("Low", "High"))

xgb_cm <- caret::confusionMatrix(xgb_pred, test_df$high_usage, positive = "High")
xgb_roc <- roc_auc_manual(test_df$high_usage, xgb_prob, positive = "High")

# Collect metrics
get_metrics <- function(cm, auc_val, name) {
  tibble(
    model = name,
    accuracy = as.numeric(cm$overall["Accuracy"]),
    precision = as.numeric(cm$byClass["Precision"]),
    recall = as.numeric(cm$byClass["Recall"]),
    auc = auc_val
  )
}

ml_metrics <- bind_rows(
  get_metrics(glm_cm, glm_roc$auc, "Logistic Regression"),
  get_metrics(rf_cm,  rf_roc$auc,  "Random Forest"),
  get_metrics(xgb_cm, xgb_roc$auc, "Gradient Boosting (xgboost)")
)

write_csv(ml_metrics, file.path("output/tables", "ml_classification_metrics.csv"))

# ROC plot
roc_all <- bind_rows(
  glm_roc$roc_df %>% mutate(model = "Logistic"),
  rf_roc$roc_df  %>% mutate(model = "RandomForest"),
  xgb_roc$roc_df %>% mutate(model = "XGBoost")
)

p <- ggplot(roc_all, aes(x = fpr, y = tpr, group = model)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  labs(title = "ROC Curves (Test Set)", x = "False Positive Rate", y = "True Positive Rate")

save_plot(p, "ml_roc_curves.png")

# Save models
saveRDS(glm_fit, file.path("output/models", "glm_cls.rds"))
saveRDS(rf_fit,  file.path("output/models", "rf_cls.rds"))
saveRDS(xgb_fit, file.path("output/models", "xgb_cls.rds"))

cat("ML models complete: classification metrics + ROC saved.\n")
