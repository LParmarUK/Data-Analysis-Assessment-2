source("src/00_setup.R")

# 1) Load + clean + aggregate
source("src/01_load_clean_aggregate.R")

# 2) EDA (stats + plots)
source("src/02_eda.R")

# 3) Regression + ANOVA
source("src/03_regression_anova.R")

# 4) ARIMA forecasting
source("src/04_arima_forecast.R")

# 5) ML models + classification metrics (Accuracy/Precision/Recall/ROC/AUC)
source("src/05_ml_models.R")

# 6) Variable importance / impact summary
source("src/06_variable_importance.R")

cat("\nDONE. Check output/plots and output/tables.\n")
