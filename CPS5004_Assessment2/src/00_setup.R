suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(forecast)
  library(caret)
  library(randomForest)
  library(xgboost)
  library(car)
  library(imputeTS)
  library(ggplot2)
})

dir.create("output", showWarnings = FALSE)
dir.create("output/plots", showWarnings = FALSE, recursive = TRUE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("output/models", showWarnings = FALSE, recursive = TRUE)

source("src/utils.R")

set.seed(42)
