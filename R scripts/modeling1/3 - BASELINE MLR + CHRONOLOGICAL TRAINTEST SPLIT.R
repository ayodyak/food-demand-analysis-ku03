# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# TASK 5 - PREDICTIVE STATISTICAL MODELLING
# STEP 3 - BASELINE MLR + CHRONOLOGICAL TRAIN/TEST SPLIT
# ============================================================

library(tidyverse)

# ============================================================
# 1. Create required folders
# ============================================================

if (!dir.exists("output")) {
  dir.create("output")
}

if (!dir.exists("plots")) {
  dir.create("plots")
}

# ============================================================
# 2. Use prepared modelling data
# ============================================================

analysis_data <- model_data %>%
  arrange(date)

# ============================================================
# 3. Correct categorical variable representation
# ============================================================
#
# Month is treated as an ordinary categorical variable.
# January is the reference category.
#
# This avoids polynomial contrasts such as:
# month.L, month.Q, month.C, etc.
#
# ============================================================

analysis_data <- analysis_data %>%
  mutate(
    
    day_of_week = factor(
      day_of_week,
      levels = c(
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday"
      )
    ),
    
    month = factor(
      month,
      levels = c(
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December"
      ),
      ordered = FALSE
    )
  )

# ============================================================
# 4. Chronological 80/20 train-test split
# ============================================================
#
# Earlier observations = training data
# Later observations   = test data
#
# This avoids using future observations to train the model.
#
# ============================================================

n_total <- nrow(analysis_data)

train_size <- floor(
  0.80 * n_total
)

train_data <- analysis_data %>%
  slice(
    1:train_size
  )

test_data <- analysis_data %>%
  slice(
    (train_size + 1):n_total
  )

# ============================================================
# 5. Display split information
# ============================================================

cat("============================================================\n")
cat("CHRONOLOGICAL TRAIN / TEST SPLIT\n")
cat("============================================================\n\n")

cat(
  "Total observations:",
  n_total,
  "\n"
)

cat(
  "Training observations:",
  nrow(train_data),
  "\n"
)

cat(
  "Test observations:",
  nrow(test_data),
  "\n"
)

cat(
  "Training proportion:",
  round(
    nrow(train_data) / n_total * 100,
    2
  ),
  "%\n"
)

cat(
  "Test proportion:",
  round(
    nrow(test_data) / n_total * 100,
    2
  ),
  "%\n\n"
)

cat(
  "Training period:",
  as.character(min(train_data$date)),
  "to",
  as.character(max(train_data$date)),
  "\n"
)

cat(
  "Test period:",
  as.character(min(test_data$date)),
  "to",
  as.character(max(test_data$date)),
  "\n"
)

# ============================================================
# 6. Baseline Multiple Linear Regression
# ============================================================
#
# Response:
#   daily_demand
#
# Predictors:
#   day_of_week
#   month
#   prev_day_demand
#   prev_week_demand
#   rolling_7_day_avg_demand
#
# ============================================================

baseline_mlr <- lm(
  daily_demand ~
    day_of_week +
    month +
    prev_day_demand +
    prev_week_demand +
    rolling_7_day_avg_demand,
  data = train_data
)

# ============================================================
# 7. Display baseline model summary
# ============================================================

cat("\n============================================================\n")
cat("BASELINE MULTIPLE LINEAR REGRESSION\n")
cat("============================================================\n\n")

print(
  summary(
    baseline_mlr
  )
)

# ============================================================
# 8. Extract model statistics
# ============================================================

baseline_summary <- summary(
  baseline_mlr
)

baseline_r_squared <- baseline_summary$r.squared

baseline_adjusted_r_squared <-
  baseline_summary$adj.r.squared

baseline_residual_se <-
  baseline_summary$sigma

baseline_f_statistic <-
  baseline_summary$fstatistic[1]

baseline_df1 <-
  baseline_summary$fstatistic[2]

baseline_df2 <-
  baseline_summary$fstatistic[3]

baseline_model_p <- pf(
  baseline_f_statistic,
  baseline_df1,
  baseline_df2,
  lower.tail = FALSE
)

cat("\n============================================================\n")
cat("BASELINE MODEL SUMMARY\n")
cat("============================================================\n\n")

cat(
  "R-squared:",
  round(
    baseline_r_squared,
    4
  ),
  "\n"
)

cat(
  "Adjusted R-squared:",
  round(
    baseline_adjusted_r_squared,
    4
  ),
  "\n"
)

cat(
  "Residual standard error:",
  round(
    baseline_residual_se,
    4
  ),
  "\n"
)

cat(
  "F-statistic:",
  round(
    baseline_f_statistic,
    4
  ),
  "\n"
)

cat(
  "Model p-value:",
  format.pval(
    baseline_model_p,
    digits = 5
  ),
  "\n"
)

# ============================================================
# 9. Generate training predictions
# ============================================================

train_predictions <- predict(
  baseline_mlr,
  newdata = train_data
)

# ============================================================
# 10. Generate test predictions
# ============================================================

test_predictions <- predict(
  baseline_mlr,
  newdata = test_data
)

# ============================================================
# 11. Define performance functions
# ============================================================

RMSE <- function(
    actual,
    predicted
) {
  
  sqrt(
    mean(
      (actual - predicted)^2
    )
  )
}

MAE <- function(
    actual,
    predicted
) {
  
  mean(
    abs(
      actual - predicted
    )
  )
}

R_squared <- function(
    actual,
    predicted
) {
  
  1 -
    sum(
      (actual - predicted)^2
    ) /
    sum(
      (actual - mean(actual))^2
    )
}

# ============================================================
# 12. Training performance
# ============================================================

train_rmse <- RMSE(
  train_data$daily_demand,
  train_predictions
)

train_mae <- MAE(
  train_data$daily_demand,
  train_predictions
)

train_r_squared <- R_squared(
  train_data$daily_demand,
  train_predictions
)

# ============================================================
# 13. Test performance
# ============================================================

test_rmse <- RMSE(
  test_data$daily_demand,
  test_predictions
)

test_mae <- MAE(
  test_data$daily_demand,
  test_predictions
)

test_r_squared <- R_squared(
  test_data$daily_demand,
  test_predictions
)

# ============================================================
# 14. Display predictive performance
# ============================================================

cat("\n============================================================\n")
cat("BASELINE MODEL PREDICTIVE PERFORMANCE\n")
cat("============================================================\n\n")

cat("TRAINING DATA\n")

cat(
  "RMSE:",
  round(
    train_rmse,
    2
  ),
  "\n"
)

cat(
  "MAE:",
  round(
    train_mae,
    2
  ),
  "\n"
)

cat(
  "R-squared:",
  round(
    train_r_squared,
    4
  ),
  "\n\n"
)

cat("TEST DATA - UNSEEN OBSERVATIONS\n")

cat(
  "RMSE:",
  round(
    test_rmse,
    2
  ),
  "\n"
)

cat(
  "MAE:",
  round(
    test_mae,
    2
  ),
  "\n"
)

cat(
  "R-squared:",
  round(
    test_r_squared,
    4
  ),
  "\n"
)

# ============================================================
# 15. Create prediction comparison dataset
# ============================================================

test_predictions_data <- test_data %>%
  select(
    date,
    daily_demand,
    day_of_week,
    month,
    prev_day_demand,
    prev_week_demand,
    rolling_7_day_avg_demand
  ) %>%
  mutate(
    predicted_demand = test_predictions,
    prediction_error =
      daily_demand -
      predicted_demand,
    absolute_error =
      abs(prediction_error)
  )

# ============================================================
# 16. Display prediction examples
# ============================================================

cat("\n============================================================\n")
cat("TEST-SET PREDICTION EXAMPLES\n")
cat("============================================================\n\n")

print(
  test_predictions_data %>%
    select(
      date,
      daily_demand,
      predicted_demand,
      prediction_error,
      absolute_error
    ) %>%
    slice_head(n = 15)
)

# ============================================================
# 17. Save training/test datasets
# ============================================================

write_csv(
  train_data,
  "output/task5_training_data.csv"
)

write_csv(
  test_data,
  "output/task5_test_data.csv"
)

# ============================================================
# 18. Save test predictions
# ============================================================

write_csv(
  test_predictions_data,
  "output/task5_baseline_mlr_test_predictions.csv"
)

# ============================================================
# 19. Save model coefficients
# ============================================================

coefficient_table <- as.data.frame(
  summary(
    baseline_mlr
  )$coefficients
) %>%
  rownames_to_column(
    "Term"
  )

write_csv(
  coefficient_table,
  "output/task5_baseline_mlr_coefficients.csv"
)

# ============================================================
# 20. Save performance table
# ============================================================

baseline_performance <- tibble(
  
  Dataset = c(
    "Training",
    "Test"
  ),
  
  RMSE = c(
    train_rmse,
    test_rmse
  ),
  
  MAE = c(
    train_mae,
    test_mae
  ),
  
  R_squared = c(
    train_r_squared,
    test_r_squared
  )
)

write_csv(
  baseline_performance,
  "output/task5_baseline_mlr_performance.csv"
)

# ============================================================
# 21. Actual vs predicted plot
# ============================================================

p_actual_predicted <- ggplot(
  test_predictions_data,
  aes(
    x = daily_demand,
    y = predicted_demand
  )
) +
  geom_point(
    alpha = 0.6
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Baseline MLR: Actual vs Predicted Daily Demand",
    x = "Actual Daily Demand (Food Units)",
    y = "Predicted Daily Demand (Food Units)"
  ) +
  theme_minimal()

print(
  p_actual_predicted
)

ggsave(
  "plots/20_task5_baseline_actual_vs_predicted.png",
  p_actual_predicted,
  width = 10,
  height = 7,
  dpi = 300
)

# ============================================================
# 22. Test-set prediction time series
# ============================================================

p_prediction_time <- ggplot(
  test_predictions_data,
  aes(
    x = date
  )
) +
  geom_line(
    aes(
      y = daily_demand,
      linetype = "Actual"
    )
  ) +
  geom_line(
    aes(
      y = predicted_demand,
      linetype = "Predicted"
    )
  ) +
  labs(
    title = "Baseline MLR: Actual and Predicted Daily Demand",
    x = "Date",
    y = "Food Units",
    linetype = "Series"
  ) +
  theme_minimal()

print(
  p_prediction_time
)

ggsave(
  "plots/21_task5_baseline_prediction_time_series.png",
  p_prediction_time,
  width = 12,
  height = 6,
  dpi = 300
)

# ============================================================
# 23. Residual data for later diagnostics
# ============================================================

train_residual_data <- train_data %>%
  mutate(
    fitted_demand = train_predictions,
    residual = daily_demand - fitted_demand
  )

test_residual_data <- test_data %>%
  mutate(
    predicted_demand = test_predictions,
    residual = daily_demand - predicted_demand
  )

write_csv(
  train_residual_data,
  "output/task5_baseline_mlr_training_residuals.csv"
)

write_csv(
  test_residual_data,
  "output/task5_baseline_mlr_test_residuals.csv"
)

# ============================================================
# 24. Save baseline model
# ============================================================

saveRDS(
  baseline_mlr,
  "output/task5_baseline_mlr.rds"
)

# ============================================================
# 25. Final message
# ============================================================

cat("\n============================================================\n")
cat("TASK 5 - STEP 3 COMPLETED\n")
cat("============================================================\n\n")

cat(
  "Baseline MLR saved to:\n",
  "output/task5_baseline_mlr.rds\n\n"
)

cat(
  "Training data saved to:\n",
  "output/task5_training_data.csv\n\n"
)

cat(
  "Test data saved to:\n",
  "output/task5_test_data.csv\n\n"
)

cat(
  "Performance results saved to:\n",
  "output/task5_baseline_mlr_performance.csv\n\n"
)

cat(
  "Test predictions saved to:\n",
  "output/task5_baseline_mlr_test_predictions.csv\n\n"
)

cat(
  "Plots saved to:\n",
  "plots/20_task5_baseline_actual_vs_predicted.png\n"
)

cat(
  "plots/21_task5_baseline_prediction_time_series.png\n"
)

cat("\nOriginal modelling dataset was NOT modified.\n")