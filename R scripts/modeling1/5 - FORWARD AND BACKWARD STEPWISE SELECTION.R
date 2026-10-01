# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# TASK 5 - PREDICTIVE STATISTICAL MODELLING
# STEP 5 - FORWARD AND BACKWARD STEPWISE SELECTION
# ============================================================

library(tidyverse)

# ============================================================
# 1. Create output folders if they do not already exist
# ============================================================

if (!dir.exists("output")) {
  dir.create("output")
}

if (!dir.exists("plots")) {
  dir.create("plots")
}

# ============================================================
# 2. Check that required objects from previous steps exist
# ============================================================

required_objects <- c(
  "train_data",
  "test_data",
  "baseline_mlr"
)

missing_objects <- required_objects[
  !sapply(
    required_objects,
    exists
  )
]

if (length(missing_objects) > 0) {
  
  stop(
    paste(
      "The following required objects are missing:",
      paste(
        missing_objects,
        collapse = ", "
      )
    )
  )
}

# ============================================================
# 3. Candidate models
# ============================================================
#
# Full candidate model:
#
# daily_demand ~
#   day_of_week +
#   month +
#   prev_day_demand +
#   prev_week_demand +
#   rolling_7_day_avg_demand
#
# These are the predictors used in the baseline MLR.
#
# ============================================================

full_model <- lm(
  daily_demand ~
    day_of_week +
    month +
    prev_day_demand +
    prev_week_demand +
    rolling_7_day_avg_demand,
  data = train_data
)

# Null model contains only the intercept

null_model <- lm(
  daily_demand ~ 1,
  data = train_data
)

# ============================================================
# 4. Forward stepwise selection using AIC
# ============================================================

cat("============================================================\n")
cat("FORWARD STEPWISE SELECTION\n")
cat("============================================================\n\n")

forward_mlr <- step(
  
  null_model,
  
  scope = list(
    lower = formula(null_model),
    upper = formula(full_model)
  ),
  
  direction = "forward",
  
  trace = TRUE
)

# ============================================================
# 5. Display forward-selected model
# ============================================================

cat("\n============================================================\n")
cat("FORWARD SELECTED MODEL\n")
cat("============================================================\n\n")

cat("Selected formula:\n\n")

print(
  formula(
    forward_mlr
  )
)

cat("\nAIC:\n")

print(
  AIC(
    forward_mlr
  )
)

cat("\nModel summary:\n\n")

print(
  summary(
    forward_mlr
  )
)

# ============================================================
# 6. Backward stepwise selection using AIC
# ============================================================

cat("\n============================================================\n")
cat("BACKWARD STEPWISE SELECTION\n")
cat("============================================================\n\n")

backward_mlr <- step(
  
  full_model,
  
  direction = "backward",
  
  trace = TRUE
)

# ============================================================
# 7. Display backward-selected model
# ============================================================

cat("\n============================================================\n")
cat("BACKWARD SELECTED MODEL\n")
cat("============================================================\n\n")

cat("Selected formula:\n\n")

print(
  formula(
    backward_mlr
  )
)

cat("\nAIC:\n")

print(
  AIC(
    backward_mlr
  )
)

cat("\nModel summary:\n\n")

print(
  summary(
    backward_mlr
  )
)

# ============================================================
# 8. Compare models using AIC
# ============================================================

aic_comparison <- tibble(
  
  Model = c(
    "Baseline MLR",
    "Forward Selection",
    "Backward Selection"
  ),
  
  AIC = c(
    AIC(
      baseline_mlr
    ),
    AIC(
      forward_mlr
    ),
    AIC(
      backward_mlr
    )
  ),
  
  Number_of_Coefficients = c(
    length(
      coef(
        baseline_mlr
      )
    ),
    length(
      coef(
        forward_mlr
      )
    ),
    length(
      coef(
        backward_mlr
      )
    )
  )
)

cat("\n============================================================\n")
cat("AIC MODEL COMPARISON\n")
cat("============================================================\n\n")

print(
  aic_comparison
)

# ============================================================
# 9. Function for prediction performance
# ============================================================

calculate_metrics <- function(
    model,
    data,
    model_name
) {
  
  predictions <- predict(
    model,
    newdata = data
  )
  
  actual <- data$daily_demand
  
  # RMSE
  rmse <- sqrt(
    mean(
      (actual - predictions)^2
    )
  )
  
  # MAE
  mae <- mean(
    abs(
      actual - predictions
    )
  )
  
  # R-squared
  r_squared <- 1 -
    sum(
      (actual - predictions)^2
    ) /
    sum(
      (actual - mean(actual))^2
    )
  
  tibble(
    Model = model_name,
    RMSE = rmse,
    MAE = mae,
    R_squared = r_squared
  )
}

# ============================================================
# 10. Training performance comparison
# ============================================================

training_model_comparison <- bind_rows(
  
  calculate_metrics(
    baseline_mlr,
    train_data,
    "Baseline MLR"
  ),
  
  calculate_metrics(
    forward_mlr,
    train_data,
    "Forward Selection"
  ),
  
  calculate_metrics(
    backward_mlr,
    train_data,
    "Backward Selection"
  )
)

cat("\n============================================================\n")
cat("TRAINING PERFORMANCE COMPARISON\n")
cat("============================================================\n\n")

print(
  training_model_comparison %>%
    mutate(
      RMSE = round(
        RMSE,
        2
      ),
      MAE = round(
        MAE,
        2
      ),
      R_squared = round(
        R_squared,
        4
      )
    )
)

# ============================================================
# 11. Test performance comparison
# ============================================================

test_model_comparison <- bind_rows(
  
  calculate_metrics(
    baseline_mlr,
    test_data,
    "Baseline MLR"
  ),
  
  calculate_metrics(
    forward_mlr,
    test_data,
    "Forward Selection"
  ),
  
  calculate_metrics(
    backward_mlr,
    test_data,
    "Backward Selection"
  )
)

# Add number of coefficients

test_model_comparison <- test_model_comparison %>%
  mutate(
    Number_of_Coefficients = c(
      
      length(
        coef(
          baseline_mlr
        )
      ),
      
      length(
        coef(
          forward_mlr
        )
      ),
      
      length(
        coef(
          backward_mlr
        )
      )
    )
  )

cat("\n============================================================\n")
cat("TEST PERFORMANCE COMPARISON\n")
cat("============================================================\n\n")

print(
  test_model_comparison %>%
    mutate(
      RMSE = round(
        RMSE,
        2
      ),
      MAE = round(
        MAE,
        2
      ),
      R_squared = round(
        R_squared,
        4
      )
    )
)

# ============================================================
# 12. Save AIC comparison
# ============================================================

write_csv(
  aic_comparison,
  "output/task5_stepwise_aic_comparison.csv"
)

# ============================================================
# 13. Save training performance
# ============================================================

write_csv(
  training_model_comparison,
  "output/task5_stepwise_training_performance.csv"
)

# ============================================================
# 14. Save test performance
# ============================================================

write_csv(
  test_model_comparison,
  "output/task5_stepwise_test_performance.csv"
)

# ============================================================
# 15. Save forward model coefficients
# ============================================================

forward_coefficients <- as.data.frame(
  summary(
    forward_mlr
  )$coefficients
) %>%
  rownames_to_column(
    "Term"
  )

write_csv(
  forward_coefficients,
  "output/task5_forward_mlr_coefficients.csv"
)

# ============================================================
# 16. Save backward model coefficients
# ============================================================

backward_coefficients <- as.data.frame(
  summary(
    backward_mlr
  )$coefficients
) %>%
  rownames_to_column(
    "Term"
  )

write_csv(
  backward_coefficients,
  "output/task5_backward_mlr_coefficients.csv"
)

# ============================================================
# 17. Save selected model formulas
# ============================================================
#
# paste(..., collapse = " ")
# is used because deparse() can return multiple strings
# for a formula spread over several lines.
#
# ============================================================

selected_formulas <- tibble(
  
  Model = c(
    "Baseline MLR",
    "Forward Selection",
    "Backward Selection"
  ),
  
  Formula = c(
    
    paste(
      deparse(
        formula(
          baseline_mlr
        )
      ),
      collapse = " "
    ),
    
    paste(
      deparse(
        formula(
          forward_mlr
        )
      ),
      collapse = " "
    ),
    
    paste(
      deparse(
        formula(
          backward_mlr
        )
      ),
      collapse = " "
    )
  )
)

cat("\n============================================================\n")
cat("SELECTED MODEL FORMULAS\n")
cat("============================================================\n\n")

print(
  selected_formulas
)

write_csv(
  selected_formulas,
  "output/task5_stepwise_selected_formulas.csv"
)

# ============================================================
# 18. Save forward model
# ============================================================

saveRDS(
  forward_mlr,
  "output/task5_forward_mlr.rds"
)

# ============================================================
# 19. Save backward model
# ============================================================

saveRDS(
  backward_mlr,
  "output/task5_backward_mlr.rds"
)

# ============================================================
# 20. Create test-set predictions for all models
# ============================================================

test_predictions_stepwise <- test_data %>%
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
    
    baseline_prediction =
      predict(
        baseline_mlr,
        newdata = test_data
      ),
    
    forward_prediction =
      predict(
        forward_mlr,
        newdata = test_data
      ),
    
    backward_prediction =
      predict(
        backward_mlr,
        newdata = test_data
      )
  )

# ============================================================
# 21. Calculate prediction errors
# ============================================================

test_predictions_stepwise <- test_predictions_stepwise %>%
  mutate(
    
    baseline_error =
      daily_demand -
      baseline_prediction,
    
    forward_error =
      daily_demand -
      forward_prediction,
    
    backward_error =
      daily_demand -
      backward_prediction
  )

# ============================================================
# 22. Save test predictions
# ============================================================

write_csv(
  test_predictions_stepwise,
  "output/task5_stepwise_test_predictions.csv"
)

# ============================================================
# 23. Create prediction comparison plot
# ============================================================

plot_stepwise <- ggplot(
  test_predictions_stepwise,
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
      y = baseline_prediction,
      linetype = "Baseline MLR"
    )
  ) +
  
  geom_line(
    aes(
      y = forward_prediction,
      linetype = "Forward Selection"
    )
  ) +
  
  geom_line(
    aes(
      y = backward_prediction,
      linetype = "Backward Selection"
    )
  ) +
  
  labs(
    title = "Baseline and Stepwise Models: Test-Set Predictions",
    x = "Date",
    y = "Daily Demand",
    linetype = "Series"
  ) +
  
  theme_minimal()

print(
  plot_stepwise
)

# ============================================================
# 24. Save prediction plot
# ============================================================

ggsave(
  "plots/26_task5_stepwise_model_predictions.png",
  plot_stepwise,
  width = 12,
  height = 6,
  dpi = 300
)

# ============================================================
# 25. Print final saved-file information
# ============================================================

cat("\n============================================================\n")
cat("TASK 5 - STEP 5 COMPLETED\n")
cat("============================================================\n\n")

cat(
  "AIC comparison saved to:\n",
  "output/task5_stepwise_aic_comparison.csv\n\n"
)

cat(
  "Training performance saved to:\n",
  "output/task5_stepwise_training_performance.csv\n\n"
)

cat(
  "Test performance saved to:\n",
  "output/task5_stepwise_test_performance.csv\n\n"
)

cat(
  "Selected formulas saved to:\n",
  "output/task5_stepwise_selected_formulas.csv\n\n"
)

cat(
  "Forward coefficients saved to:\n",
  "output/task5_forward_mlr_coefficients.csv\n\n"
)

cat(
  "Backward coefficients saved to:\n",
  "output/task5_backward_mlr_coefficients.csv\n\n"
)

cat(
  "Forward model saved to:\n",
  "output/task5_forward_mlr.rds\n\n"
)

cat(
  "Backward model saved to:\n",
  "output/task5_backward_mlr.rds\n\n"
)

cat(
  "Test predictions saved to:\n",
  "output/task5_stepwise_test_predictions.csv\n\n"
)

cat(
  "Prediction plot saved to:\n",
  "plots/26_task5_stepwise_model_predictions.png\n"
)