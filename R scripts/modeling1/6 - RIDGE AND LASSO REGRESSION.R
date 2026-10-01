# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# TASK 5 - PREDICTIVE STATISTICAL MODELLING
# STEP 6 - RIDGE AND LASSO REGRESSION
# ============================================================

library(tidyverse)
library(glmnet)

# ============================================================
# 1. Create output folders
# ============================================================

if (!dir.exists("output")) {
  dir.create("output")
}

if (!dir.exists("plots")) {
  dir.create("plots")
}

# ============================================================
# 2. Check required objects
# ============================================================

required_objects <- c(
  "train_data",
  "test_data",
  "baseline_mlr",
  "forward_mlr",
  "backward_mlr"
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
# 3. Define candidate predictor formula
# ============================================================
#
# Same predictors as the baseline MLR:
#
# day_of_week
# month
# prev_day_demand
# prev_week_demand
# rolling_7_day_avg_demand
#
# Ridge and LASSO will shrink the coefficients rather than
# simply removing predictors manually.
#
# ============================================================

ridge_lasso_formula <- daily_demand ~
  day_of_week +
  month +
  prev_day_demand +
  prev_week_demand +
  rolling_7_day_avg_demand

# ============================================================
# 4. Create model matrices
# ============================================================
#
# model.matrix() converts categorical variables into dummy
# variables so that Ridge and LASSO can be fitted.
#
# The same formula is used for training and test data.
#
# ============================================================

x_train <- model.matrix(
  ridge_lasso_formula,
  data = train_data
)[, -1]

x_test <- model.matrix(
  ridge_lasso_formula,
  data = test_data
)[, -1]

y_train <- train_data$daily_demand
y_test <- test_data$daily_demand

# ============================================================
# 5. Check dimensions
# ============================================================

cat("============================================================\n")
cat("RIDGE / LASSO MODEL MATRIX\n")
cat("============================================================\n\n")

cat(
  "Training observations:",
  nrow(x_train),
  "\n"
)

cat(
  "Testing observations:",
  nrow(x_test),
  "\n"
)

cat(
  "Number of predictor columns:",
  ncol(x_train),
  "\n\n"
)

# ============================================================
# 6. Set seed for reproducibility
# ============================================================

set.seed(3081)

# ============================================================
# 7. Ridge regression
# ============================================================
#
# alpha = 0  --> Ridge
#
# cv.glmnet selects the tuning parameter lambda using
# cross-validation on the training data.
#
# standardize = TRUE scales predictors before fitting.
#
# ============================================================

cat("\n============================================================\n")
cat("RIDGE REGRESSION\n")
cat("============================================================\n\n")

ridge_cv <- cv.glmnet(
  
  x = x_train,
  y = y_train,
  
  alpha = 0,
  
  nfolds = 10,
  
  standardize = TRUE,
  
  type.measure = "mse"
)

ridge_lambda_min <- ridge_cv$lambda.min

ridge_lambda_1se <- ridge_cv$lambda.1se

cat(
  "Ridge lambda.min:",
  ridge_lambda_min,
  "\n"
)

cat(
  "Ridge lambda.1se:",
  ridge_lambda_1se,
  "\n"
)

# ============================================================
# 8. Final Ridge model using lambda.min
# ============================================================

ridge_model <- glmnet(
  
  x = x_train,
  y = y_train,
  
  alpha = 0,
  
  lambda = ridge_lambda_min,
  
  standardize = TRUE
)

# ============================================================
# 9. Ridge coefficients
# ============================================================

ridge_coef <- coef(
  ridge_model
)

cat("\nRidge coefficients:\n\n")

print(
  ridge_coef
)

# ============================================================
# 10. LASSO regression
# ============================================================
#
# alpha = 1 --> LASSO
#
# LASSO can shrink some coefficients exactly to zero,
# thereby performing variable selection.
#
# ============================================================

cat("\n============================================================\n")
cat("LASSO REGRESSION\n")
cat("============================================================\n\n")

lasso_cv <- cv.glmnet(
  
  x = x_train,
  y = y_train,
  
  alpha = 1,
  
  nfolds = 10,
  
  standardize = TRUE,
  
  type.measure = "mse"
)

lasso_lambda_min <- lasso_cv$lambda.min

lasso_lambda_1se <- lasso_cv$lambda.1se

cat(
  "LASSO lambda.min:",
  lasso_lambda_min,
  "\n"
)

cat(
  "LASSO lambda.1se:",
  lasso_lambda_1se,
  "\n"
)

# ============================================================
# 11. Final LASSO model using lambda.min
# ============================================================

lasso_model <- glmnet(
  
  x = x_train,
  y = y_train,
  
  alpha = 1,
  
  lambda = lasso_lambda_min,
  
  standardize = TRUE
)

# ============================================================
# 12. LASSO coefficients
# ============================================================

lasso_coef <- coef(
  lasso_model
)

cat("\nLASSO coefficients:\n\n")

print(
  lasso_coef
)

# ============================================================
# 13. Count non-zero LASSO coefficients
# ============================================================

lasso_coef_values <- as.vector(
  lasso_coef
)

lasso_nonzero <- sum(
  lasso_coef_values != 0
)

cat("\n============================================================\n")
cat("LASSO VARIABLE SELECTION\n")
cat("============================================================\n\n")

cat(
  "Total coefficients including intercept:",
  length(lasso_coef_values),
  "\n"
)

cat(
  "Non-zero coefficients:",
  lasso_nonzero,
  "\n"
)

cat(
  "Zero coefficients:",
  length(lasso_coef_values) - lasso_nonzero,
  "\n"
)

# ============================================================
# 14. Predictions
# ============================================================

ridge_train_predictions <- predict(
  ridge_model,
  newx = x_train
)[, 1]

ridge_test_predictions <- predict(
  ridge_model,
  newx = x_test
)[, 1]

lasso_train_predictions <- predict(
  lasso_model,
  newx = x_train
)[, 1]

lasso_test_predictions <- predict(
  lasso_model,
  newx = x_test
)[, 1]

# ============================================================
# 15. Performance function
# ============================================================

calculate_metrics <- function(
    actual,
    predicted,
    model_name
) {
  
  rmse <- sqrt(
    mean(
      (actual - predicted)^2
    )
  )
  
  mae <- mean(
    abs(
      actual - predicted
    )
  )
  
  r_squared <- 1 -
    sum(
      (actual - predicted)^2
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
# 16. Ridge and LASSO training performance
# ============================================================

regularized_training <- bind_rows(
  
  calculate_metrics(
    y_train,
    ridge_train_predictions,
    "Ridge"
  ),
  
  calculate_metrics(
    y_train,
    lasso_train_predictions,
    "LASSO"
  )
)

cat("\n============================================================\n")
cat("RIDGE / LASSO TRAINING PERFORMANCE\n")
cat("============================================================\n\n")

print(
  regularized_training %>%
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
# 17. Ridge and LASSO test performance
# ============================================================

regularized_test <- bind_rows(
  
  calculate_metrics(
    y_test,
    ridge_test_predictions,
    "Ridge"
  ),
  
  calculate_metrics(
    y_test,
    lasso_test_predictions,
    "LASSO"
  )
)

cat("\n============================================================\n")
cat("RIDGE / LASSO TEST PERFORMANCE\n")
cat("============================================================\n\n")

print(
  regularized_test %>%
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
# 18. Create combined model comparison
# ============================================================
#
# Compare all models developed so far:
#
# Baseline MLR
# Forward Selection
# Backward Selection
# Ridge
# LASSO
#
# ============================================================

all_training_models <- bind_rows(
  
  calculate_metrics(
    train_data$daily_demand,
    predict(
      baseline_mlr,
      newdata = train_data
    ),
    "Baseline MLR"
  ),
  
  calculate_metrics(
    train_data$daily_demand,
    predict(
      forward_mlr,
      newdata = train_data
    ),
    "Forward Selection"
  ),
  
  calculate_metrics(
    train_data$daily_demand,
    predict(
      backward_mlr,
      newdata = train_data
    ),
    "Backward Selection"
  ),
  
  regularized_training
)

all_test_models <- bind_rows(
  
  calculate_metrics(
    test_data$daily_demand,
    predict(
      baseline_mlr,
      newdata = test_data
    ),
    "Baseline MLR"
  ),
  
  calculate_metrics(
    test_data$daily_demand,
    predict(
      forward_mlr,
      newdata = test_data
    ),
    "Forward Selection"
  ),
  
  calculate_metrics(
    test_data$daily_demand,
    predict(
      backward_mlr,
      newdata = test_data
    ),
    "Backward Selection"
  ),
  
  regularized_test
)

# ============================================================
# 19. Display complete training comparison
# ============================================================

cat("\n============================================================\n")
cat("ALL MODEL TRAINING PERFORMANCE\n")
cat("============================================================\n\n")

print(
  all_training_models %>%
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
# 20. Display complete test comparison
# ============================================================

cat("\n============================================================\n")
cat("ALL MODEL TEST PERFORMANCE\n")
cat("============================================================\n\n")

print(
  all_test_models %>%
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
# 21. Save model performance
# ============================================================

write_csv(
  all_training_models,
  "output/task5_all_models_training_performance.csv"
)

write_csv(
  all_test_models,
  "output/task5_all_models_test_performance.csv"
)

# ============================================================
# 22. Save lambda values
# ============================================================

lambda_results <- tibble(
  
  Model = c(
    "Ridge",
    "LASSO"
  ),
  
  Lambda_Min = c(
    ridge_lambda_min,
    lasso_lambda_min
  ),
  
  Lambda_1SE = c(
    ridge_lambda_1se,
    lasso_lambda_1se
  )
)

write_csv(
  lambda_results,
  "output/task5_ridge_lasso_lambda_values.csv"
)

# ============================================================
# 23. Save Ridge coefficients
# ============================================================

ridge_coefficients <- as.matrix(
  ridge_coef
) %>%
  as.data.frame() %>%
  rownames_to_column(
    "Term"
  )

names(
  ridge_coefficients
)[2] <- "Coefficient"

write_csv(
  ridge_coefficients,
  "output/task5_ridge_coefficients.csv"
)

# ============================================================
# 24. Save LASSO coefficients
# ============================================================

lasso_coefficients <- as.matrix(
  lasso_coef
) %>%
  as.data.frame() %>%
  rownames_to_column(
    "Term"
  )

names(
  lasso_coefficients
)[2] <- "Coefficient"

write_csv(
  lasso_coefficients,
  "output/task5_lasso_coefficients.csv"
)

# ============================================================
# 25. Save only non-zero LASSO coefficients
# ============================================================

lasso_nonzero_coefficients <- lasso_coefficients %>%
  filter(
    Coefficient != 0
  )

write_csv(
  lasso_nonzero_coefficients,
  "output/task5_lasso_nonzero_coefficients.csv"
)

# ============================================================
# 26. Save test predictions
# ============================================================

regularized_predictions <- test_data %>%
  select(
    date,
    daily_demand
  ) %>%
  mutate(
    
    ridge_prediction =
      ridge_test_predictions,
    
    lasso_prediction =
      lasso_test_predictions,
    
    ridge_error =
      daily_demand -
      ridge_prediction,
    
    lasso_error =
      daily_demand -
      lasso_prediction
  )

write_csv(
  regularized_predictions,
  "output/task5_ridge_lasso_test_predictions.csv"
)

# ============================================================
# 27. Save cross-validation plots
# ============================================================

png(
  "plots/27_task5_ridge_cross_validation.png",
  width = 1400,
  height = 900,
  res = 200
)

plot(
  ridge_cv,
  main = "Ridge Regression Cross-Validation"
)

dev.off()

png(
  "plots/28_task5_lasso_cross_validation.png",
  width = 1400,
  height = 900,
  res = 200
)

plot(
  lasso_cv,
  main = "LASSO Regression Cross-Validation"
)

dev.off()

# ============================================================
# 28. Test prediction comparison plot
# ============================================================

regularized_plot_data <- test_data %>%
  select(
    date,
    daily_demand
  ) %>%
  mutate(
    Ridge = ridge_test_predictions,
    LASSO = lasso_test_predictions
  ) %>%
  pivot_longer(
    cols = c(
      Ridge,
      LASSO
    ),
    names_to = "Model",
    values_to = "Predicted_Demand"
  )

p_regularized <- ggplot(
  regularized_plot_data,
  aes(
    x = date,
    y = Predicted_Demand,
    linetype = Model
  )
) +
  
  geom_line() +
  
  geom_line(
    data = test_data,
    aes(
      x = date,
      y = daily_demand,
      linetype = "Actual"
    )
  ) +
  
  labs(
    title = "Ridge and LASSO: Test-Set Predictions",
    x = "Date",
    y = "Daily Demand",
    linetype = "Series"
  ) +
  
  theme_minimal()

print(
  p_regularized
)

ggsave(
  "plots/29_task5_ridge_lasso_predictions.png",
  p_regularized,
  width = 12,
  height = 6,
  dpi = 300
)

# ============================================================
# 29. Save regularized models
# ============================================================

saveRDS(
  ridge_model,
  "output/task5_ridge_model.rds"
)

saveRDS(
  lasso_model,
  "output/task5_lasso_model.rds"
)

saveRDS(
  ridge_cv,
  "output/task5_ridge_cv.rds"
)

saveRDS(
  lasso_cv,
  "output/task5_lasso_cv.rds"
)

# ============================================================
# 30. Final message
# ============================================================

cat("\n============================================================\n")
cat("TASK 5 - STEP 6 COMPLETED\n")
cat("============================================================\n\n")

cat(
  "All-model training performance saved to:\n",
  "output/task5_all_models_training_performance.csv\n\n"
)

cat(
  "All-model test performance saved to:\n",
  "output/task5_all_models_test_performance.csv\n\n"
)

cat(
  "Ridge lambda values saved to:\n",
  "output/task5_ridge_lasso_lambda_values.csv\n\n"
)

cat(
  "Ridge coefficients saved to:\n",
  "output/task5_ridge_coefficients.csv\n\n"
)

cat(
  "LASSO coefficients saved to:\n",
  "output/task5_lasso_coefficients.csv\n\n"
)

cat(
  "Non-zero LASSO coefficients saved to:\n",
  "output/task5_lasso_nonzero_coefficients.csv\n\n"
)

cat(
  "Ridge/LASSO predictions saved to:\n",
  "output/task5_ridge_lasso_test_predictions.csv\n\n"
)

cat(
  "Plots saved to:\n",
  "plots/27_task5_ridge_cross_validation.png\n",
  "plots/28_task5_lasso_cross_validation.png\n",
  "plots/29_task5_ridge_lasso_predictions.png\n"
)