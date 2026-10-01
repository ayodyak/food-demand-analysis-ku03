# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# TASK 5 - PREDICTIVE STATISTICAL MODELLING
# STEP 7 - FINAL MODEL COMPARISON
# ============================================================

library(tidyverse)

# ============================================================
# 1. Create final model comparison table
# ============================================================

final_model_comparison <- tibble(
  
  Model = c(
    "Baseline MLR",
    "Forward Selection",
    "Backward Selection",
    "Ridge",
    "LASSO"
  ),
  
  Training_RMSE = c(
    RMSE(
      train_data$daily_demand,
      predict(
        baseline_mlr,
        newdata = train_data
      )
    ),
    
    RMSE(
      train_data$daily_demand,
      predict(
        forward_mlr,
        newdata = train_data
      )
    ),
    
    RMSE(
      train_data$daily_demand,
      predict(
        backward_mlr,
        newdata = train_data
      )
    ),
    
    RMSE(
      y_train,
      ridge_train_predictions
    ),
    
    RMSE(
      y_train,
      lasso_train_predictions
    )
  ),
  
  Training_MAE = c(
    MAE(
      train_data$daily_demand,
      predict(
        baseline_mlr,
        newdata = train_data
      )
    ),
    
    MAE(
      train_data$daily_demand,
      predict(
        forward_mlr,
        newdata = train_data
      )
    ),
    
    MAE(
      train_data$daily_demand,
      predict(
        backward_mlr,
        newdata = train_data
      )
    ),
    
    MAE(
      y_train,
      ridge_train_predictions
    ),
    
    MAE(
      y_train,
      lasso_train_predictions
    )
  ),
  
  Training_R_squared = c(
    R_squared(
      train_data$daily_demand,
      predict(
        baseline_mlr,
        newdata = train_data
      )
    ),
    
    R_squared(
      train_data$daily_demand,
      predict(
        forward_mlr,
        newdata = train_data
      )
    ),
    
    R_squared(
      train_data$daily_demand,
      predict(
        backward_mlr,
        newdata = train_data
      )
    ),
    
    R_squared(
      y_train,
      ridge_train_predictions
    ),
    
    R_squared(
      y_train,
      lasso_train_predictions
    )
  ),
  
  Test_RMSE = c(
    RMSE(
      test_data$daily_demand,
      predict(
        baseline_mlr,
        newdata = test_data
      )
    ),
    
    RMSE(
      test_data$daily_demand,
      predict(
        forward_mlr,
        newdata = test_data
      )
    ),
    
    RMSE(
      test_data$daily_demand,
      predict(
        backward_mlr,
        newdata = test_data
      )
    ),
    
    RMSE(
      y_test,
      ridge_test_predictions
    ),
    
    RMSE(
      y_test,
      lasso_test_predictions
    )
  ),
  
  Test_MAE = c(
    MAE(
      test_data$daily_demand,
      predict(
        baseline_mlr,
        newdata = test_data
      )
    ),
    
    MAE(
      test_data$daily_demand,
      predict(
        forward_mlr,
        newdata = test_data
      )
    ),
    
    MAE(
      test_data$daily_demand,
      predict(
        backward_mlr,
        newdata = test_data
      )
    ),
    
    MAE(
      y_test,
      ridge_test_predictions
    ),
    
    MAE(
      y_test,
      lasso_test_predictions
    )
  ),
  
  Test_R_squared = c(
    R_squared(
      test_data$daily_demand,
      predict(
        baseline_mlr,
        newdata = test_data
      )
    ),
    
    R_squared(
      test_data$daily_demand,
      predict(
        forward_mlr,
        newdata = test_data
      )
    ),
    
    R_squared(
      test_data$daily_demand,
      predict(
        backward_mlr,
        newdata = test_data
      )
    ),
    
    R_squared(
      y_test,
      ridge_test_predictions
    ),
    
    R_squared(
      y_test,
      lasso_test_predictions
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
    ),
    
    length(
      coef(
        ridge_model
      )
    ),
    
    length(
      coef(
        lasso_model
      )
    )
  )
)

# ============================================================
# 2. Add AIC where available
# ============================================================

final_model_comparison <- final_model_comparison %>%
  mutate(
    AIC = c(
      AIC(
        baseline_mlr
      ),
      AIC(
        forward_mlr
      ),
      AIC(
        backward_mlr
      ),
      NA_real_,
      NA_real_
    )
  ) %>%
  select(
    Model,
    Number_of_Coefficients,
    AIC,
    Training_RMSE,
    Training_MAE,
    Training_R_squared,
    Test_RMSE,
    Test_MAE,
    Test_R_squared
  )

# ============================================================
# 3. Display final comparison
# ============================================================

cat("\n============================================================\n")
cat("FINAL MODEL COMPARISON\n")
cat("============================================================\n\n")

print(
  final_model_comparison %>%
    mutate(
      AIC = round(
        AIC,
        2
      ),
      Training_RMSE = round(
        Training_RMSE,
        2
      ),
      Training_MAE = round(
        Training_MAE,
        2
      ),
      Training_R_squared = round(
        Training_R_squared,
        4
      ),
      Test_RMSE = round(
        Test_RMSE,
        2
      ),
      Test_MAE = round(
        Test_MAE,
        2
      ),
      Test_R_squared = round(
        Test_R_squared,
        4
      )
    )
)

# ============================================================
# 4. Save final comparison
# ============================================================

write_csv(
  final_model_comparison,
  "output/task5_final_model_comparison.csv"
)

# ============================================================
# 5. Create concise test performance table for report
# ============================================================

report_test_comparison <- final_model_comparison %>%
  select(
    Model,
    Test_RMSE,
    Test_MAE,
    Test_R_squared
  )

write_csv(
  report_test_comparison,
  "output/task5_report_test_performance_comparison.csv"
)

# ============================================================
# 6. Final model information
# ============================================================

cat("\n============================================================\n")
cat("FINAL SELECTED STEPWISE MODEL\n")
cat("============================================================\n\n")

print(
  formula(
    backward_mlr
  )
)

cat("\nThe forward and backward methods selected the same formula.\n")

# ============================================================
# 7. Save final selected model
# ============================================================

saveRDS(
  backward_mlr,
  "output/task5_final_selected_mlr.rds"
)

# ============================================================
# 8. Final output message
# ============================================================

cat("\n============================================================\n")
cat("TASK 5 - STEP 7 COMPLETED\n")
cat("============================================================\n\n")

cat(
  "Final model comparison saved to:\n",
  "output/task5_final_model_comparison.csv\n\n"
)

cat(
  "Report test-performance table saved to:\n",
  "output/task5_report_test_performance_comparison.csv\n\n"
)

cat(
  "Final selected MLR saved to:\n",
  "output/task5_final_selected_mlr.rds\n"
)