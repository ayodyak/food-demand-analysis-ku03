# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# TASK 5 - STEP 4
# BASELINE MLR DIAGNOSTICS
# ============================================================

library(tidyverse)
library(car)
library(lmtest)

# ============================================================
# 1. Training residual data
# ============================================================

diagnostic_data <- train_residual_data %>%
  arrange(date)

# ============================================================
# 2. Basic residual summary
# ============================================================

cat("============================================================\n")
cat("BASELINE MLR RESIDUAL SUMMARY\n")
cat("============================================================\n\n")

print(
  summary(
    diagnostic_data$residual
  )
)

cat("\nMean residual:\n")

print(
  mean(
    diagnostic_data$residual
  )
)

cat("\nStandard deviation of residuals:\n")

print(
  sd(
    diagnostic_data$residual
  )
)

# ============================================================
# 3. Standard four diagnostic plots
# ============================================================

png(
  "plots/22_task5_baseline_mlr_diagnostic_plots.png",
  width = 1600,
  height = 1600,
  res = 200
)

par(
  mfrow = c(2, 2)
)

plot(
  baseline_mlr,
  which = 1,
  main = "Residuals vs Fitted"
)

plot(
  baseline_mlr,
  which = 2,
  main = "Normal Q-Q"
)

plot(
  baseline_mlr,
  which = 3,
  main = "Scale-Location"
)

plot(
  baseline_mlr,
  which = 5,
  main = "Residuals vs Leverage"
)

dev.off()

# ============================================================
# 4. Breusch-Pagan test for heteroscedasticity
# ============================================================

bp_test <- bptest(
  baseline_mlr
)

cat("\n============================================================\n")
cat("BREUSCH-PAGAN TEST\n")
cat("============================================================\n\n")

print(
  bp_test
)

# ============================================================
# 5. Shapiro-Wilk normality test
# ============================================================

shapiro_test <- shapiro.test(
  diagnostic_data$residual
)

cat("\n============================================================\n")
cat("SHAPIRO-WILK TEST\n")
cat("============================================================\n\n")

print(
  shapiro_test
)

# ============================================================
# 6. Durbin-Watson test for autocorrelation
# ============================================================

dw_test <- dwtest(
  baseline_mlr,
  alternative = "two.sided"
)

cat("\n============================================================\n")
cat("DURBIN-WATSON TEST\n")
cat("============================================================\n\n")

print(
  dw_test
)

# ============================================================
# 7. Residual autocorrelation function
# ============================================================

png(
  "plots/23_task5_baseline_residual_acf.png",
  width = 1400,
  height = 900,
  res = 200
)

acf(
  diagnostic_data$residual,
  main = "ACF of Baseline MLR Training Residuals"
)

dev.off()

# ============================================================
# 8. Calculate VIF / GVIF again for baseline model
# ============================================================

vif_results <- car::vif(
  baseline_mlr
)

cat("\n============================================================\n")
cat("BASELINE MLR VIF / GVIF RESULTS\n")
cat("============================================================\n\n")

print(
  vif_results
)

# ============================================================
# 9. Identify influential observations
# ============================================================

cooks_values <- cooks.distance(
  baseline_mlr
)

influence_threshold <- 4 / nrow(
  train_data
)

influential_data <- train_data %>%
  mutate(
    cooks_distance = cooks_values,
    observation = row_number()
  ) %>%
  filter(
    cooks_distance > influence_threshold
  ) %>%
  arrange(
    desc(cooks_distance)
  )

cat("\n============================================================\n")
cat("INFLUENTIAL OBSERVATIONS\n")
cat("============================================================\n\n")

cat(
  "Cook's distance threshold:",
  round(
    influence_threshold,
    6
  ),
  "\n"
)

cat(
  "Number of potentially influential observations:",
  nrow(influential_data),
  "\n\n"
)

print(
  influential_data %>%
    select(
      observation,
      date,
      daily_demand,
      cooks_distance
    ) %>%
    slice_head(
      n = 20
    )
)

# ============================================================
# 10. Save influential observations
# ============================================================

write_csv(
  influential_data,
  "output/task5_baseline_mlr_influential_observations.csv"
)

# ============================================================
# 11. Save diagnostic test summary
# ============================================================

diagnostic_results <- tibble(
  
  Test = c(
    "Breusch-Pagan",
    "Shapiro-Wilk",
    "Durbin-Watson"
  ),
  
  Statistic = c(
    unname(
      bp_test$statistic
    ),
    unname(
      shapiro_test$statistic
    ),
    unname(
      dw_test$statistic
    )
  ),
  
  P_value = c(
    bp_test$p.value,
    shapiro_test$p.value,
    dw_test$p.value
  )
)

write_csv(
  diagnostic_results,
  "output/task5_baseline_mlr_diagnostic_tests.csv"
)

# ============================================================
# 12. Residual vs fitted plot
# ============================================================

p_residual_fitted <- ggplot(
  diagnostic_data,
  aes(
    x = fitted_demand,
    y = residual
  )
) +
  geom_point(
    alpha = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_smooth(
    method = "loess",
    se = TRUE
  ) +
  labs(
    title = "Baseline MLR: Residuals vs Fitted Values",
    x = "Fitted Daily Demand",
    y = "Residual"
  ) +
  theme_minimal()

print(
  p_residual_fitted
)

ggsave(
  "plots/24_task5_baseline_residuals_vs_fitted.png",
  p_residual_fitted,
  width = 10,
  height = 7,
  dpi = 300
)

# ============================================================
# 13. Residuals over time
# ============================================================

p_residual_time <- ggplot(
  diagnostic_data,
  aes(
    x = date,
    y = residual
  )
) +
  geom_line() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Baseline MLR: Residuals Over Time",
    x = "Date",
    y = "Residual"
  ) +
  theme_minimal()

print(
  p_residual_time
)

ggsave(
  "plots/25_task5_baseline_residuals_over_time.png",
  p_residual_time,
  width = 12,
  height = 6,
  dpi = 300
)

# ============================================================
# 14. Final message
# ============================================================

cat("\n============================================================\n")
cat("TASK 5 - STEP 4 COMPLETED\n")
cat("============================================================\n\n")

cat(
  "Diagnostic tests saved to:\n",
  "output/task5_baseline_mlr_diagnostic_tests.csv\n\n"
)

cat(
  "Influential observations saved to:\n",
  "output/task5_baseline_mlr_influential_observations.csv\n\n"
)

cat(
  "Diagnostic plots saved to:\n",
  "plots/22_task5_baseline_mlr_diagnostic_plots.png\n",
  "plots/23_task5_baseline_residual_acf.png\n",
  "plots/24_task5_baseline_residuals_vs_fitted.png\n",
  "plots/25_task5_baseline_residuals_over_time.png\n"
)