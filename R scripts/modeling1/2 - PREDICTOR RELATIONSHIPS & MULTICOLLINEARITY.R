# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# TASK 5 - PREDICTIVE STATISTICAL MODELLING
# STEP 2 - PREDICTOR RELATIONSHIPS & MULTICOLLINEARITY
# ============================================================

library(tidyverse)
library(car)

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
# 2. Use the prepared modelling dataset
# ============================================================

analysis_data <- model_data

# ============================================================
# 3. Confirm dataset
# ============================================================

cat("============================================================\n")
cat("TASK 5 - STEP 2\n")
cat("PREDICTOR RELATIONSHIPS & MULTICOLLINEARITY\n")
cat("============================================================\n\n")

cat(
  "Observations:",
  nrow(analysis_data),
  "\n"
)

cat(
  "Variables:",
  ncol(analysis_data),
  "\n\n"
)

# ============================================================
# 4. Candidate predictor groups
# ============================================================

cat("============================================================\n")
cat("CANDIDATE PREDICTORS\n")
cat("============================================================\n\n")

cat("Calendar predictors:\n")
cat("  day_of_week\n")
cat("  week_of_year\n")
cat("  month\n")
cat("  year\n")
cat("  is_weekend\n")
cat("  time_index\n\n")

cat("Historical-demand predictors:\n")
cat("  prev_day_demand\n")
cat("  prev_week_demand\n")
cat("  rolling_7_day_avg_demand\n")

# ============================================================
# 5. Check structural redundancy
# ============================================================

cat("\n============================================================\n")
cat("STRUCTURAL REDUNDANCY CHECK\n")
cat("============================================================\n\n")

# Verify whether weekend status is completely determined
# by day of week.

weekend_mapping <- analysis_data %>%
  count(
    day_of_week,
    is_weekend
  ) %>%
  arrange(
    day_of_week
  )

print(weekend_mapping)

cat("\nInterpretation:\n")
cat(
  "is_weekend is a deterministic transformation of day_of_week.\n"
)

cat(
  "Therefore, both should not be included simultaneously\n"
)

cat(
  "in the same ordinary linear regression model.\n"
)

# ============================================================
# 6. Numerical predictor correlation matrix
# ============================================================

cat("\n============================================================\n")
cat("NUMERICAL PREDICTOR CORRELATION MATRIX\n")
cat("============================================================\n\n")

numeric_data <- analysis_data %>%
  select(
    daily_demand,
    week_of_year,
    time_index,
    prev_day_demand,
    prev_week_demand,
    rolling_7_day_avg_demand
  )

correlation_matrix <- cor(
  numeric_data,
  use = "complete.obs"
)

print(
  round(
    correlation_matrix,
    3
  )
)

# ============================================================
# 7. Save correlation matrix
# ============================================================

correlation_table <- as.data.frame(
  correlation_matrix
) %>%
  rownames_to_column(
    "Variable"
  )

write_csv(
  correlation_table,
  "output/task5_numeric_correlation_matrix.csv"
)

# ============================================================
# 8. Correlation of predictors with daily demand
# ============================================================

cat("\n============================================================\n")
cat("CORRELATION WITH DAILY DEMAND\n")
cat("============================================================\n\n")

target_correlations <- tibble(
  Predictor = colnames(
    correlation_matrix
  )[colnames(correlation_matrix) != "daily_demand"],
  
  Correlation = correlation_matrix[
    "daily_demand",
    colnames(correlation_matrix) != "daily_demand"
  ]
) %>%
  arrange(
    desc(abs(Correlation))
  )

print(
  target_correlations %>%
    mutate(
      Correlation = round(
        Correlation,
        3
      )
    )
)

write_csv(
  target_correlations,
  "output/task5_predictor_target_correlations.csv"
)

# ============================================================
# 9. Correlation heatmap
# ============================================================

correlation_long <- correlation_table %>%
  pivot_longer(
    cols = -Variable,
    names_to = "Variable_2",
    values_to = "Correlation"
  )

p_corr <- ggplot(
  correlation_long,
  aes(
    x = Variable,
    y = Variable_2,
    fill = Correlation
  )
) +
  geom_tile() +
  geom_text(
    aes(
      label = round(
        Correlation,
        2
      )
    ),
    size = 3
  ) +
  scale_fill_gradient2(
    limits = c(-1, 1),
    midpoint = 0
  ) +
  labs(
    title = "Correlation Matrix of Numerical Variables",
    x = NULL,
    y = NULL,
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(p_corr)

ggsave(
  "plots/19_task5_correlation_heatmap.png",
  p_corr,
  width = 10,
  height = 8,
  dpi = 300
)

# ============================================================
# 10. Build initial full candidate regression
# ============================================================
#
# IMPORTANT:
# day_of_week is used instead of is_weekend because
# is_weekend is completely determined by day_of_week.
#
# Both variables are NOT included together.
#
# ============================================================

initial_mlr <- lm(
  daily_demand ~
    day_of_week +
    week_of_year +
    month +
    year +
    time_index +
    prev_day_demand +
    prev_week_demand +
    rolling_7_day_avg_demand,
  data = analysis_data
)

# ============================================================
# 11. Display initial model
# ============================================================

cat("\n============================================================\n")
cat("INITIAL MULTIPLE LINEAR REGRESSION\n")
cat("============================================================\n\n")

print(
  summary(
    initial_mlr
  )
)

# ============================================================
# 12. VIF / GVIF
# ============================================================

cat("\n============================================================\n")
cat("MULTICOLLINEARITY ASSESSMENT\n")
cat("VIF / GVIF\n")
cat("============================================================\n\n")

vif_results <- car::vif(
  initial_mlr
)

print(vif_results)

# ============================================================
# 13. Convert VIF output into a table
# ============================================================

if (is.matrix(vif_results)) {
  
  vif_table <- as.data.frame(
    vif_results
  ) %>%
    rownames_to_column(
      "Variable"
    )
  
  # Calculate adjusted GVIF
  if (
    "GVIF" %in% names(vif_table) &
    "Df" %in% names(vif_table)
  ) {
    
    vif_table <- vif_table %>%
      mutate(
        Adjusted_GVIF =
          GVIF ^ (
            1 /
              (2 * Df)
          )
      )
    
  }
  
} else {
  
  vif_table <- tibble(
    Variable = names(vif_results),
    VIF = as.numeric(
      vif_results
    )
  )
}

cat("\n------------------------------------------------------------\n")
cat("VIF TABLE\n")
cat("------------------------------------------------------------\n\n")

print(vif_table)

write_csv(
  vif_table,
  "output/task5_vif_results.csv"
)

# ============================================================
# 14. Identify potentially high VIF values
# ============================================================

cat("\n============================================================\n")
cat("VIF INTERPRETATION CHECK\n")
cat("============================================================\n\n")

if (
  "Adjusted_GVIF" %in% names(vif_table)
) {
  
  high_vif <- vif_table %>%
    filter(
      Adjusted_GVIF > 5
    )
  
  cat(
    "Variables with adjusted GVIF > 5:",
    nrow(high_vif),
    "\n"
  )
  
  if (nrow(high_vif) > 0) {
    print(high_vif)
  }
  
} else {
  
  high_vif <- vif_table %>%
    filter(
      VIF > 5
    )
  
  cat(
    "Variables with VIF > 5:",
    nrow(high_vif),
    "\n"
  )
  
  if (nrow(high_vif) > 0) {
    print(high_vif)
  }
}

# ============================================================
# 15. Examine categorical predictor relationships
# ============================================================

cat("\n============================================================\n")
cat("CALENDAR VARIABLE DISTRIBUTIONS\n")
cat("============================================================\n\n")

cat("Day of week:\n")
print(
  table(
    analysis_data$day_of_week
  )
)

cat("\nMonth:\n")
print(
  table(
    analysis_data$month
  )
)

cat("\nYear:\n")
print(
  table(
    analysis_data$year
  )
)

cat("\nWeekend status:\n")
print(
  table(
    analysis_data$is_weekend
  )
)

# ============================================================
# 16. Save initial MLR model
# ============================================================

saveRDS(
  initial_mlr,
  "output/task5_initial_mlr.rds"
)

# ============================================================
# 17. Save analysis dataset used
# ============================================================

write_csv(
  analysis_data,
  "output/task5_analysis_data_step2.csv"
)

# ============================================================
# 18. Final message
# ============================================================

cat("\n============================================================\n")
cat("TASK 5 - STEP 2 COMPLETED\n")
cat("============================================================\n\n")

cat(
  "Correlation matrix saved to output/.\n"
)

cat(
  "Target correlations saved to output/.\n"
)

cat(
  "VIF results saved to output/.\n"
)

cat(
  "Correlation heatmap saved to plots/.\n"
)

cat(
  "Initial MLR model saved to output/.\n\n"
)

cat(
  "Important structural finding:\n"
)

cat(
  "day_of_week and is_weekend are not used simultaneously\n"
)

cat(
  "because is_weekend is completely determined by day_of_week.\n"
)

cat(
  "\nOriginal modelling dataset was NOT modified.\n"
)