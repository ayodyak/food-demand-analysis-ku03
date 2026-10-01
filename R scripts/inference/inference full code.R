# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# TASK 4 - STATISTICAL INFERENCE
# ============================================================

library(tidyverse)
library(car)

# ============================================================
# 1. Create output folder
# ============================================================

if (!dir.exists("plots")) {
  dir.create("plots")
}

# ============================================================
# 2. Create inference dataset
# ============================================================
# Only observed demand days are used.
#
# IMPORTANT:
# daily_demand_data is NOT modified.
# Missing/closed days are not treated as zero demand.

inference_data <- daily_demand_data %>%
  filter(!is.na(daily_demand)) %>%
  droplevels()

cat("============================================================\n")
cat("TASK 4 - STATISTICAL INFERENCE\n")
cat("============================================================\n\n")

cat("Number of observations:", nrow(inference_data), "\n")
cat("Number of variables:", ncol(inference_data), "\n\n")


# ============================================================
# PART A
# COMPARISON OF MEANS
# WEEKDAY VS WEEKEND
# ============================================================

cat("\n============================================================\n")
cat("PART A - COMPARISON OF MEANS\n")
cat("WEEKDAY VS WEEKEND\n")
cat("============================================================\n\n")

# ------------------------------------------------------------
# Hypotheses
# ------------------------------------------------------------

cat("HYPOTHESES\n")
cat("H0: Mean weekday demand = Mean weekend demand\n")
cat("H1: Mean weekday demand != Mean weekend demand\n\n")

cat("Significance level: alpha = 0.05\n\n")


# ------------------------------------------------------------
# Descriptive statistics
# ------------------------------------------------------------

weekday_weekend_summary <- inference_data %>%
  group_by(is_weekend) %>%
  summarise(
    n = n(),
    mean_demand = mean(daily_demand),
    median_demand = median(daily_demand),
    sd_demand = sd(daily_demand),
    variance = var(daily_demand),
    .groups = "drop"
  )

cat("DESCRIPTIVE STATISTICS\n")
print(weekday_weekend_summary)


# ------------------------------------------------------------
# Welch two-sample t-test
# ------------------------------------------------------------
# Welch's t-test is used because the two groups can have
# different variances.

mean_test <- t.test(
  daily_demand ~ is_weekend,
  data = inference_data,
  var.equal = FALSE,
  conf.level = 0.95
)

cat("\n------------------------------------------------------------\n")
cat("WELCH TWO-SAMPLE T-TEST\n")
cat("------------------------------------------------------------\n")

print(mean_test)


# ------------------------------------------------------------
# Extract important results
# ------------------------------------------------------------

cat("\n------------------------------------------------------------\n")
cat("MEAN COMPARISON RESULTS\n")
cat("------------------------------------------------------------\n")

cat(
  "Test statistic:",
  round(mean_test$statistic, 4),
  "\n"
)

cat(
  "Degrees of freedom:",
  round(mean_test$parameter, 4),
  "\n"
)

cat(
  "p-value:",
  format.pval(mean_test$p.value, digits = 5),
  "\n"
)

cat(
  "95% confidence interval:",
  round(mean_test$conf.int[1], 2),
  "to",
  round(mean_test$conf.int[2], 2),
  "\n"
)

cat(
  "Estimated mean difference:",
  round(
    mean(
      inference_data$daily_demand[
        inference_data$is_weekend == "Yes"
      ]
    ) -
      mean(
        inference_data$daily_demand[
          inference_data$is_weekend == "No"
        ]
      ),
    2
  ),
  "\n"
)

if (mean_test$p.value < 0.05) {
  
  cat(
    "\nDecision: Reject H0 at the 5% significance level.\n"
  )
  
} else {
  
  cat(
    "\nDecision: Fail to reject H0 at the 5% significance level.\n"
  )
  
}


# ============================================================
# PART B
# COMPARISON OF VARIANCES
# WEEKDAY VS WEEKEND
# ============================================================

cat("\n============================================================\n")
cat("PART B - COMPARISON OF VARIANCES\n")
cat("WEEKDAY VS WEEKEND\n")
cat("============================================================\n\n")

cat("HYPOTHESES\n")
cat("H0: Weekday demand variance = Weekend demand variance\n")
cat("H1: Weekday demand variance != Weekend demand variance\n\n")

cat("Significance level: alpha = 0.05\n\n")


# ------------------------------------------------------------
# Variance summary
# ------------------------------------------------------------

variance_summary <- inference_data %>%
  group_by(is_weekend) %>%
  summarise(
    n = n(),
    variance = var(daily_demand),
    standard_deviation = sd(daily_demand),
    .groups = "drop"
  )

cat("VARIANCE SUMMARY\n")
print(variance_summary)


# ------------------------------------------------------------
# F-test for equality of variances
# ------------------------------------------------------------

variance_test <- var.test(
  daily_demand ~ is_weekend,
  data = inference_data,
  alternative = "two.sided",
  conf.level = 0.95
)

cat("\n------------------------------------------------------------\n")
cat("F-TEST FOR EQUALITY OF VARIANCES\n")
cat("------------------------------------------------------------\n")

print(variance_test)


# ------------------------------------------------------------
# Extract results
# ------------------------------------------------------------

cat("\n------------------------------------------------------------\n")
cat("VARIANCE COMPARISON RESULTS\n")
cat("------------------------------------------------------------\n")

cat(
  "F statistic:",
  round(variance_test$statistic, 4),
  "\n"
)

cat(
  "Degrees of freedom:",
  variance_test$parameter[1],
  "and",
  variance_test$parameter[2],
  "\n"
)

cat(
  "p-value:",
  format.pval(variance_test$p.value, digits = 5),
  "\n"
)

cat(
  "95% confidence interval for variance ratio:",
  round(variance_test$conf.int[1], 3),
  "to",
  round(variance_test$conf.int[2], 3),
  "\n"
)

if (variance_test$p.value < 0.05) {
  
  cat(
    "\nDecision: Reject H0 at the 5% significance level.\n"
  )
  
} else {
  
  cat(
    "\nDecision: Fail to reject H0 at the 5% significance level.\n"
  )
  
}


# ============================================================
# PART C
# ONE-WAY ANOVA
# DEMAND BY DAY OF WEEK
# ============================================================

cat("\n============================================================\n")
cat("PART C - ONE-WAY ANOVA\n")
cat("DAILY DEMAND BY DAY OF WEEK\n")
cat("============================================================\n\n")

cat("HYPOTHESES\n")
cat("H0: Mean daily demand is equal across all seven days.\n")
cat("H1: At least one day has a different mean daily demand.\n\n")

cat("Significance level: alpha = 0.05\n\n")


# ------------------------------------------------------------
# Descriptive statistics by day
# ------------------------------------------------------------

day_anova_summary <- inference_data %>%
  group_by(day_of_week) %>%
  summarise(
    n = n(),
    mean_demand = mean(daily_demand),
    median_demand = median(daily_demand),
    sd_demand = sd(daily_demand),
    variance = var(daily_demand),
    .groups = "drop"
  )

cat("DESCRIPTIVE STATISTICS BY DAY\n")
print(day_anova_summary)


# ------------------------------------------------------------
# Fit ANOVA model
# ------------------------------------------------------------

day_anova_model <- aov(
  daily_demand ~ day_of_week,
  data = inference_data
)

cat("\n------------------------------------------------------------\n")
cat("ANOVA TABLE - DAY OF WEEK\n")
cat("------------------------------------------------------------\n")

day_anova_result <- summary(day_anova_model)

print(day_anova_result)


# ------------------------------------------------------------
# Extract ANOVA p-value
# ------------------------------------------------------------

day_anova_p <- summary(day_anova_model)[[1]][["Pr(>F)"]][1]

day_anova_f <- summary(day_anova_model)[[1]][["F value"]][1]

cat("\nF statistic:", round(day_anova_f, 4), "\n")
cat(
  "p-value:",
  format.pval(day_anova_p, digits = 5),
  "\n"
)

if (day_anova_p < 0.05) {
  
  cat(
    "\nDecision: Reject H0 at the 5% significance level.\n"
  )
  
  cat(
    "At least one day-of-week mean differs significantly.\n"
  )
  
} else {
  
  cat(
    "\nDecision: Fail to reject H0 at the 5% significance level.\n"
  )
  
}


# ============================================================
# PART D
# ANOVA ASSUMPTION CHECKS
# DAY OF WEEK
# ============================================================

cat("\n============================================================\n")
cat("ANOVA ASSUMPTION CHECKS - DAY OF WEEK\n")
cat("============================================================\n\n")


# ------------------------------------------------------------
# 1. Residual normality
# ------------------------------------------------------------

day_residuals <- residuals(day_anova_model)

shapiro_day <- shapiro.test(day_residuals)

cat("SHAPIRO-WILK TEST FOR RESIDUAL NORMALITY\n")
print(shapiro_day)

cat("\nInterpretation rule:\n")
cat("p-value > 0.05 -> no significant evidence against normality.\n")
cat("p-value < 0.05 -> evidence against normality.\n")


# ------------------------------------------------------------
# 2. Homogeneity of variances
# ------------------------------------------------------------

cat("\n------------------------------------------------------------\n")
cat("LEVENE'S TEST FOR HOMOGENEITY OF VARIANCES\n")
cat("------------------------------------------------------------\n")

levene_day <- leveneTest(
  daily_demand ~ day_of_week,
  data = inference_data
)

print(levene_day)


# ------------------------------------------------------------
# 3. Diagnostic plots
# ------------------------------------------------------------

png(
  "plots/14_day_anova_diagnostics.png",
  width = 1200,
  height = 900,
  res = 150
)

par(mfrow = c(2, 2))

plot(day_anova_model)

dev.off()

par(mfrow = c(1, 1))


# ============================================================
# PART E
# TUKEY POST-HOC TEST
# DAY OF WEEK
# ============================================================

cat("\n============================================================\n")
cat("PART E - TUKEY POST-HOC TEST\n")
cat("DAY OF WEEK\n")
cat("============================================================\n\n")

if (day_anova_p < 0.05) {
  
  cat(
    "ANOVA is significant, therefore Tukey's HSD is performed.\n\n"
  )
  
  tukey_day <- TukeyHSD(
    day_anova_model,
    "day_of_week"
  )
  
  print(tukey_day)
  
  # Save Tukey results
  tukey_day_df <- as.data.frame(
    tukey_day$day_of_week
  )
  
  tukey_day_df$comparison <- rownames(tukey_day_df)
  
  rownames(tukey_day_df) <- NULL
  
  write_csv(
    tukey_day_df,
    "plots/tukey_day_of_week.csv"
  )
  
} else {
  
  cat(
    "ANOVA is not significant.\n"
  )
  
  cat(
    "No post-hoc comparison is required.\n"
  )
  
}


# ============================================================
# PART F
# ONE-WAY ANOVA
# DEMAND BY MONTH
# ============================================================

cat("\n============================================================\n")
cat("PART F - ONE-WAY ANOVA\n")
cat("DAILY DEMAND BY MONTH\n")
cat("============================================================\n\n")

cat("HYPOTHESES\n")
cat("H0: Mean daily demand is equal across all months.\n")
cat("H1: At least one month has a different mean daily demand.\n\n")

cat("Significance level: alpha = 0.05\n\n")


# ------------------------------------------------------------
# Monthly descriptive statistics
# ------------------------------------------------------------

month_anova_summary <- inference_data %>%
  group_by(month) %>%
  summarise(
    n = n(),
    mean_demand = mean(daily_demand),
    median_demand = median(daily_demand),
    sd_demand = sd(daily_demand),
    variance = var(daily_demand),
    .groups = "drop"
  )

cat("DESCRIPTIVE STATISTICS BY MONTH\n")
print(month_anova_summary)


# ------------------------------------------------------------
# Fit monthly ANOVA
# ------------------------------------------------------------

month_anova_model <- aov(
  daily_demand ~ month,
  data = inference_data
)

cat("\n------------------------------------------------------------\n")
cat("ANOVA TABLE - MONTH\n")
cat("------------------------------------------------------------\n")

month_anova_result <- summary(month_anova_model)

print(month_anova_result)


# ------------------------------------------------------------
# Extract results
# ------------------------------------------------------------

month_anova_p <- summary(month_anova_model)[[1]][["Pr(>F)"]][1]

month_anova_f <- summary(month_anova_model)[[1]][["F value"]][1]

cat("\nF statistic:", round(month_anova_f, 4), "\n")

cat(
  "p-value:",
  format.pval(month_anova_p, digits = 5),
  "\n"
)

if (month_anova_p < 0.05) {
  
  cat(
    "\nDecision: Reject H0 at the 5% significance level.\n"
  )
  
  cat(
    "At least one monthly mean differs significantly.\n"
  )
  
} else {
  
  cat(
    "\nDecision: Fail to reject H0 at the 5% significance level.\n"
  )
  
}


# ============================================================
# PART G
# ANOVA ASSUMPTION CHECKS
# MONTH
# ============================================================

cat("\n============================================================\n")
cat("ANOVA ASSUMPTION CHECKS - MONTH\n")
cat("============================================================\n\n")


# ------------------------------------------------------------
# Residual normality
# ------------------------------------------------------------

month_residuals <- residuals(month_anova_model)

shapiro_month <- shapiro.test(month_residuals)

cat("SHAPIRO-WILK TEST FOR RESIDUAL NORMALITY\n")
print(shapiro_month)


# ------------------------------------------------------------
# Homogeneity of variances
# ------------------------------------------------------------

cat("\n------------------------------------------------------------\n")
cat("LEVENE'S TEST FOR HOMOGENEITY OF VARIANCES\n")
cat("------------------------------------------------------------\n")

levene_month <- leveneTest(
  daily_demand ~ month,
  data = inference_data
)

print(levene_month)


# ------------------------------------------------------------
# Diagnostic plots
# ------------------------------------------------------------

png(
  "plots/15_month_anova_diagnostics.png",
  width = 1200,
  height = 900,
  res = 150
)

par(mfrow = c(2, 2))

plot(month_anova_model)

dev.off()

par(mfrow = c(1, 1))


# ============================================================
# PART H
# TUKEY POST-HOC TEST
# MONTH
# ============================================================

cat("\n============================================================\n")
cat("PART H - TUKEY POST-HOC TEST\n")
cat("MONTH\n")
cat("============================================================\n\n")

if (month_anova_p < 0.05) {
  
  cat(
    "ANOVA is significant, therefore Tukey's HSD is performed.\n\n"
  )
  
  tukey_month <- TukeyHSD(
    month_anova_model,
    "month"
  )
  
  print(tukey_month)
  
  tukey_month_df <- as.data.frame(
    tukey_month$month
  )
  
  tukey_month_df$comparison <- rownames(
    tukey_month_df
  )
  
  rownames(tukey_month_df) <- NULL
  
  write_csv(
    tukey_month_df,
    "plots/tukey_month.csv"
  )
  
} else {
  
  cat(
    "ANOVA is not significant.\n"
  )
  
  cat(
    "No post-hoc comparison is required.\n"
  )
  
}


# ============================================================
# PART I
# SAVE DESCRIPTIVE STATISTICS
# ============================================================

write_csv(
  weekday_weekend_summary,
  "plots/task4_weekday_weekend_summary.csv"
)

write_csv(
  variance_summary,
  "plots/task4_variance_summary.csv"
)

write_csv(
  day_anova_summary,
  "plots/task4_day_anova_summary.csv"
)

write_csv(
  month_anova_summary,
  "plots/task4_month_anova_summary.csv"
)


# ============================================================
# PART J
# VISUALISATIONS FOR STATISTICAL INFERENCE
# ============================================================

# ------------------------------------------------------------
# Weekday vs weekend
# ------------------------------------------------------------

p_weekend <- ggplot(
  inference_data,
  aes(
    x = is_weekend,
    y = daily_demand
  )
) +
  geom_boxplot() +
  labs(
    title = "Daily Demand: Weekdays vs Weekends",
    x = "Day Type",
    y = "Daily Demand (Food Units)"
  ) +
  theme_minimal()

print(p_weekend)

ggsave(
  "plots/16_inference_weekday_vs_weekend.png",
  p_weekend,
  width = 9,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------
# Day-of-week ANOVA visualisation
# ------------------------------------------------------------

p_day_anova <- ggplot(
  inference_data,
  aes(
    x = day_of_week,
    y = daily_demand
  )
) +
  geom_boxplot() +
  labs(
    title = "Daily Demand Across Days of the Week",
    x = "Day of Week",
    y = "Daily Demand (Food Units)"
  ) +
  theme_minimal()

print(p_day_anova)

ggsave(
  "plots/17_anova_day_of_week.png",
  p_day_anova,
  width = 11,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------
# Monthly ANOVA visualisation
# ------------------------------------------------------------

p_month_anova <- ggplot(
  inference_data,
  aes(
    x = month,
    y = daily_demand
  )
) +
  geom_boxplot() +
  labs(
    title = "Daily Demand Across Months",
    x = "Month",
    y = "Daily Demand (Food Units)"
  ) +
  theme_minimal()

print(p_month_anova)

ggsave(
  "plots/18_anova_month.png",
  p_month_anova,
  width = 12,
  height = 6,
  dpi = 300
)


# ============================================================
# PART K
# FINAL SUMMARY
# ============================================================

cat("\n============================================================\n")
cat("TASK 4 STATISTICAL INFERENCE COMPLETE\n")
cat("============================================================\n\n")

cat("Analyses completed:\n")
cat("1. Weekday vs weekend comparison of means\n")
cat("2. Weekday vs weekend comparison of variances\n")
cat("3. One-way ANOVA - day of week\n")
cat("4. Tukey post-hoc test - day of week (if required)\n")
cat("5. One-way ANOVA - month\n")
cat("6. Tukey post-hoc test - month (if required)\n")
cat("7. ANOVA assumption checks\n")
cat("8. Statistical inference visualisations\n\n")

cat(
  "Results and plots have been saved in:",
  file.path(getwd(), "plots"),
  "\n"
)

cat("\nOriginal daily_demand_data was NOT modified.\n")