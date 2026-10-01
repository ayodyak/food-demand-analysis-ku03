# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# Exploratory Data Analysis - Daily Demand
# ============================================================

library(tidyverse)
library(lubridate)

# ============================================================
# 1. Create plots folder
# ============================================================

if (!dir.exists("plots")) {
  dir.create("plots")
}

# ============================================================
# 2. Create analysis dataset
# ============================================================
# IMPORTANT:
# daily_demand_data is NOT modified.
#
# Only days with observed daily demand are used for
# distributional and relationship analysis.

eda_data <- daily_demand_data %>%
  filter(!is.na(daily_demand))

# ============================================================
# 3. Basic EDA summary
# ============================================================

cat("============================================================\n")
cat("EXPLORATORY DATA ANALYSIS SUMMARY\n")
cat("============================================================\n")

cat("Total calendar days:", nrow(daily_demand_data), "\n")
cat("Observed demand days:", nrow(eda_data), "\n")
cat(
  "Missing demand days:",
  sum(is.na(daily_demand_data$daily_demand)),
  "\n"
)

cat(
  "Mean daily demand:",
  round(mean(eda_data$daily_demand), 2),
  "\n"
)

cat(
  "Median daily demand:",
  median(eda_data$daily_demand),
  "\n"
)

cat(
  "Standard deviation:",
  round(sd(eda_data$daily_demand), 2),
  "\n"
)

cat(
  "Minimum daily demand:",
  min(eda_data$daily_demand),
  "\n"
)

cat(
  "First quartile:",
  quantile(eda_data$daily_demand, 0.25),
  "\n"
)

cat(
  "Third quartile:",
  quantile(eda_data$daily_demand, 0.75),
  "\n"
)

cat(
  "Maximum daily demand:",
  max(eda_data$daily_demand),
  "\n"
)

# ============================================================
# 4. Daily demand distribution
# ============================================================

p1 <- ggplot(
  eda_data,
  aes(x = daily_demand)
) +
  geom_histogram(
    bins = 35,
    na.rm = TRUE
  ) +
  labs(
    title = "Distribution of Daily Food Demand",
    x = "Daily Demand (Food Units)",
    y = "Number of Days"
  ) +
  theme_minimal()

print(p1)

ggsave(
  "plots/01_daily_demand_distribution.png",
  p1,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 5. Daily demand density plot
# ============================================================

p2 <- ggplot(
  eda_data,
  aes(x = daily_demand)
) +
  geom_density(
    na.rm = TRUE
  ) +
  labs(
    title = "Density Distribution of Daily Food Demand",
    x = "Daily Demand (Food Units)",
    y = "Density"
  ) +
  theme_minimal()

print(p2)

ggsave(
  "plots/02_daily_demand_density.png",
  p2,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 6. Daily demand boxplot
# ============================================================

p3 <- ggplot(
  eda_data,
  aes(y = daily_demand)
) +
  geom_boxplot(
    width = 0.35
  ) +
  labs(
    title = "Boxplot of Daily Food Demand",
    x = NULL,
    y = "Daily Demand (Food Units)"
  ) +
  theme_minimal()

print(p3)

ggsave(
  "plots/03_daily_demand_boxplot.png",
  p3,
  width = 7,
  height = 6,
  dpi = 300
)

# ============================================================
# 7. Full daily demand time series
# ============================================================

p4 <- ggplot(
  daily_demand_data,
  aes(
    x = date,
    y = daily_demand
  )
) +
  geom_line(
    na.rm = FALSE
  ) +
  labs(
    title = "Daily Food Demand Over Time",
    x = "Date",
    y = "Daily Demand (Food Units)"
  ) +
  theme_minimal()

print(p4)

ggsave(
  "plots/04_daily_demand_time_series.png",
  p4,
  width = 12,
  height = 6,
  dpi = 300
)

# ============================================================
# 8. Daily demand with 7-day rolling average
# ============================================================

p5 <- ggplot(
  daily_demand_data,
  aes(x = date)
) +
  geom_line(
    aes(y = daily_demand),
    na.rm = FALSE
  ) +
  geom_line(
    aes(y = rolling_7_day_avg_demand),
    na.rm = TRUE,
    linewidth = 1
  ) +
  labs(
    title = "Daily Food Demand and 7-Day Rolling Average",
    x = "Date",
    y = "Food Units"
  ) +
  theme_minimal()

print(p5)

ggsave(
  "plots/05_demand_rolling_7_day_average.png",
  p5,
  width = 12,
  height = 6,
  dpi = 300
)

# ============================================================
# 9. Demand by day of week
# ============================================================

day_summary <- eda_data %>%
  group_by(day_of_week) %>%
  summarise(
    mean_demand = mean(daily_demand),
    median_demand = median(daily_demand),
    sd_demand = sd(daily_demand),
    n = n(),
    .groups = "drop"
  )

cat("\n============================================================\n")
cat("DEMAND BY DAY OF WEEK\n")
cat("============================================================\n")

print(day_summary)

p6 <- ggplot(
  eda_data,
  aes(
    x = day_of_week,
    y = daily_demand
  )
) +
  geom_boxplot() +
  labs(
    title = "Daily Food Demand by Day of Week",
    x = "Day of Week",
    y = "Daily Demand (Food Units)"
  ) +
  theme_minimal()

print(p6)

ggsave(
  "plots/06_demand_by_day_of_week.png",
  p6,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 10. Mean demand by day of week
# ============================================================

p7 <- ggplot(
  day_summary,
  aes(
    x = day_of_week,
    y = mean_demand
  )
) +
  geom_col() +
  labs(
    title = "Average Daily Demand by Day of Week",
    x = "Day of Week",
    y = "Mean Daily Demand"
  ) +
  theme_minimal()

print(p7)

ggsave(
  "plots/07_mean_demand_by_day.png",
  p7,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 11. Demand by month
# ============================================================

month_summary <- eda_data %>%
  group_by(month) %>%
  summarise(
    mean_demand = mean(daily_demand),
    median_demand = median(daily_demand),
    sd_demand = sd(daily_demand),
    n = n(),
    .groups = "drop"
  )

cat("\n============================================================\n")
cat("DEMAND BY MONTH\n")
cat("============================================================\n")

print(month_summary)

p8 <- ggplot(
  eda_data,
  aes(
    x = month,
    y = daily_demand
  )
) +
  geom_boxplot() +
  labs(
    title = "Daily Food Demand by Month",
    x = "Month",
    y = "Daily Demand (Food Units)"
  ) +
  theme_minimal()

print(p8)

ggsave(
  "plots/08_demand_by_month.png",
  p8,
  width = 12,
  height = 6,
  dpi = 300
)

# ============================================================
# 12. Demand vs previous-day demand
# ============================================================

prev_day_data <- eda_data %>%
  filter(!is.na(prev_day_demand))

p9 <- ggplot(
  prev_day_data,
  aes(
    x = prev_day_demand,
    y = daily_demand
  )
) +
  geom_point(alpha = 0.45) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Daily Demand vs Previous-Day Demand",
    x = "Previous-Day Demand (Food Units)",
    y = "Current Daily Demand (Food Units)"
  ) +
  theme_minimal()

print(p9)

ggsave(
  "plots/09_demand_vs_previous_day.png",
  p9,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 13. Demand vs previous-week demand
# ============================================================

prev_week_data <- eda_data %>%
  filter(!is.na(prev_week_demand))

p10 <- ggplot(
  prev_week_data,
  aes(
    x = prev_week_demand,
    y = daily_demand
  )
) +
  geom_point(alpha = 0.45) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Daily Demand vs Previous-Week Demand",
    x = "Previous-Week Demand (Food Units)",
    y = "Current Daily Demand (Food Units)"
  ) +
  theme_minimal()

print(p10)

ggsave(
  "plots/10_demand_vs_previous_week.png",
  p10,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 14. Demand vs rolling 7-day average
# ============================================================

rolling_data <- eda_data %>%
  filter(!is.na(rolling_7_day_avg_demand))

p11 <- ggplot(
  rolling_data,
  aes(
    x = rolling_7_day_avg_demand,
    y = daily_demand
  )
) +
  geom_point(alpha = 0.45) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Daily Demand vs Previous 7-Day Average",
    x = "Previous 7-Day Average Demand",
    y = "Current Daily Demand (Food Units)"
  ) +
  theme_minimal()

print(p11)

ggsave(
  "plots/11_demand_vs_rolling_7_day_average.png",
  p11,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 15. Correlation analysis
# ============================================================

correlation_data <- eda_data %>%
  select(
    daily_demand,
    prev_day_demand,
    prev_week_demand,
    rolling_7_day_avg_demand
  )

correlation_matrix <- cor(
  correlation_data,
  use = "pairwise.complete.obs"
)

cat("\n============================================================\n")
cat("CORRELATION MATRIX\n")
cat("============================================================\n")

print(
  round(
    correlation_matrix,
    3
  )
)

# ============================================================
# 16. Weekend vs weekday comparison
# ============================================================

weekend_summary <- eda_data %>%
  group_by(is_weekend) %>%
  summarise(
    mean_demand = mean(daily_demand),
    median_demand = median(daily_demand),
    sd_demand = sd(daily_demand),
    n = n(),
    .groups = "drop"
  )

cat("\n============================================================\n")
cat("WEEKDAY VS WEEKEND DEMAND\n")
cat("============================================================\n")

print(weekend_summary)

p12 <- ggplot(
  eda_data,
  aes(
    x = is_weekend,
    y = daily_demand
  )
) +
  geom_boxplot() +
  labs(
    title = "Daily Food Demand: Weekdays vs Weekends",
    x = "Weekend Status",
    y = "Daily Demand (Food Units)"
  ) +
  theme_minimal()

print(p12)

ggsave(
  "plots/12_weekday_vs_weekend.png",
  p12,
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 17. Year comparison
# ============================================================

year_summary <- eda_data %>%
  group_by(year) %>%
  summarise(
    mean_demand = mean(daily_demand),
    median_demand = median(daily_demand),
    sd_demand = sd(daily_demand),
    n = n(),
    .groups = "drop"
  )

cat("\n============================================================\n")
cat("DEMAND BY YEAR\n")
cat("============================================================\n")

print(year_summary)

p13 <- ggplot(
  eda_data,
  aes(
    x = year,
    y = daily_demand
  )
) +
  geom_boxplot() +
  labs(
    title = "Daily Food Demand by Year",
    x = "Year",
    y = "Daily Demand (Food Units)"
  ) +
  theme_minimal()

print(p13)

ggsave(
  "plots/13_demand_by_year.png",
  p13,
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 18. Identify unusually high-demand days
# ============================================================

q1 <- quantile(
  eda_data$daily_demand,
  0.25
)

q3 <- quantile(
  eda_data$daily_demand,
  0.75
)

iqr_demand <- q3 - q1

upper_fence <- q3 + 1.5 * iqr_demand

high_demand_days <- eda_data %>%
  filter(
    daily_demand > upper_fence
  ) %>%
  arrange(
    desc(daily_demand)
  )

cat("\n============================================================\n")
cat("HIGH-DEMAND DAYS\n")
cat("============================================================\n")

cat(
  "Q1:",
  q1,
  "\n"
)

cat(
  "Q3:",
  q3,
  "\n"
)

cat(
  "IQR:",
  iqr_demand,
  "\n"
)

cat(
  "Upper outlier threshold:",
  upper_fence,
  "\n"
)

cat(
  "Number of high-demand days:",
  nrow(high_demand_days),
  "\n"
)

print(
  head(
    high_demand_days,
    15
  )
)

# ============================================================
# 19. Save numerical summaries
# ============================================================

write_csv(
  day_summary,
  "plots/day_of_week_summary.csv"
)

write_csv(
  month_summary,
  "plots/month_summary.csv"
)

write_csv(
  weekend_summary,
  "plots/weekend_summary.csv"
)

write_csv(
  year_summary,
  "plots/year_summary.csv"
)

write_csv(
  high_demand_days,
  "plots/high_demand_days.csv"
)

# ============================================================
# 20. Final EDA message
# ============================================================

cat("\n============================================================\n")
cat("EDA COMPLETE\n")
cat("============================================================\n")

cat(
  "Plots saved in:",
  file.path(getwd(), "plots"),
  "\n"
)

cat(
  "Number of plots created: 13\n"
)

cat(
  "The original daily_demand_data dataset was not modified.\n"
)