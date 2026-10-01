
# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# Data Visualizations
# ============================================================

library(tidyverse)
library(lubridate)

# ============================================================
# 0. CREATE PLOTS FOLDER
# ============================================================

if (!dir.exists("plots")) {
  dir.create("plots")
}

cat("Plots will be saved in:", file.path(getwd(), "plots"), "\n")


# ============================================================
# 1. BASIC DATA PREPARATION
# ============================================================

# Make sure bakery_clean is being used
visual_data <- bakery_clean

# Convert time into a usable hour variable
visual_data <- visual_data %>%
  mutate(
    hour = hour(as.POSIXct(time)),
    weekday = wday(
      date,
      label = TRUE,
      abbr = FALSE,
      week_start = 1
    )
  )

# Check the resulting time variables
cat("Earliest hour:", min(visual_data$hour, na.rm = TRUE), "\n")
cat("Latest hour:", max(visual_data$hour, na.rm = TRUE), "\n")


# ============================================================
# PLOT 1
# TRANSACTION QUANTITY DISTRIBUTION — FULL
# ============================================================

p1 <- ggplot(
  visual_data,
  aes(x = Quantity)
) +
  geom_histogram(
    binwidth = 1,
    boundary = 0.5,
    color = "black"
  ) +
  labs(
    title = "Distribution of Quantity per Transaction Line",
    subtitle = "Full observed range",
    x = "Quantity of Food Units",
    y = "Number of Transaction Lines"
  ) +
  theme_minimal()

ggsave(
  filename = "plots/01_quantity_distribution_full.png",
  plot = p1,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# PLOT 2
# TRANSACTION QUANTITY DISTRIBUTION — ZOOMED
# ============================================================

p2 <- ggplot(
  visual_data %>%
    filter(Quantity <= 10),
  aes(x = Quantity)
) +
  geom_histogram(
    binwidth = 1,
    boundary = 0.5,
    color = "black"
  ) +
  scale_x_continuous(
    breaks = 1:10
  ) +
  labs(
    title = "Distribution of Transaction Quantity",
    subtitle = "Zoomed view: quantities from 1 to 10",
    x = "Quantity of Food Units",
    y = "Number of Transaction Lines"
  ) +
  theme_minimal()

ggsave(
  filename = "plots/02_quantity_distribution_zoomed.png",
  plot = p2,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# PLOT 3
# UNIT PRICE DISTRIBUTION — FULL
# ============================================================

p3 <- ggplot(
  visual_data,
  aes(x = unit_price)
) +
  geom_histogram(
    bins = 60,
    color = "black"
  ) +
  scale_x_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, 5)
  ) +
  labs(
    title = "Distribution of Unit Prices",
    subtitle = "Full observed price range",
    x = "Unit Price (€)",
    y = "Number of Transaction Lines"
  ) +
  theme_minimal()

ggsave(
  filename = "plots/03_unit_price_distribution_full.png",
  plot = p3,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# PLOT 4
# UNIT PRICE DISTRIBUTION — ZOOMED
# ============================================================

p4 <- ggplot(
  visual_data %>%
    filter(unit_price <= 5),
  aes(x = unit_price)
) +
  geom_histogram(
    binwidth = 0.10,
    color = "black"
  ) +
  scale_x_continuous(
    limits = c(0, 5),
    breaks = seq(0, 5, 0.5)
  ) +
  labs(
    title = "Distribution of Unit Prices",
    subtitle = "Zoomed view: prices from €0 to €5",
    x = "Unit Price (€)",
    y = "Number of Transaction Lines"
  ) +
  theme_minimal()

ggsave(
  filename = "plots/04_unit_price_distribution_zoomed.png",
  plot = p4,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# PLOT 5
# TOP 15 ARTICLES BY TRANSACTION RECORDS
# ============================================================

article_records <- visual_data %>%
  count(article, sort = TRUE) %>%
  slice_head(n = 15) %>%
  arrange(n)

p5 <- ggplot(
  article_records,
  aes(
    x = n,
    y = reorder(article, n)
  )
) +
  geom_col() +
  labs(
    title = "Top 15 Food Articles by Transaction Records",
    x = "Number of Transaction Records",
    y = "Food Article"
  ) +
  theme_minimal()

ggsave(
  filename = "plots/05_top_articles_by_transaction_records.png",
  plot = p5,
  width = 10,
  height = 7,
  dpi = 300
)


# ============================================================
# PLOT 6
# TOP 15 ARTICLES BY TOTAL FOOD UNITS SOLD
# ============================================================

article_demand <- visual_data %>%
  group_by(article) %>%
  summarise(
    total_quantity = sum(Quantity),
    .groups = "drop"
  ) %>%
  arrange(desc(total_quantity)) %>%
  slice_head(n = 15) %>%
  arrange(total_quantity)

p6 <- ggplot(
  article_demand,
  aes(
    x = total_quantity,
    y = reorder(article, total_quantity)
  )
) +
  geom_col() +
  labs(
    title = "Top 15 Food Articles by Total Quantity Sold",
    subtitle = "Total food units recorded across the study period",
    x = "Total Food Units Sold",
    y = "Food Article"
  ) +
  theme_minimal()

ggsave(
  filename = "plots/06_top_articles_by_total_quantity.png",
  plot = p6,
  width = 10,
  height = 7,
  dpi = 300
)


# ============================================================
# 2. DAILY DEMAND DATASET
# ============================================================

daily_demand <- visual_data %>%
  group_by(date) %>%
  summarise(
    total_quantity = sum(Quantity),
    transaction_records = n(),
    unique_tickets = n_distinct(ticket_number),
    .groups = "drop"
  ) %>%
  arrange(date)


# ============================================================
# PLOT 7
# DAILY FOOD DEMAND TIME SERIES
# ============================================================

p7 <- ggplot(
  daily_demand,
  aes(
    x = date,
    y = total_quantity
  )
) +
  geom_line(linewidth = 0.6) +
  labs(
    title = "Daily Food Demand",
    subtitle = "Total food units sold per observed day",
    x = "Date",
    y = "Total Food Units Sold"
  ) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = "plots/07_daily_food_demand_time_series.png",
  plot = p7,
  width = 12,
  height = 6,
  dpi = 300
)


# ============================================================
# PLOT 8
# DAILY FOOD DEMAND WITH MOVING AVERAGES
# ============================================================

daily_demand_ma <- daily_demand %>%
  arrange(date) %>%
  mutate(
    moving_average_7 = zoo::rollmean(
      total_quantity,
      k = 7,
      fill = NA,
      align = "right"
    ),
    moving_average_30 = zoo::rollmean(
      total_quantity,
      k = 30,
      fill = NA,
      align = "right"
    )
  )

p8 <- ggplot(
  daily_demand_ma,
  aes(x = date)
) +
  geom_line(
    aes(y = total_quantity),
    linewidth = 0.4
  ) +
  geom_line(
    aes(y = moving_average_7),
    linewidth = 0.8
  ) +
  geom_line(
    aes(y = moving_average_30),
    linewidth = 1.0
  ) +
  labs(
    title = "Daily Food Demand with Moving Averages",
    subtitle = "Daily demand compared with 7-day and 30-day moving averages",
    x = "Date",
    y = "Food Units Sold"
  ) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = "plots/08_daily_food_demand_moving_averages.png",
  plot = p8,
  width = 12,
  height = 6,
  dpi = 300
)


# ============================================================
# PLOT 9A
# DAILY FOOD DEMAND HISTOGRAM
# ============================================================

p9a <- ggplot(
  daily_demand,
  aes(x = total_quantity)
) +
  geom_histogram(
    bins = 30,
    color = "black"
  ) +
  labs(
    title = "Distribution of Daily Food Demand",
    x = "Total Food Units Sold per Day",
    y = "Number of Observed Days"
  ) +
  theme_minimal()

ggsave(
  filename = "plots/09a_daily_demand_histogram.png",
  plot = p9a,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# PLOT 9B
# DAILY FOOD DEMAND BOXPLOT
# ============================================================

p9b <- ggplot(
  daily_demand,
  aes(y = total_quantity)
) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Daily Food Demand",
    subtitle = "Distribution of total food units sold per observed day",
    x = "",
    y = "Total Food Units Sold per Day"
  ) +
  theme_minimal()

ggsave(
  filename = "plots/09b_daily_demand_boxplot.png",
  plot = p9b,
  width = 7,
  height = 7,
  dpi = 300
)


# ============================================================
# 3. DAY-OF-WEEK DEMAND
# ============================================================

weekday_demand <- visual_data %>%
  group_by(date, weekday) %>%
  summarise(
    daily_quantity = sum(Quantity),
    .groups = "drop"
  ) %>%
  group_by(weekday) %>%
  summarise(
    average_daily_demand = mean(daily_quantity),
    median_daily_demand = median(daily_quantity),
    .groups = "drop"
  ) %>%
  mutate(
    weekday = factor(
      weekday,
      levels = c(
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday"
      )
    )
  )


# ============================================================
# PLOT 10
# AVERAGE DAILY DEMAND BY DAY OF WEEK
# ============================================================

p10 <- ggplot(
  weekday_demand,
  aes(
    x = weekday,
    y = average_daily_demand
  )
) +
  geom_col() +
  labs(
    title = "Average Daily Food Demand by Day of Week",
    subtitle = "Average total food units sold per observed day",
    x = "Day of Week",
    y = "Average Food Units Sold"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  filename = "plots/10_average_demand_by_day_of_week.png",
  plot = p10,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# 4. HOURLY DEMAND
# ============================================================

hourly_demand <- visual_data %>%
  group_by(date, hour) %>%
  summarise(
    hourly_quantity = sum(Quantity),
    .groups = "drop"
  ) %>%
  group_by(hour) %>%
  summarise(
    average_hourly_demand = mean(hourly_quantity),
    median_hourly_demand = median(hourly_quantity),
    .groups = "drop"
  )


# ============================================================
# PLOT 11
# AVERAGE HOURLY FOOD DEMAND
# ============================================================

p11 <- ggplot(
  hourly_demand,
  aes(
    x = hour,
    y = average_hourly_demand
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(
      min(hourly_demand$hour),
      max(hourly_demand$hour),
      by = 1
    )
  ) +
  labs(
    title = "Average Hourly Food Demand",
    subtitle = "Average food units sold by hour of day",
    x = "Hour of Day",
    y = "Average Food Units Sold"
  ) +
  theme_minimal()

ggsave(
  filename = "plots/11_average_hourly_food_demand.png",
  plot = p11,
  width = 10,
  height = 6,
  dpi = 300
)


# ============================================================
# 5. DAY × HOUR DEMAND HEATMAP
# ============================================================

day_hour_demand <- visual_data %>%
  group_by(date, weekday, hour) %>%
  summarise(
    hourly_quantity = sum(Quantity),
    .groups = "drop"
  ) %>%
  group_by(weekday, hour) %>%
  summarise(
    average_demand = mean(hourly_quantity),
    .groups = "drop"
  ) %>%
  mutate(
    weekday = factor(
      weekday,
      levels = c(
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday"
      )
    )
  )


# ============================================================
# PLOT 12
# DAY × HOUR DEMAND HEATMAP
# ============================================================

p12 <- ggplot(
  day_hour_demand,
  aes(
    x = hour,
    y = weekday,
    fill = average_demand
  )
) +
  geom_tile() +
  scale_x_continuous(
    breaks = seq(
      min(day_hour_demand$hour),
      max(day_hour_demand$hour),
      by = 1
    )
  ) +
  labs(
    title = "Average Food Demand by Day and Hour",
    subtitle = "Average food units sold for each day-of-week and hour combination",
    x = "Hour of Day",
    y = "Day of Week",
    fill = "Average\nFood Units"
  ) +
  theme_minimal()

ggsave(
  filename = "plots/12_day_hour_demand_heatmap.png",
  plot = p12,
  width = 11,
  height = 7,
  dpi = 300
)


# ============================================================
# 6. DISPLAY PLOTS IN RSTUDIO
# ============================================================

print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
print(p7)
print(p8)
print(p9a)
print(p9b)
print(p10)
print(p11)
print(p12)


# ============================================================
# 7. FINAL PLOT SUMMARY
# ============================================================

cat("\n============================================================\n")
cat("VISUALIZATION COMPLETE\n")
cat("============================================================\n")

cat("Plots saved to:", file.path(getwd(), "plots"), "\n\n")

cat("Files created:\n")

cat("01_quantity_distribution_full.png\n")
cat("02_quantity_distribution_zoomed.png\n")
cat("03_unit_price_distribution_full.png\n")
cat("04_unit_price_distribution_zoomed.png\n")
cat("05_top_articles_by_transaction_records.png\n")
cat("06_top_articles_by_total_quantity.png\n")
cat("07_daily_food_demand_time_series.png\n")
cat("08_daily_food_demand_moving_averages.png\n")
cat("09a_daily_demand_histogram.png\n")
cat("09b_daily_demand_boxplot.png\n")
cat("10_average_demand_by_day_of_week.png\n")
cat("11_average_hourly_food_demand.png\n")
cat("12_day_hour_demand_heatmap.png\n")

cat("\nTotal visualization files: 13\n")

