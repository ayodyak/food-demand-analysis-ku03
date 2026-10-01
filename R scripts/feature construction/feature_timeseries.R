# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# Transformed Daily Demand Dataset
# ============================================================

library(tidyverse)
library(lubridate)
library(slider)

# ============================================================
# 1. Create a complete calendar
# ============================================================

complete_calendar <- tibble(
  date = seq(
    from = min(bakery_clean$date),
    to   = max(bakery_clean$date),
    by   = "day"
  )
)

# ============================================================
# 2. Calculate observed daily food demand
# ============================================================

daily_observed_demand <- bakery_clean %>%
  group_by(date) %>%
  summarise(
    daily_demand = sum(Quantity, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 3. Create NEW daily-demand dataset
# ============================================================
# bakery_clean is NOT modified.
#
# Dates without transaction records are initially NA.
# These dates will subsequently be treated as closure days.

daily_demand_data <- complete_calendar %>%
  left_join(
    daily_observed_demand,
    by = "date"
  )

# ============================================================
# 4. Identify closure / no-transaction dates
# ============================================================

closure_dates <- daily_demand_data %>%
  filter(is.na(daily_demand)) %>%
  pull(date)

cat("============================================================\n")
cat("CLOSURE / NO-TRANSACTION DATES\n")
cat("============================================================\n")

cat(
  "Number of dates without transaction records:",
  length(closure_dates),
  "\n"
)

# ============================================================
# 5. Treat closure dates as zero demand
# ============================================================
# Assumption:
# No transaction records on these dates represent
# non-operating / closure days.

daily_demand_data <- daily_demand_data %>%
  mutate(
    daily_demand = if_else(
      is.na(daily_demand),
      0,
      daily_demand
    )
  )

# ============================================================
# 6. Create calendar variables
# ============================================================

daily_demand_data <- daily_demand_data %>%
  mutate(
    
    # --------------------------------------------------------
    # Day of week
    # --------------------------------------------------------
    
    day_of_week = factor(
      weekdays(date),
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
    
    # --------------------------------------------------------
    # Week of year
    # --------------------------------------------------------
    
    week_of_year = isoweek(date),
    
    # --------------------------------------------------------
    # Month
    # --------------------------------------------------------
    
    month = factor(
      month(
        date,
        label = TRUE,
        abbr = FALSE
      ),
      levels = month(
        1:12,
        label = TRUE,
        abbr = FALSE
      )
    ),
    
    # --------------------------------------------------------
    # Year
    # --------------------------------------------------------
    
    year = factor(
      year(date)
    ),
    
    # --------------------------------------------------------
    # Weekend indicator
    # --------------------------------------------------------
    
    is_weekend = factor(
      if_else(
        wday(date, week_start = 1) >= 6,
        "Yes",
        "No"
      ),
      levels = c("No", "Yes")
    ),
    
    # --------------------------------------------------------
    # Sequential time index
    # --------------------------------------------------------
    
    time_index = row_number()
  )

# ============================================================
# 7. Create previous-day demand
# ============================================================
#
# Demand from the immediately preceding calendar day.
#
# Because closure days are now represented by zero,
# a closure yesterday produces:
#
# prev_day_demand = 0
#
# ============================================================

daily_demand_data <- daily_demand_data %>%
  mutate(
    prev_day_demand = lag(
      daily_demand,
      n = 1
    )
  )

# ============================================================
# 8. Create previous-week demand
# ============================================================
#
# Demand exactly seven calendar days earlier.
#
# ============================================================

daily_demand_data <- daily_demand_data %>%
  mutate(
    prev_week_demand = lag(
      daily_demand,
      n = 7
    )
  )

# ============================================================
# 9. Create previous 7-day average demand
# ============================================================
#
# Average demand across the previous seven calendar days.
#
# IMPORTANT:
# Current day's demand is NOT included.
#
# Example:
#
# Today's date = Day 10
#
# rolling average uses:
#
# Days 3, 4, 5, 6, 7, 8, 9
#
# ============================================================

daily_demand_data <- daily_demand_data %>%
  mutate(
    rolling_7_day_avg_demand = slider::slide_dbl(
      daily_demand,
      ~ mean(.x),
      .before = 7,
      .after = -1,
      .complete = FALSE
    )
  )

# ============================================================
# 10. Check transformed dataset
# ============================================================

cat("\n============================================================\n")
cat("TRANSFORMED DAILY DEMAND DATASET\n")
cat("============================================================\n")

cat(
  "Number of rows:",
  nrow(daily_demand_data),
  "\n"
)

cat(
  "Number of columns:",
  ncol(daily_demand_data),
  "\n"
)

cat(
  "First date:",
  as.character(min(daily_demand_data$date)),
  "\n"
)

cat(
  "Last date:",
  as.character(max(daily_demand_data$date)),
  "\n"
)

cat(
  "Total daily demand:",
  sum(daily_demand_data$daily_demand),
  "\n"
)

cat(
  "Number of closure days:",
  sum(daily_demand_data$daily_demand == 0),
  "\n"
)

cat(
  "Number of positive-demand days:",
  sum(daily_demand_data$daily_demand > 0),
  "\n"
)

# ============================================================
# 11. Display variable names
# ============================================================

cat("\n============================================================\n")
cat("VARIABLES\n")
cat("============================================================\n")

print(names(daily_demand_data))

# ============================================================
# 12. Display first 15 observations
# ============================================================

cat("\n============================================================\n")
cat("FIRST 15 OBSERVATIONS\n")
cat("============================================================\n")

print(
  daily_demand_data %>%
    slice_head(n = 15)
)

# ============================================================
# 13. Check observations around a closure day
# ============================================================

cat("\n============================================================\n")
cat("CHECK AROUND CLOSURE DATE\n")
cat("============================================================\n")

print(
  daily_demand_data %>%
    filter(
      date >= as.Date("2021-01-04") &
        date <= as.Date("2021-01-09")
    )
)

# ============================================================
# 14. Check closure dates
# ============================================================

cat("\n============================================================\n")
cat("CLOSURE DATE CHECK\n")
cat("============================================================\n")

print(
  daily_demand_data %>%
    filter(daily_demand == 0) %>%
    select(
      date,
      day_of_week,
      month,
      year,
      is_weekend,
      daily_demand
    ),
  n = Inf
)

# ============================================================
# 15. Check missing values
# ============================================================

cat("\n============================================================\n")
cat("MISSING VALUES\n")
cat("============================================================\n")

missing_summary <- sapply(
  daily_demand_data,
  function(x) sum(is.na(x))
)

print(missing_summary)

# ============================================================
# 16. Check zero-demand days
# ============================================================

cat("\n============================================================\n")
cat("ZERO-DEMAND DAYS\n")
cat("============================================================\n")

cat(
  "Zero-demand days:",
  sum(daily_demand_data$daily_demand == 0),
  "\n"
)

cat(
  "Percentage of calendar days represented by zero demand:",
  round(
    mean(daily_demand_data$daily_demand == 0) * 100,
    2
  ),
  "%\n"
)

# ============================================================
# 17. Descriptive statistics after transformation
# ============================================================

cat("\n============================================================\n")
cat("TRANSFORMED DAILY DEMAND STATISTICS\n")
cat("============================================================\n")

daily_demand_stats <- tibble(
  Statistic = c(
    "Count",
    "Mean",
    "Standard deviation",
    "Minimum",
    "First quartile",
    "Median",
    "Third quartile",
    "Maximum"
  ),
  
  Value = c(
    nrow(daily_demand_data),
    mean(daily_demand_data$daily_demand),
    sd(daily_demand_data$daily_demand),
    min(daily_demand_data$daily_demand),
    quantile(daily_demand_data$daily_demand, 0.25),
    median(daily_demand_data$daily_demand),
    quantile(daily_demand_data$daily_demand, 0.75),
    max(daily_demand_data$daily_demand)
  )
)

print(daily_demand_stats)

# ============================================================
# 18. Check lag variables
# ============================================================

cat("\n============================================================\n")
cat("LAG VARIABLE CHECK\n")
cat("============================================================\n")

lag_summary <- daily_demand_data %>%
  summarise(
    missing_prev_day = sum(is.na(prev_day_demand)),
    missing_prev_week = sum(is.na(prev_week_demand)),
    missing_rolling_7_day = sum(is.na(rolling_7_day_avg_demand))
  )

print(lag_summary)

# ============================================================
# 19. Check transformed dataset structure
# ============================================================

cat("\n============================================================\n")
cat("DATASET STRUCTURE\n")
cat("============================================================\n")

glimpse(daily_demand_data)

# ============================================================
# 20. Save transformed dataset
# ============================================================
#
# Save a separate CSV file.
# bakery_clean remains unchanged.

write_csv(
  daily_demand_data,
  "daily_demand_transformed.csv"
)

# ============================================================
# 21. Final confirmation
# ============================================================

cat("\n============================================================\n")
cat("TRANSFORMATION COMPLETE\n")
cat("============================================================\n")

cat(
  "Transformed dataset saved as:",
  file.path(getwd(), "daily_demand_transformed.csv"),
  "\n"
)

cat(
  "Original bakery_clean dataset was NOT modified.\n"
)

cat(
  "Final number of observations:",
  nrow(daily_demand_data),
  "\n"
)

cat(
  "Final number of variables:",
  ncol(daily_demand_data),
  "\n"
)