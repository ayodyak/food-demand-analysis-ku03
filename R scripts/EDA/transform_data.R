# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# Daily Demand Dataset Transformation
# ============================================================
install.packages("slider")
library(tidyverse)
library(lubridate)

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
# 2. Calculate daily food demand from bakery_clean
# ============================================================

daily_observed_demand <- bakery_clean %>%
  group_by(date) %>%
  summarise(
    daily_demand = sum(Quantity, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 3. Create the new daily-demand dataset
#    bakery_clean is NOT modified
# ============================================================

daily_demand_data <- complete_calendar %>%
  left_join(
    daily_observed_demand,
    by = "date"
  )

# ============================================================
# 4. Create calendar variables
# ============================================================

daily_demand_data <- daily_demand_data %>%
  mutate(
    
    # Day of week
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
    
    # Week number
    week_of_year = isoweek(date),
    
    # Month
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
    
    # Year
    year = factor(
      year(date)
    ),
    
    # Weekend indicator
    is_weekend = if_else(
      wday(date, week_start = 1) >= 6,
      "Yes",
      "No"
    ),
    
    # Sequential time trend
    time_index = row_number()
  )

# Convert weekend to factor
daily_demand_data$is_weekend <- factor(
  daily_demand_data$is_weekend,
  levels = c("No", "Yes")
)

# ============================================================
# 5. Create previous-day demand
# ============================================================
# This uses the immediately previous calendar date.
# If that date has no observed demand, the result is NA.

daily_demand_data <- daily_demand_data %>%
  mutate(
    prev_day_demand = lag(
      daily_demand,
      n = 1
    )
  )

# ============================================================
# 6. Create previous-week demand
# ============================================================
# Demand exactly 7 calendar days earlier.

daily_demand_data <- daily_demand_data %>%
  mutate(
    prev_week_demand = lag(
      daily_demand,
      n = 7
    )
  )

# ============================================================
# 7. Create previous 7-day average demand
# ============================================================
# IMPORTANT:
# The current day's demand is NOT included.
#
# The average is calculated from the previous 7 calendar days.
# At least 1 observed value is required.

daily_demand_data <- daily_demand_data %>%
  mutate(
    rolling_7_day_avg_demand = slider::slide_dbl(
      daily_demand,
      ~ if (all(is.na(.x))) {
        NA_real_
      } else {
        mean(.x, na.rm = TRUE)
      },
      .before = 7,
      .after = -1,
      .complete = FALSE
    )
  )

# ============================================================
# 8. Check the transformed dataset
# ============================================================

cat("============================================================\n")
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
  "Observed demand days:",
  sum(!is.na(daily_demand_data$daily_demand)),
  "\n"
)

cat(
  "Days with no transaction records:",
  sum(is.na(daily_demand_data$daily_demand)),
  "\n"
)

# ============================================================
# 9. Display variable names
# ============================================================

cat("\n============================================================\n")
cat("VARIABLES\n")
cat("============================================================\n")

print(names(daily_demand_data))

# ============================================================
# 10. Display first 15 observations
# ============================================================

cat("\n============================================================\n")
cat("FIRST 15 OBSERVATIONS\n")
cat("============================================================\n")

print(
  daily_demand_data %>%
    slice_head(n = 15)
)

# ============================================================
# 11. Display observations around a missing date
# ============================================================

cat("\n============================================================\n")
cat("CHECK AROUND MISSING DATE\n")
cat("============================================================\n")

print(
  daily_demand_data %>%
    filter(
      date >= as.Date("2021-01-04") &
        date <= as.Date("2021-01-09")
    )
)

# ============================================================
# 12. Check missing values in the transformed variables
# ============================================================

cat("\n============================================================\n")
cat("MISSING VALUES\n")
cat("============================================================\n")

print(
  sapply(
    daily_demand_data,
    function(x) sum(is.na(x))
  )
)

# ============================================================
# 13. Final structure
# ============================================================

cat("\n============================================================\n")
cat("DATASET STRUCTURE\n")
cat("============================================================\n")

glimpse(daily_demand_data)