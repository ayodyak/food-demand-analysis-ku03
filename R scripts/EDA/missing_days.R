# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# Identify Missing Calendar Dates
# ============================================================

library(tidyverse)

# ============================================================
# 1. Create the complete calendar
# ============================================================

complete_calendar <- tibble(
  date = seq(
    from = min(bakery_clean$date),
    to   = max(bakery_clean$date),
    by   = "day"
  )
)

# ============================================================
# 2. Identify dates with no transaction records
# ============================================================

missing_transaction_dates <- complete_calendar %>%
  anti_join(
    bakery_clean %>%
      distinct(date),
    by = "date"
  )

# ============================================================
# 3. Display the missing dates
# ============================================================

cat("============================================================\n")
cat("MISSING CALENDAR DATES\n")
cat("============================================================\n")

cat(
  "Total calendar days:",
  nrow(complete_calendar),
  "\n"
)

cat(
  "Observed transaction dates:",
  n_distinct(bakery_clean$date),
  "\n"
)

cat(
  "Dates with no transaction records:",
  nrow(missing_transaction_dates),
  "\n\n"
)

print(missing_transaction_dates)

# ============================================================
# Check weekday pattern of missing dates
# ============================================================

missing_transaction_dates %>%
  mutate(
    day_of_week = weekdays(date)
  ) %>%
  count(day_of_week, sort = TRUE)

# Show all missing dates
print(
  missing_transaction_dates %>%
    mutate(
      day_of_week = weekdays(date)
    ),
  n = Inf
)