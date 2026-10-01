# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# Data Quality Validation
# ============================================================

library(tidyverse)

# ============================================================
# 1. Dataset overview
# ============================================================

cat("===== DATASET OVERVIEW =====\n")

cat("Rows:", nrow(bakery_typed), "\n")
cat("Columns:", ncol(bakery_typed), "\n")


# ============================================================
# 2. Missing values
# ============================================================

cat("\n===== MISSING VALUES =====\n")

missing_by_variable <- sapply(
  bakery_typed,
  function(x) sum(is.na(x))
)

print(missing_by_variable)

cat(
  "Total missing values:",
  sum(missing_by_variable),
  "\n"
)


# ============================================================
# 3. Blank / empty article values
# ============================================================

cat("\n===== ARTICLE VALIDATION =====\n")

blank_articles <- sum(
  is.na(bakery_typed$article) |
    trimws(bakery_typed$article) == ""
)

cat(
  "Blank or empty article values:",
  blank_articles,
  "\n"
)

cat(
  "Unique article values:",
  n_distinct(bakery_typed$article),
  "\n"
)


# ============================================================
# 4. Exact duplicate rows
# ============================================================

cat("\n===== DUPLICATE VALIDATION =====\n")

exact_duplicates <- sum(duplicated(bakery_typed))

cat(
  "Exact duplicate rows:",
  exact_duplicates,
  "\n"
)


# ============================================================
# 5. Date validation
# ============================================================

cat("\n===== DATE VALIDATION =====\n")

cat(
  "Missing dates:",
  sum(is.na(bakery_typed$date)),
  "\n"
)

cat(
  "Minimum date:",
  as.character(min(bakery_typed$date, na.rm = TRUE)),
  "\n"
)

cat(
  "Maximum date:",
  as.character(max(bakery_typed$date, na.rm = TRUE)),
  "\n"
)

cat(
  "Unique observed dates:",
  n_distinct(bakery_typed$date),
  "\n"
)


# ============================================================
# 6. Time validation
# ============================================================

cat("\n===== TIME VALIDATION =====\n")

cat(
  "Missing times:",
  sum(is.na(bakery_typed$time)),
  "\n"
)

cat(
  "Minimum recorded time:",
  as.character(min(bakery_typed$time, na.rm = TRUE)),
  "\n"
)

cat(
  "Maximum recorded time:",
  as.character(max(bakery_typed$time, na.rm = TRUE)),
  "\n"
)

cat(
  "Unique recorded times:",
  n_distinct(bakery_typed$time),
  "\n"
)


# ============================================================
# 7. Ticket number validation
# ============================================================

cat("\n===== TICKET NUMBER VALIDATION =====\n")

cat(
  "Missing ticket numbers:",
  sum(is.na(bakery_typed$ticket_number)),
  "\n"
)

cat(
  "Unique tickets:",
  n_distinct(bakery_typed$ticket_number),
  "\n"
)

# Check for blank ticket numbers
blank_tickets <- sum(
  is.na(bakery_typed$ticket_number) |
    trimws(bakery_typed$ticket_number) == ""
)

cat(
  "Blank ticket numbers:",
  blank_tickets,
  "\n"
)


# ============================================================
# 8. Quantity validation
# ============================================================

cat("\n===== QUANTITY VALIDATION =====\n")

cat(
  "Missing quantities:",
  sum(is.na(bakery_typed$Quantity)),
  "\n"
)

cat(
  "Minimum quantity:",
  min(bakery_typed$Quantity, na.rm = TRUE),
  "\n"
)

cat(
  "Maximum quantity:",
  max(bakery_typed$Quantity, na.rm = TRUE),
  "\n"
)

cat(
  "Zero quantities:",
  sum(bakery_typed$Quantity == 0, na.rm = TRUE),
  "\n"
)

cat(
  "Negative quantities:",
  sum(bakery_typed$Quantity < 0, na.rm = TRUE),
  "\n"
)

cat(
  "Large positive quantities (>= 10):",
  sum(bakery_typed$Quantity >= 10, na.rm = TRUE),
  "\n"
)


# ============================================================
# 9. Unit price validation
# ============================================================

cat("\n===== UNIT PRICE VALIDATION =====\n")

cat(
  "Missing unit prices:",
  sum(is.na(bakery_typed$unit_price)),
  "\n"
)

cat(
  "Minimum unit price:",
  min(bakery_typed$unit_price, na.rm = TRUE),
  "\n"
)

cat(
  "Maximum unit price:",
  max(bakery_typed$unit_price, na.rm = TRUE),
  "\n"
)

cat(
  "Zero-price records:",
  sum(bakery_typed$unit_price == 0, na.rm = TRUE),
  "\n"
)

cat(
  "Negative-price records:",
  sum(bakery_typed$unit_price < 0, na.rm = TRUE),
  "\n"
)


# ============================================================
# 10. Check article names for leading/trailing spaces
# ============================================================

cat("\n===== ARTICLE FORMAT VALIDATION =====\n")

leading_trailing_spaces <- sum(
  bakery_typed$article != trimws(bakery_typed$article),
  na.rm = TRUE
)

cat(
  "Articles with leading/trailing spaces:",
  leading_trailing_spaces,
  "\n"
)


# ============================================================
# 11. Check for suspicious duplicate transaction combinations
# ============================================================

cat("\n===== REPEATED TRANSACTION COMBINATIONS =====\n")

repeated_combinations <- bakery_typed %>%
  group_by(
    date,
    time,
    ticket_number,
    article,
    Quantity,
    unit_price
  ) %>%
  summarise(
    frequency = n(),
    .groups = "drop"
  ) %>%
  filter(frequency > 1)

cat(
  "Repeated identical transaction combinations:",
  nrow(repeated_combinations),
  "\n"
)

print(head(repeated_combinations, 10))


# ============================================================
# 12. Check tickets appearing on multiple dates
# ============================================================

cat("\n===== TICKET CONSISTENCY =====\n")

tickets_multiple_dates <- bakery_typed %>%
  group_by(ticket_number) %>%
  summarise(
    number_of_dates = n_distinct(date),
    .groups = "drop"
  ) %>%
  filter(number_of_dates > 1)

cat(
  "Tickets appearing on multiple dates:",
  nrow(tickets_multiple_dates),
  "\n"
)


# ============================================================
# 13. Check tickets appearing at multiple times
# ============================================================

tickets_multiple_times <- bakery_typed %>%
  group_by(ticket_number) %>%
  summarise(
    number_of_times = n_distinct(time),
    .groups = "drop"
  ) %>%
  filter(number_of_times > 1)

cat(
  "Tickets appearing at multiple times:",
  nrow(tickets_multiple_times),
  "\n"
)


# ============================================================
# 14. Overall data quality summary
# ============================================================

cat("\n===== DATA QUALITY SUMMARY =====\n")

cat(
  "Total rows:", nrow(bakery_typed), "\n"
)

cat(
  "Total columns:", ncol(bakery_typed), "\n"
)

cat(
  "Total missing values:",
  sum(is.na(bakery_typed)),
  "\n"
)

cat(
  "Exact duplicate rows:",
  sum(duplicated(bakery_typed)),
  "\n"
)

cat(
  "Negative quantities:",
  sum(bakery_typed$Quantity < 0, na.rm = TRUE),
  "\n"
)

cat(
  "Zero quantities:",
  sum(bakery_typed$Quantity == 0, na.rm = TRUE),
  "\n"
)

cat(
  "Negative prices:",
  sum(bakery_typed$unit_price < 0, na.rm = TRUE),
  "\n"
)

cat(
  "Zero prices:",
  sum(bakery_typed$unit_price == 0, na.rm = TRUE),
  "\n"
)