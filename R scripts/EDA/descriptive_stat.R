# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# Dataset Descriptive Statistics
# ============================================================

library(tidyverse)

# ============================================================
# 1. OVERALL DATASET INFORMATION
# ============================================================

cat("============================================================\n")
cat("OVERALL DATASET INFORMATION\n")
cat("============================================================\n")

cat("Number of records:", nrow(bakery_clean), "\n")
cat("Number of variables:", ncol(bakery_clean), "\n")

cat(
  "Total missing values:",
  sum(is.na(bakery_clean)),
  "\n"
)

cat(
  "Number of unique tickets:",
  n_distinct(bakery_clean$ticket_number),
  "\n"
)

cat(
  "Number of unique articles:",
  n_distinct(bakery_clean$article),
  "\n"
)


# ============================================================
# 2. DATE INFORMATION
# ============================================================

cat("\n============================================================\n")
cat("DATE INFORMATION\n")
cat("============================================================\n")

first_date <- min(bakery_clean$date, na.rm = TRUE)
last_date <- max(bakery_clean$date, na.rm = TRUE)

calendar_days <- as.numeric(last_date - first_date) + 1
observed_dates <- n_distinct(bakery_clean$date)
unobserved_dates <- calendar_days - observed_dates

cat(
  "First transaction date:",
  as.character(first_date),
  "\n"
)

cat(
  "Last transaction date:",
  as.character(last_date),
  "\n"
)

cat(
  "Calendar span (days):",
  calendar_days,
  "\n"
)

cat(
  "Observed transaction dates:",
  observed_dates,
  "\n"
)

cat(
  "Calendar dates with no records:",
  unobserved_dates,
  "\n"
)


# ============================================================
# 3. TIME INFORMATION
# ============================================================

cat("\n============================================================\n")
cat("TIME INFORMATION\n")
cat("============================================================\n")

first_time <- min(bakery_clean$time, na.rm = TRUE)
last_time <- max(bakery_clean$time, na.rm = TRUE)

cat(
  "Earliest transaction time:",
  as.character(first_time),
  "\n"
)

cat(
  "Latest transaction time:",
  as.character(last_time),
  "\n"
)

cat(
  "Unique transaction times:",
  n_distinct(bakery_clean$time),
  "\n"
)


# ============================================================
# 4. TICKET / ORDER INFORMATION
# ============================================================

cat("\n============================================================\n")
cat("TICKET / ORDER INFORMATION\n")
cat("============================================================\n")

ticket_summary <- bakery_clean %>%
  group_by(ticket_number) %>%
  summarise(
    records_per_ticket = n(),
    total_quantity = sum(Quantity),
    .groups = "drop"
  )

cat(
  "Number of unique tickets:",
  nrow(ticket_summary),
  "\n"
)

cat(
  "Average records per ticket:",
  round(mean(ticket_summary$records_per_ticket), 2),
  "\n"
)

cat(
  "Median records per ticket:",
  median(ticket_summary$records_per_ticket),
  "\n"
)

cat(
  "Minimum records per ticket:",
  min(ticket_summary$records_per_ticket),
  "\n"
)

cat(
  "Maximum records per ticket:",
  max(ticket_summary$records_per_ticket),
  "\n"
)

cat(
  "Average food units per ticket:",
  round(mean(ticket_summary$total_quantity), 2),
  "\n"
)

cat(
  "Median food units per ticket:",
  median(ticket_summary$total_quantity),
  "\n"
)

cat(
  "Maximum food units in a ticket:",
  max(ticket_summary$total_quantity),
  "\n"
)


# ============================================================
# 5. ARTICLE INFORMATION
# ============================================================

cat("\n============================================================\n")
cat("ARTICLE INFORMATION\n")
cat("============================================================\n")

article_frequency <- bakery_clean %>%
  count(article, sort = TRUE)

cat(
  "Number of unique articles:",
  nrow(article_frequency),
  "\n"
)

cat(
  "Most frequently recorded article:",
  article_frequency$article[1],
  "\n"
)

cat(
  "Records for most frequent article:",
  article_frequency$n[1],
  "\n"
)

cat("\nTop 15 articles by transaction records:\n")

print(
  head(article_frequency, 15)
)


# ============================================================
# 6. QUANTITY DESCRIPTIVE STATISTICS
# ============================================================

cat("\n============================================================\n")
cat("QUANTITY DESCRIPTIVE STATISTICS\n")
cat("============================================================\n")

quantity_stats <- tibble(
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
    nrow(bakery_clean),
    mean(bakery_clean$Quantity, na.rm = TRUE),
    sd(bakery_clean$Quantity, na.rm = TRUE),
    min(bakery_clean$Quantity, na.rm = TRUE),
    unname(quantile(bakery_clean$Quantity, 0.25, na.rm = TRUE)),
    median(bakery_clean$Quantity, na.rm = TRUE),
    unname(quantile(bakery_clean$Quantity, 0.75, na.rm = TRUE)),
    max(bakery_clean$Quantity, na.rm = TRUE)
  )
)

print(quantity_stats)


# ============================================================
# 7. UNIT PRICE DESCRIPTIVE STATISTICS
# ============================================================

cat("\n============================================================\n")
cat("UNIT PRICE DESCRIPTIVE STATISTICS\n")
cat("============================================================\n")

price_stats <- tibble(
  Statistic = c(
    "Count",
    "Mean",
    "Standard deviation",
    "Minimum",
    "First quartile",
    "Median",
    "Third quartile",
    "Maximum",
    "Number of unique prices"
  ),
  Value = c(
    nrow(bakery_clean),
    mean(bakery_clean$unit_price, na.rm = TRUE),
    sd(bakery_clean$unit_price, na.rm = TRUE),
    min(bakery_clean$unit_price, na.rm = TRUE),
    unname(quantile(bakery_clean$unit_price, 0.25, na.rm = TRUE)),
    median(bakery_clean$unit_price, na.rm = TRUE),
    unname(quantile(bakery_clean$unit_price, 0.75, na.rm = TRUE)),
    max(bakery_clean$unit_price, na.rm = TRUE),
    n_distinct(bakery_clean$unit_price)
  )
)

print(price_stats)


# ============================================================
# 8. ZERO-PRICE INFORMATION
# ============================================================

cat("\n============================================================\n")
cat("ZERO-PRICE INFORMATION\n")
cat("============================================================\n")

zero_price_count <- sum(
  bakery_clean$unit_price == 0,
  na.rm = TRUE
)

cat(
  "Zero-price records:",
  zero_price_count,
  "\n"
)

cat(
  "Percentage of records with zero price:",
  round(
    zero_price_count / nrow(bakery_clean) * 100,
    4
  ),
  "%\n"
)


# ============================================================
# 9. QUANTITY DISTRIBUTION
# ============================================================

cat("\n============================================================\n")
cat("QUANTITY DISTRIBUTION\n")
cat("============================================================\n")

cat(
  "Records with Quantity = 1:",
  sum(bakery_clean$Quantity == 1, na.rm = TRUE),
  "\n"
)

cat(
  "Records with Quantity > 1:",
  sum(bakery_clean$Quantity > 1, na.rm = TRUE),
  "\n"
)

cat(
  "Records with Quantity >= 10:",
  sum(bakery_clean$Quantity >= 10, na.rm = TRUE),
  "\n"
)

cat(
  "Maximum Quantity:",
  max(bakery_clean$Quantity, na.rm = TRUE),
  "\n"
)


# ============================================================
# 10. DATA TYPE SUMMARY
# ============================================================

cat("\n============================================================\n")
cat("DATA TYPE SUMMARY\n")
cat("============================================================\n")

variable_info <- tibble(
  Variable = names(bakery_clean),
  Class = sapply(
    bakery_clean,
    function(x) paste(class(x), collapse = ", ")
  ),
  Storage_Type = sapply(
    bakery_clean,
    typeof
  )
)

print(variable_info)


# ============================================================
# 11. DAILY RECORD INFORMATION
# ============================================================

cat("\n============================================================\n")
cat("RECORDS PER OBSERVED DATE\n")
cat("============================================================\n")

daily_records <- bakery_clean %>%
  group_by(date) %>%
  summarise(
    transaction_records = n(),
    unique_tickets = n_distinct(ticket_number),
    total_quantity = sum(Quantity),
    .groups = "drop"
  )

cat(
  "Average transaction records per observed day:",
  round(mean(daily_records$transaction_records), 2),
  "\n"
)

cat(
  "Median transaction records per observed day:",
  median(daily_records$transaction_records),
  "\n"
)

cat(
  "Minimum transaction records per observed day:",
  min(daily_records$transaction_records),
  "\n"
)

cat(
  "Maximum transaction records per observed day:",
  max(daily_records$transaction_records),
  "\n"
)

cat(
  "Average daily food units:",
  round(mean(daily_records$total_quantity), 2),
  "\n"
)

cat(
  "Median daily food units:",
  median(daily_records$total_quantity),
  "\n"
)

cat(
  "Minimum daily food units:",
  min(daily_records$total_quantity),
  "\n"
)

cat(
  "Maximum daily food units:",
  max(daily_records$total_quantity),
  "\n"
)


# ============================================================
# 12. FINAL DATASET SUMMARY
# ============================================================

cat("\n============================================================\n")
cat("FINAL DATASET SUMMARY\n")
cat("============================================================\n")

cat(
  "Records:",
  nrow(bakery_clean),
  "\n"
)

cat(
  "Variables:",
  ncol(bakery_clean),
  "\n"
)

cat(
  "Date range:",
  as.character(first_date),
  "to",
  as.character(last_date),
  "\n"
)

cat(
  "Observed dates:",
  observed_dates,
  "\n"
)

cat(
  "Unique tickets:",
  n_distinct(bakery_clean$ticket_number),
  "\n"
)

cat(
  "Unique articles:",
  n_distinct(bakery_clean$article),
  "\n"
)

cat(
  "Total food units recorded:",
  sum(bakery_clean$Quantity, na.rm = TRUE),
  "\n"
)

cat(
  "Average quantity per transaction line:",
  round(mean(bakery_clean$Quantity, na.rm = TRUE), 2),
  "\n"
)

cat(
  "Average unit price:",
  round(mean(bakery_clean$unit_price, na.rm = TRUE), 2),
  "\n"
)