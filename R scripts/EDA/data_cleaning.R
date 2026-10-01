# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# Data Cleaning
# ============================================================

library(tidyverse)

# ============================================================
# 1. Create a copy of the validated dataset
# ============================================================

bakery_clean <- bakery_typed

# ============================================================
# 2. Record the original number of rows
# ============================================================

rows_before <- nrow(bakery_clean)

# ============================================================
# 3. Remove exact duplicate rows
# ============================================================

bakery_clean <- bakery_clean %>%
  distinct()

rows_after_duplicates <- nrow(bakery_clean)

duplicates_removed <- rows_before - rows_after_duplicates

# ============================================================
# 4. Remove negative quantity records
# ============================================================

negative_records_removed <- sum(
  bakery_clean$Quantity < 0,
  na.rm = TRUE
)

bakery_clean <- bakery_clean %>%
  filter(Quantity >= 0)

rows_after_cleaning <- nrow(bakery_clean)

# ============================================================
# 5. Cleaning summary
# ============================================================

cat("===== CLEANING SUMMARY =====\n")

cat(
  "Rows before cleaning:",
  rows_before,
  "\n"
)

cat(
  "Duplicate rows removed:",
  duplicates_removed,
  "\n"
)

cat(
  "Negative quantity records removed:",
  negative_records_removed,
  "\n"
)

cat(
  "Rows after cleaning:",
  rows_after_cleaning,
  "\n"
)

cat(
  "Total rows removed:",
  rows_before - rows_after_cleaning,
  "\n"
)

cat(
  "Percentage of rows retained:",
  round((rows_after_cleaning / rows_before) * 100, 2),
  "%\n"
)

# ============================================================
# 6. Validate the cleaned dataset
# ============================================================

cat("\n===== POST-CLEANING VALIDATION =====\n")

cat(
  "Remaining duplicate rows:",
  sum(duplicated(bakery_clean)),
  "\n"
)

cat(
  "Remaining negative quantities:",
  sum(bakery_clean$Quantity < 0, na.rm = TRUE),
  "\n"
)

cat(
  "Remaining zero quantities:",
  sum(bakery_clean$Quantity == 0, na.rm = TRUE),
  "\n"
)

cat(
  "Maximum positive quantity:",
  max(bakery_clean$Quantity, na.rm = TRUE),
  "\n"
)