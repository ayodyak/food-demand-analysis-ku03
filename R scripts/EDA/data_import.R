# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# Task 3: Dataset Import and Variable Type Inspection
# ============================================================

# ------------------------------------------------------------
# 1. Load required package
# ------------------------------------------------------------

library(tidyverse)

# ------------------------------------------------------------
# 2. Import the raw dataset
# ------------------------------------------------------------

bakery_raw <- read_csv(
  "data/Bakery sales.csv",
  show_col_types = TRUE
)

# ------------------------------------------------------------
# 3. Check dataset dimensions
# ------------------------------------------------------------

cat("Number of rows:", nrow(bakery_raw), "\n")
cat("Number of columns:", ncol(bakery_raw), "\n")

# ------------------------------------------------------------
# 4. View variable names
# ------------------------------------------------------------

names(bakery_raw)

# ------------------------------------------------------------
# 5. View the first few observations
# ------------------------------------------------------------

head(bakery_raw)

# ------------------------------------------------------------
# 6. Inspect variable classes
# ------------------------------------------------------------

sapply(bakery_raw, class)

# ------------------------------------------------------------
# 7. Inspect variable storage types
# ------------------------------------------------------------

sapply(bakery_raw, typeof)

# ------------------------------------------------------------
# 8. Detailed structure of the dataset
# ------------------------------------------------------------

str(bakery_raw)

# ------------------------------------------------------------
# 9. Tidyverse overview of the dataset
# ------------------------------------------------------------

glimpse(bakery_raw)

# ------------------------------------------------------------
# 10. Statistical summary
# ------------------------------------------------------------

summary(bakery_raw)

# ============================================================
# Variable Type Conversion Only
# ============================================================

# Create a copy of the raw dataset
bakery_typed <- bakery_raw

# Remove the original source/index column
bakery_typed <- bakery_typed %>%
  select(-`...1`)

# Convert ticket number from numeric to character
bakery_typed$ticket_number <- as.character(bakery_typed$ticket_number)

# Convert Quantity from numeric to integer
bakery_typed$Quantity <- as.integer(bakery_typed$Quantity)

# Convert unit_price from text such as "0,90 €" to numeric
bakery_typed$unit_price <- parse_number(
  bakery_typed$unit_price,
  locale = locale(decimal_mark = ",")
)

# Check the resulting variable types
sapply(bakery_typed, class)

# View the first 10 rows
head(bakery_typed, 10)

# View the last 10 rows
tail(bakery_typed, 10)

# Open the complete dataset in RStudio's data viewer
View(bakery_typed)