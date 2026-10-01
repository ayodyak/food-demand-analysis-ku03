# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# TASK 5 - PREDICTIVE STATISTICAL MODELLING
# STEP 1 - DATA LOADING AND PREPARATION
# ============================================================

library(tidyverse)
library(car)
library(glmnet)

# ============================================================
# 1. PROJECT FOLDERS
# ============================================================

# Create output folder if it does not already exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Create plots folder if it does not already exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# ============================================================
# 2. LOAD TASK 5 DATASET
# ============================================================

model_data <- read_csv(
  "data/daily_demand_model_data.csv",
  show_col_types = FALSE
)

# ============================================================
# 3. BASIC DATA CHECK
# ============================================================

cat("============================================================\n")
cat("TASK 5 - PREDICTIVE STATISTICAL MODELLING\n")
cat("STEP 1 - DATA PREPARATION\n")
cat("============================================================\n\n")

cat("Number of observations:", nrow(model_data), "\n")
cat("Number of variables:", ncol(model_data), "\n\n")

cat("VARIABLE NAMES\n")
print(names(model_data))

cat("\n------------------------------------------------------------\n")
cat("DATA STRUCTURE\n")
cat("------------------------------------------------------------\n")

str(model_data)

# ============================================================
# 4. CHECK MISSING VALUES
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("MISSING VALUE CHECK\n")
cat("------------------------------------------------------------\n")

missing_summary <- model_data %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  )

print(missing_summary)

# ============================================================
# 5. CHECK DUPLICATE RECORDS
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("DUPLICATE RECORD CHECK\n")
cat("------------------------------------------------------------\n")

duplicate_count <- sum(duplicated(model_data))

cat("Number of duplicate rows:", duplicate_count, "\n")

# ============================================================
# 6. CONVERT CATEGORICAL VARIABLES TO FACTORS
# ============================================================

model_data <- model_data %>%
  mutate(
    day_of_week = factor(
      day_of_week,
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
    
    month = factor(
      month,
      levels = c(
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December"
      ),
      ordered = TRUE
    ),
    
    is_weekend = factor(
      is_weekend,
      levels = c("No", "Yes")
    )
  )

# ============================================================
# 7. VERIFY VARIABLE TYPES
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("VARIABLE TYPES AFTER PREPARATION\n")
cat("------------------------------------------------------------\n")

str(model_data)

# ============================================================
# 8. RESPONSE VARIABLE CHECK
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("RESPONSE VARIABLE: DAILY DEMAND\n")
cat("------------------------------------------------------------\n")

cat("Minimum demand:",
    min(model_data$daily_demand),
    "\n")

cat("Maximum demand:",
    max(model_data$daily_demand),
    "\n")

cat("Mean demand:",
    round(mean(model_data$daily_demand), 2),
    "\n")

cat("Median demand:",
    median(model_data$daily_demand),
    "\n")

cat("Standard deviation:",
    round(sd(model_data$daily_demand), 2),
    "\n")

# ============================================================
# 9. PREDICTOR SUMMARY
# ============================================================

cat("\n------------------------------------------------------------\n")
cat("PREDICTOR VARIABLES\n")
cat("------------------------------------------------------------\n")

cat("Response variable:\n")
cat("  daily_demand\n\n")

cat("Potential predictors:\n")
cat("  day_of_week\n")
cat("  week_of_year\n")
cat("  month\n")
cat("  year\n")
cat("  is_weekend\n")
cat("  time_index\n")
cat("  prev_day_demand\n")
cat("  prev_week_demand\n")
cat("  rolling_7_day_avg_demand\n")

# ============================================================
# 10. SAVE PREPARED DATASET
# ============================================================

write_csv(
  model_data,
  "output/task5_model_data_prepared.csv"
)

cat("\n------------------------------------------------------------\n")
cat("DATA PREPARATION COMPLETE\n")
cat("------------------------------------------------------------\n")

cat(
  "Prepared dataset saved to:\n",
  file.path(getwd(), "output/task5_model_data_prepared.csv"),
  "\n"
)

cat("\nOriginal dataset was NOT modified.\n")