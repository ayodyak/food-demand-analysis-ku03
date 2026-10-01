# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# Create Final Modelling Dataset
# ============================================================

# Remove the first 7 calendar days because
# prev_week_demand cannot be calculated for them.
#
# daily_demand_data is NOT modified.

daily_demand_model_data <- daily_demand_data %>%
  slice(8:n())

# ============================================================
# Check the resulting dataset
# ============================================================

cat("============================================================\n")
cat("FINAL MODELLING DATASET\n")
cat("============================================================\n")

cat(
  "Rows after removing first 7 days:",
  nrow(daily_demand_model_data),
  "\n"
)

cat(
  "Columns:",
  ncol(daily_demand_model_data),
  "\n"
)

cat(
  "First date:",
  as.character(min(daily_demand_model_data$date)),
  "\n"
)

cat(
  "Last date:",
  as.character(max(daily_demand_model_data$date)),
  "\n"
)

# ============================================================
# Check missing values in modelling variables
# ============================================================

cat("\n============================================================\n")
cat("MISSING VALUES IN MODELLING VARIABLES\n")
cat("============================================================\n")

print(
  sapply(
    daily_demand_model_data,
    function(x) sum(is.na(x))
  )
)

# ============================================================
# Display first observations
# ============================================================

cat("\n============================================================\n")
cat("FIRST 10 OBSERVATIONS OF MODELLING DATASET\n")
cat("============================================================\n")

print(
  daily_demand_model_data %>%
    slice_head(n = 10)
)