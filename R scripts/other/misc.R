# ============================================================
# Save cleaned dataset
# ============================================================

write_csv(
  bakery_clean,
  "data/Bakery_sales_cleaned.csv"
)

cat(
  "Cleaned dataset saved to: data/Bakery_sales_cleaned.csv\n"
)

# Read the cleaned CSV
bakery_clean_csv <- read_csv(
  "data/Bakery_sales_cleaned.csv",
  show_col_types = FALSE
)

# View it in RStudio
View(bakery_clean_csv)

# ============================================================
# Export transformed daily demand dataset
# ============================================================

# Create data folder if it does not already exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Export transformed dataset
write_csv(
  daily_demand_data,
  "data/daily_demand_data.csv"
)

# Confirm export
cat("============================================================\n")
cat("TRANSFORMED DATASET EXPORTED\n")
cat("============================================================\n")

# ============================================================
# Export final modelling dataset
# ============================================================

if (!dir.exists("data")) {
  dir.create("data")
}

write_csv(
  daily_demand_model_data,
  "data/daily_demand_model_data.csv"
)

cat("\n============================================================\n")
cat("MODELLING DATASET EXPORTED\n")
cat("============================================================\n")

