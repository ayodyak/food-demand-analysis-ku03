# ============================================================
# IT3081 Statistical Modelling
# French Bakery Food Demand Analysis
# Outlier Detection
# ============================================================

library(tidyverse)

# ============================================================
# 1. QUANTITY OUTLIER DETECTION USING IQR
# ============================================================

cat("============================================================\n")
cat("QUANTITY OUTLIER DETECTION\n")
cat("============================================================\n")

Q1_quantity <- quantile(
  bakery_clean$Quantity,
  0.25,
  na.rm = TRUE
)

Q3_quantity <- quantile(
  bakery_clean$Quantity,
  0.75,
  na.rm = TRUE
)

IQR_quantity <- IQR(
  bakery_clean$Quantity,
  na.rm = TRUE
)

lower_quantity <- Q1_quantity - 1.5 * IQR_quantity
upper_quantity <- Q3_quantity + 1.5 * IQR_quantity

quantity_outliers <- bakery_clean %>%
  filter(
    Quantity < lower_quantity |
      Quantity > upper_quantity
  )

cat("Q1:", Q1_quantity, "\n")
cat("Q3:", Q3_quantity, "\n")
cat("IQR:", IQR_quantity, "\n")
cat("Lower bound:", lower_quantity, "\n")
cat("Upper bound:", upper_quantity, "\n")

cat(
  "Number of Quantity outliers:",
  nrow(quantity_outliers),
  "\n"
)

cat(
  "Percentage of Quantity outliers:",
  round(
    nrow(quantity_outliers) / nrow(bakery_clean) * 100,
    2
  ),
  "%\n"
)

cat(
  "Maximum Quantity among outliers:",
  max(quantity_outliers$Quantity, na.rm = TRUE),
  "\n"
)


# ============================================================
# 2. UNIT PRICE OUTLIER DETECTION USING IQR
# ============================================================

cat("\n============================================================\n")
cat("UNIT PRICE OUTLIER DETECTION\n")
cat("============================================================\n")

Q1_price <- quantile(
  bakery_clean$unit_price,
  0.25,
  na.rm = TRUE
)

Q3_price <- quantile(
  bakery_clean$unit_price,
  0.75,
  na.rm = TRUE
)

IQR_price <- IQR(
  bakery_clean$unit_price,
  na.rm = TRUE
)

lower_price <- Q1_price - 1.5 * IQR_price
upper_price <- Q3_price + 1.5 * IQR_price

price_outliers <- bakery_clean %>%
  filter(
    unit_price < lower_price |
      unit_price > upper_price
  )

cat("Q1:", Q1_price, "\n")
cat("Q3:", Q3_price, "\n")
cat("IQR:", IQR_price, "\n")
cat("Lower bound:", lower_price, "\n")
cat("Upper bound:", upper_price, "\n")

cat(
  "Number of unit price outliers:",
  nrow(price_outliers),
  "\n"
)

cat(
  "Percentage of unit price outliers:",
  round(
    nrow(price_outliers) / nrow(bakery_clean) * 100,
    2
  ),
  "%\n"
)

cat(
  "Maximum unit price among outliers:",
  max(price_outliers$unit_price, na.rm = TRUE),
  "\n"
)


# ============================================================
# 3. VIEW THE MOST EXTREME QUANTITY OBSERVATIONS
# ============================================================

cat("\n============================================================\n")
cat("LARGEST QUANTITY OBSERVATIONS\n")
cat("============================================================\n")

largest_quantity <- bakery_clean %>%
  arrange(desc(Quantity)) %>%
  select(
    date,
    time,
    ticket_number,
    article,
    Quantity,
    unit_price
  ) %>%
  slice_head(n = 20)

print(largest_quantity)


# ============================================================
# 4. VIEW THE HIGHEST PRICE OBSERVATIONS
# ============================================================

cat("\n============================================================\n")
cat("HIGHEST PRICE OBSERVATIONS\n")
cat("============================================================\n")

highest_prices <- bakery_clean %>%
  arrange(desc(unit_price)) %>%
  select(
    date,
    time,
    ticket_number,
    article,
    Quantity,
    unit_price
  ) %>%
  slice_head(n = 20)

print(highest_prices)


# ============================================================
# 5. QUANTITY OUTLIERS BY ARTICLE
# ============================================================

cat("\n============================================================\n")
cat("QUANTITY OUTLIERS BY ARTICLE\n")
cat("============================================================\n")

quantity_article_summary <- bakery_clean %>%
  group_by(article) %>%
  summarise(
    Q1 = quantile(Quantity, 0.25, na.rm = TRUE),
    Q3 = quantile(Quantity, 0.75, na.rm = TRUE),
    IQR = IQR(Quantity, na.rm = TRUE),
    upper_bound = Q3 + 1.5 * IQR,
    outlier_count = sum(
      Quantity > upper_bound,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(outlier_count))

print(
  head(quantity_article_summary, 20)
)


# ============================================================
# 6. UNIT PRICE OUTLIERS WITHIN EACH ARTICLE
# ============================================================

cat("\n============================================================\n")
cat("PRICE OUTLIERS WITHIN ARTICLE\n")
cat("============================================================\n")

price_article_summary <- bakery_clean %>%
  group_by(article) %>%
  summarise(
    Q1 = quantile(unit_price, 0.25, na.rm = TRUE),
    Q3 = quantile(unit_price, 0.75, na.rm = TRUE),
    IQR = IQR(unit_price, na.rm = TRUE),
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR,
    outlier_count = sum(
      unit_price < lower_bound |
        unit_price > upper_bound,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(outlier_count))

print(
  head(price_article_summary, 20)
)


# ============================================================
# 7. BOX PLOTS
# ============================================================

# Quantity boxplot
ggplot(
  bakery_clean,
  aes(y = Quantity)
) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Transaction Quantity",
    y = "Quantity"
  ) +
  theme_minimal()


# Unit price boxplot
ggplot(
  bakery_clean,
  aes(y = unit_price)
) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Unit Price",
    y = "Unit Price (€)"
  ) +
  theme_minimal()