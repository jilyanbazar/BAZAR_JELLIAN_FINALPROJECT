pkgs <- c("readxl", "dplyr", "ggplot2", "readr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

DATA_FILE <- if (file.exists("DRUGSTORE_DATASET_1.xlsx")) {
  "DRUGSTORE_DATASET_1.xlsx"
} else {
  "DRUGSTORE_DATASET.xlsx"
}
LOW_STOCK    <- 60    # units at or below this = "Low"
EXPIRY_SOON  <- 180   # days; expiring within this = "Expiring soon"
TODAY        <- Sys.Date()
dir.create("outputs", showWarnings = FALSE)

# The Expiry_Date column mixes text dates ("8/15/2027") with raw Excel
# serial numbers (46390). This converts both into proper Dates.
parse_expiry <- function(x) {
  x <- trimws(as.character(x))
  is_serial <- grepl("^[0-9]+(\\.[0-9]+)?$", x)
  out <- rep(as.Date(NA), length(x))
  out[is_serial]  <- as.Date(as.numeric(x[is_serial]), origin = "1899-12-30")
  out[!is_serial] <- as.Date(x[!is_serial], format = "%m/%d/%Y")
  out
}

drug <- read_excel(
  DATA_FILE,
  col_types = c("text", "text", "text", "text",
                "numeric", "numeric", "text", "text")
) %>%
  mutate(
    Expiry_Date    = parse_expiry(Expiry_Date),
    Rx_Required    = trimws(Rx_Required) == "Yes",
    Stock_Value    = Unit_Price_USD * Stock_Qty,
    Days_To_Expiry = as.numeric(Expiry_Date - TODAY),
    Stock_Status   = case_when(
      Stock_Qty <= LOW_STOCK     ~ "Low",
      Stock_Qty <= LOW_STOCK * 2 ~ "Medium",
      TRUE                       ~ "Healthy"
    ),
    Expiry_Status  = case_when(
      Days_To_Expiry < 0            ~ "Expired",
      Days_To_Expiry <= EXPIRY_SOON ~ "Expiring soon",
      TRUE                          ~ "OK"
    )
  )

cat("\n--- Data check ---\n")
cat("Rows:", nrow(drug), "| Missing values:", sum(is.na(drug)), "\n")
cat("Unique Product_IDs:", n_distinct(drug$Product_ID) == nrow(drug), "\n")
glimpse(drug)

overview <- tibble(
  Measure = c("Products", "Total units", "Stock value (USD)",
              "Prescription-only products", "Low-stock products",
              "Expired products", "Expiring soon"),
  Value = c(nrow(drug),
            sum(drug$Stock_Qty),
            round(sum(drug$Stock_Value), 2),
            sum(drug$Rx_Required),
            sum(drug$Stock_Status == "Low"),
            sum(drug$Expiry_Status == "Expired"),
            sum(drug$Expiry_Status == "Expiring soon"))
)
cat("\n--- Overview ---\n"); print(overview)

by_category <- drug %>%
  group_by(Category) %>%
  summarise(Products = n(),
            Units = sum(Stock_Qty),
            Value_USD = round(sum(Stock_Value), 2),
            Avg_Price = round(mean(Unit_Price_USD), 2),
            .groups = "drop") %>%
  arrange(desc(Value_USD))

by_brand <- drug %>%
  group_by(Brand) %>%
  summarise(Products = n(), Units = sum(Stock_Qty),
            Value_USD = round(sum(Stock_Value), 2), .groups = "drop") %>%
  arrange(desc(Units))

rx_split <- drug %>%
  group_by(Rx_Required) %>%
  summarise(Products = n(), Units = sum(Stock_Qty),
            Value_USD = round(sum(Stock_Value), 2), .groups = "drop")

low_stock     <- drug %>% filter(Stock_Status == "Low") %>% arrange(Stock_Qty)
expiring_soon <- drug %>% filter(Expiry_Status != "OK")  %>% arrange(Expiry_Date)
top_value     <- drug %>% arrange(desc(Stock_Value)) %>% slice_head(n = 5)

cat("\n--- By category ---\n");   print(by_category)
cat("\n--- By brand ---\n");      print(by_brand)
cat("\n--- Rx vs non-Rx ---\n");  print(rx_split)
cat("\n--- Low stock ---\n");     print(select(low_stock, Product_ID, Product_Name, Stock_Qty))
cat("\n--- Expiring soon/expired ---\n")
print(select(expiring_soon, Product_ID, Product_Name, Expiry_Date, Days_To_Expiry))
cat("\n--- Top 5 by stock value ---\n")
print(select(top_value, Product_Name, Stock_Value))

p1 <- ggplot(by_category, aes(reorder(Category, Value_USD), Value_USD)) +
  geom_col(fill = "#2a9d8f") + coord_flip() +
  labs(title = "Stock value by category", x = NULL, y = "USD") +
  theme_minimal(base_size = 13)

p2 <- ggplot(drug, aes(reorder(Product_Name, Stock_Qty), Stock_Qty,
                       fill = Stock_Status)) +
  geom_col() + coord_flip() +
  geom_hline(yintercept = LOW_STOCK, linetype = "dashed") +
  scale_fill_manual(values = c(Low = "#e63946", Medium = "#f4a261",
                               Healthy = "#2a9d8f")) +
  labs(title = "Stock level by product", x = NULL, y = "Units", fill = "Status") +
  theme_minimal(base_size = 13)

p3 <- ggplot(drug, aes(Expiry_Date, reorder(Product_Name, Expiry_Date),
                       colour = Rx_Required)) +
  geom_point(size = 3) +
  geom_vline(xintercept = as.numeric(TODAY), linetype = "dashed") +
  scale_colour_manual(values = c(`TRUE` = "#e63946", `FALSE` = "#457b9d"),
                      labels = c(`TRUE` = "Rx required", `FALSE` = "Over the counter")) +
  labs(title = "Expiry dates (dashed line = today)", x = "Expiry date",
       y = NULL, colour = NULL) +
  theme_minimal(base_size = 13)

p4 <- ggplot(drug, aes(Unit_Price_USD, Stock_Qty, colour = Category,
                       label = Product_ID)) +
  geom_point(size = 3) + geom_text(vjust = -0.8, size = 3, show.legend = FALSE) +
  labs(title = "Price vs stock quantity", x = "Unit price (USD)", y = "Units") +
  theme_minimal(base_size = 13)

print(p1); print(p2); print(p3); print(p4)

ggsave("outputs/stock_value_by_category.png", p1, width = 8, height = 5, dpi = 150)
ggsave("outputs/stock_level_by_product.png",  p2, width = 8, height = 6, dpi = 150)
ggsave("outputs/expiry_timeline.png",         p3, width = 8, height = 6, dpi = 150)
ggsave("outputs/price_vs_stock.png",          p4, width = 8, height = 5, dpi = 150)

write_csv(drug,        "outputs/drugstore_clean.csv")
write_csv(by_category, "outputs/summary_by_category.csv")
write_csv(low_stock,   "outputs/low_stock_report.csv")
write_csv(expiring_soon, "outputs/expiry_report.csv")