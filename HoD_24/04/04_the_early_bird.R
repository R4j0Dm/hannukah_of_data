# Required packages ------------------------------------------------------


library(tidyverse)

# Import data ------------------------------------------------------------


customers <- read_csv(here::here("data/noahs-customers.csv"))
products <- read_csv(here::here("data/noahs-products.csv"))
orders <- read_csv(here::here("data/noahs-orders.csv"))
orders_item <- read_csv(here::here("data/noahs-orders_items.csv"))

# 1. Identify bakery products -----------------------------------------------


# Find all products from the bakery
# In this database, bakery have SKUs that start with "BKY"

pastries_sku <- products |>
  filter(str_detect(sku, "^BKY")) |>
  pull(sku)

# 2. Find all orders containing pastries ---------------------------------


pastries_order <- orders_item |>
  filter(sku %in% pastries_sku) |>
  pull(orderid)

# 3. Identify frequent early morning pastry customers --------------------


early_bird_id <- orders |>
  filter(
    orderid %in% pastries_order &
    hour(shipped) <= 5
  ) |>
  count(customerid) |>
  filter(n > 1) |>
  arrange(desc(n)) |>
  slice_head(n = 1) |>
  pull(customerid)

# 4. Retrieve Early Bird Customer's Contact Information -----------------


early_bird_girl_phone <- customers |>
  filter(customerid == early_bird_id) |>
  pull(phone)
