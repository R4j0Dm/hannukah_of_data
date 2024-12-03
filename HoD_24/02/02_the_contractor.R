
# Import necessary packages ----------------------------------------------


library(tidyverse)


# Import data files ---------------------------------------------------------


orders <- read_csv(here::here("HoD_24/data/noahs-orders.csv"))
orders_items <- read_csv(here::here("HoD_24/data/noahs-orders_items.csv"))
customers <- read_csv(here::here("HoD_24/data/noahs-customers.csv"))
products <- read_csv(here::here("HoD_24/data/noahs-products.csv"))



# 1. Find the SKU (Stock Keeping Unit?) for Rug Cleaner ------------


Rug_cleaner_sku <- products |>
  filter(str_detect(desc, pattern = "^Rug")) |> # Select items Starting with 'Rug' in description
  pull(sku) # Extrat the product identifier (SKU)

# Ther is only one item corresponding to the query



# 2. Identify customers who ordered a Rug Cleaner in 2017 ----------------


# Find all order IDs that include a Rug Cleaner
Rug_orders <- orders_items |>
  filter(sku == Rug_cleaner_sku) |>
  pull(orderid)


# Filter these orders to only those made in 2017
# and extract the customer IDs
rug_customers_2017 <- orders |>
  mutate(order_year = year(ordered)) |>
  filter(order_year == 2017 & orderid %in% Rug_orders) |>
  select(ends_with("id")) |>
  pull(customerid)



# 3. Find the Contractor named JP ----------------------------------------


# First: separate full names into first and last names
customers_data <- customers |>
  select(customerid, name, phone) |>
  separate_wider_delim( # Split the names at spaces
    name,
    delim = " ",
    names = c("first_name", "last_name", NA),
    too_few = "align_start"
  )

# Find the specific customer with the initial JP
contractor_phone_number <- customers_data |>
  filter(
    str_detect(first_name, pattern = "^J") &
    str_detect(last_name, "^P")) |>
  filter(customerid %in% rug_customers_2017) |>
  pull(phone)
