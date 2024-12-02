library(tidyverse)

customers <- read_csv(here::here("HoD_24/data/noahs-customers.csv"))

# Creates a mapping of letters to their corresponding phone keypad numbers
# a, b, c → 2, etc.
keypad_letters <- tibble(
  letter = c(letters),
  number = c(rep(2, 3), rep(3, 3), rep(4, 3), rep(5, 3), rep(6, 3), rep(7, 4), rep(8, 3), rep(9, 4))
)


# Convert text to its numeric keypad representation
text_to_number <- function(text) {
  text |>
    str_to_lower() |>
    str_split("") |>
    unlist() |>
    tibble(letter = _) |>
    left_join(keypad_letters, by = "letter") |>
    pull(number) |>
    paste0(collapse = "")
}

customers_fin <- customers |>
  select(name, phone) |>
  separate_wider_delim(name, delim = " ", names = c("first_name", "last_name", NA), too_few = "align_start") |>
  mutate(
    unformated_phone_num = str_remove_all(phone, "-"),
    name_to_num = map_chr(last_name, text_to_number, .progress = TRUE)
  )

customers_fin |>
  filter(unformated_phone_num == name_to_num) |>
  pull(phone)
