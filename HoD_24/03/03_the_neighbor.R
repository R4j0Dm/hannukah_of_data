
# Import necessary packages ----------------------------------------------

library(tidyverse)


# Import data files ------------------------------------------------------

customers <- read_csv(here::here("HoD_24/data/noahs-customers.csv"))


# 1. Informations we have ------------------------------------------------

# The guy was born in the year of rabbit
rabbit_years <- seq(1927, 2024, by = 12)

# He is also the neighbor of the contractor
the_contractor <- customers |>
  #+ this filter use the contractor phone number from the previous challange
  filter(phone == contractor_phone_number)


# A function to find if the customers is born a cancer
born_cancer <- function(date) {
  month <- month(date)
  day <- day(date)

  (month == 6 & day >= 21) | (month == 7 & day <= 21)
}


# 2. Identify the neighbor -----------------------------------------------

neighbor_phone_number <- customers |>
  mutate(
    birth_year = year(birthdate)
  ) |>
  filter(
    birth_year %in% rabbit_years &
    born_cancer(birthdate) &
    citystatezip == the_contractor$citystatezip
  ) |>
  pull(phone)

neighbor_phone_number
