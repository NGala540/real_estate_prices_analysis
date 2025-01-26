# Installing the libraries
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("naniar")

# Attaching the libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(naniar)

# Combining the data into a single data frame with division for rent and buy

getwd()

# Change the path to files before running the code
data <- bind_rows(lapply(
  list.files(
    path = file.path("C:", "Users", "User", "Desktop", "Analiza Danych", 
                     "projekt", "real_state_prices_analysis", 
                     "analiza_danych_projekt_zespolowy", "Nieruchomosci w Polsce"),
    pattern = "^apartments_pl_\\d{4}_\\d{2}\\.csv$", 
    full.names = TRUE), 
  read.csv))

data_rent <- bind_rows(lapply(
  list.files(
    path = file.path("C:", "Users", "User", "Desktop", "Analiza Danych", 
                     "projekt", "real_state_prices_analysis", 
                     "analiza_danych_projekt_zespolowy", "Nieruchomosci w Polsce"),
    pattern = "^apartments_rent_pl_\\d{4}_\\d{2}\\.csv$", 
    full.names = TRUE), 
  read.csv))

vis_miss(
  data,
  warn_large_data = FALSE
)

gg_miss_fct(data, fct = city)

gg_miss_upset(data,
              nsets = 28)
