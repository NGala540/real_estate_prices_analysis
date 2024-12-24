# Installing the libraries
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")

# Attaching the libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

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

# Removing the unnecessary columns
data <- data %>% 
  select(-id, -longitude, -latitude)

data_rent <- data_rent %>% 
  select(-id, -longitude, -latitude)

colSums(is.na(data))



nrow(data[complete.cases(data), ])/nrow(data)*100

ggplot(data = data, aes(x = price)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(labels = scales::dollar) +
  labs(title = "Histogram of prices of apartments in Poland",
       x = "Price",
       y = "Frequency")

# Looking for negative values in the dataset
any(data < 0)
sapply(data, function(x) any(x < 0))
sapply(data, function(x) sum(x < 0, na.rm = TRUE))

ggplot(data, aes(y = buildYear)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Boxplot of buildYear", y = "Build Year") +
  theme_minimal()

# Creating a histogram to show the distribution of buildYear
ggplot(data, aes(y=buildYear)) +
  geom_histogram(binwidth=50, color="black") +
  labs(title = "Histogram of buildYear", y = "Build Year") +
  theme_minimal()

# Creating a histogram to show the distribution of floor
ggplot(data, aes(y=floor)) +
  geom_histogram(binwidth=1, color="black") +
  labs(title = "Histogram of floor", y = "floor") +
  theme_minimal()

# Impute missing buildYear values with the median
median_buildYear <- median(data$buildYear, na.rm = TRUE)
data$buildYear[is.na(data$buildYear)] <- median_buildYear

# Impute missing floor values with the median
median_floor <- median(data$floor, na.rm = TRUE)
data$floor[is.na(data$floor)] <- median_floor

summary(data)

boxplot(data$price)

# Convert characters to numbers in Boolean columns
data$hasParkingSpace <- ifelse(data$hasParkingSpace == "yes", 1, 0)
data$hasBalcony <- ifelse(data$hasBalcony == "yes", 1, 0)
data$hasElevator <- ifelse(data$hasElevator == "yes", 1, 0)
data$hasSecurity <- ifelse(data$hasSecurity == "yes", 1, 0)
data$hasStorageRoom <- ifelse(data$hasStorageRoom == "yes", 1, 0)



