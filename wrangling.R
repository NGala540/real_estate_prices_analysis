# Installing the libraries
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("naniar")
install.packages("mice")

# Attaching the libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(naniar)
library(plotly)
library(mice)

# Combining the data into a single data frame with division for rent and buy

setwd("C:/Users/Naz/Documents/real_estate_prices_analysis")
getwd()

# Change the path to files before running the code
data <- bind_rows(lapply(
  list.files(
    path = file.path("C:/Users/Naz/Documents/real_estate_prices_analysis/analiza_danych_projekt_zespolowy/Nieruchomosci w Polsce"),
    pattern = "^apartments_pl_\\d{4}_\\d{2}\\.csv$", 
    full.names = TRUE), 
  read.csv))

data_rent <- bind_rows(lapply(
  list.files(
    path = file.path("C:/Users/Naz/Documents/real_estate_prices_analysis/analiza_danych_projekt_zespolowy/Nieruchomosci w Polsce"),
    pattern = "^apartments_rent_pl_\\d{4}_\\d{2}\\.csv$", 
    full.names = TRUE), 
  read.csv))

# Removing the unnecessary columns
data <- data %>% 
  select(-id, -longitude, -latitude)

data_rent <- data_rent %>% 
  select(-id, -longitude, -latitude)

# Checking how many NA values are there in each column
colSums(is.na(data))


# Checking how many rows are complete (no NA values)
nrow(data[complete.cases(data), ])/nrow(data)*100

ggplot(data = data, aes(x = price)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(labels = scales::dollar) +
  labs(title = "Histogram of prices of apartments in Poland",
       x = "Price",
       y = "Frequency")


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

# Cut the prices into several levels
data$priceCut <- cut(data$price, breaks = c(0, 500000,750000,1000000, max(data$price)),
                     labels = c("<500k", "500k-750k", "750k-1M", ">1M"))

# Analyzing some missing values
gg_miss_fct(data, fct = priceCut) #interesting – most of the NA values of buildYear are at the lowest price level (<500k)
gg_miss_upset(data, nsets = 25) #buildYear is the most prevalent NA value, 2nd is floor, 3rd is the combination of them



# Multiple imputation (median is not the best choice since there is a buildYear-price correlation)


# Create method vector for imp - use "" for variables you don't want to impute
meth <- rep("", ncol(data))
names(meth) <- colnames(data)
meth["buildYear"] <- "cart"
meth["floor"] <- "cart"

# Multiple data imputation
imp <- mice(data, m=5, method = meth)

# Pool results into final dataset
complete_data <- complete(imp)

# Manual imputation of median into complete_data$buildYear
median_complete_data_buildYear <- median(complete_data$buildYear, na.rm = TRUE)
complete_data$buildYear[is.na(complete_data$buildYear)] <- median_complete_data_buildYear

# Manual imputation of median into complete_data$floor
median_complete_data_floor <- median(complete_data$floor, na.rm = TRUE)
complete_data$floor[is.na(complete_data$floor)] <- median_complete_data_floor

# Checking the price and buildYear correlation in the dataset imputed with just median and the one imputed with CART
cor(data$price, data$buildYear)
cor(complete_data$price, complete_data$buildYear)

# The correlation has increased after imputation with CART method. Explanation: the missing values were not random and 
# there was a correlation between price and buildYear. The median imputation has decreased the correlation between price 
# and buildYear.

# Checking the price and floor correlation in the dataset imputed with just median and the one imputed with CART
cor(data$price, data$floor)
cor(complete_data$price, complete_data$floor)

# No significant changes in correlation observed between the two imputations.

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

# Convert characters to numbers in Boolean columns

complete_data$hasParkingSpace <- ifelse(data$hasParkingSpace == "yes", 1, 0)
complete_data$hasBalcony <- ifelse(data$hasBalcony == "yes", 1, 0)
complete_data$hasElevator <- ifelse(data$hasElevator == "yes", 1, 0)
complete_data$hasSecurity <- ifelse(data$hasSecurity == "yes", 1, 0)
complete_data$hasStorageRoom <- ifelse(data$hasStorageRoom == "yes", 1, 0)

colSums(is.na(complete_data))
