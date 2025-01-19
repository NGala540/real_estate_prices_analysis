# Installing the libraries
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("naniar")
install.packages("mice")
install.packages("outliers")
install.packages("here")
install.packages("tidyr")
install.packages("plotly")
install.packages("corrplot")
install.packages("grid.extra")

# Attaching the libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(naniar)
library(plotly)
library(mice)
library(outliers)
library(scales)
library(here)
library(tidyr)
library(plotly)
library(gridExtra)

# Set working directory
setwd(here())

complete_buy <- read.csv("cleanBuy.csv")
complete_rent <- read.csv("cleanRent.csv")

# Boxplot for price distribution across cities
ggplot(complete_buy, aes(x = reorder(city, price, median), y = price)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Price Distribution by City", x = "City", y = "Price") +
  scale_y_continuous(labels = label_comma())

# Create price per m2 variable
complete_buy <- complete_buy %>% 
  mutate(price_per_m2 = price / squareMeters)

# Scatter plot with trend line
ggplot(complete_buy, aes(x = squareMeters, y = price)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~city) +
  labs(title = "Price vs Size by City") +
  scale_y_continuous(labels = label_comma())

# Compare prices for properties with/without amenities
complete_buy %>%
  gather(amenity, has_amenity, c(hasBalcony, hasElevator, hasParkingSpace, hasStorageRoom, hasSecurity, )) %>%
  ggplot(aes(x = has_amenity, y = price)) +
  geom_boxplot() +
  facet_wrap(~amenity) +
  labs(title = "Price Distribution by Amenities") +
  scale_y_continuous(labels = label_comma())

# Amenities (all but storageRoom) seem to have a noticeable positive impact on the price of the appartment

# Correlation matrix for distance variables
library(corrplot)
distance_vars <- select(complete_buy, contains("Distance"), price)
correlation_matrix <- cor(distance_vars, use = "complete.obs")
corrplot(correlation_matrix, method = "color")

# No strong correlation found. Weak correlation for clinicDistance and restaurantDistance seemingly points to the fact that
# the closer the clinic, the more expensive the appartment. 

# Building age vs price – buy
plot_age_buy <- ggplot(complete_buy, aes(x = buildYear, y = price)) +
  geom_smooth() +
  labs(title = "Price vs Building Age – purchase") +
  scale_y_continuous(labels = label_comma())

# Building age vs price – rent
plot_age_rent <- ggplot(complete_rent, aes(x = buildYear, y = price)) +
  geom_smooth() +
  labs(title = "Price vs Building Age – rent") +
  scale_y_continuous(labels = label_comma())
grid.arrange(plot_age_buy, plot_age_rent, ncol = 2)
# Interesting things can be observed – model is less certain about the smooth price for appartments for rent built in the 70s,
# which might suggest there's less of them available for rent

# appartments in very old buildings tend to be very expensive, but there are not many of them. Commie blocks are the least
# expensive for both rent and purchase. Out of modern buldings those built around 2008 seem to be the most expensive

# prices for the newest buildings seem to be falling with each year.


# Condition impact 
ggplot(complete_buy, aes(x = condition, y = price)) +
  geom_violin() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Price Distribution by Property Condition") +
  scale_y_continuous(labels = label_comma())
# No strong influence of condition on price

# Plot for median purchase price by city 
plot_median_buy <- complete_buy %>%
  group_by(city) %>%
  summarise(median_price = median(price)) %>%
  ggplot(aes(x = reorder(city, median_price), y = median_price)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Median Purchase Price by City", x = "City", y = "Median Price") +
  scale_y_continuous(labels = label_comma())

# Plot for median rent price by city
plot_median_rent <- complete_rent %>%
  group_by(city) %>%
  summarise(median_price = median(price)) %>%
  ggplot(aes(x = reorder(city, median_price), y = median_price)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Median Rent Price by City", x = "City", y = "Median Price") +
  scale_y_continuous(labels = label_comma())

grid.arrange(plot_median_buy, plot_median_rent, ncol = 2)