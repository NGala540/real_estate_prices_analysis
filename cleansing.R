# Installing the libraries
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("naniar")
install.packages("mice")
install.packages("outliers")
install.packages("here")

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

# Set working directory
setwd(here())

# Set universal data path
data_path <- file.path("analiza_danych_projekt_zespolowy", "Nieruchomosci w Polsce")

# Combining the data into a single data frame with division for rent and buy
buy <- bind_rows(lapply(
  list.files(
    path = data_path,
    pattern = "^apartments_pl_\\d{4}_\\d{2}\\.csv$", 
    full.names = TRUE), 
  read.csv))

rent <- bind_rows(lapply(
  list.files(
    path = data_path,
    pattern = "^apartments_rent_pl_\\d{4}_\\d{2}\\.csv$", 
    full.names = TRUE), 
  read.csv))

# Grubbs' test for outliers
grubbs.test(buy$price, type = 11, opposite = FALSE) # 2 outliers identified, statistically insignificant
grubbs.test(rent$price, type = 11, opposite = FALSE) # 2 outliers identified, statistically significant



# Interqunatile Range Method for outliers
buy_Q1 <- quantile(buy$price, 0.25, na.rm = TRUE)
buy_Q3 <- quantile(buy$price, 0.75, na.rm = TRUE)
buy_IQR_value <- buy_Q3 - buy_Q1
buy_lower_bound <- max(0, buy_Q1 - 1.5 * buy_IQR_value)  # Adjust lower bound to 0, because price cannot be negative
buy_upper_bound <- Q3 + 1.5 * buy_IQR_value

rent_Q1 <- quantile(rent$price, 0.25, na.rm = TRUE)
rent_Q3 <- quantile(rent$price, 0.75, na.rm = TRUE)
rent_IQR_value <- rent_Q3 - rent_Q1
rent_lower_bound <- max(0, rent_Q1 - 1.5 * rent_IQR_value)  # Adjust lower bound to 0, because price cannot be negative
rent_upper_bound <- rent_Q3 + 1.5 * rent_IQR_value

# Identify outliers 
buy_outliers <- buy$price[buy$price < buy_lower_bound | buy$price > buy_upper_bound]
rent_outliers <- rent$price[rent$price < rent_lower_bound | rent$price > rent_upper_bound]

# See how many values are outliers related to the original dataset
length(buy_outliers)/length(buy$price) * 100  # 5.5% is a bit much to remove
length(rent_outliers)/length(rent$price) * 100 # 7.75% – even more

# Taking a closer look at the rent outliers
View(rent[rent$price == 346, ]) # while 346 for rent sounds small, there's more observations close to this price
View(rent[rent$price == 23000, ]) # there are 3 observations with this price, all in Warsaw centre with a lot of POI

# Q-Q plot for log-transformed rent prices, as appartment prices are usually log-normally distributed
qqnorm(log(rent$price)) # Some heavy tails here
qqline(log(rent$price))

qqnorm(log(buy$price)) # Less heavy tails
qqline(log(buy$price))

# Removing the unnecessary columns
buy <- buy %>% 
  select(-id, -longitude, -latitude)

rent <- rent %>% 
  select(-id, -longitude, -latitude)

# Checking how many NA values are there in each column
colSums(is.na(buy))
colSums(is.na(rent))

# Checking how many rows are complete (no NA values)
nrow(buy[complete.cases(buy), ])/nrow(buy)*100 # 67% originally
nrow(rent[complete.cases(rent), ])/nrow(rent)*100 # 63% originally

# Histogram of prices of aparments in Poland
ggplot(data = buy, aes(x = price)) +
  geom_histogram(bins = 100) +
  labs(title = "Histogram of prices of aparments in Poland",
       x = "Price in PLN",
       y = "Frequency") +
  scale_x_continuous(labels = comma)  # Format x-axis labels

# Histogram of rental prices of apartments in Poland
ggplot(data = rent, aes(x = price)) +
  geom_histogram(bins = 100) +
  labs(title = "Histogram of rental prices of apartments in Poland",
       x = "Price in PLN",
       y = "Frequency") +
  scale_x_continuous(labels = comma)  # Format x-axis labels

# Creating a histogram to show the distribution of buildYear for apartments for purchase
ggplot(buy, aes(y=buildYear)) +
  geom_histogram(binwidth=50, color="black") +
  labs(title = "Histogram of buildYear", y = "Build Year") +
  theme_minimal()

# Creating a histogram to show the distribution of buildYear for rental apartments
ggplot(rent, aes(y=buildYear)) +
  geom_histogram(binwidth=50, color="black") +
  labs(title = "Histogram of buildYear", y = "Build Year") +
  theme_minimal() # Rental appartments are significantly newer not accounting for missing values

# Cut the prices into several levels
buy$priceCut <- cut(buy$price, breaks = c(0, 500000,750000,1000000, max(buy$price)),
                     labels = c("<500k", "500k-750k", "750k-1M", ">1M"))

# Analyzing some missing values
gg_miss_fct(buy, fct = priceCut) # most of the NA values of buildYear are at the lowest price level (<500k)
gg_miss_upset(buy, nsets = 25) # buildYear is the most prevalent NA value, 2nd is floor, 3rd is the combination of them

# Multiple imputation (median is not the best choice since the data is Not Missing At Random)
# Create method vector for imp - use "" for variables you don't want to impute
buy_meth <- rep("", ncol(buy))
names(buy_meth) <- colnames(buy)
buy_meth["buildYear"] <- "cart"
buy_meth["floor"] <- "cart"

# Multiple data imputation
buy_imp <- mice(buy, m=5, method = buy_meth)

# Pool results into final dataset
complete_buy <- complete(buy_imp) # Now there's much less missing data in buildYear column
# Slightly less missing values were imputed in the floor column, since the relation between the floor and the rest
# of the variables is not as strong as in the case of buildYear

colSums(is.na(complete_buy))

# Replace missing values with imputed values (median)
complete_buy <- complete_buy %>%
  mutate(
    buildYear = ifelse(is.na(buildYear), median(buildYear, na.rm = TRUE), buildYear),
    floor = ifelse(is.na(floor), median(floor, na.rm = TRUE), floor),
    floorCount = ifelse(is.na(floorCount), median(floorCount, na.rm = TRUE), floorCount),
    clinicDistance = ifelse(is.na(clinicDistance), median(clinicDistance, na.rm = TRUE), clinicDistance),
    schoolDistance = ifelse(is.na(schoolDistance), median(schoolDistance, na.rm = TRUE), schoolDistance),
    pharmacyDistance = ifelse(is.na(pharmacyDistance), median(pharmacyDistance, na.rm = TRUE), pharmacyDistance),
    postOfficeDistance = ifelse(is.na(postOfficeDistance), median(postOfficeDistance, na.rm = TRUE), postOfficeDistance),
    restaurantDistance = ifelse(is.na(restaurantDistance), median(restaurantDistance, na.rm = TRUE), restaurantDistance),
    collegeDistance = ifelse(is.na(collegeDistance), median(collegeDistance, na.rm = TRUE), collegeDistance),
    kindergartenDistance = ifelse(is.na(kindergartenDistance), median(kindergartenDistance, na.rm = TRUE), kindergartenDistance),
  ) # No missing values left

# Multiple imputation (median is not the best choice since the data is Not Missing At Random)
# Create method vector for imp - use "" for variables you don't want to impute
rent_meth <- rep("", ncol(rent))
names(rent_meth) <- colnames(rent)
rent_meth["buildYear"] <- "cart"
rent_meth["floor"] <- "cart"

# Multiple data imputation
rent_imp <- mice(rent, m=5, method = rent_meth)

colSums(is.na(complete_rent))

# Pool results into final dataset
complete_rent <- complete(rent_imp) # Now there's much less missing data in buildYear column

# Replace missing values with imputed values (median)
complete_rent <- complete_rent %>%
  mutate(
    buildYear = ifelse(is.na(buildYear), median(buildYear, na.rm = TRUE), buildYear),
    floor = ifelse(is.na(floor), median(floor, na.rm = TRUE), floor),
    floorCount = ifelse(is.na(floorCount), median(floorCount, na.rm = TRUE), floorCount),
    clinicDistance = ifelse(is.na(clinicDistance), median(clinicDistance, na.rm = TRUE), clinicDistance),
    schoolDistance = ifelse(is.na(schoolDistance), median(schoolDistance, na.rm = TRUE), schoolDistance),
    pharmacyDistance = ifelse(is.na(pharmacyDistance), median(pharmacyDistance, na.rm = TRUE), pharmacyDistance),
    postOfficeDistance = ifelse(is.na(postOfficeDistance), median(postOfficeDistance, na.rm = TRUE), postOfficeDistance),
    restaurantDistance = ifelse(is.na(restaurantDistance), median(restaurantDistance, na.rm = TRUE), restaurantDistance),
    collegeDistance = ifelse(is.na(collegeDistance), median(collegeDistance, na.rm = TRUE), collegeDistance),
    kindergartenDistance = ifelse(is.na(kindergartenDistance), median(kindergartenDistance, na.rm = TRUE), kindergartenDistance),
  ) # No missing numeric values left


# Columns to check for empty strings in categorical data columns
cols_to_check <- c("type", "buildingMaterial", "condition", "hasParkingSpace", "hasElevator", "hasBalcony", "hasStorageRoom",
                   "hasSecurity")

# Check for empty strings (including white spaces)
sapply(complete_buy[cols_to_check], function(x) sum(x == "" | trimws(x) == ""))
sapply(complete_rent[cols_to_check], function(x) sum(x == "" | trimws(x) == ""))

# Function to show proportions before and after imputation
show_proportions <- function(x) {
  round(prop.table(table(x)) * 100, 2)  # Rounded to 2 decimal places
}

# Function to impute while maintaining proportions
proportional_impute <- function(x, seed = 123) {
  if(is.character(x)) {
    # Show original proportions (excluding empty values)
    cat("Original proportions:\n")
    print(show_proportions(x[x != ""]))
    
    # Get proportions (excluding empty values)
    props <- prop.table(table(x[x != ""]))
    
    # Number of missing values
    n_missing <- sum(x == "")
    cat("\nNumber of missing values:", n_missing, "\n")
    
    # Generate random values based on existing proportions
    set.seed(seed)
    replacement_values <- sample(
      names(props), 
      size = n_missing, 
      prob = props, 
      replace = TRUE
    )
    
    # Replace empty values
    x[x == ""] <- replacement_values
    x <- as.factor(x)
    
    # Show new proportions
    cat("\nNew proportions after imputation:\n")
    print(show_proportions(x))
    
    return(x)
  }
  return(x)
}

# Apply the function to buy and rent data sets
cols_to_impute <- c("type", "buildingMaterial", "condition", "hasParkingSpace", "hasElevator", "hasBalcony", "hasStorageRoom",
                    "hasSecurity")
complete_buy[cols_to_impute] <- lapply(complete_buy[cols_to_impute], proportional_impute)
complete_rent[cols_to_impute] <- lapply(complete_rent[cols_to_impute], proportional_impute)

# Save the data sets with imputed values
write.csv(complete_buy, "cleanBuy.csv", row.names = FALSE)
write.csv(complete_rent, "cleanRent.csv", row.names = FALSE)

