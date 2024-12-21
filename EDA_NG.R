# Loading data
load_data <- function(name_struct) {
  setwd("C:/Users/nortg/Desktop/Studia/Magister/Analiza danych/real_state_prices_analysis/analiza_danych_projekt_zespolowy/Nieruchomosci w Polsce/")
  files_names = list.files(pattern=name_struct)
  myfiles = lapply(files_names, read.csv)
  setwd("C:/Users/nortg/Desktop/Studia/Magister/Analiza danych/real_state_prices_analysis/")

  # Checking if all files has the same columns
  column_names = colnames(myfiles[[1]])
  for (i in 2:length(myfiles)) {
    cat("Number of columns not in line (1 and", i,") :", 
          sum(!(colnames(myfiles[[i]]) == column_names)), "\n")
    }
  
  for (i in seq_along(myfiles)) {myfiles[[i]]$month = to_date(files_names[i])}
  
  # bind together dataframe
  return(do.call(rbind, myfiles))
}

# adding new column to each DF with month
to_date <- function(primary_date) {
  date_part <- sub(".*(\\d{4}_\\d{2}).*", "\\1", primary_date)
  complete_date <- paste0(date_part, "_01")
  # print(complete_date)
  
  parsed_date <- tryCatch(
    as.Date(complete_date, format = "%Y_%m_%d"),
    error = function(e) {
      message("Failed to parse date: ", e)
      return(NA)
    }
  )
  
  # print(parsed_date)
  return(parsed_date)
}

apartments_sell = load_data("^apartments_pl_[0-9]+_[0-9]+\\.csv$")
apartments_rent = load_data("^apartments_rent_pl_[0-9]+_[0-9]+\\.csv$")

# Loading Libraries
if (!require(naniar)) install.packages("naniar")
if (!require(modelsummary)) install.packages("modelsummary")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(outliers)) install.packages("outliers")
if (!require(reshape2)) install.packages("reshape2")
library(naniar)
library(modelsummary)
library(ggplot2)
library(outliers)
library(reshape2)

# getting rid of ids
apartments_sell <- apartments_sell[, !names(apartments_sell) == "id"]

# summary
datasummary_skim(apartments_sell)
datasummary_skim(apartments_rent)

# Outliers
grubbs_test_sell <- sapply(apartments_sell[,!names(apartments_sell) %in% 
                                             c("city", 
                                              "type",
                                              "ownership",
                                              "buildingMaterial", 
                                              "condition", 
                                              "hasParkingSpace", 
                                              "hasBalcony", 
                                              "hasElevator",
                                              "hasSecurity",
                                              "hasStorageRoom",
                                              "id",
                                              "month")], grubbs.test)

# Due to Grubbs test potentialy we have an outliers in floor, floorCount,
# poiCount, schoolDistance, postOfficeDistance, kinderGartenDistance, 
# restaurantDistance, pharmacyDistance, price

boxplot(apartments_sell[c("schoolDistance", "postOfficeDistance", 
                          "kindergartenDistance", "restaurantDistance", 
                          "pharmacyDistance")])
boxplot(apartments_sell[c("floor", "floorCount")])
boxplot(apartments_sell[c("poiCount")])
boxplot(apartments_sell[c("price")])

# Outliers seems to be relevant, there is lack of extreme outliers and they 
# shouldn't be deleted or addressed

# Checking the nature of "other" blanks and replacing them with NA
apartments_sell[5,"buildingMaterial"]
apartments_sell[apartments_sell==""] <- NA

# Number of missing data with its ratio in DF
missing <- data.frame(miss_var_summary(apartments_sell))

# plotting missing data
NA_vector <- c("floor", "buildYear", "collegeDistance", "floorCount", 
               "clinicDistance", "restaurantDistance", "pharmacyDistance", 
               "postOfficeDistance", "kindergartenDistance", "schoolDistance",
               "condition", "buildingMaterial", "type", "hasElevator")

gg_miss_upset(apartments_sell, nsets=14)
gg_miss_upset(apartments_sell[,names(apartments_sell)!="condition"], nsets=13)
gg_miss_fct(apartments_sell[,c(NA_vector, "city")], fct = city)

# Coding NAs
for (i in 1:length(NA_vector)) {
  col_name <- paste("NA",NA_vector[i], sep="_")
  apartments_sell[[col_name]] <- ifelse(is.na(apartments_sell[[NA_vector[i]]]), 1, 0)
}

# Coding categorical values
apartments_sell$condition <- 
  apartments_sell$condition %>% 
  ordered(c("low", "premium")) %>% 
  as.numeric()-1

yes_no_variables <- c("hasParkingSpace", "hasBalcony", "hasElevator", 
                      "hasSecurity", "hasStorageRoom")

for (i in 1:length(yes_no_variables)){
  apartments_sell[[yes_no_variables[i]]] <- 
    apartments_sell[[yes_no_variables[i]]] %>% 
    ordered(c("no", "yes")) %>% 
    as.numeric()-1
}

# Heat maps
exclude_HM <- c("city", "type", "ownership", "buildingMaterial", "month", 
                "condition", "hasElevator")

apartments_sell[,!names(apartments_sell) %in% exclude_HM] %>%
  cor(use = "pairwise.complete.obs") %>%
  round(digits=2) %>%
  melt() %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

for (i in 1:length(exclude_HM)){
  gg_miss_fct(apartments_sell[,c(NA_vector, exclude_HM[i])], fct = exclude_HM[i])
}


# Data validation
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(dlookr)) install.packages("dlookr")
if (!require(editrules)) install.packages("editrules")
if (!require(VIM)) install.packages("VIM")
if (!require(deducorrect)) install.packages("deducorrect")
if (!require(ISLR)) install.packages("ISLR")
library(tidyverse)
library(dlookr)
library(editrules)
library(VIM)
library(deducorrect)
library(ISLR)

RULE <- editfile("RULES.txt")
violated <- violatedEdits(RULE, HR_data)
summary(violated)
plot(violated)
# There is no violation!


# Getting rid of column that doesn't differentiate the set
HR_data <- HR_data[,!names(HR_data) %in% c("EmployeeCount", "EmployeeNumber", 
                                           "StandardHours", "OverTime")]

HR_data <- drop_na(HR_data, "Attrition")


# Data inputation
if (!require(mice)) install.packages("mice")
library(mice)
HR_data_clean <- mice(HR_data, method="cart")