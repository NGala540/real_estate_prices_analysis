# Loading data
setwd("C:/Users/nortg/Desktop/Studia/Magister/Analiza danych/real_state_prices_analysis/analiza_danych_projekt_zespolowy/Nieruchomosci w Polsce/")
temp = list.files(pattern="^apartments_pl_[0-9]+_[0-9]+\\.csv$")
myfiles = lapply(temp, read.csv)
setwd("C:/Users/nortg/Desktop/Studia/Magister/Analiza danych/real_state_prices_analysis/")

# Checking if all files has the same columns
column_names = colnames(myfiles[[1]])
for (i in myfiles) {print(sum(!(colnames(i) == column_names)))}

#TODO: 
# add column with month to each DF
# bind together dataframe
# do the same for rent files

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

# summary
datasummary_skim(HR_data)
# I still don't understand differance between rate and income
# PerformanceRating have only two values 3 or 4, majority of which is 3
# EmployeeCount, StandardHours and Over18 don't differentiate the set

# Outliers
grubbs_test <- sapply(HR_data[,!names(HR_data) %in% c("Attrition", 
                                                      "BusinessTravel", 
                                                      "Department", 
                                                      "EducationField", 
                                                      "Gender", 
                                                      "JobRole", 
                                                      "MaritalStatus",
                                                      "OverTime",
                                                      "Over18")], grubbs.test)

# Due to Grubbs test potentialy we have an outliers in YearsAtCompany, 
# YearsInCurrentRole and YearsSinceLastPromotion

boxplot(HR_data[c("YearsAtCompany", "YearsInCurrentRole", "YearsSinceLastPromotion")])
# Outliers seems to be relevant, and shouldn't be deleted or addressed


# Number of missing data with its ratio in DF
missing <- data.frame(miss_var_summary(HR_data))

# number off missing futures at once with ratio in DF
missing_cases <- data.frame(miss_case_table(HR_data))

# plotting missing data
vis_miss(HR_data)
vis_miss(HR_data, cluster=TRUE, sort_miss = TRUE)
gg_miss_fct(HR_data[c("MonthlyIncome", "Age", "Attrition")], fct=Attrition)
gg_miss_upset(HR_data, nsets=3)

# Coding NAs
HR_data$Age_na <- ifelse(is.na(HR_data$Age), 1, 0)
HR_data$Attrition_na <- ifelse(is.na(HR_data$Attrition), 1, 0)
HR_data$MonthlyIncome_na <- ifelse(is.na(HR_data$MonthlyIncome), 1, 0)

# Coding categorical values
HR_data$Attrition <- 
  HR_data$Attrition %>% 
  ordered(c("No", "Yes")) %>% 
  as.numeric()-1

HR_data$BusinessTravel <- 
  HR_data$BusinessTravel %>% 
  ordered(c("Non-Travel", "Travel_Rarely", "Travel_Frequently")) %>% 
  as.numeric()-1

HR_data$Gender <- 
  HR_data$Gender %>% 
  ordered(c("Female", "Male")) %>% 
  as.numeric()-1

HR_data$OverTime <-
  HR_data$OverTime %>% 
  ordered(c("No", "Yes")) %>% 
  as.numeric()-1

# Heat map error :(
exclude_HM <- c("Age", "Attrition", "MonthlyIncome", "Department",
                "EducationField","JobRole", "MaritalStatus", "Over18",
                "EmployeeCount", "EmployeeNumber", "StandardHours")

HR_data[,!names(HR_data) %in% exclude_HM] %>%
  cor() %>%
  round(digits=2) %>%
  melt() %>%
  ggplot(aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()

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