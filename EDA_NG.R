# Loading data
HR_data <- read.csv("HR.csv")

# Loading Libraries
if (!require(naniar)) install.packages("naniar")
if (!require(modelsummary)) install.packages("modelsummary")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(outliers)) install.packages("outliers")
library(naniar)
library(modelsummary)
library(ggplot2)
library(outliers)

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
