#Lab 2 Week 4
#read data from csv file
df = read.csv("C:/Users/User/OneDrive - Universiti Teknologi PETRONAS/Documents/Churn_Train.csv")

#view table in the form of table
View(df)

#checking missing values
sum(is.na(df))

#due to missing values, we replace them with new values using mice library
library("mice")
imputed_data <- mice(df, m = 1, method="pmm", seed = 123)
complete_data <- complete(imputed_data, 1)
complete_data$Total.Charges
View(complete_data)

# UNIVARIATE ANALYSIS

#Descriptive Statistics
describe(complete_data)
normality(complete_data)
plot_normality(complete_data)

# BIVARIATE/MULTIVARIATE ANALYSIS

library("dplyr")

correlate(complete_data, Tenure, Total.Charges)
complete_data %>%
  correlate() %>%
    plot()

#EDA based on target variables

categ <- target_by(complete_data, Churn) # we choose Churn variable

cat_num <- relate(categ, Total.Charges)
cat_num
summary(cat_num)
plot(cat_num)

cat_cat <- relate(categ, Contract)
cat_cat
summary(cat_cat)
plot(cat_cat)

num_num <- relate(num, Monthly.Charges)
num_num
summary(num_num)
plot(num_num)

library("dlookr")
complete_data %>%
    eda_paged_report(target = "Churn", title = "Exploratory Data Analysis Report", 
                     subtitle = "Lab 2 Week 4", output_dir = "./", 
                     output_file = "EDA_Lab2.pdf", 
                     theme = c("orange", "blue"))



