library(readr)
path = "https://jaredsmurray.github.io/sta371g_s19/data/"
salary = read_csv(paste0(path, 'salary_gender.csv'))
# Turn the year hired variable into years of experience; the command
# below makes a new column in the salary data frame called Exp
salary$Exp = 96-salary$YrHired

head(salary)

# This line ensures that R treats the Gender variable as categorical
# when it comes time to fit regression models
salary$Gender = factor(salary$Gender)

salaryfit = lm(Salary~Gender, data=salary)
coef(salaryfit)
confint(salaryfit)

salaryfit_exp = lm(Salary~Gender+Exp, data=salary)
summary(salaryfit_exp)

# The plotModel function is very useful for 
# models with interactions
library(mosaic)
plotModel(salaryfit_exp, Salary~Exp)

housing = read_csv(paste0(path, 'MidCity.csv'))
# Rescale the response to be in $1000 & Size in 1000sqft
housing$Price = housing$Price/1000
housing$Size = housing$SqFt/1000

# We can also do some transformations *inside* of an lm formula
# For example, below we convert Nbhd into a factor
housing_fit = lm(Price~factor(Nbhd) + Size, data=housing)
coef(housing_fit)
  
plotModel(housing_fit, Price~Size)

lm(Price~Size, data=housing)

plotModel(salaryfit_exp, Salary~Exp)

salaryfit_int = lm(Salary~Gender*Exp, data=salary)
summary(salaryfit_int)

plotModel(salaryfit_int, Salary~Exp)

gpa = read_csv(paste0(path, 'grades.csv'))
lm(MBAGPA ~ BachGPA*Age, data=gpa)

