#Question 3
install.packages("mice")
install.packages("modelsummary")
library(mice)
library(modelsummary)

#Question 4
#install wages and save as data frame
wages <- read.csv("wages.csv", stringsAsFactors = FALSE)
wages <- as.data.frame(wages)

#Question 5
#drop missing observations from hgc or tenure
wages <- wages[complete.cases(wages[c("hgc", "tenure")]), ]

#Question 6
#generate summary table
datasummary_skim(data = wages, output = "table1.tex")

#Question 7 Actions
# listwise deletion 
model1 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=wages)
model1

# mean imputation
wages$logwage_mean <- ifelse(is.na(wages$logwage), mean(wages$logwage, na.rm=TRUE), wages$logwage)
model2 <- lm(logwage_mean ~ hgc + college + tenure + I(tenure^2) + age + married, data=wages)
model2

# impute with predictions from model 1
wages$logwage_pred <- ifelse(is.na(wages$logwage), predict(model1, newdata=wages), wages$logwage)  
model3 <- lm(logwage_pred ~ hgc + college + tenure + I(tenure^2) + age + married, data=wages)
model3

# multiple imputation
set.seed(123)
mi_wages <- mice(wages, m=5, method='pmm', printFlag=FALSE) 
models_mi <- with(mi_wages, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))
model4 <- pool(models_mi)
model4

# Create regression table comparing models
modelsummary(list("Listwise deletion" = model1, 
                  "Mean imputation" = model2,
                  "Prediction imputation" = model3,
                  "Multiple imputation" = model4),
             fmt = "%.3f",
             output="table.tex")


