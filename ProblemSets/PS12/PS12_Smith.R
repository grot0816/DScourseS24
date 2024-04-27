library(sampleSelection)
library(tidyverse)
library(modelsummary)
library(xtable)
library(knitr)

#Question4
#read in wages12.csv
wages12 <- as.data.frame(read.csv("wages12.csv", header = TRUE))

#5
#format college, married and union as factors
wages12$college <- as.factor(wages12$college)
wages12$married <- as.factor(wages12$married)
wages12$union <- as.factor(wages12$union)

#6
# Create the summary table
model <- lm(logwage ~ college + exper + married + kids + union, data = wages12)

modelsummary <- summary(model)

#print summary table
library(xtable)
latex_table <- xtable(modelsummary)

print(latex_table, file = "modelsummary.tex")

#provide rate of number of missing logwage variables
missing_logwage <- sum(is.na(wages12$logwage)) / nrow(wages12)
missing_logwage

#Question 7
# a. Estimate the regression using only complete cases (listwise deletion)
complete_cases_model <- lm(logwage ~ hgc + union + college + exper + I(exper^2),
                           data = wages12, na.action = na.exclude)

# b. Perform mean imputation to fill in missing log wages
wages12$logwage_mean_imputed <- ifelse(is.na(wages12$logwage), 
                                        mean(wages12$logwage, na.rm = TRUE),
                                        wages12$logwage)

mean_imputation_model <- lm(logwage_mean_imputed ~ hgc + union + college + exper + I(exper^2),
                            data = wages12)

# c. Use the sampleSelection package to correct for possible non-random missingness
# Create a valid variable to flag non-missing log wage observations
wages12$valid <- !is.na(wages12$logwage)

# Recode the log wage variable so that invalid observations are now equal to 0
wages12$logwage_recode <- ifelse(wages12$valid, wages12$logwage, 0)

# Estimate the Heckman selection (Heckit) model
selection_model <- selection(selection = valid ~ hgc + union + college + exper + married + kids,
                             outcome = logwage_recode ~ hgc + union + college + exper + I(exper^2),
                             data = wages12, method = "2step")

# Create a regression table using modelsummary
options("modelsummary_format_numeric_latex" = "plain")

models <- list("Complete Cases" = complete_cases_model,
               "Mean Imputation" = mean_imputation_model,
               "Heckman Selection" = selection_model)

models

modelsummary(models, stars = TRUE)

#Question 8
#Estimate a probit model of preferences for working in a union job
probit_model <- glm(union ~ hgc + college + exper + married + kids, 
                    data = wagedata, 
                    family = binomial(link = "probit"))

# Display the summary of the probit model
summary(probit_model)

#Question 9


# Compute predicted probabilities of the original model
original_probs <- predict(model, type = "response")
print(summary(original_probs))
#mean: .2373
#3rd quartile: .2938

# Get the coefficients of the original model
original_coef <- coef(model)
original_coef


# Create a new set of coefficients where married and kids are set to zero
new_coef <- original_coef
new_coef["married"] <- 0
new_coef["kids"] <- 0

# Update the model with the new coefficients
model$coefficients <- new_coef

# Compute predicted probabilities associated with the new parameter values
new_probs <- predict(model, type = "response")

# Compare the average of each set of predicted probabilities
mean_original_probs <- mean(original_probs)
mean_new_probs <- mean(new_probs)

cat("Average predicted probability (original model):", mean_original_probs, "\n")
cat("Average predicted probability (counterfactual policy):", mean_new_probs, "\n")
