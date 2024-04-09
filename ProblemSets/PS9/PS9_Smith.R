install.packages("tidymodels")
library(tidyverse)
library(tidymodels)
library(recipes)
library(magrittr)
library(glmnet)


#Question 4
#load data
housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")


#Question 5
set.seed(123456)

#Question 6
library(rsample)
split <- initial_split(housing, prop = 0.8)
housing_train <- training(split)
housing_test  <- testing(split)

#Question 7
housing_recipe <- recipe(medv ~ ., data = housing) %>% 
  step_log(all_outcomes()) %>% 
  step_bin2factor(chas) %>% 
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax: ptratio:b:lstat:dis:nox) %>% 
  step_poly(crim,zn,indus,rm,age,rad,tax,ptratio,b, lstat,dis,nox, degree=6)
 
#run the recipe
housing_prep <- housing_recipe %>% 
  prep(housing_train, retain = TRUE)

housing_train_prepped <- housing_prep %>% 
  juice 

housing_test_prepped <- housing_prep %>% 
  bake(new_data = housing_test)

#Create x and y training and test data
housing_train_x <- housing_train_prepped %>% 
  select(-medv) 

housing_test_x <- housing_test_prepped %>% 
  select(-medv) 

housing_train_y <- housing_train_prepped %>% 
  select( medv) 

housing_test_y <- housing_test_prepped %>% 
  select( medv)

#Print dims and number of variables
cat("Dimensions of housing_train:", dim(housing_train), "\n") #404, 14
cat("Number of variables in original housing data:", ncol(housing), "\n") #14
cat("Number of variables in housing_train_x:", ncol(housing_train_x), "\n") #74


#Question 8
#estimate using a lasso model
library(parsnip)
library(tune)
library(dials)

cv_lasso <- cv.glmnet(model.matrix(~ ., data=housing_train_x), housing_train_y$medv, 
                      alpha = 1, nfolds = 6)
optimal_lambda_lasso <- cv_lasso$lambda.min
cat("Optimal lambda for LASSO:", optimal_lambda_lasso, "\n")

lasso_model <- glmnet(model.matrix(~ ., data = housing_train_x), housing_train_y$medv,
                      alpha = 1, lambda = optimal_lambda_lasso)
train_preds_lasso <- predict(lasso_model, model.matrix(~ ., data = housing_train_x))
test_preds_lasso <- predict(lasso_model, model.matrix(~ ., data = housing_test_x))

train_rmse_lasso <- sqrt(mean((train_preds_lasso - housing_train_y$medv)^2))
test_rmse_lasso <- sqrt(mean((test_preds_lasso - housing_test_y$medv)^2))
cat("In-sample RMSE for LASSO:", train_rmse_lasso, "\n") 
cat("Out-of-sample RMSE for LASSO:", test_rmse_lasso, "\n")


#Question 9 Ridge Regression
cv_ridge <- cv.glmnet(model.matrix(~ ., data = housing_train_x), housing_train_y$medv,
                      alpha = 0, nfolds = 6)
optimal_lambda_ridge <- cv_ridge$lambda.min
cat("Optimal lambda for Ridge:", optimal_lambda_ridge, "\n")

ridge_model <- glmnet(model.matrix(~ ., data = housing_train_x), housing_train_y$medv, 
                      alpha = 0, lambda = optimal_lambda_ridge)
test_preds_ridge <- predict(ridge_model, model.matrix(~ ., data = housing_test_x))

test_rmse_ridge <- sqrt(mean((test_preds_ridge - housing_test_y$medv)^2))
cat("Out-of-sample RMSE for Ridge:", test_rmse_ridge, "\n")
