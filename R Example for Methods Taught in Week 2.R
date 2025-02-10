#Some Procedures for Week 2

#Clear
cat("\014")  #Clean screen (code to send `ctrl`+`l` to console to clear screen)
rm(list=ls()) #Clean Memory (removes all user-defined datasets and functions)

#Load Libraries
library(glmnet)
library(ggplot2)
library(tidyverse)

# Load data for Regression (this one is stored on R)
data(mtcars)

# Print the first 5 rows
head(mtcars, 5)

# Number of rows (observations)
nrow(mtcars)

# Number of columns (variables)
ncol(mtcars)

#We can drop a couple of variables by subsetting, e.g., dropping columns 8 and 9 (which, here, are less easy to interpret)
mtcars = subset(mtcars, select = -c(8,9) )

#Plot two way correlation of Miles Per Gallon and Horsepower.
ggplot(mtcars, aes(x=hp, y=mpg)) + geom_point() + geom_smooth(method="lm") #geom_smooth() plots conditional means using linear model fit

#Split data into test and training
set.seed(1) #Set seed for reproducibility
train <- sample(1:nrow(mtcars), 3*nrow(mtcars)/4) #split 3/4 (24 obs) and 1/4 (8) 
# ^randomly selects 24 numbers that correspond to 24 random rows of the 32-row mtcars dataset
mtcars_train <-mtcars[train,] # assigns the 24 corresponding rows of mtcars to the training dataset
mtcarsx_train <-mtcars_train[,-1] # x vars are everything apart from mpg (1st var in mtcars dataset)
mtcarsy_train <-mtcars_train[,1] # y var is mpg (1st var in mtcars dataset)

#Test data is the one not in train
mtcars_test<-mtcars[-train,] 
#^ all rows in mtcars dataset that aren't the rows assigned to training rows
mtcarsx_test<-mtcars_test[,-1]
mtcarsy_test <-mtcars_test[,1]

#Linear Regression Model of MPG as a function of car characteristics
lm_cars<- lm(mpg~cyl+disp+hp+drat+wt+qsec+gear+carb, data = mtcars_train) #Create the linear regression

#Review the results
summary(lm_cars) 

#Prediction to test data
lm_cars_predict<-predict(lm_cars, newdata = mtcars_test[,-1]) 
# ^ predict function using existing regression model on newdata

#Empirical MSE in TEST data
lm_cars_test_MSE<-mean((lm_cars_predict-mtcars_test$mpg)^2)
# ^mechanically calculates MSE using linear reg model tested on test data

#You can write it more succinctly as: 
# lm_cars<- lm(mpg~., data = mtcars) ####
# ^Create the linear regression

#If you do not want to include cyl, you can write it as 
#lm_cars2<- lm(mpg~ .-cyl, data = mtcars) #Create the linear regression

#For Robust Standard Errors,
library(sandwich)
library(lmtest)
coeftest(lm_cars, vcov = vcovHC(lm_cars, type="HC3"))


#Ridge on Train Dataset, setting lambda = 3 (alpha is lasso penalty, lambda ridge)
ridge.mod<-glmnet(mtcarsx_train, mtcarsy_train, alpha = 0, lambda = 3, thresh = 1e-12)
coef(ridge.mod) #coefficients

#Ridge with Cross-Validation - the cv. packages requires "matrix" rather than dataframe
cv.out <- cv.glmnet(as.matrix(mtcarsx_train), as.matrix(mtcarsy_train), alpha = 0, nfolds = 3)
plot(cv.out)
lambda_ridge_cv <- cv.out$lambda.min #cross-validation is the lambda minimising empirical MSE in training data

#Re-Estimate Ridge with lambda chosen by Cross validation
ridge.mod<-glmnet(mtcarsx_train, mtcarsy_train, alpha = 0, lambda = lambda_ridge_cv, thresh = 1e-12)

#Fit on Test Data
ridge.pred <- predict(ridge.mod, s = lambda_ridge_cv, newx = as.matrix(mtcarsx_test))
ridge_MSE<- mean((ridge.pred - mtcarsy_test) ^ 2) #Note how it outperforms OLS

#We can repeat the same exercise for LASSO
#Ridge with Cross-Validation - the cv. packages requires "matrix" rather than dataframe
cv.out <- cv.glmnet(as.matrix(mtcarsx_train), as.matrix(mtcarsy_train), alpha = 1, nfolds = 3)
plot(cv.out)
lambda_LASSO_cv <- cv.out$lambda.min #cross-validation is the lambda minimising empirical MSE in training data

#Re-Estimate Ridge with lambda chosen by Cross validation
LASSO.mod<-glmnet(mtcarsx_train, mtcarsy_train, alpha = 1, lambda = lambda_LASSO_cv, thresh = 1e-12)
coef(LASSO.mod) #note that some parameter estimates are set to 0 --> Model selection!

#Fit on Test Data
LASSO.pred <- predict(LASSO.mod, s = lambda_LASSO_cv, newx = as.matrix(mtcarsx_test))
LASSO_MSE<- mean((LASSO.pred - mtcarsy_test) ^ 2) #Note how it outperforms OLS

