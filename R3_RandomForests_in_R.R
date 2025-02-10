#Some Procedures for Week 5

#Clear
cat("\014")  #Clean screen
rm(list=ls()) #Clean Memory

#Load Libraries
library(glmnet)
library(ggplot2)
library(tidyverse)
library(tree)
library(rpart)
library(rpart.plot)

#New Libraries for this week
library(adabag) #AdaBoost
library(caret)  #create data partition/training dataset
library(randomForest) #randomforest
library(psych)  #Easy package for PCA

#This part is based on https://www.projectpro.io/recipes/apply-adaboost-or-classification-r
data <- iris               # reads the dataset

#Exploration of the Data
head(data)           # head() returns the top 6 rows of the dataframe

summary(data)       # returns the statistical summary of the data columns

dim(data)

# createDataPartition() function from the caret package to split the original dataset into a training and testing set and split data into training (80%) and testing set (20%)
parts = createDataPartition(data$Species, p = 0.75, list = F)
train = data[parts, ]
test = data[-parts, ]

###Boosting####

# train a model using our training data
model_adaboost <- boosting(Species~., data=train, boos=TRUE, mfinal=50)
summary(model_adaboost)

# Load data for Regression (this one is stored on R)
#use model to make predictions on test data
pred_ada_test = predict(model_adaboost, test)

# Returns the prediction values of test data along with the confusion matrix
pred_ada_test


### Random Forest ###
set.seed(1312)
model_RF<-randomForest(Species~.,data=train, ntree=100)
pred_RF_test = predict(model_RF, test)
mean(model_RF[["err.rate"]])


### PCA ###
library(psych)
pc <- prcomp(train[,-5], center = TRUE, scale. = TRUE) #remove 5th entry which is non-numeric
attributes(pc)
