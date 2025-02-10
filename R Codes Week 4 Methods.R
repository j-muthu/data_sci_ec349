#Example of Logit, Classification Trees and Bagging

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

# Load data for Regression (this one is stored on R)
data(iris)

#Suppose we want to understand whether a flower is virginica or not
iris$binary_virginica<-ifelse(iris$Species=="virginica",1,0) #You could use factor too

#Split data into test and training
set.seed(1) #Set seed for reproducibility
train <- sample(1:nrow(iris), 3*nrow(iris)/4) #split 3/4 and 1/4
iris_train <-iris[train,]
irisx_train <-iris_train[,-c(5,6)] #there are two outcomes
irisy_train <-iris_train[,c(5,6)]

#Test data is the one not in train
iris_test<-iris[-train,]
irisx_test<-iris_test[,-c(5,6)]
irisy_test <-iris_test[,c(5,6)]

#Logistic Regression of Setosa or not on Petal Width, Length, Sepal Width, Length
logit <- glm(binary_virginica ~ Petal.Width+Sepal.Length, data = iris_train, family = binomial(link="logit"))

#Review the results
coef(logit) 
summary(logit)

#Predicted Values for Test Data based on the model estimates
logit_predict<-predict(logit, newdata = irisx_test)

#What happens when you include all variables?
logit <- glm(binary_virginica ~ Petal.Width+Petal.Length+Sepal.Width+Sepal.Length, data = iris, family = binomial(link="logit"))

#Lack of Variation and Multicollinearity
cor(iris$Petal.Width,iris$Petal.Length)
cor(iris$Petal.Width,iris$Sepal.Length)

#Regression Tree
set.seed(1312) #reproducibility

#With tree library
tree1<-tree(Species~Petal.Width+Sepal.Length, data=iris_train)

#partition graph
partition.tree(tree1) 
points(iris_train[, c("Petal.Width","Sepal.Length")], cex=.4)

plot(tree1)
text(tree1, pretty = 1)
title(main = "Unpruned Classification Tree")

#With rpart library
rpart_tree<-rpart(Species~Petal.Width+Sepal.Length, data=iris_train)
rpart.plot(rpart_tree)

#Confusion Matrix for Training Data
pred_train = predict(rpart_tree, type="class")
table(pred_train, iris_train$Species)

#Confusion Matrix for Test Data
pred_test = predict(rpart_tree, iris_test, type="class")
table(pred_test, iris_test$Species)


#Bagging
library(ipred)       #for fitting bagged decision trees
set.seed(1312)       #make this example reproducible

#fit the bagged model
bag <- bagging(Species~Petal.Width+Sepal.Length, data=iris_train, nbagg = 50,   
  coob = TRUE, control = rpart.control(minsplit = 2, cp = 0.1)
)

#display fitted bagged model
bag

