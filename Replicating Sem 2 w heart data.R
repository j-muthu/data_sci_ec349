# loading packages & data ####
rm(list = ls())
library(tidyverse)
library(glmnet)
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)

# read in csv
heart_raw <- read_csv("Heart.csv")

# we want to predict whether an individual has heart disease
# ie value of AHD variable

# converting categorical variables to factors
heart_cleaned = heart_raw %>%
  drop_na() %>%
  mutate(across(c(ChestPain, Thal), factor)) %>%
  mutate(AHD = ifelse(AHD == "No",
                      0,
                      1)) %>%
  select(-...1)

# create training and test data ####
set.seed(1)
training_list = sample(1:nrow(heart_cleaned), 3*nrow(heart_cleaned)/4)
training_set = heart_cleaned %>%
  filter(row_number() %in% training_list)

# covariates in training set
training_set_x = training_set %>%
  select(-AHD)

# y var in training set
training_set_y = training_set %>%
  select(AHD)

test_set = heart_cleaned %>%
  filter(!row_number() %in% training_list)

# covariates in test set
test_set_x = test_set %>%
  select(-AHD)

# y var in test set
test_set_y = test_set %>%
  select(AHD)
#_______________________________________________________________________________

#### LM (LPM) ####
lm_heart = lm(AHD ~., data = training_set)
summary(lm_heart)

# compute predicted values
lm_pred = predict(lm_heart, newdata = test_set_x)

# add to a predictions dataset
predictions_dataset = test_set_y %>%
  add_column(lm_pred)

# plotting predictions against actual values of AHD
# plus regression line (with standard errors)
ggplot(predictions_dataset, aes(x = lm_pred, y = AHD)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

# compute mse
mse_lm = mean((predictions_dataset$lm_pred - predictions_dataset$AHD)^2)
#_______________________________________________________________________________

#### LOGIT ####
logit_heart = glm(AHD ~., data = training_set,
                  family = binomial(link = "logit"))
summary(logit_heart)

# compute predicted values
# note: logit is a linear regression of log-odds on covariates
# so without specifying type = "response", we have predicted log-odds
# type = "response" converts these log-odds into probabilities using logistic function
logit_pred = predict(logit_heart, newdata = test_set_x,
                     type = "response")

# add to a predictions dataset
predictions_dataset = predictions_dataset %>%
  add_column(logit_pred)

# plotting predictions against actual values of AHD
# plus regression line (with standard errors)
ggplot(predictions_dataset, aes(x = logit_pred, y = AHD)) + 
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = TRUE)

# compute mse
mse_logit = mean((predictions_dataset$logit_pred - predictions_dataset$AHD)^2)
#_______________________________________________________________________________

#### LM W PENALISATION (RIDGE, LASSO) ####

# creating matrix of training data covariates and y values for these glmnet regressions
# `-1` removes column of intercepts
X = model.matrix(~. - 1, data = training_set_x)
y = model.matrix(~. -1, data = training_set_y)

# ridge regression (alpha is lasso penalty, lambda is ridge penalty, so alpha = 0)
# lambda not explicitly chosen - glmnet fits the model for a sequence (100) of lambda values
# and provides regressions for each lambda
ridge_lm = glmnet(X, y, alpha = 0)

# LASSO (alpha = 1)
lasso_lm = glmnet(X, y, alpha = 1)

# Find optimal lambda using cross-validation
cv_ridge = cv.glmnet(X, y, alpha = 0)
plot(cv_ridge)
cv_lasso = cv.glmnet(X, y, alpha = 0)
plot(cv_lasso)

# Best lambda values - can use minimum
  # (or can use highest lambda value within 1 se of the minimum value
  # i.e. statistically indistiguishable but encourages the most parsimony)
best_lambda_ridge <- cv_ridge$lambda.min
best_lambda_lasso <- cv_lasso$lambda.min

# re-estimate using cross-validated lambdas
ridge_lm = glmnet(X, y, alpha = 0, lambda = best_lambda_ridge)
coef(ridge_lm)
lasso_lm = glmnet(X, y, alpha = 1, lambda = best_lambda_lasso)
coef(lasso_lm)

# Make predictions on test set
X_test = model.matrix(~. - 1, data = test_set_x)
ridge_lm_pred = predict(ridge_lm, newx = X_test, s = best_lambda_ridge)
lasso_lm_pred = predict(lasso_lm, newx = X_test, s = best_lambda_lasso)

# add ridge and lasso logit predictions to predictions dataset
predictions_dataset = predictions_dataset %>% 
  bind_cols(ridge_lm_pred,
            lasso_lm_pred) %>%
  rename(ridge_lm_pred = last_col(offset = 1),
         lasso_lm_pred = last_col())

# plotting predictions against actual values of AHD
# plus regression line (with standard errors)
ggplot(predictions_dataset, aes(x = ridge_lm_pred, y = AHD)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

ggplot(predictions_dataset, aes(x = lasso_lm_pred, y = AHD)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

# MSEs
mse_lm_ridge = mean((predictions_dataset$ridge_lm_pred - predictions_dataset$AHD)^2)
mse_lm_lasso = mean((predictions_dataset$lasso_lm_pred - predictions_dataset$AHD)^2)
#_______________________________________________________________________________

#### LOGIT W PENALISATION (RIDGE, LASSO) ####

# ridge regression (alpha = 0)
ridge_logit = glmnet(X, y, family = "binomial", alpha = 0)

# LASSO (alpha = 1)
lasso_logit = glmnet(X, y, family = "binomial", alpha = 1)

# Find optimal lambda using cross-validation
cv_ridge <- cv.glmnet(X, y, family = "binomial", alpha = 0)
plot(cv_ridge)
cv_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1)
plot(cv_lasso)

# Best lambda values
best_lambda_ridge <- cv_ridge$lambda.min
best_lambda_lasso <- cv_lasso$lambda.min

# re-estimate using cross-validated lambdas
ridge_logit = glmnet(X, y, family = "binomial",
                     alpha = 0, lambda = best_lambda_ridge)
coef(ridge_logit)
lasso_logit = glmnet(X, y, family = "binomial",alpha = 1,
                     lambda = best_lambda_lasso)
coef(lasso_logit)

# Make predictions on test set
ridge_logit_pred = predict(ridge_logit, newx = X_test, s = best_lambda_ridge, type = "response")
lasso_logit_pred = predict(lasso_logit, newx = X_test, s = best_lambda_lasso, type = "response")

# add ridge and lasso logit predictions to predictions dataset
predictions_dataset = predictions_dataset %>% 
  bind_cols(ridge_logit_pred,
            lasso_logit_pred) %>%
  rename(ridge_logit_pred = last_col(offset = 1),
         lasso_logit_pred = last_col())

# plotting predictions against actual values of AHD
# plus regression line (with standard errors)
ggplot(predictions_dataset, aes(x = ridge_logit_pred, y = AHD)) + 
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = TRUE)

ggplot(predictions_dataset, aes(x = lasso_logit_pred, y = AHD)) + 
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = TRUE)

# MSEs
mse_logit_ridge = mean((predictions_dataset$ridge_logit_pred - predictions_dataset$AHD)^2)
mse_logit_lasso = mean((predictions_dataset$lasso_logit_pred - predictions_dataset$AHD)^2)
#_______________________________________________________________________________

#### REGRESSION TREES WITH `tree` ####

# converting AHD to factor
training_set_AHD_fact = training_set %>%
  mutate(AHD = as.factor(AHD))

tree_heart = tree(AHD ~., data = training_set_AHD_fact)
summary(tree_heart)
tree_heart

# plot tree
plot(tree_heart)
  text(tree_heart, pretty = 1)

# generate predicted values
# using `type = "vector"` to predict the probability that AHD = 1
# can use `type = "class"` to predict the actual class
# this produces 2 columns: the first has the probability AHD = 0,
# the second the probability AHD = 1 (which is what we're interested in)
tree_pred = predict(tree_heart, newdata = test_set_x, type = "vector")

# add predictions to predictions dataset
# note we only keep the 2nd column of `tree_pred` as explained above
predictions_dataset = predictions_dataset %>% 
  bind_cols(tree_pred[,2]) %>%
  rename(tree_pred = last_col())

# plot predicted values against actual values
ggplot(predictions_dataset, aes(x = tree_pred, y = AHD)) + 
  geom_point() +
  geom_smooth(method = "lm",
              se = TRUE)

# calculating MSE
# note this is the same as the misclassification rate
# for a binary (0/1) variable
# note - tree is very sensitive to the sample (overfits - high variance)
# tendency to overfit is because of sequential design of trees
mse_tree = mean((predictions_dataset$AHD - predictions_dataset$tree_pred)^2)

# pruning tree
set.seed(789)
cvtree_heart = cv.tree(tree_heart, FUN = prune.tree)
names(cvtree_heart)
cvtree_heart

# plotting size of tree and cost-complexity parameter 
# against deviance (number of misclassifications)
# we can visually see that a tree size of 6 (6 terminal nodes) gives minimal deviance
# increasing the tree size beyond that is resulting in overfitting
par(mfrow = c(1,2))
plot(cvtree_heart$size, cvtree_heart$dev, type = "b")
plot(cvtree_heart$k, cvtree_heart$dev, type = "b")
#returning plots back to 1 plot per figure
par(mfrow = c(1,1))

# the minimal deviance obtains this value, 6
optimal_size = cvtree_heart$size[which.min(cvtree_heart$dev)]
optimal_size

# prune tree
prune_heart = prune.tree(tree_heart, best = optimal_size)
plot(prune_heart)
  text(prune_heart, pretty = 1)
  
# generate predicted values from pruned tree
prune_tree_pred = predict(prune_heart, newdata = test_set_x, type = "vector")

# add predictions to predictions dataset
predictions_dataset = predictions_dataset %>% 
  bind_cols(prune_tree_pred[,2]) %>%
  rename(prune_tree_pred = last_col())

# calculating MSE
# note this is the same as the misclassification rate
# for a binary (0/1) variable
mse_prune_tree = mean((predictions_dataset$AHD - predictions_dataset$prune_tree_pred)^2)
#_______________________________________________________________________________

#### REGRESSION TREES WITH `rpart` ####

# AHD is a binary variable, so the `class` method is assumed
tree_heart_rpart = rpart(AHD ~., data = training_set_AHD_fact, method = "class")
summary(tree_heart_rpart)
tree_heart_rpart

# plot tree
rpart.plot(tree_heart_rpart)

# generate predicted values
tree_pred_rpart = predict(tree_heart_rpart, newdata = test_set_x, type = "class")

# add predictions to predictions dataset
predictions_dataset = predictions_dataset %>% 
  bind_cols(as.numeric(tree_pred_rpart)-1) %>%
  rename(tree_pred_rpart = last_col())

# plot predicted values against actual values
ggplot(predictions_dataset, aes(x = tree_pred_rpart, y = AHD)) + 
  geom_point() +
  geom_smooth(method = "lm",
              se = TRUE)

# calculating MSE
# note this is the same as the misclassification rate
# for a binary (0/1) variable
# note - tree is very sensitive to the sample (overfits - high variance)
# tendency to overfit is because of sequential design of trees
mse_tree_rpart = mean((predictions_dataset$AHD - predictions_dataset$tree_pred_rpart)^2)

# plot cost-complexity parameter of tree
plotcp(tree_heart_rpart)

# use CV to pick optimal cp parameter
optimal_cp = tree_heart_rpart$cptable[which.min(tree_heart_rpart$cptable[,"xerror"]), "CP"]
optimal_cp

# prune tree
prune_heart_rpart = prune(tree_heart_rpart, cp = optimal_cp)

# plot pruned tree
rpart.plot(prune_heart_rpart)

# generate predicted values from pruned tree
# note these predictions are saved as factors
prune_tree_rpart_pred = predict(prune_heart_rpart, newdata = test_set_x, type = "class")

# add predictions to predictions dataset
predictions_dataset = predictions_dataset %>% 
  bind_cols(as.numeric(prune_tree_rpart_pred)-1) %>%
  rename(prune_tree_rpart_pred = last_col())

# calculating MSE
# note this is the same as the misclassification rate
# for a binary (0/1) variable
mse_prune_tree_rpart = mean((predictions_dataset$AHD - predictions_dataset$prune_tree_rpart_pred)^2)
#_______________________________________________________________________________

#### BAGGING ####
set.seed(8)
# number of regressors is total number of x variables
bag_heart = randomForest(AHD ~., data = training_set_AHD_fact, mtry = ncol(training_set_AHD_fact)-1,
                         importance = TRUE)
bag_heart

# generate predictions
bag_pred = predict(bag_heart, newdata = test_set_x)

# add predictions to predictions dataset
predictions_dataset = predictions_dataset %>% 
  bind_cols(as.numeric(bag_pred)-1) %>%
  rename(bag_pred = last_col())

# calculating MSE
# note this is the same as the misclassification rate
# for a binary (0/1) variable
mse_bag = mean((predictions_dataset$bag_pred - predictions_dataset$AHD)^2)
#_______________________________________________________________________________

#### RANDOM FOREST ####
set.seed(9)
# number of x variables selected for inclusion in each tree
# is lower than the number of x variables
# I choose 5
forest_heart = randomForest(AHD ~., data = training_set_AHD_fact, mtry = 5,
                         importance = TRUE)
forest_heart

# generate predictions
forest_pred = predict(forest_heart, newdata = test_set_x)

# add predictions to predictions dataset
predictions_dataset = predictions_dataset %>% 
  bind_cols(as.numeric(forest_pred)-1) %>%
  rename(forest_pred = last_col())

# calculating MSE
# note this is the same as the misclassification rate
# for a binary (0/1) variable
mse_forest = mean((predictions_dataset$forest_pred - predictions_dataset$AHD)^2)

# viewing importance of each variable
importance(forest_heart)
varImpPlot(forest_heart)
#_______________________________________________________________________________

#### Comparison ####

# Get variables starting with "mse_"
mse_vars = ls(pattern = "^mse_")

# Create dataframe with variable names and values
mse_df = data.frame(
  method = sub("^mse_", "", mse_vars),
  mse = sapply(mse_vars, get),
  stringsAsFactors = FALSE)

mse_df = mse_df %>%
  arrange(mse)

view(mse_df)
mse_df

# see predictions_dataset for probabilistic/factor predictions under each model
view(predictions_dataset)
