##################################
#         LAB 2 Assignment 1     #
##################################
library(glmnet)
library(dplyr)
library(tidyr)

tecator = read.csv("tecator.csv")
n = dim(tecator)[1]
set.seed(12345)
#split data
id = sample (1:n, floor(n*0.5)) #id = 50% 
train = tecator[id,] #train 50% 
test = tecator[-id,] #test 50%

#########
#   1   #
#########

#train:
train_df = as.data.frame(train)
train_LM = lm(Fat~. -Sample -Protein -Moisture -Fat, data=train_df) 
pred_train=predict(train_LM,train_df) 
MSE_train = mean((train_df$Fat-pred_train)^2)
MSE_train

#test:
test_df = as.data.frame(test)
pred_test=predict(train_LM,test_df) 
MSE_test = mean((test_df$Fat-pred_test)^2)
MSE_test

#########
#   2   #
#########
#comment: in report (function from lecture)

#########
#   3   #
#########
y = train_df$Fat
x=as.matrix(train_df[c(2:101)])
lasso=glmnet(x, y,family="gaussian",alpha=1)
plot(lasso, xvar = "lambda")
#-0.5<lambda <0

#########
#   4   #
#########
ridge=glmnet(x, y,family="gaussian",alpha=0)
plot(ridge, xvar = "lambda")
#comment: coef --> 0 simultaneously

#########
#   5   #
#########

lasso_crossvalidation=cv.glmnet(x, y,family="gaussian",alpha=1)
plot(lasso_crossvalidation) #plot of CV score
log(lasso_crossvalidation$lambda.min) #optimal penalty factor
lasso_optimal=glmnet(x, y,family="gaussian",alpha=1, lambda=lasso_crossvalidation$lambda.min) #optimal lasso function
coef(lasso_optimal)#8 coeff
#comment: smaller log lambda --> smaller MSE but difference is not significant (in CI) 
y_test = test_df$Fat #select values from fat column
x_test=as.matrix(test_df[c(2:101)])   #features 
y_test_lasso = predict(lasso_optimal,x_test) #using features from test data to predict y using lasso model

plot(y_test,col = "orange")
points( y_test_lasso, col = "blue")







