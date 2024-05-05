rm(list=ls())
set.seed(100)
#Read dataset
crime_data=read.table("M:/OMSA/ISYE6501/HW5/crime.txt",header=TRUE)
head(crime_data)

#Divide data in training and testing data
random_row=sample(1:nrow(crime_data),as.integer(0.7*nrow(crime_data),replace=FALSE))
train_data=crime_data[random_row,]
test_data=crime_data[-random_row,]

#converting variables and response into matrix for lasso method
xtrain=scale(as.matrix(train_data)[,-16],center=TRUE,scale=TRUE)
ytrain=scale(as.matrix(train_data)[,16],center=TRUE,scale=TRUE)
xtest=scale(as.matrix(test_data)[,-16],center=TRUE,scale=TRUE)
ytest=scale(as.matrix(test_data)[,16],center=TRUE,scale=TRUE)

#use lasso method
library("glmnet")
cv.lasso=cv.glmnet(xtrain,ytrain,alpha=1,family="gaussian")
plot(cv.lasso)

#find best lamda
best_lamda=cv.lasso$lambda.min
best_lamda

#generate model using best lamda
best_lasso_model=cv.glmnet(xtrain,ytrain,alpha=1,family="gaussian",lamda=best_lamda)
coef(best_lasso_model)
predict_train=predict(best_lasso_model,xtrain)
SSE <- sum((predict_train-ytrain)^2)
SST <- sum((ytrain - mean(ytrain))^2)
R_square <- 1 - SSE / SST
RMSE = sqrt(SSE/nrow(train_data))
R_square
RMSE

#prediction on test model
predict_test=predict(best_lasso_model,xtest)
SSE_2 <- sum((predict_test-ytest)^2)
SST_2 <- sum((ytest - mean(ytest))^2)
R_square_2 <- 1 - SSE_2 / SST_2
RMSE_2 = sqrt(SSE_2/nrow(test_data))
R_square_2
RMSE_2
