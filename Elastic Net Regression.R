rm(list=ls())
set.seed(100)
library("caret")

#Read dataset
crime_data=read.table("M:/OMSA/ISYE6501/HW5/crime.txt",header=TRUE)
head(crime_data)

#Divide data in training and testing data
random_row=sample(1:nrow(crime_data),as.integer(0.7*nrow(crime_data),replace=FALSE))
train_data=crime_data[random_row,]
test_data=crime_data[-random_row,]

#converting variables and response into matrix for eng method
xtrain=scale(as.matrix(train_data)[,-16],center=TRUE,scale=TRUE)
ytrain=scale(as.matrix(train_data)[,16],center=TRUE,scale=TRUE)
xtest=scale(as.matrix(test_data)[,-16],center=TRUE,scale=TRUE)
ytest=scale(as.matrix(test_data)[,16],center=TRUE,scale=TRUE)

#make model
eng_model=train(Crime~.,data=as.matrix(scale(train_data)),method="glmnet",trControl=trainControl("cv",number=10),preProcess=c("center","scale"),tuneLength=10)
eng_model$bestTune


#prediction on train
predict_train=predict(eng_model, xtrain)
predict_train=predict(eng_model,xtrain)
SSE <- sum((predict_train-ytrain)^2)
SST <- sum((ytrain - mean(ytrain))^2)
R_square <- 1 - SSE / SST
RMSE = sqrt(SSE/nrow(train_data))
R_square
RMSE

#prediction on test
predict_test=predict(eng_model,xtest)
SSE_2 <- sum((predict_test-ytest)^2)
SST_2 <- sum((ytest - mean(ytest))^2)
R_square_2 <- 1 - SSE_2 / SST_2
RMSE_2 = sqrt(SSE_2/nrow(test_data))
R_square_2
RMSE_2
