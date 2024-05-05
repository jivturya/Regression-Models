rm(list=ls())
set.seed(100)

#Read dataset
crime_data=read.table("M:/OMSA/ISYE6501/HW5/crime.txt",header=TRUE)
head(crime_data)

#Divide data in training and testing data
random_row=sample(1:nrow(crime_data),as.integer(0.7*nrow(crime_data),replace=FALSE))
train_data=crime_data[random_row,]
test_data=crime_data[-random_row,]

#stepwise regression
library(MASS)

#initial model
model_1=lm(Crime~.,data=train_data)
summary(model_1)

#stepwise function model
step_model=stepAIC(model_1,direction="both",trace=FALSE)
step_model
summary(step_model)

#check model fit on test data
model_2=lm(Crime~Ed+Po1+Po2+LF+M.F+NW+U1+U2+Wealth+Ineq+Prob,data=test_data)
summary(model_2)





