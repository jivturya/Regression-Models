rm(list=ls())
set.seed(100)


#Read data to R
crime_table=read.table("M:/OMSA/ISYE6501/HW5/crime.txt",header=TRUE)
head(crime_table)

#Check data points for coorelation
library(corrplot)
corrplot(cor(crime_table))

#First regression model fit
model_1=lm(Crime~.,data=crime_table)
summary(model_1)


#input required hw data set to test
test_data=data.frame(M = 14.00,So = 0,Ed = 10.0,Po1 = 12.0,Po2 = 15.5,LF = 0.640,M.F = 94.0,Pop = 150,
                     NW = 1.1,U1 = 0.120,U2 = 3.6,Wealth = 3200,Ineq = 20.1,Prob = 0.04,Time = 39.0)

#Test test data on regression model_1
model_1_fit=predict(model_1,test_data)
model_1_fit


#Regression model_2
model_2=lm(Crime~M+So+Ed+Po1+LF+M.F+Pop+NW+U1+Wealth+Prob+Time,data=crime_table)
summary(model_2)

#Test test data on regression model_2
model_2_fit=predict(model_2,test_data)
model_2_fit

#Regression model_3
model_3=lm(Crime~M+Ed+Po1+U2+Ineq+Prob,data=crime_table)
summary(model_3)

#Test test data on regression model_3
model_3_fit=predict(model_3,test_data)
model_3_fit

#Crime data plot
qqnorm(crime_table$Crime)
