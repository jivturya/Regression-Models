rm(list=ls())
set.seed(1)

german_data=read.table("M:/OMSA/ISYE6501/HW7/germancredit.txt")
str(german_data)

#convert response to binary output
german_data$V21[german_data$V21==1]=0
german_data$V21[german_data$V21==2]=1
head(german_data)

#Split the data into 70% training and 30%validation and test
train_data=german_data[1:700,]
test_data=german_data[701:1000,]
table(train_data$V21)

#creat logistic regressio model
model_1=glm(V21~.,family=binomial(link="logit"),data=train_data)
summary(model_1) 

#choose variables based on Pr value
model_2=glm(V21~V1+V2+V3+V4+V5+V6+V8+V10+V14,family=binomial(link="logit"),data=train_data)
summary(model_2)

#Dropping more vaiables based on Pr Value
model_3=glm(V21~V1+V2+V4+V6+V8+V10+V14,family=binomial(link="logit"),data=train_data)
summary(model_3)


#Choosing model1, predict values and round up values
predict=predict(model_1,test_data,type="response")

#Choose optimal Cutoff
library(InformationValue)
optimal_cutoff=optimalCutoff(test_data$V21,predict)[1]
optimal_cutoff

#rounding up using optical cutoff
predicted_roundup=as.integer(predict>optimal_cutoff)
predicted_roundup

#confusion matrix
confusion_1=confusionMatrix(predicted_roundup,test_data$V21,threshold=optimal_cutoff)

#misclassification error
misClassError(test_data$V21,predicted_roundup,threshold=optimal_cutoff)

#sensitivity and specificity
sensitivity(test_data$V21,predicted_roundup,threshold=optimal_cutoff)
specificity(test_data$V21,predicted_roundup,threshol=optimal_cutoff)


#Choosing model2, predict values and round up values
predict_2=predict(model_2,test_data,type="response")

#Choose optimal Cutoff
library(InformationValue)
optimal_cutoff_2=optimalCutoff(test_data$V21,predict_2)[1]

#rounding up using optical cutoff
predicted_roundup_2=as.integer(predict_2>optimal_cutoff_2)

#confusion matrix
confusion_2=confusionMatrix(predicted_roundup_2,test_data$V21,threshold=optimal_cutoff_2)
confusion_2

#Choosing model3, predict values and round up values
predict_3=predict(model_3,test_data,type="response")

#Choose optimal Cutoff
library(InformationValue)
optimal_cutoff_3=optimalCutoff(test_data$V21,predict_3)[1]

#rounding up using optical cutoff
predicted_roundup_3=as.integer(predict_3>optimal_cutoff_3)

#confusion matrix
confusion_3=confusionMatrix(predicted_roundup_3,test_data$V21,threshold=optimal_cutoff_3)
confusion_3
confusion_2
confusion_1

