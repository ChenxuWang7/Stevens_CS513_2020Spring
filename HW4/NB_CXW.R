######################################
##Course Code : CS-513-B
##Prgram Name : HW04_NB
##First  Name : Chenxu
##Last   Name : Wang
##Student ID  : 10457625
##Purpose     :Use the Naïve Bayes methodology to develop a classification model for the Diagnosis. 
######################################
rm(list=ls())

#########################################################
#install.packages('e1071', dependencies = TRUE)


library(e1071)
library(class) 
#Load the “breast-cancer-wisconsin.data.csv”
BCWdata<-read.csv("breast-cancer-wisconsin.data.csv", na.strings=c("?"))
View(BCWdata)


# Convert all the integer data types to factor data type.
BCWdata[sapply(BCWdata, is.integer)] <- lapply(BCWdata[sapply(BCWdata, is.integer)],as.factor)

#delete the rows with missing value
BCWdata_missing<-na.omit(BCWdata)

#The objects can be accessed by simply giving their names
attach(BCWdata_missing)

#set a seed in order to get the same prediction when the same datasets are tested many times.
set.seed(111)

#Pick 30% data for testing and 70% data for training. 
index<-sort(sample(nrow(BCWdata_missing),round(.3*nrow(BCWdata_missing))))
training<-BCWdata_missing[-index,c(-1)]
testing<-BCWdata_missing[index,c(-1)]


#using naive Bayes to prefict the dateset
NB_class<-naiveBayes(Class~., data=training)
NB_prediction<-predict(NB_class, testing)

#Compare the prediction with actual values
table(actual=testing$Class, Prediction= NB_prediction)

#Calculate the number of wrong prediction
NB_wrong<-sum(testing$Class!=NB_prediction)
NB_wrong

#Calculate the rate of wrong prediction
NB_error_rate<-NB_wrong/length(testing$Class)
NB_error_rate

#################### The End ########################




