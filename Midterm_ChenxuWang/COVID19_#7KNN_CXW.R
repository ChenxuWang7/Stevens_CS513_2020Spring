######################################
##Course Code : CS-513-B
##Prgram Name : Midterm #5 
##First  Name : Chenxu
##Last   Name : Wang
##Student ID  : 10457625
##Purpose     : Construct a Knn model to predict infection 
##              based on the other variables.
rm(list=ls())
######################################

#Load the “COVID19_v3.csv”
file<-filename<-file.choose()
COV<-  read.csv(file, na.strings = "?" ) 
View(COV)

#delete the rows with missing value
COV_missing<-na.omit(COV)
View(COV_missing)

#set a seed in order to get the same prediction when the same datasets are tested many times.
set.seed(123)

#Pick 30% data for testing and 70% data for training 
index<-sort(sample(nrow(COV_missing),round(.30*nrow(COV_missing))))
training<- COV_missing[-index,-1]
testing<- COV_missing[index,-1]

library(kknn)
#When k=5
#use the kknn model to classify k_test data
predict_k5 <- kknn(formula=Infected~., training, testing, k=5,kernel ="rectangular")
#check out the prediction of kknn model
fit_5 <- fitted(predict_k5)
table(Actual=testing$Infected,Fitted=fit_5)

#Calculate the number of wrong prediction
knn_wrong<-sum(testing$Infected!=fit_5)
knn_wrong

#Calculate the rate of accuracy prediction
knn_accuracy_rate<-1-knn_wrong/length(testing$Infected)
knn_accuracy_rate









