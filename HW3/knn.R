######################################
##Course Code : CS-513-B
##Prgram Name : HW03-knn  
##First  Name : Chenxu
##Last   Name : Wang
##Student ID  : 10457625
rm(list=ls())
######################################
#Load the “breast-cancer-wisconsin.data.csv”
kdataset<-read.csv("breast-cancer-wisconsin.data.csv", na.strings = "?")

#delete the rows with missing value
kdataset_missing<-na.omit(kdataset)
View(kdataset_missing)
#The objects can be accessed by simply giving their names
attach(kdataset_missing)

#choose 70 percent data from orignal data as training data
id_training <- sample(nrow(kdataset_missing),nrow(kdataset_missing)*0.70)
k_training = kdataset_missing[id_training,]
#choose 30 percent data from orignal data as testing data
k_test = kdataset_missing[-id_training,]

library(kknn)
#When k=3
#use the kknn model to classify k_test data
predict_k3 <- kknn(formula=Class~., k_training, k_test, k=3,kernel ="rectangular")
#check out the prediction of kknn model
fit_3 <- fitted(predict_k3)
table(Actual=k_test$Class,Fitted=fit_3)

#When k=5
#use the kknn model to classify k_test data
predict_k5 <- kknn(formula=Class~., k_training, k_test, k=5,kernel ="rectangular")
#check out the prediction of kknn model
fit_5 <- fitted(predict_k5)
table(Actual=k_test$Class,Fitted=fit_5)

#When k=10
#use the kknn model to classify k_test data
predict_k10 <- kknn(formula=Class~., k_training, k_test, k=10,kernel ="rectangular")
#check out the prediction of kknn model
fit_10 <- fitted(predict_k10)
table(Actual=k_test$Class,Fitted=fit_10)


######################## The END ######################







