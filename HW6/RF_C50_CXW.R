######################################
##Course Code : CS-513-B
##Prgram Name : HW06_RF_C50
##First  Name : Chenxu
##Last   Name : Wang
##Student ID  : 10457625
##Purpose     : 6.1 Use the C5.0 methodology to develop a classification model for the Diagnosis.
##              6.2 Use the Random Forest methodology to develop a classification model for the Diagnosis 
##             and identify important features.
######################################
rm(list=ls())
dev.off()
#########################################################


#Load the “breast-cancer-wisconsin.data.csv”
filename<-file.choose()
BCWdata<-read.csv(filename, na.strings=c("?"))
#BCWdata<-read.csv("breast-cancer-wisconsin.data.csv", na.strings=c("?"))
View(BCWdata)


# Convert all the integer data types to factor data type.
BCWdata[sapply(BCWdata, is.integer)] <- lapply(BCWdata[sapply(BCWdata, is.integer)],as.factor)

#The objects can be accessed by simply giving their names
attach(BCWdata)

#set a seed in order to get the same prediction when the same datasets are tested many times.
set.seed(123)

#Pick 30% data for testing and 70% data for training. 
index<-sort(sample(nrow(BCWdata),round(.3*nrow(BCWdata))))
training<-BCWdata[-index,c(-1)]
testing<-BCWdata[index,c(-1)]


######################### 6.1   C5.0 methodology ########################
#install.packages("C50")
library(C50)
#The categories are represented by the Factor1~Factor9.
C50_class <- C5.0( Class~.,data=training )
summary(C50_class )

dev.off()
#Show the plot of the C5.0
plot(C50_class)

#Predict the testing dataset
C50_predict<-predict( C50_class ,testing , type="class" )

#Check out the prediction of CART tree model
table(actual=testing$Class,C50=C50_predict)

#Calculate the rate of wrong prediction
wrong<- (testing$Class!=C50_predict)
c50_rate<-sum(wrong)/length(testing$Class)
c50_rate

######################### 6.2 Random Forest methodology ########################

##install.packages('randomForest')
library(randomForest)

#Delete the data includes a missing value
training <-na.omit(training)
testing  <- na.omit(testing)


#The categories are represented by the Factor1~Factor9.
fit<- randomForest(Class~.,data=training, importance=TRUE, na.action=na.omit, ntree=100)
importance(fit)
varImpPlot(fit)

#Predict the testing dataset
Prediction<- predict(fit, testing)

#Check out the prediction of CART tree model
table(actual=testing$Class, Prediction)

#Calculate the rate of wrong prediction
wrong<-(testing$Class!=Prediction)
error_rate<-sum(wrong)/length(testing$Class)
error_rate

