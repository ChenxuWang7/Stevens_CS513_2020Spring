######################################
##Course Code : CS-513-B
##Prgram Name : Midterm #5 
##First  Name : Chenxu
##Last   Name : Wang
##Student ID  : 10457625
##Purpose     : Naive Bayes model to classify infection
##              based on the other variables.
rm(list=ls())
######################################

library(e1071)
library(class) 
#Load the “COVID19_v3.csv”
file<-filename<-file.choose()
COV<-  read.csv(file, na.strings = "?" ) 
View(COV)

# Convert all the integer data types to factor data type.
#COV[sapply(COV, is.integer)] <- lapply(COV[sapply(COV, is.integer)],as.factor)

#delete the rows with missing value
COV_missing<-na.omit(COV)
View(COV_missing)

#The objects can be accessed by simply giving their names
attach(COV_missing)

#Discretize the "MonthAtHospital" into "less than 6 months" and "6 or more months"
group_M <- c(0,6,33)
Dis_M <- cut(COV_missing[,c(6)], breaks=group_M, labels=c("less than 6 Months","6 or more months"),
             include.lowest=T, right=F)
Compare_Dis_M<- data.frame(COV_missing[,c(6)], Dis_M)

group_A <- c(0, 35, 51, 100) 
Dis_A <- cut(COV_missing[,c(2)], breaks=group_A, labels=c("less than 35","35 to 50","51 or over"),
             include.lowest=T, right=F)
Compare_Dis_A<- data.frame(COV_missing[,c(2)], Dis_A)


#Combine the original date with the Dis_A and Dis_M
COV_Disd<- data.frame(COV_missing, Dis_A, Dis_M)
View(COV_Disd)

#set a seed in order to get the same prediction when the same datasets are tested many times.
set.seed(111)

#Pick 30% data for testing and 70% data for training 
#avoid the original age, the original MonthAtHospital and ID data
index<-sort(sample(nrow(COV_Disd),round(.3*nrow(COV_Disd))))
training<-COV_Disd[-index,c(-1,-2,-6)]
testing<-COV_Disd[index,c(-1,-2,-6)]

#Using naive Bayes to prefict the dateset
NB_Infection<-naiveBayes(Infected~., data=training)
NB_prediction<-predict(NB_Infection, testing)

#Compare the prediction with actual values
table(actual=testing$Infected, Prediction= NB_prediction)

#Calculate the number of wrong prediction
NB_wrong<-sum(testing$Infected!=NB_prediction)
NB_wrong

#Calculate the rate of accuracy prediction
NB_accuracy_rate<-1-NB_wrong/length(testing$Infected)
NB_accuracy_rate

#################### The End ########################




