######################################
##Course Code : CS-513-B
##Prgram Name : HW05.2-Dtree
##First  Name : Chenxu
##Last   Name : Wang
##Student ID  : 10457625
##Purpose     :Use the CART methodology to develop a classification model for the Diagnosis. 
######################################
rm(list=ls())

#########################################################
#install.packages("rpart")  # CART standard package

#install.packages("rpart")
#install.packages("rpart.plot")     # Enhanced tree plots
#install.packages("rattle")         # Fancy tree plot
#install.packages("RColorBrewer")   # colors needed for rattle

library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

#Load the “breast-cancer-wisconsin.data.csv”
filename<-file.choose()
BCWdata<-read.csv(filename, na.strings=c("?"))
#BCWdata<-read.csv("breast-cancer-wisconsin.data.csv", na.strings=c("?"))
View(BCWdata)


# Convert all the integer data types to factor data type.
BCWdata[sapply(BCWdata, is.integer)] <- lapply(BCWdata[sapply(BCWdata, is.integer)],as.factor)

#Delete the data includes a missing value
#BCWdata<-na.omit(BCWdata)

#The objects can be accessed by simply giving their names
attach(BCWdata)

#set a seed in order to get the same prediction when the same datasets are tested many times.
set.seed(123)

#Pick 30% data for testing and 70% data for training. 
index<-sort(sample(nrow(BCWdata),round(.3*nrow(BCWdata))))
training<-BCWdata[-index,c(-1)]
testing<-BCWdata[index,c(-1)]

#Grow the tree
dev.off()
#The categories are represented by the Factor1~Factor9.
CART_class<-rpart(Class~., data=training)


#Show the plot of the CART tree.
rpart.plot(CART_class)

#Predict the testing dataset
CART_prediction<-predict(CART_class, testing,type="class")

#Check out the prediction of CART tree model
table(Actual=testing$Class, CART=CART_prediction)

#Calculate the number of wrong prediction
CART_wrong<-sum(testing$Class!=CART_prediction)
CART_wrong

#Calculate the rate of wrong prediction
CART_error_rate<-CART_wrong/length(testing$Class)
CART_error_rate

#Show the clearer graph of CART tree
prp(CART_class)

#Show much fancier graph of CART tree
fancyRpartPlot(CART_class)






