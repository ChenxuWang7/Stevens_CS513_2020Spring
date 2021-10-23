######################################
##Course Code : CS-513-B
##Prgram Name : problme 2 & 3
##First  Name : Chenxu
##Last   Name : Wang
##Student ID  : 10457625
##              porblem2  Use the Random Forest methodology
##Purpose     : problem3 Use the C5.0 methodology 
##             
######################################
rm(list=ls())
dev.off()
#########################################################


#Load the “Admission_cat.csv”
filename<-file.choose()
ADM_c<-read.csv(filename, na.strings=c("?"))
#BCWdata<-read.csv("Admission_cat.csv", na.strings=c("?"))
View(ADM_c)

#Transfer GPA into numeric type
ADM_c[ADM_c$GPA=="Low","GPA_C"]<- 1
ADM_c[ADM_c$GPA=="Medium","GPA_C"]<- 2
ADM_c[ADM_c$GPA=="High","GPA_C"]<- 3
ADM_c[ADM_c$GPA=="very High",  "GPA_C"]<- 4

#Transfer GRE into numeric type
ADM_c[ADM_c$GRE=="Low","GRE_C"]<- 1
ADM_c[ADM_c$GRE=="Medium","GRE_C"]<- 2
ADM_c[ADM_c$GRE=="High","GRE_C"]<- 3
ADM_c[ADM_c$GRE=="very High",  "GRE_C"]<- 4



ADM<- data.frame(ADM_c[,c(-1, -4, -5)])

# Convert all the integer data types to factor data type.
ADM[sapply(ADM, is.integer)] <- lapply(ADM[sapply(ADM, is.integer)],as.factor)

#The objects can be accessed by simply giving their names
attach(ADM)

#set a seed in order to get the same prediction when the same datasets are tested many times.
set.seed(1234)

#Pick 30% data for testing and 70% data for training. 
index<-sort(sample(nrow(ADM),round(.3*nrow(ADM))))
training<-ADM[-index,]
testing<-ADM[index,]

######################### 1 Random Forest methodology ########################

##install.packages('randomForest')
library(randomForest)

#Delete the data includes a missing value
training <-na.omit(training)
testing  <- na.omit(testing)


#The categories are represented by the GPA GRE and RANK.
fit<- randomForest(ADMIT~.,data=training, importance=TRUE, na.action=na.omit, ntree=100)
importance(fit)
varImpPlot(fit)

#Predict the testing dataset
Prediction<- predict(fit, testing)

#Check out the prediction of CART tree model
table(actual=testing$ADMIT, Prediction)

#Calculate the rate of wrong prediction
wrong<-(testing$ADMIT!=Prediction)
error_rate<-sum(wrong)/length(testing$ADMIT)
accuracy <- (1- error_rate)
accuracy

######################### 2   C5.0 methodology ########################
#install.packages("C50")
library(C50)
#The categories are represented.
C50_class <- C5.0( ADMIT~.,data=training )
summary(C50_class )

dev.off()
#Show the plot of the C5.0
plot(C50_class)

#Predict the testing dataset
C50_predict<-predict(C50_class ,testing , type="class" )

#Check out the prediction of CART tree model
table(actual=testing$ADMIT,C50=C50_predict)

#Calculate the rate of wrong prediction
wrong<- (testing$ADMIT!=C50_predict)
c50_rate<-sum(wrong)/length(testing$ADMIT)
accuracy <- (1- c50_rate)
accuracy
