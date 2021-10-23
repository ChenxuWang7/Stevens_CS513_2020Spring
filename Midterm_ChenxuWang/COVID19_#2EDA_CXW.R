######################################
##Course Code : CS-513-B
##Prgram Name : Midterm #2 
##First  Name : Chenxu
##Last   Name : Wang
##Student ID  : 10457625
##Purpose     : EDA analysis for a fictional COVID19 healthcare workers data set
rm(list=ls())
######################################

#Load the “COVID19_v3.csv”
file<-filename<-file.choose()
COV<-  read.csv(file, na.strings = "?" ) 
View(COV)

#convert MaritalStatus into 'factor'type
#COV$MaritalStatus<- as.factor(COV$MaritalStatus)

#I. Summarizing each column (e.g. min, max, mean )
summary(COV)

#The objects can be accessed by simply giving their names
attach(COV)

#II. Identifying missing values
table(is.na(COV))

is.na(COV)
missing<-COV[is.na(MonthAtHospital+Age+Cases),]
missing

#III. Displaying the frequency table of "Infected" vs. "MaritalStatus"
#Displaying the frequency table of “Class” vs. F6
Inf_Mari<-xtabs(~Infected+MaritalStatus, data=COV)
ftable(Inf_Mari)

#IV. Displaying the scatter plot of "Age", "MaritalStatus" and "MonthAtHospital", one pair at a time
pairs(~Age+MaritalStatus+MonthAtHospital,data=COV,main="The scatter plot of Age, MaritalStatus, MonthAtHospital",
      pch=21, bg=c("red","green","blue")[factor(COV$MaritalStatus)])

#V. Show box plots for columns: "Age", "MaritalStatus" and "MonthAtHospital"
boxplot(COV[c(2,4,6)],main="The box plot of Age, MaritalStatus, MonthAtHospital")

#VI.Replaying the missing values of "case" with the "mean" of "cases"
COV[is.na(COV$Cases),"Cases"]<-mean(COV$Cases,na.rm=TRUE)
View(COV)

#Replaying the missing values of "Age" with the "mean" of "Age"
#COV[is.na(COV$Age),"Age"]<-mean(COV$Age,na.rm=TRUE)

#Replaying the missing values of "MonthAtHospital" with the "mean" of "MonthAtHospital"
#COV[is.na(COV$MonthAtHospital),"MonthAtHospital"]<-mean(COV$MonthAtHospital,na.rm=TRUE)









