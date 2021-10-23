######################################
##Course Code : CS-513-B
##Prgram Name : HW02-Exploratory Data Analysis or EDA
##First  Name : Chenxu
##Last   Name : Wang
##Student ID  : 10457625
rm(list=ls())
######################################

##########        Problem 1       ##########
#Load the “breast-cancer-wisconsin.data.csv”
myDataSet<-read.csv("breast-cancer-wisconsin.data.csv", na.strings=c("?"))
View(myDataSet)

#Summarizing each column (e.g. min, max, mean )
summary(myDataSet)
#The objects can be accessed by simply giving their names
attach(myDataSet)

summary_F1<-data.frame(Min=c(min(F1)),Max=c(max(F1)),Mean=c(mean(F1)))
summary_F1

summary_F2<-data.frame(Min=c(min(F2)),Max=c(max(F2)),Mean=c(mean(F2)))
summary_F2

summary_F3<-data.frame(Min=c(min(F3)),Max=c(max(F3)),Mean=c(mean(F3)))
summary_F3

summary_F4<-data.frame(Min=c(min(F4)),Max=c(max(F4)),Mean=c(mean(F4)))
summary_F4

summary_F5<-data.frame(Min=c(min(F5)),Max=c(max(F5)),Mean=c(mean(F5)))
summary_F5

#Caculates the summaries of F6 without NA
summary_F6<-data.frame(Min=c(min(F6, na.rm=TRUE)),Max=c(max(F6,na.rm=TRUE)),Mean=c(mean(F6, na.rm=TRUE)))
summary_F6

summary_F7<-data.frame(Min=c(min(F7)),Max=c(max(F7)),Mean=c(mean(F7)))
summary_F7

summary_F8<-data.frame(Min=c(min(F8)),Max=c(max(F8)),Mean=c(mean(F8)))
summary_F8

summary_F9<-data.frame(Min=c(min(F9)),Max=c(max(F9)),Mean=c(mean(F9)))
summary_F9

summary_Class<-data.frame(Min=c(min(Class)),Max=c(max(Class)),Mean=c(mean(Class)))
summary_Class


#Identifying missing values
table(is.na(myDataSet))

#Caculating the sum of missing values.
sum(is.na(myDataSet))

#Finding out the colume which has the missing values
colSums(is.na(myDataSet))

#Replacing the missing values with the “mean” of the column.
F6[is.na(F6)]<-summary_F6[3]
sum(is.na(F6))
View(F6)
#Displaying the frequency table of “Class” vs. F6
C_F6Table<-xtabs(~Class+F6, data=myDataSet)
ftable(C_F6Table)
#Displaying the scatter plot of F1 to F6, one pair at a time
pairs(myDataSet[2:7],data=myDataSet,main="The scatter plot of F1 to F6",
      pch=21, bg=c("red","green3","blue")[factor(myDataSet$Class)])

#Show histogram box plot for columns F7 to F9
boxplot(myDataSet[8:10],main="The box plot of F7 to F9")

##########        Problem 2          ##########
#Delete all the objects from your R- environment
rm(list=ls()) 

# Reload the “breast-cancer-wisconsin.data.csv” from canvas into R. 
myDataSet<-read.csv("breast-cancer-wisconsin.data.csv", na.strings=c("?"))
View(myDataSet)
nrow(myDataSet)

# Remove any row with a missing value in any of the columns.
myDataSet_missing<-na.omit(myDataSet)
View(myDataSet_missing)
nrow(myDataSet_missing)

########################The END####################
