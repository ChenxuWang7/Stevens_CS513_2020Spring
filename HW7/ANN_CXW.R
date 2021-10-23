######################################
##Course Code : CS-513-B
##Prgram Name : HW07_ANN
##First  Name : Chenxu
##Last   Name : Wang
##Student ID  : 10457625
##Purpose     : Use the ANN methodology with five (5) nodes in the hidden
##              layer, to develop a classification model for the Diagnosis.
######################################
rm(list=ls())
dev.off()
#########################################################
#Load the “wisc_bc_ContinuousVar.csv”
filename<-file.choose()
WBC_conti<-read.csv(filename, na.strings=c("?"))
View(WBC_conti)
summary(WBC_conti)

#set a seed in order to get the same prediction when the same datasets are tested many times.
set.seed(123)

#Delete the data includes a missing value
#BC_conti <-na.omit(BC_conti)

#collumnNames <- names(BC_conti)

WBC_conti[WBC_conti$diagnosis=="M","Output"]<- 1 
WBC_conti[WBC_conti$diagnosis=="B","Output"]<- 0 
BC_conti <- WBC_conti[,c(-1, -2)]

#The objects can be accessed by simply giving their names
attach(BC_conti)
#Pick 30% data for testing and 70% data for training. 
index<-sort(sample(nrow(BC_conti),round(.3*nrow(BC_conti))))
training<-BC_conti[-index,]
testing<-BC_conti[index,]

######################### 7.1   ANN methodology ########################

#install.packages("neuralnet")
library("neuralnet")

#Train the neural network with five (5) nodes in the hidden layer
#Threshold is numeric value specifying the thredhold for the partial
#derivatives of the error function as stopping criteria
net_sqrt<- neuralnet(Output~., BC_conti, hidden=5, threshold = 0.01)
print(net_sqrt)

#plot the neural network
plot(net_sqrt)

#Test the neural network on some training data
net_results <- compute(net_sqrt, testing) # Run them through the neural network

#Lets see what properties net_sqrt has
str(net_results)
#Lets see the results
print(net_results$net.result)

#Lets display a better version of the results
cleanoutput <- cbind(testing$Output, as.data.frame(net_results$net.result))

colnames(cleanoutput) <- c("Expected Output", "Neural Net Output")
print(cleanoutput)

























