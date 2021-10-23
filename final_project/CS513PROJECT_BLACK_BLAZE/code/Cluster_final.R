######################################
##Course Code : CS-513-B
##Prgram Name : Cluster_finalProject
##First  Name : Chenxu
##Last   Name : Wang
##Student ID  : 10457625
##Purpose     : Using hclust, categorize the “attrition_data.csv” 
##              data into clusters based on.
##              Using k-means, categorize the “attrition_data.csv” 
##              data into clusters based on. 
######################################
rm(list=ls())
dev.off()
#########################################################
#Load the “attrition_data.csv”
filename<-file.choose()
aData<-read.csv(filename, na.strings=c("?"))
View(aData)
summary(aData)

#Transfer ETHNICITY to numeric
aData<-aData[c(-2935),]#delete the null data
aData[aData$ETHNICITY=="WHITE","ETHNICITY_C"]<- 0
aData[aData$ETHNICITY=="ASIAN","ETHNICITY_C"]<- 1
aData[aData$ETHNICITY=="BLACK","ETHNICITY_C"]<- 1
aData[aData$ETHNICITY=="HISPA","ETHNICITY_C"]<- 1
aData[aData$ETHNICITY=="TWO",  "ETHNICITY_C"]<- 1
aData[aData$ETHNICITY=="PACIF","ETHNICITY_C"]<- 1
aData[aData$ETHNICITY=="AMIND","ETHNICITY_C"]<- 1

aData[aData$SEX=="M","SEX_C"]<- 1 
aData[aData$SEX=="F","SEX_C"]<- 0 

aData[aData$MARITAL_STATUS=="Divorced","MARITAL_STATUS_C"]<- 1 
aData[aData$MARITAL_STATUS=="Married", "MARITAL_STATUS_C"]<- 1 
aData[aData$MARITAL_STATUS=="Single",  "MARITAL_STATUS_C"]<- 0 

aData[aData$NUMBER_OF_TEAM_CHANGED=="0","NUMBER_OF_TEAM_CHANGED_C"] <- 0
aData[aData$NUMBER_OF_TEAM_CHANGED=="1","NUMBER_OF_TEAM_CHANGED_C"] <- 1
aData[aData$NUMBER_OF_TEAM_CHANGED=="2","NUMBER_OF_TEAM_CHANGED_C"] <- 2
aData[aData$NUMBER_OF_TEAM_CHANGED=="3","NUMBER_OF_TEAM_CHANGED_C"] <- 3
aData[aData$NUMBER_OF_TEAM_CHANGED=="3+","NUMBER_OF_TEAM_CHANGED_C"]<- 4

aData[aData$REHIRE=="TRUE","REHIRE_C"]<- 1 
aData[aData$REHIRE=="FALSE","REHIRE_C"]<- 0 

aData[aData$IS_FIRST_JOB=="N","IS_FIRST_JOB_C"]<- 1 
aData[aData$IS_FIRST_JOB=="Y","IS_FIRST_JOB_C"]<- 0 

aData[aData$TRAVELLED_REQUIRED=="N","TRAVELLED_REQUIRED_C"]<- 1 
aData[aData$TRAVELLED_REQUIRED=="Y","TRAVELLED_REQUIRED_C"]<- 0 

aData[aData$IS_FIRST_JOB=="N","IS_FIRST_JOB_C"]<- 1 
aData[aData$IS_FIRST_JOB=="Y","IS_FIRST_JOB_C"]<- 0 

aData[aData$DISABLED_EMP=="N","DISABLED_EMP_C"]<- 1 
aData[aData$DISABLED_EMP=="Y","DISABLED_EMP_C"]<- 0 

aData[aData$DISABLED_VET=="N","DISABLED_VET_C"]<- 1 
aData[aData$DISABLED_VET=="Y","DISABLED_VET_C"]<- 0 


#Omit "EMP_ID(-1)", "ETHNICITY(-5)", "SEX(-6)", "MARITAL_STATUS(-7)", "NUMBER_OF_TEAM_CHANGED(-10)", "REFERRAL_SOURCE(-11)", 
#"HIRE_MONTH(-12)", "REHIRE(-13)", "TERMINATION_YEAR(-14)"
#, "IS_FIRST_JOB(-15)", "TRAVELLED_REQUIRED(-16)", "DISABLED_EMP(-18)", "DISABLED_VET(-19)",
#"EDUCATION_LEVEL(-20)", "STATUS(-21)", "JOB_GROUP(-22)"
aData_c <- aData[,c(-1, -5, -6, -7, -10, -11, -12,-13, -14, -15 , -16, -18, -19, -20, -21, -22)]

#Delete the data includes a missing value
aData_c <-na.omit(aData_c)

#Standardize variables
aData_c <- scale(aData_c) 


################### hclust  ####################
#set a seed in order to get the same prediction when the same datasets are tested many times.
set.seed(1234)
#Calcualte the distance matrix between the rows of the data matrix
a_distance<-dist(aData_c)
#Hierarchical cluster analysis on a set of dissimilarities
hclust_results<-hclust(a_distance)
#Show the plot
dev.off()
plot(hclust_results, cex=0.6, hang=-1)
#Cut the tree into 6 clusters
hclust_6<-cutree(hclust_results,6)
#Tabulate the clustered rows against the “STATUS” column.
table(hclust_6,aData[,21])

################### k-means  ####################
#install.packages("fpc")
library(fpc)
dev.off()
summary(aData_c)
set.seed(1234)
#Perform k-means clustering on a data matrix.
#Cut the dataset into 6 clusters
kmeans_6<- kmeans(aData_c, 6,nstart = 2)
kmeans_6$cluster

#Tabulate the clustered rows against the “STATUS” column.
table(kmeans_6$cluster,aData[,21])

#Show the plot
plotcluster(aData_c, kmeans_6$cluster)

# When  we cut the datasets into 6 clusters, the cluster 1st 3rd 6th, more employee tend to stay at company;
# the clusters 2nd 4th 5th trend to leave the company. After checking the plot, we can simply set the two clusters
# up. So, the first cluster have a trend to stay at company; the others have a balance between leaving and staying.

#Perform k-means clustering on a data matrix.
#Cut the dataset into 2 clusters
set.seed(1234)
kmeans_2<- kmeans(aData_c, 2,nstart = 2)
kmeans_2$cluster

#Tabulate the clustered rows against the “STATUS” column.
table(kmeans_2$cluster,aData[,21])

#Show the plot
dev.off()
plotcluster(aData_c, kmeans_2$cluster)




