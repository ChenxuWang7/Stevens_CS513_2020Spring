######################################
##Course Code : CS-513-B
##Prgram Name : Problem 1
##First  Name : Chenxu
##Last   Name : Wang
##Student ID  : 10457625
##Purpose     :
##             1 Using k-means
1 ###          2 Using hclust
######################################
rm(list=ls())
dev.off()
#########################################################
#Load the “Admission.csv”
filename<-file.choose()
ADM<-read.csv(filename, na.strings=c("?"))
View(ADM)

#Delete the data includes a missing value
ADM <-na.omit(ADM)
#Omit "Applicant"
AD<- ADM[,c(-1, -2, -5)]

#Standardize variables
AD <- scale(AD) 

###################1 k-means  ####################
#Perform k-means clustering on a data matrix.
kmeans_2<- kmeans(AD,2,nstart = 2)
kmeans_2$cluster
#Tabulate the clustered rows against the “Admit” column.
table(kmeans_2$cluster,ADM[,2])


################### 2 hclust  ####################
#Calcualte the distance matrix between the rows of the data matrix
BC_distance<-dist(AD)

#Hierarchical cluster analysis on a set of dissimilarities
hclust_results<-hclust(BC_distance)
#Show the plot
plot(hclust_results)
#Cut the tree into 2 clusters
hclust_2<-cutree(hclust_results,2)
#Tabulate the clustered rows against the “Admit” column.
table(hclust_2,ADM[,2])







