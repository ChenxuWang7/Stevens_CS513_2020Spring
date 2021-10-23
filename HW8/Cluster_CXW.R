######################################
##Course Code : CS-513-B
##Prgram Name : HW08_Cluster
##First  Name : Chenxu
##Last   Name : Wang
##Student ID  : 10457625
##Purpose     :8.1 Using hclust, categorize the “wisc_bc_ContinuousVar.csv” 
##             data into two (2) clusters based on.
##             8.2 Using k-means, categorize the “wisc_bc_ContinuousVar.csv” 
##             data into two (2) clusters based on. 
######################################
rm(list=ls())
dev.off()
#########################################################
#Load the “wisc_bc_ContinuousVar.csv”
filename<-file.choose()
WBC_conti<-read.csv(filename, na.strings=c("?"))
View(WBC_conti)

#Delete the data includes a missing value
WBC_conti <-na.omit(WBC_conti)
#Omit "ID" and "diagnosis"
BC_conti <- WBC_conti[,c(-1, -2)]


#Standardize variables
BC_conti <- scale(BC_conti) 

#The objects can be accessed by simply giving their names
#attach(BC_conti)

###################8.1 hclust  ####################
#Calcualte the distance matrix between the rows of the data matrix
BC_distance<-dist(BC_conti)

#Hierarchical cluster analysis on a set of dissimilarities
hclust_results<-hclust(BC_distance)
#Show the plot
plot(hclust_results)
#Cut the tree into 2 clusters
hclust_2<-cutree(hclust_results,2)
#Tabulate the clustered rows against the “diagnosis” column.
table(hclust_2,WBC_conti[,2])

###################8.2 k-means  ####################
?kmeans
#Perform k-means clustering on a data matrix.
kmeans_2<- kmeans(BC_conti,2,nstart = 10)
kmeans_2$cluster
#Tabulate the clustered rows against the “diagnosis” column.
table(kmeans_2$cluster,WBC_conti[,2])








