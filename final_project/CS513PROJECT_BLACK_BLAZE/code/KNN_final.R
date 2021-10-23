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
aData[aData$ETHNICITY=="BLACK","ETHNICITY_C"]<- 2
aData[aData$ETHNICITY=="HISPA","ETHNICITY_C"]<- 3
aData[aData$ETHNICITY=="TWO",  "ETHNICITY_C"]<- 4
aData[aData$ETHNICITY=="PACIF","ETHNICITY_C"]<- 5
aData[aData$ETHNICITY=="AMIND","ETHNICITY_C"]<- 6


#Transfer logistic (REHIRE) to numeric
aData[aData$REHIRE=="TRUE","REHIRE_C"]<- 1 
aData[aData$REHIRE=="FALSE","REHIRE_C"]<- 0 

#Discretize ANNUAL_RATE
group_A <- c(0, 50000, 100000, 1250924) 
ANNUAL_RATE_A <- cut(aData[,c(2)], breaks=group_A, labels=c("less than 50000","50000 to 100000","100000 or over"),
                     include.lowest=T, right=F)
Compare_ANNUAL_RATE_A<- data.frame(aData[,c(2)], ANNUAL_RATE_A)

#Discretize HRLY_RATE
group_A <- c(0, 30, 60, 608) 
HRLY_RATE_A <- cut(aData[,c(3)], breaks=group_A, labels=c("less than 30","30 to 60","60 or over"),
                   include.lowest=T, right=F)
Compare_HRLY_RATE_A<- data.frame(aData[,c(3)], HRLY_RATE_A)

#Discretize JOBCODE
group_A <- c(0, 30000, 70000, 99793) 
JOBCODE_A <- cut(aData[,c(4)], breaks=group_A, labels=c("less than 30000","30000 to 70000","70000 or over"),
                 include.lowest=T, right=F)
Compare_JOBCODE_A<- data.frame(aData[,c(4)], JOBCODE_A)

#Discretize AGE
group_A <- c(0, 30, 50, 64) 
AGE_A <- cut(aData[,c(9)], breaks=group_A, labels=c("less than 30","30 to 50","50 or over"),
             include.lowest=T, right=F)
Compare_AGE_A<- data.frame(aData[,c(9)], AGE_A)


#Combine the original date without:
#EMP_ID(-1), REFERRAL_SOURCE(-11), HIRE_MONTH(-12), TERMINATION_YEAR(-14), JOB_GROUP(-22)
adata<- data.frame(aData[,c(-1,-2, -3, -4, -5, -9, -11, -12, -13, -14, -22)], 
                   aData$REHIRE_C, ANNUAL_RATE_A, HRLY_RATE_A, JOBCODE_A, AGE_A, aData$ETHNICITY_C)


#Delete the data includes a missing value
adata <-na.omit(adata)

# Convert all the integer data types to factor data type.
adata[sapply(adata, is.integer)] <- lapply(adata[sapply(adata, is.integer)],as.factor)
adata[sapply(adata, is.logical)] <- lapply(adata[sapply(adata, is.logical)],as.factor)

View(adata)
summary(adata)
#The objects can be accessed by simply giving their names
attach(adata)

#set a seed in order to get the same prediction when the same datasets are tested many times.
set.seed(1234)

#Pick 30% data for testing and 70% data for training. 
index<-sort(sample(nrow(adata),round(.3*nrow(adata))))
training<-adata[-index,]
testing<-adata[index,]

dev.off()

######################### KNN  ########################

library(kknn)
predict_k1 <- kknn(formula= STATUS~., training , testing, k=1,kernel ="rectangular"  )

fit <- fitted(predict_k1)
length(fit)
length(testing$STATUS)
table(testing$STATUS,fit)

wrong<- (testing$STATUS!=fit)
rate<-sum(wrong)/length(wrong)
rate

for(i in c(1,2,5,10,15,20)){
  predict <- kknn(formula= STATUS~., training , testing, k=i,kernel ="rectangular"  )
  
  fit <- fitted(predict)
  
  #e.	Measure the performance of knn
  
  wrong<- (testing$STATUS!=fit)
  rate<-sum(wrong)/length(wrong)
  rate
  print('***************')
  print(i)
  print( table(testing$STATUS,fit))
  print( rate)
  print('***************') 
}







