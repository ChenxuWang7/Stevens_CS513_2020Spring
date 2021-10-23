#  First Name      : Huijun
#  Last Name       : Sun
#  Id              : 10457635
#  purpose         : Using Support Vector Machine approach to create a classification model

rm(list = ls())

filename<-file.choose()

dsn<-  read.csv(filename)

#View(dsn)

#nrow(dsn)

dsn3<-na.omit(dsn[,-14])


#na.fail(dsn3)

#nrow(dsn3)

#View(dsn3)


############********************************* solution 3 (omit age & anuual rate) (wrong_rate: 43%)
mean<-mean(as.numeric(as.character(dsn3[,"ANNUAL_RATE"])))
mean

quantile_rate<-quantile(as.numeric(as.character(dsn3[,"ANNUAL_RATE"])))
rate25<-quantile_rate[2]
rate50<-quantile_rate[3]
rate75<-quantile_rate[4]

quantile_age<-quantile(as.numeric(as.character(dsn3[,"AGE"])))
age25<-quantile_age[2]
age50<-quantile_age[3]
age75<-quantile_age[4]

#quantile_rate
#quantile_age
###******************************

myData<-dsn3

ten_division<-quantile(as.numeric(as.character(myData[,"ANNUAL_RATE"])),seq(0.1,1,0.1))
#ten_division
ten_division_hrly_rate<-quantile(as.numeric(as.character(myData[,"HRLY_RATE"])),seq(0.1,1,0.1))
#ten_division_hrly_rate

m<-1;
while(m<=nrow(myData)){
  ####annual_rate
  temp<-as.numeric(as.character(myData[m,2]));
  #if(temp<rate25){
  #  myData[m,2]<-1;
  #}else if(temp<rate50){
  #  myData[m,2]<-2;
  #}else if(temp<rate75){
  #  myData[m,2]<-3;
  #}else{
  #  myData[m,2]<-4;
  #}
  
  n_count<-9;
  while(n_count>0)
  {
    if(temp>ten_division[n_count]){
       myData[m,2]<-n_count;
       break;
    }else{}
    n_count<-n_count-1;
  }
  if(n_count==0)
  {
    myData[m,2]<-n_count;
  }
  
  
  temp<-as.numeric(as.character(myData[m,3]));
  m_count<-9;
  while(m_count>0)
  {
    if(temp>ten_division_hrly_rate[m_count]){
      myData[m,3]<-m_count;
      break;
    }else{}
    m_count<-m_count-1;
  }
  if(m_count==0)
  {
    myData[m,3]<-m_count;
  }
  
  
  ####age
  temp<-as.numeric(as.character(myData[m,9]));
  if(temp<age25){
    myData[m,9]<-1;
  }else if(temp<age50){
    myData[m,9]<-2;
  }else if(temp<age75){
    myData[m,9]<-3;
  }else{
    myData[m,9]<-4;
  }
  m<-m+1;
}

#View(myData)

myData<-myData[c(-2935),]
myData[myData$ETHNICITY=="WHITE","ETHNICITY"]<- 0
myData[myData$ETHNICITY=="ASIAN","ETHNICITY"]<- 1
myData[myData$ETHNICITY=="BLACK","ETHNICITY"]<- 2
myData[myData$ETHNICITY=="HISPA","ETHNICITY"]<- 3
myData[myData$ETHNICITY=="TWO",  "ETHNICITY"]<- 4
myData[myData$ETHNICITY=="PACIF","ETHNICITY"]<- 5
myData[myData$ETHNICITY=="AMIND","ETHNICITY"]<- 6

#View(myData)

index<-sort(sample(nrow(myData),round(.3*nrow(myData))))

training<-myData[-index,c(2,5,6,7,8,9,10,13,14,16,19,20)]

test<-myData[index,c(2,5,6,7,8,9,10,13,14,16,19,20)]

library(e1071)

svm <- svm(factor(STATUS)~ANNUAL_RATE+ ETHNICITY + SEX + MARITAL_STATUS 
           +JOB_SATISFACTION+AGE+NUMBER_OF_TEAM_CHANGED+IS_FIRST_JOB
           +PERFORMANCE_RATING+EDUCATION_LEVEL, data = training)


#index<-sort(sample(nrow(myData),round(.3*nrow(myData))))
#training<-myData[-index,]
#test<-myData[index,]

#svm <- svm(STATUS~ANNUAL_RATE+HRLY_RATE+PREVYR_2+PREVYR_1+PREVYR_3+PREVYR_4, data = training)

svm_predict <- predict(svm,  test)

#View(svm_predict)

svm_table<-table(Actual=test[,"STATUS"],SVM=svm_predict)
svm_table
SVM_wrong<-sum(test[,"STATUS"]!=svm_predict)
SVM_wrong

error_rate=SVM_wrong/length(test$STATUS)
error_rate



# create more specific plots

library(dplyr)

myData.subset<-subset(myData,select=c("ANNUAL_RATE","HRLY_RATE","STATUS"),STATUS%in%c("T","A"))

plot(x=myData.subset$ANNUAL_RATE,y=myData.subset$HRLY_RATE,col=myData.subset$STATUS,pch=19)

svm.model<-svm(STATUS~.,data = myData.subset,kernel="linear",cost=1,scale = F)

points(myData.subset[svm.model$index,c(1,2)],col="blue",cex=2)

w=t(svm.model$coefs)%*%svm.model$SV

b=-svm.model$rho

abline(a=-b/w[1,2],b=-w[1,1]/w[1,2],col="red",lty=5)

#dev.off()

#model.myData<-svm(STATUS~ANNUAL_RATE+HRLY_RATE,data=myData)
#plot(model.myData,myData,ANNUAL_RATE~HRLY_RATE,slice=list(ANNUAL_RATE=as.numeric(ten_division[4]),HRLY_RATE=as.numeric( ten_division_hrly_rate[5])),pch=9)

#Confusion Matrix and Statistics
install.packages("caret")

library(caret)

confusionMatrix(svm_table)     
