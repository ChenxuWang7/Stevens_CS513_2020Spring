#  First Name      : Huijun
#  Last Name       : Sun
#  Id              : 10457635
#  purpose         : Using Classification And Regression Tree approach to classify datasets


rm(list = ls())

filename<-file.choose()

dsn<-  read.csv(filename,header = TRUE, sep = ',', na.strings = '?', stringsAsFactors = FALSE)

#View(dsn)

#nrow(dsn)

dsn3<-na.omit(dsn[,-14])

#na.fail(dsn3)

#nrow(dsn3)

#View(dsn3)

install.packages("rpart")
install.packages("rpart.plot")     # Enhanced tree plots
install.packages("rattle")         # Fancy tree plot
install.packages("RColorBrewer")   # colors needed for rattle
library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle


############********************************* solution 1 (wrong_rate: 44%)
dsn3<-dsn3[c(-2935),]

index<-sort(sample(nrow(dsn3),round(.3*nrow(dsn3))))

training<-dsn3[-index,]

test<-dsn3[index,]

CART_class<-rpart(formula = STATUS~. ,method = "class",data=training)

#rpart.plot(CART_class)
fancyRpartPlot(CART_class)

CART_predict<-predict(CART_class,test, type="class")
#View(CART_predict)

table(Actual=test[,"STATUS"],CART=CART_predict)

CART_wrong<-sum(test[,"STATUS"]!=CART_predict)
CART_wrong

error_rate=CART_wrong/length(test$STATUS)
error_rate

############*********************************


############********************************* solution 2 (omit age & anuual rate) (wrong_rate: 41%)
mean<-mean(as.numeric(as.character(dsn3[,"ANNUAL_RATE"])))
#mean

quantile_rate<-quantile(as.numeric(as.character(dsn3[,"ANNUAL_RATE"])))
rate25<-quantile_rate[2]
rate50<-quantile_rate[3]
rate75<-quantile_rate[4]

quantile_age<-quantile(as.numeric(as.character(dsn3[,"AGE"])))
age25<-quantile_age[2]
age50<-quantile_age[3]
age75<-quantile_age[4]

###******************************
myData<-dsn3
m<-1;
while(m<=nrow(myData)){
  ####annual_rate
  temp<-as.numeric(as.character(myData[m,2]));
  if(temp<rate25){
    myData[m,2]<-1;
  }else if(temp<rate50){
    myData[m,2]<-2;
  }else if(temp<rate75){
    myData[m,2]<-3;
  }else{
    myData[m,2]<-4;
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


index<-sort(sample(nrow(myData),round(.3*nrow(myData))))

training<-myData[-index,c(2,5,6,7,8,9,10,13,14,16,19,20)]

test<-myData[index,c(2,5,6,7,8,9,10,13,14,16,19,20)]

CART_class<-rpart(formula = STATUS~ANNUAL_RATE+ ETHNICITY + SEX + MARITAL_STATUS 
                  +JOB_SATISFACTION+AGE+NUMBER_OF_TEAM_CHANGED+IS_FIRST_JOB
                  +PERFORMANCE_RATING+EDUCATION_LEVEL ,method = "class",data=training)

#rpart.plot(CART_class)
fancyRpartPlot(CART_class)

CART_predict<-predict(CART_class,test, type="class")

#View(CART_predict)
table(Actual=test[,"STATUS"],CART=CART_predict)

CART_wrong<-sum(test[,"STATUS"]!=CART_predict)
CART_wrong

error_rate=CART_wrong/length(test$STATUS)
error_rate

