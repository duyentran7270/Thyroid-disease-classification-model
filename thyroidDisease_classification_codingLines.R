rm(list=ls())

library(rpart)
library(rpart.plot)
library(caTools)
library(class)
library(ggplot2)
library(dplyr)
library(scales)

mydata<-read.csv('Thyroid_data.csv',header=TRUE,sep=',')
str(mydata)

table(mydata$CLASS)
prop.table(table(mydata$CLASS))

ggplot(mydata,aes(x=T3))+
  geom_histogram()

ggplot(mydata,aes(x=TST))+
  geom_histogram()

ggplot(mydata,aes(x=TSTR))+
  geom_histogram()

ggplot(mydata,aes(x=TSH))+
  geom_histogram()

ggplot(mydata,aes(x=MAD.TSH))+
  geom_histogram()

mydata1<-select(mydata,T3,TST,TSTR,TSH,MAD.TSH)
mydata2<-select(mydata,CLASS)
mydata1=apply(mydata1,2,rescale,to=c(0,1))
mydata3<-data.frame(mydata2,mydata1)

sample<-sample.split(mydata$CLASS,SplitRatio=0.7)
train<-subset(mydata3,sample==TRUE)
test<-subset(mydata3,sample==FALSE)

prop.table(table(train$CLASS))
prop.table(table(test$CLASS))

mydata4<-mutate(mydata3,Train=sample)

treemdel1<-rpart(CLASS~.,data=train,method='class',minbucket=3)
rpart.plot(treemdel1)

treeTrain<-predict(treemdel1,train,type='class')
length(treeTrain)
treeTest<-predict(treemdel1,test,type='class')
length(treeTest)

table(train$CLASS,treeTrain)
table(test$CLASS,treeTest)  

treemdel1
rpart.rules(treemdel1)  

knnTest=knn(train[,2:6],test[,2:6],as.factor(train$CLASS),20)

table(treeTest)
table(knnTest)

sample0<-sample.split(mydata$CLASS,SplitRatio=0.7)
train0<-subset(mydata,sample==TRUE)
test0<-subset(mydata,sample==FALSE)  
knnTest0=knn(train0[,2:6],test0[,2:6],as.factor(train0$CLASS),20)  