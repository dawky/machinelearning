##Summary
"In the test below, we try to quantify how well people do certain activities. We use data from 
accelerometers on the belt,forearm, arm, and dumbell of 6 participants. They were asked to perform
barbell lifts correctly and incorrectly in 5 different ways. When fitting the model, I find the random 
forest model good for the experiment. The job I do are listed below. Information about the experiment
is available from the website here:
http://web.archive.org/web/20161224072740/http:/groupware.les.inf.pucrio.br/har"

##Data preprocessing
#In the following parts, I loaded the data and split it into training and testing sets.
library(caret)
setwd("D:/r/coursera/machinelearning")
dtrain<-read.csv("pml-training.csv")
dtest<-read.csv("pml-testing.csv")
set.seed(2000)
inTrain<-createDataPartition(y=dtrain$classe,p=0.7,list=F)
dtrain1<-dtrain[inTrain,]
dtest1<-dtrain[-inTrain,]

#I applied the nearZeroVar function to delete those unimportant factors
nzv<-nearZeroVar(dtrain1)
dtrain1<-dtrain[,-nzv]

##Delete NA
NAd<-is.na(dtrain1)
NAc<-which(colSums(NAd)/nrow(dtrain1)>0.95)
dtrain1<-dtrain1[,-NAc]

##Build model using the random forest
fit<-train(classe~.,data=dtrain1,method="rf")

##Evaluation of the model and see the out-of-sample error
confusionMatrix(test$classe, preds)

#the accuracy is high and the out-of-sample error is small, which means this is a good model

#apply the model to the test set
test<-predict(fit, newdata=dtest1)


