#Quiz 4

#Q1
library(ElemStatLearn);library(caret)
data(vowel.train);data(vowel.test) 

vowel.train$y<-as.factor(vowel.train$y) 
vowel.test$y<-as.factor(vowel.test$y)  

set.seed(33833)
modRF<-train(y~.,method="rf",data=vowel.train)
modGBM<-train(y~.,method="gbm",data=vowel.train) 

pRF<-predict(modRF,vowel.test)
pGBM<-predict(modGBM,vowel.test) 

missClassFactor=function(values,prediction)
        {sum(prediction!=values)/length(values)} 

1-missClassFactor(vowel.test$y,pRF) 
1-missClassFactor(vowel.test$y,pGBM) 

Agree<-pRF==pGBM
pred.agree<-pRF[Agree]
test.agree<-vowel.test[Agree,] 
1-missClassFactor(test.agree$y,pred.agree) 

#Q2
library (gbm);library(AppliedPredictiveModeling);
library(caret) 
set.seed(3433) 
data(AlzheimerDisease) 
adData=data.frame(diagnosis,predictors) 
inTrain=createDataPartition(adData $diagnosis,p=3/4)[[1]]
training=adData[inTrain,]
testing=adData[-inTrain,] 

set.seed(62433) 
modRF<-train(diagnosis~.,method="rf",data=training ) 
modGBM<-train(diagnosis~., method="gbm", data=training) 
modLDA<-train(diagnosis~., method="lda",data=training) 

pRF<-predict(modRF,testing) 
pGBM<-predict(modGBM,testing) 
pLDA<-predict(modLDA,testing) 

#ensembling models
pDF<-data.frame(pRF,pGBM,pLDA,wage=testing$diagnosis) 
comMod<-train(wage~.,method="rf",data=pDF) 

pCom<-predict(comMod,testing$diagnosis) 

#Accuracies
confusionMatrix(pCom,testing$diagnosis)
# 0.8415
confusionMatrix(pRF,testing$diagnosis) 
#0.7927
confusionMatrix(pGBM,testing$diagnosis) 
#0.8049
confusionMatrix(pLDA,testing$diagnosis) 
#0.8049

#Alternatives
#1-missClassFactor(testing$diagnosis,pCom) 
#1-missClassFactor(testing$diagnosis,pRF) 
#1-missClassFactor(testing$diagnosis,pGBM) 
#1-missClassFactor(testing$diagnosis,pLDA) 

#Q3
library(AppliedPredictiveModeling);library(caret);
data(concrete)
set.seed(3523) 

inTrain<-createDataPartition(y=concrete    $
         CompressiveStrength,p=3/4)[[1]]
training<-concrete     [inTrain,]
testing<-concrete   [-inTrain,] 

set.seed(233)

modLasso<-train(CompressiveStrength~.,method="lasso", 
         data=training) 

modLasso
plot.enet(modLasso$finalModel,xvar="penalty",use.color=T) 

#Q4
library(lubridate) 
library(forecast) 

#Use R to import the URL  that contains the data i.e. dat
training<-dat    [year(dat$date)<2012,]
testing<-dat      [year(dat$date)>2011,] 
tstrain<-ts (training$visitsTumblr) 
 
# fit a time series model
fit<-bats(tstrain) 
pred<-forecast(fit,level=95,h=dim(testing)[1]) 
predComb<-cbind(testing,data.frame(pred)) 

# % of actual values in the 95% prediction interval bounds
predComb$in95<-(predComb$Lo.95<predComb$visitsTumblr)&
               (predComb$visitsTumblr<predComb$Hi.95) 
prop.table(table(predComb$in95))[2] 

#Q5
set.seed(3523) 
library(AppliedPredictiveModeling)
data(concrete) 
inTrain = createDataPartition(concrete 
$CompressiveStrength, p = 3/4)[[1]]
training=concrete [inTrain,]
testing=concrete  [-inTrain,]

set.seed(325)
fit<-svm(CompressiveStrength~.,data=training) 
pred<-predict(fit,testing) 
acc<-accuracy(pred,testing$CompressiveStrength)
acc[2] 





