#Quiz 2

#Q1
library(AppliedPredictiveModeling) 
library(caret)
data(AlzheimerDisease) 

adData=data.frame(diagnosis,predictors)
trainIndex=createDataPartition(diagnosis,p=0.50,list=FALSE) 
training=adData[trainIndex,]
testing=adData[-trainIndex,] 

#Q2
library(AppliedPredictiveModeling)
data(concrete) 
library(caret) 
set.seed(1000)
inTrain=createDataPartition(mixtures  $CompressiveStrength,
                            p=3/4)[[1]]
training=mixtures    [inTrain,]
testing=mixtures   [inTrain,] 

library(Hmisc) 
cutCompStr<-cut2(training$CompressiveStrength,g=4) 
summary(cutCompStr) 
index<-seq_along(1:nrow(training)) 
ggplot(data=training,aes(y=index,x=cutCompStr))+
      geom_boxplot() +geom_jitter(col="purple")       
      
# A non random pattern in the plot (outcome vs index)
# that does not appear to be explained by any predictor.
# It suggests a missing variable.

#Q3
library(AppliedPredictiveModeling)
data(concrete) 
library(caret) 
set.seed(1000)
inTrain=createDataPartition(mixtures     $CompressiveStrength,
                            p=3/4)[[1]]
training=mixtures       [inTrain,]
testing=mixtures     [inTrain,] 

hist(training$Superplasticizer)
hist(log(training$Superplasticizer))
log(training$Superplasticizer) 

# there are zero values, when log() tranform, those values
# will be -Inf

#Q4
library(caret) 
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease) 
adData=data.frame(diagnosis,predictors)
inTrain= createDataPartition(adData$diagnosis,p=3/4)[[1]]
training=adData[inTrain,]
testing =adData[-inTrain,] 

preProc<-preProcess(training[,grep("^IL",names
        (testing))],method="pca",thresh=0.8) 
preProc$rotation

# 7

#Q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease) 
adData=data.frame(diagnosis,predictors) 
inTrain= createDataPartition(adData$diagnosis,p=3/4)[[1]]
training=adData[inTrain,]
testing =adData[-inTrain,] 

training<-data.frame(training$diagnosis,training
        [,grep("^IL",names(testing))])  
modelFit1<-train(training.diagnosis ~.,method=
           "glm",data=training)  
confusionMatrix(testing$diagnosis,predict(modelFit1,
                testing)) 

modelFit2<-train(training.diagnosis~.,method="glm",
         data=training,preProcess="pca",trControl=
         trainControl(preProcOptions=list(thresh=0.8))) 
 
confusionMatrix(testing$diagnosis,predict(modelFit2,
                testing)) 


 #Non PCA Accuracy = 0.65 ; PCA Accuracy = 0.72

        

                          