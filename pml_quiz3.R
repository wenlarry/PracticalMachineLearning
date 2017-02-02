#Quiz 3

#Q1
library(AppliedPredictiveModeling) 
data(segmentationOriginal) 
library(caret) 

data<-segmentationOriginal   
train<-segmentationOriginal[segmentationOriginal$Case
       =="Train",]  
test<-segmentationOriginal [segmentationOriginal$Case
      =="Test",]
#Remove Case var
train<-train[,-2]
test<-test[,-2]

set.seed(125) 
cartMod<-train(Class~.,method="rpart",data=train) 
cartMod$finalModel 
library(rattle) 
fancyRpartPlot(cartMod$finalModel) 

#Q3
library(pgmm) 
data(olive) 
olive= olive[,-1] 
treeMod<-train(Area~., data=olive,method="rpart") 
newdata=as.data.frame(t(colMeans(olive))) 
predict (treeMod,newdata) 

#Q4
library(ElemStatLearn) 
data(SAheart)
set.seed(8484) 
train=sample(1:dim(SAheart )[1]/2,replace=F) 
trainSA=SAheart [train,]
testSA=SAheart [-train,]

set.seed(13234)
lrMod<-train(chd~age      +alcohol +obesity+tobacco
            +typea +ldl     ,method="glm",family=
            "binomial",data=trainSA)    
predictTrain<-predict(lrMod,trainSA) 
predictTest<-predict(lrMod,testSA) 

misclass =function(values,prediction){sum(((prediction
         >0.5)*1) !=values)/length(values)} 

misclass(testSA$chd,predictTest) 
misclass(trainSA$chd,predictTrain) 

#5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y<-as.factor(vowel.train$y) 
vowel.test$y<-as.factor(vowel.test$y) 
set.seed(33833) 
modRF<-train(y~.,method="rf",data=vowel.train,
       prox=TRUE) 
modRF
varImp(modRF) 






