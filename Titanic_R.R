titanic=read.csv('titanic.csv',na.strings = '') # na.string is help for remove the na value
head(titanic)

library('dplyr')
titanic=select(titanic,-c('Name','Ticket','Cabin','PassengerId'))
head(titanic)
apply(titanic,2,function(x) sum(is.na(x)))
titanic$Age[is.na(titanic$Age)]=mean(titanic$Age,na.rm = TRUE)
titanic$Embarked[is.na(titanic$Embarked)]='S'

library('caTools')
sample=sample.split(titanic$Survived,SplitRatio = 0.70)

trainset=subset(titanic,sample==TRUE)
testset=subset(titanic,sample==FALSE)

library('randomForest')
titanic_rf=randomForest(Survived~.,data=trainset,family=binomial())   # glm(generalize linear model)

testset$tes_pred=predict(titanic_rf,testset,type = 'response') # response is used for convert the value in between 0 to 1
testset$test_pred_binary=ifelse(testset$tes_pred>0.50,1,0)

library('MLmetrics')
Accuracy(testset$test_pred_binary,testset$Survived)
ConfusionMatrix(testset$test_pred_binary,testset$Survived)
Recall(testset$Survived,testset$test_pred_binary,positive = 1)
Precision(testset$Survived,testset$test_pred_binary,positive = 1)
F1_Score(testset$Survived,testset$test_pred_binary,positive = 1)

## Make a prediction function
NewPredictions <- function(model, newdata){
  new.predictions <- predict(object = model, newdata = newdata)
  return(new.predictions)
  
}

modellist <- vector(mode = 'list')
# Save fitted model here.
modellist$modelobject <- titanic_rf
modellist$NewPredictions <- NewPredictions
saveRDS(object = modellist , file = 'titanic_rds.rds')


table(titanic$Parch)

