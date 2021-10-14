library(plumber)
library(randomForest)
#* return the input
#* 
#* @get /patrol

function(messg = ""){
  list(messg = paste0("Hi I am listening '", messg, "'"))
}

## Load the model
modellist = readRDS("titanic_rds.rds")
#* @get /predict
#* 0 for male and 1 for female
#* 1 for c 2 for s and 3 for q
predictions <- function(Pclass,Sex, Age, SibSp,Parch,Fare,Embarked){
  Pclass <-  as.numeric(Pclass)
  Sex=as.numeric(Sex)
  Age  <- as.numeric(Age)
  SibSp <-  as.numeric(SibSp)
  Parch <-  as.numeric(Parch)
  Fare <-  as.numeric(Fare)
  Embarked=Embarked
  
  
  X.new <- data.frame(Sex=Sex,
                      Pclass=Pclass ,
                      Age=Age,
                      SibSp=SibSp,
                      Parch=Parch,
                      Fare=Fare,
                      Embarked)
  
  #predict based on input
  
  
  ##predict(titanic_rds, new_data= X.new, type ="class")
  y.pred <- modellist$NewPredictions(model = modellist$modelobject, newdata = X.new)
  
  return(y.pred)
  
}



