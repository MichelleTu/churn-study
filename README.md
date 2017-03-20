# churn-study
library(readr)
library(rpart)
library(ROSE)
library(pROC)
library(caret)

Singel_Tree_Solution<- 
  function(fomula, Churn.data_s, ways, type, ...)
  {
    source("??.R")
    EX<-list()
    
    for (i in 1:5)
    {
      print(i)
      #data division
      id <- createDataPartition(data[,tgt], p = 1/2, list = FALSE)
      fold1 <- data[id, ]
      fold2 <- data[-id, ]
      
      Model_Fold1 <- ways$fit(form, fold1, type)
      trainingScore_Fold1 <- ways$pred(Model_Fold1, fold1)
      testingScore_Fold1  <- ways$pred(Model_Fold1, fold2)
      
      ModelFold2 <- ways$fit(form, fold2, type)
      trainingScore_Fold2 <- learner$pred(Model_Fold2, fold2)
      testingScore_Fold2  <- learner$pred(Model_Fold2, fold1)
      EX[[i]] <- 
        list(fold1_Label = fold1[, tgt],
             fold2_Label = fold2[, tgt],
             fold1_TrainingScore = trainingScore_Fold1,
             fold2_TrainingScore = trainingScore_Fold2,
             fold1_TestingScore = testingScore_Fold1,
             fold2_TestingScore = testingScore_Fold2      
        )
    }
    return(EX)
  }


#  prior probability

prior <- list(
  fit = function (form, data, type) { 
    source("prior.R")
    model  <- prior(form, data, type = type)
  },
  pred = function(object, data){
    out  <- predict(object, data, type = "probability") 
    out <- out[ ,2]
  }
)

# loss matrix
loss_matx <- list(
  fit = function (form, data, type) { 
    source("loss_matrix.R")
    model  <- loss_matix(form, data, type = type)
  },
  pred = function(object, data){
    out  <- predict(object, data, type = "probability") 
    out <- out[ ,2]
  }
)

# oversampling
oversamp <- list(
  fit = function (form, data, type) { 
    source("oversampling.R")
    model  <- oversampling(form, data)
  },
  pred = function(object, data){
    out  <- predict(object, data, type = "probability") 
    out <- out[ ,2]
  }
)


# undersampling
undersamp <- list(
  fit = function (form, data, type) { 
    source("undersampling.R")
    model  <- undersamping(form, data)
  },
  pred = function(object, data){
    out  <- predict(object, data, type = "probability") 
    out <- out[ ,2]
  }
)

# SMOTE
smo <- list(
  fit = function (form, data, type) { 
    source("smote.R")
    model  <- smote(form, data, type)
    return(model)
  },
  pred = function(object, data){
    out <- predict(object, data, type = "probability")
    out <- out[ ,2]
  }
)
