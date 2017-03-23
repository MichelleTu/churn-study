# ==============================================
#  Experimental comparision of Sampling solution 
# ==============================================

SamplingSolution <- 
  function(form, data, learner, sampling = NULL,...)
  {
    require("caret")
    source("Numeralize.R")
    tgt <- which(names(data) == as.character(form[[2]]))
    #EX  <- vector("list", 5)
    EX<-list()
    
    for (i in 1:5)
    {
      #data division
      id <- createDataPartition(data[,tgt], p = 1/2, list = FALSE)
      fold1 <- data[id, ]
      fold2 <- data[-id, ]
      fold1New <- fold1
      fold2New <- fold2
      if (!is.null(sampling))
      {
        sourcefile <- paste(sampling, c(".R"), sep="")
        source(sourcefile)
        fold1New <- do.call(sampling, list(form, fold1, ...))
        fold2New <- do.call(sampling, list(form, fold2, ...))
      }  
      
      ModelFold1 <- learner$fit(form, fold1New)
      trainingScoreFold1 <- learner$pred(ModelFold1, fold1)
      testingScoreFold1  <- learner$pred(ModelFold1, fold2)
      
      ModelFold2 <- learner$fit(form, fold2New)
      trainingScoreFold2 <- learner$pred(ModelFold2, fold2)
      testingScoreFold2  <- learner$pred(ModelFold2, fold1)
      EX[[i]] <- 
        list(fold1Label = fold1[, tgt],
             fold2Label = fold2[, tgt],
             fold1TrainingScore = trainingScoreFold1,
             fold2TrainingScore = trainingScoreFold2,
             fold1TestingScore = testingScoreFold1,
             fold2TestingScore = testingScoreFold2      
        )
    }
    return(EX)
  }



#  Logistic regression 


Logit <- list(
  fit = function (form, data) { 
    model  <- glm(form, family = binomial(link = "logit"), data)
    return(model)
  },
  pred = function(object, data){
    out  <- predict(object, data, type = "response") 
  }
)

#  Support Vector machine

SVM <- list(
  fit = function (form, data) { 
    library("kernlab")
    dataNumeric <- Numeralize(data, form)
    model  <- ksvm(form, data = dataNumeric , kernel = "rbfdot", prob.model = TRUE)
    return(model)
  },
  pred = function(object, data){
    form <- formula(object@terms)
    dataNumeric <- Numeralize(data, form)
    out  <- predict(object, dataNumeric, type = "probabilities") 
    out <- out[ ,2]
  }
)

# Decision Tree

Tree <- list(
  fit = function (form, data) { 
    library("RWeka")
    model<- J48(form, data, control = Weka_control(U = TRUE, A = TRUE))
    return(model)
  },
  pred = function(object, data){
    out <- predict(object, newdata = data, type = "probability")
    out <- out[ ,2]
  }
)

# Random Forest
RF <-  list(
  fit = function (form, data) { 
    library("randomForest")
    model<- randomForest(form, data, ntree=40)
    return(model)
  },
  pred = function(object, data){
    out <- predict(object, data, type = "prob")
    out <- out[ ,2]
  }
)

