SamplingSolution <- 
  function(form, data, ways, sampling = NULL,...)
  {
    library(caret)
    tgt <- which(names(data) == as.character(form[[2]]))
    EX  <- vector("list", 2)
    
    for (i in 1:2)
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
      
      ModelFold1 <- ways$fit(form, fold1New)
      trainingScoreFold1 <- ways$pred(ModelFold1, fold1)
      testingScoreFold1  <- ways$pred(ModelFold1, fold2)
      
      ModelFold2 <- ways$fit(form, fold2New)
      trainingScoreFold2 <- ways$pred(ModelFold2, fold2)
      testingScoreFold2  <- ways$pred(ModelFold2, fold1)
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

# CART
cart_tree <- list(
  fit = function (form, data) { 
    library(rpart)
    model<- rpart(form, data, method = "class")
    return(model)
  },
  pred = function(object, data){
    out <- predict(object, newdata = data, type = "prob")
    out <- out[ ,2]
  }
)

# C50
C50_tree <- list(
  fit = function(form,data){
    library(C50)
    model <- C5.0(form, data)
    return(model)
  },
  pred = function(object,data){
    out <- predict(object, newdata = data, type = "prob")
    out <- out[,2]
  }
)
