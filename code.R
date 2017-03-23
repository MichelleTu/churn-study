
library(rpart)
library(C50)
library(ROSE)
library(pROC)
library(caret)

SamplingSolution <- 
  function(form, data, ways, sampling = NULL,...)
  {
    
    EX  <- vector("list", 2)
    for (i in 1:2)
    {
      #data division
      id <- createDataPartition(data[,churn], p = 1/2, list = FALSE)
      fold1 <- data[id, ]
      fold2 <- data[-id, ]
      fold1New <- fold1
      fold2New <- fold2
      
      # 问题：我是不是把具体的sampling 代码写在". R"里呢，docall后面应该也是在抽样，那这两个有什么区别呢，
      # sampling 代码直接调用
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
      
      ModelFold2 <- learner$fit(form, fold2New)
      trainingScoreFold2 <- ways$pred(ModelFold2, fold2)
      testingScoreFold2  <- ways$pred(ModelFold2, fold1)
      EX[[i]] <- 
        list(fold1Label = fold1[, churn],
             fold2Label = fold2[, churn],
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
    model<- rpart(form, data, method = "class")
    return(model)
  },
  pred = function(object, data){
    out <- predict(object, newdata = data, type = "probabillity")
    out <- out[ ,2]
  }
)

# C50
c50_tree <- list(
  fit = function(form,data){
    model <- C5.0(form, data)
    return(model)
  },
  pred = function(object,data){
    out <- predict(object, newdata = data, type = "probability")
    out <- out[,2]
  }
)
# 问题：一运行就是一大段，也没有显示有error，我在console里输入EX回车的话，会说object not found
