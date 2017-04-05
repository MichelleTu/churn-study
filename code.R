SamplingSolution <- 
  function(form, Churn.data_s, ways, sampling = NULL,...)
  {
    library(caret)
    tgt <- which(names(data) == as.character(form[[2]]))
    Ex <- vector("list", 2)
    
    for(i in 1:2)
    {
      
      # data division
      id <- createDataPartition(data[,tgt], p = 1/2,list = FALSE)
      fold1 <- Churn.data_s[id,]
      fold2 <- Churn.data_s[-id,]
      fold1_new <- fold1
      fold2_new <- fold2

      if(!is.null(sampling))
      {
        sourcefile <- paste(sampling, c(".R"), sep = "")
        source(sourcefile)
        fold1_new <- do.call(sampling, list(form, fold1,...))
        fold2_new <- do.call(sampling, list(form, fold2,...))
      }
      

      Model_fold1 <- ways$fit(form, fold1_new)
      training_Score_fold1 <- ways$pred(Model_fold1, fold1)
      testing_score_fold1  <- ways$pred(Model_fold1, fold2)
      
      Model_fold2 <- ways$fit(form,fold2_new)
      training_score_fold2 <- ways$pred(Model_fold2, fold2)
      testing_score_fold2  <- ways$pred(Model_fold2,fold1)
      
      Ex[[i]] <- 
        list(fold1_label = fold1[,tgt],
             fold2_label = fold2[,tgt],
             fold1_training_score = training_Score_fold1,
             fold2_training_score = training_score_fold2,
             fold1_testing_score = testing_score_fold1,
             fold2_testing_score = testing_score_fold2
             )
    }
    return(EX)
  }


# CART
cart_tree <- list(
  fit = function(form,data){
    library(rpart)
    model <- rpart(form, data, method = "class")
    return(model)
  },
  pred = function(object, data){
    out <- predict(object,newdata = data, type = "prob")
    out <- out[ ,2]
  }
)


# C50
C50_tree <-list(
  library(C50)
  fit = function(form, data){
    model <- C5.0(form, data)
    return(model)
  },
  pred = function(object, data){
    out <- predict(object, newdata = data, type = "prob")
    out <- out[ ,2]
  }
)

