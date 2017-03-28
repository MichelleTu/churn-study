Churn-study
运算示例： 

source("code.R")
source("高级.R")
y <- SamplingSolution(class~.,Churn.data, cart_tree)
y <- SamplingSolution(churn ~ ., Churn.data_s,cart_tree)
