Churn-study
运算示例： 

source("高级.R") \\
y <- SamplingSolution(churn ~ ., Churn.data_s,cart_tree)\\
y <- SamplingSolution(churn ~ ., Churn.data_s, cart_tree, "SMOTE", percOver = 100, percUnder = 100)\\ SMOTE抽样
y <- SamplingSolution(churn ~ ., Churn.data_s, cart_tree, "RandomSampling", percOver = 500, percUnder = 100)\\ 随机抽样
