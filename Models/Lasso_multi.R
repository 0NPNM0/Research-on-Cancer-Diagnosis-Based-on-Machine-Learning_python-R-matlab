
#Lasso惩罚逻辑回归拟合模型(多元分类)

LassoMultiModel <- function(data_for_class_1_train, 
                            data_for_class_1_validate,
                            data_for_class_2_train, 
                            data_for_class_2_validate,
                            data_for_class_3_train, 
                            data_for_class_3_validate,
                            data_for_class_test){
  
  library(glmnet)
  library(nnet)
  library(pROC)
  
  #将数据集分成训练集和测试集
  set.seed(12345) #保证每次跑的结果都一样
  #train_index <- sample(1:nrow(data_for_class_1_train), nrow(data_for_class_1_train) * 0.7) 
  #train_data <- data_for_class_1[train_index, ]
  #test_data <- data_for_class_1[-train_index, ]

  
  final_confusion_matrix <- matrix(0, nrow = 2, ncol = 2)
  
  mean1 <- 0.0000
  mean2 <- 0.0000
  mean3 <- 0.0000
  
    #对1预测(0.8)
    class_label <- 1
    
    binary_data <- as.matrix(data_for_class_1_train[,-c(1)])
    binary_test_data <- as.matrix(data_for_class_1_validate[,-c(1)])
    
    binary_results <- as.numeric(as.character(ifelse(data_for_class_1_train$results == class_label, 1, 0)))
    table(binary_results)
    binary_test_results <- as.numeric(as.character(ifelse(data_for_class_1_validate$results == class_label, 1, 0)))
    table(binary_test_results)
  for(i in 1:100){
    model_lasso_1 <- cv.glmnet(x = binary_data, y = binary_results, family = "binomial", alpha = 1, standardize = TRUE, type.measure = "class")
    
    train_predictions <- predict(model_lasso_1, newx = binary_test_data, type = "response")
    #train_predictions
    
    results <- as.numeric(ifelse(train_predictions >0.5, 1, 0))
    #results
    
    # 查看模型预测准确率
    mean1 <- mean1 + mean(binary_test_results == results)
    print(mean(binary_test_results == results))
    # 查看混淆矩阵
    #table(actual = binary_test_results, results)
    
    #binary_test_results
    #results
  } 
    mean1 <- mean1 / 100
    print("mean1:")
    print(mean1)
    
    #对2预测
    class_label <- 2
    
    binary_data <- as.matrix(data_for_class_2_train[,-c(1)])
    binary_test_data <- as.matrix(data_for_class_2_validate[,-c(1)])
    
    binary_results <- as.numeric(as.character(ifelse(data_for_class_2_train$results == class_label, 1, 0)))
    table(binary_results)
    binary_test_results <- as.numeric(as.character(ifelse(data_for_class_2_validate$results == class_label, 1, 0)))
    binary_test_results
  for(i in 1:100){
    model_lasso_2 <- cv.glmnet(x = binary_data, y = binary_results, family = "binomial", alpha = 1, standardize = TRUE, type.measure = "class")
    
    train_predictions <- predict(model_lasso_2, newx = binary_test_data, type = "response")
    #train_predictions
    
    results <- as.numeric(ifelse(train_predictions >0.5, 1, 0))
    #results
    
    # 查看模型预测准确率
    mean2 <- mean2 + mean(binary_test_results == results)
    print(mean(binary_test_results == results))
    # 查看混淆矩阵
    #table(actual = binary_test_results, results)
    
    #binary_test_results
    #results
  }
    
    mean2 <- mean2 / 100
    print("mean2:")
    print(mean2)
    
    #对3预测
    class_label <- 3
    
    binary_data <- as.matrix(data_for_class_3_train[,-c(1)])
    binary_test_data <- as.matrix(data_for_class_3_validate[,-c(1)])
    
    binary_results <- as.numeric(as.character(ifelse(data_for_class_3_train$results == class_label, 1, 0)))
    table(binary_results)
    binary_test_results <- as.numeric(as.character(ifelse(data_for_class_3_validate$results == class_label, 1, 0)))
    binary_test_results
  for(i in 1:100){
    model_lasso_3 <- cv.glmnet(x = binary_data, y = binary_results, family = "binomial", alpha = 1, standardize = TRUE, type.measure = "class")
    
    train_predictions <- predict(model_lasso_3, newx = binary_test_data, type = "response")
    #train_predictions
    
    results <- as.numeric(ifelse(train_predictions >0.5, 1, 0))
    #results
    
    # 查看模型预测准确率
    mean3 <- mean3 + mean(binary_test_results == results)
    print(mean(binary_test_results == results))
    # 查看混淆矩阵
    #table(actual = binary_test_results, results)
    
    #binary_test_results
    #results
  }
  
  mean3 <- mean3 / 100
  print("mean3:")
  print(mean3)
  
  
  
  return(final_confusion_matrix)
  
}
