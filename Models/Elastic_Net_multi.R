
#Elastic-net惩罚逻辑回归拟合模型(多元分类)

ElasticNetMultiModel <- function(data_for_class_1_train, 
                            data_for_class_1_validate,
                            data_for_class_2_train, 
                            data_for_class_2_validate,
                            data_for_class_3_train, 
                            data_for_class_3_validate,
                            data_for_class_test){
  
  library(glmnet)
  library(nnet)
  library(pROC)
  
 
  #final_confusion_matrix <- matrix(0, nrow = 2, ncol = 2)
  
  mean_final <- 0.0000
  
  for(i in 1:100){
    #对1预测(0.8)
    class_label <- 1
    
    binary_data <- as.matrix(data_for_class_1_train[,-c(1)])
    binary_test_data <- as.matrix(data_for_class_1_validate[,-c(1)])
    
    binary_results <- as.numeric(as.character(ifelse(data_for_class_1_train$results == class_label, 1, 0)))
    #table(binary_results)
    binary_test_results <- as.numeric(as.character(ifelse(data_for_class_1_validate$results == class_label, 1, 0)))
    #table(binary_test_results)
    
    model_lasso_1 <- cv.glmnet(x = binary_data, y = binary_results, family = "binomial", alpha = 0.5, standardize = TRUE, type.measure = "class")
    
    train_predictions <- predict(model_lasso_1, newx = binary_test_data, type = "response")
    #train_predictions
    
    results <- as.numeric(ifelse(train_predictions >0.5, 1, 0))
    #results
    
    # 查看模型预测准确率
    print("validate 1:")
    print(mean(binary_test_results == results))
    # 查看混淆矩阵
    #table(actual = binary_test_results, results)
    
    #binary_test_results
    #results
    
    
    
    
    
    #对2预测
    class_label <- 2
    
    binary_data <- as.matrix(data_for_class_2_train[,-c(1)])
    binary_test_data <- as.matrix(data_for_class_2_validate[,-c(1)])
    
    binary_results <- as.numeric(as.character(ifelse(data_for_class_2_train$results == class_label, 1, 0)))
    #table(binary_results)
    binary_test_results <- as.numeric(as.character(ifelse(data_for_class_2_validate$results == class_label, 1, 0)))
    #table(binary_test_results)
    
    model_lasso_2 <- cv.glmnet(x = binary_data, y = binary_results, family = "binomial", alpha = 0.5, standardize = TRUE, type.measure = "class")
    
    train_predictions <- predict(model_lasso_2, newx = binary_test_data, type = "response")
    #train_predictions
    
    results <- as.numeric(ifelse(train_predictions >0.5, 1, 0))
    #results
    
    # 查看模型预测准确率
    print("validate 2:")
    print(mean(binary_test_results == results))
    # 查看混淆矩阵
    #table(actual = binary_test_results, results)
    
    #binary_test_results
    #results
    
    
    
    #对3预测
    class_label <- 3
    
    binary_data <- as.matrix(data_for_class_3_train[,-c(1)])
    binary_test_data <- as.matrix(data_for_class_3_validate[,-c(1)])
    
    binary_results <- as.numeric(as.character(ifelse(data_for_class_3_train$results == class_label, 1, 0)))
    #table(binary_results)
    binary_test_results <- as.numeric(as.character(ifelse(data_for_class_3_validate$results == class_label, 1, 0)))
    #table(binary_test_results)
    
    model_lasso_3 <- cv.glmnet(x = binary_data, y = binary_results, family = "binomial", alpha = 0.5, standardize = TRUE, type.measure = "class")
    
    train_predictions <- predict(model_lasso_3, newx = binary_test_data, type = "response")
    #train_predictions
    
    results <- as.numeric(ifelse(train_predictions >0.5, 1, 0))
    #results
    
    # 查看模型预测准确率
    print("validate 3:")
    print(mean(binary_test_results == results))
    # 查看混淆矩阵
    #table(actual = binary_test_results, results)
    
    #binary_test_results
    #results
    
    
    
    
    test_data <- as.matrix(data_for_class_test[,-c(1)])
    test_result <- as.numeric(as.character(data_for_class_test$results)) 
    
    test_predictions_1 <- predict(model_lasso_1, newx = test_data, type = "response")
    test_predictions_1
    
    test_predictions_2 <- predict(model_lasso_2, newx = test_data, type = "response")
    test_predictions_2
    
    test_predictions_3 <- predict(model_lasso_3, newx = test_data, type = "response")
    test_predictions_3
    
    type <- 1
    predict_results <- c()
    for(i in 1:nrow(data_for_class_test)){
      p1 = test_predictions_1[i]#p:prediction
      #p1
      p2 = test_predictions_2[i]
      #p2
      p3 = test_predictions_3[i]
      #p3
      
      if((p1>p2 && p2>p3) || (p1>p3 && p3 > p2)){
        final_type <- 1
      }else if((p2>p1 && p1>p3) || (p2>p3 && p3 > p1)){
        final_type <- 2
      }else if((p3>p1 && p1>p2) || (p3>p2 && p2 > p1)){
        final_type <- 3
      }
      #final_type
      
      predict_results <- c(predict_results, final_type)
      #print(predict_results)
    }
    #predict_results
    mean_final <- mean_final + mean(predict_results == test_result)
    #predict_results
    #test_result
  }
  
  print(mean_final / 100)
  
  return(final_confusion_matrix)
  
}
