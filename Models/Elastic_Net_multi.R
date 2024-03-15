
#Elastic-net惩罚逻辑回归拟合模型(多元分类)

ElasticNetMultiModel <- function(data_for_class_1_train, 
                            data_for_class_1_validate,
                            data_for_class_2_train, 
                            data_for_class_2_validate,
                            data_for_class_3_train, 
                            data_for_class_3_validate,
                            data_for_class_test){
  
  library(glmnet)
  
  
  mean_train <- 0.0000
  mean_train_1 <- 0.0000
  mean_train_2 <- 0.0000
  mean_train_3 <- 0.0000
  mean_validate_1 <- 0.0000
  mean_validate_2 <- 0.0000
  mean_validate_3 <- 0.0000
  mean_test <- 0.0000
  
  for(i in 1:500){
    
    #对1预测
    class_label <- 1
    
    binary_train_data <- as.matrix(data_for_class_1_train[,-c(1)])
    binary_validate_data <- as.matrix(data_for_class_1_validate[,-c(1)])
    
    binary_train_results <- as.numeric(as.character(ifelse(data_for_class_1_train$results == class_label, 1, 0)))
    binary_validate_results <- as.numeric(as.character(ifelse(data_for_class_1_validate$results == class_label, 1, 0)))
    
    model_enet1 <- cv.glmnet(x = binary_train_data, y = binary_train_results, family = "binomial", alpha = 0.5, standardize = TRUE, type.measure = "class")
    model_enet_1 <- cv.glmnet(x = binary_train_data, y = binary_train_results, family = "binomial", alpha = 0.5, lambda = model_enet1$lambda.min, standardize = TRUE, type.measure = "class")
    
    #训练集预测概率
    train_predictions <- predict(model_enet_1, newx = binary_train_data, type = "response")
    results <- as.numeric(ifelse(train_predictions >0.5, 1, 0))
    
    # 查看模型在训练集上的预测准确率
    print("train 1:")
    print(mean(binary_train_results == results))
    mean_train_1 <- mean_train_1 + mean(binary_train_results == results)
    
    
    #验证集预测概率
    validate_predictions <- predict(model_enet_1, newx = binary_validate_data, type = "response")
    results <- as.numeric(ifelse(validate_predictions >0.5, 1, 0))
    
    # 查看模型在验证集上的预测准确率
    print("validate 1:")
    print(mean(binary_validate_results == results))
    mean_validate_1 <- mean_validate_1 + mean(binary_validate_results == results)
    
    
    
    #对2预测
    class_label <- 2
    
    binary_train_data <- as.matrix(data_for_class_2_train[,-c(1)])
    binary_validate_data <- as.matrix(data_for_class_2_validate[,-c(1)])
    
    binary_train_results <- as.numeric(as.character(ifelse(data_for_class_2_train$results == class_label, 1, 0)))
    binary_validate_results <- as.numeric(as.character(ifelse(data_for_class_2_validate$results == class_label, 1, 0)))
    
    model_enet2 <- cv.glmnet(x = binary_train_data, y = binary_train_results, family = "binomial", alpha = 0.5, standardize = TRUE, type.measure = "class")
    model_enet_2 <- cv.glmnet(x = binary_train_data, y = binary_train_results, family = "binomial", alpha = 0.5, lambda = model_enet2$lambda.min, standardize = TRUE, type.measure = "class")
    
    #训练集预测概率
    train_predictions <- predict(model_enet_2, newx = binary_train_data, type = "response")
    results <- as.numeric(ifelse(train_predictions >0.5, 1, 0))
    
    # 查看模型在训练集上的预测准确率
    print("train 2:")
    print(mean(binary_train_results == results))
    mean_train_2 <- mean_train_2 + mean(binary_train_results == results)
    
    
    #验证集预测概率
    validate_predictions <- predict(model_enet_2, newx = binary_validate_data, type = "response")
    results <- as.numeric(ifelse(validate_predictions >0.5, 1, 0))
    
    # 查看模型在验证集上的预测准确率
    print("validate 2:")
    print(mean(binary_validate_results == results))
    mean_validate_2 <- mean_validate_2 + mean(binary_validate_results == results)
    
    
    
    #对3预测
    class_label <- 3
    
    binary_train_data <- as.matrix(data_for_class_3_train[,-c(1)])
    binary_validate_data <- as.matrix(data_for_class_3_validate[,-c(1)])
    
    binary_train_results <- as.numeric(as.character(ifelse(data_for_class_3_train$results == class_label, 1, 0)))
    binary_validate_results <- as.numeric(as.character(ifelse(data_for_class_3_validate$results == class_label, 1, 0)))
    
    model_enet3 <- cv.glmnet(x = binary_train_data, y = binary_train_results, family = "binomial", alpha = 0.5, standardize = TRUE, type.measure = "class")
    model_enet_3 <- cv.glmnet(x = binary_train_data, y = binary_train_results, family = "binomial", alpha = 0.5, lambda = model_enet3$lambda.min, standardize = TRUE, type.measure = "class")
    
    #训练集预测概率
    train_predictions <- predict(model_enet_3, newx = binary_train_data, type = "response")
    results <- as.numeric(ifelse(train_predictions >0.5, 1, 0))
    
    # 查看模型在训练集上的预测准确率
    print("train 3:")
    print(mean(binary_train_results == results))
    mean_train_3 <- mean_train_3 + mean(binary_train_results == results)
    
    
    #验证集预测概率
    validate_predictions <- predict(model_enet_3, newx = binary_validate_data, type = "response")
    results <- as.numeric(ifelse(validate_predictions >0.5, 1, 0))
    
    # 查看模型在验证集上的预测准确率
    print("validate 3:")
    print(mean(binary_validate_results == results))
    mean_validate_3 <- mean_validate_3 + mean(binary_validate_results == results)
    
  }
  
  #测试模型
  test_data <- as.matrix(data_for_class_test[,-c(1)])
  test_result <- as.numeric(as.character(data_for_class_test$results)) 
  
  test_predictions_1 <- predict(model_enet_1, newx = test_data, type = "response")
  test_predictions_1
  
  test_predictions_2 <- predict(model_enet_2, newx = test_data, type = "response")
  test_predictions_2
  
  test_predictions_3 <- predict(model_enet_3, newx = test_data, type = "response")
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
    
    predict_results <- c(predict_results, final_type)
    
  }
  
  predict_results
  test_result
  
  mean_test <- mean(predict_results == test_result)
  confusion_matrix <- table(actual = test_result, predict_results)
  
  print("mean_test:")
  accuracy <- mean_test
  print(accuracy)
  
  print("mean_train_1:")
  accuracy <- mean_train_1 / 500
  print(accuracy)
  
  print("mean_validate_1:")
  accuracy <- mean_validate_1 / 500
  print(accuracy)
  
  print("mean_train_2:")
  accuracy <- mean_train_2 / 500
  print(accuracy)
  
  print("mean_validate_2:")
  accuracy <- mean_validate_2 / 500
  print(accuracy)
  
  print("mean_train_3:")
  accuracy <- mean_train_3 / 500
  print(accuracy)
  
  print("mean_validate_3:")
  accuracy <- mean_validate_3 / 500
  print(accuracy)
  
  return(confusion_matrix)
  
}
