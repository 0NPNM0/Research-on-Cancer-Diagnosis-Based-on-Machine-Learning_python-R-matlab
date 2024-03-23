
#Elastic-Net惩罚逻辑回归拟合模型

ElasticNetBinaryModel <- function(train_data, train_results, test_data,  test_results){
  
  train_matrix <- as.matrix(train_data[, -c(1,2)])  
  test_matrix <- as.matrix(test_data[, -c(1,2)])  
  
  
  train_results <- as.numeric(as.character(train_data[, 2]))
  test_results <- as.numeric(as.character(test_data[, 2]))
  
  
  
  library(glmnet)
  library(pROC)
  
  mean_train <- 0.0000
  
  for(i in 1:500){#重复100次
    
    # 定义Lasso惩罚逻辑回归模型
    model <- cv.glmnet(x = train_matrix, y = train_results, family = "binomial", alpha = 0.5)
    
    # 使用最佳正则化参数重新拟合模型
    model_enet <- glmnet(x = train_matrix, y = train_results, family = "binomial", alpha = 0.5, lambda = model$lambda.min, standardize = TRUE, type.measure = "class")
  
    # 获取模型在训练集上的预测概率
    threshold <- 0.5
    train_predictions <- predict(model_enet, newx = train_matrix, type = "response")
    train_predictions_enet <- as.factor(ifelse(train_predictions >= threshold, 1, 0))
    
    # 查看模型预测准确率
    print("train:")
    print(mean(train_results == train_predictions_enet))
    mean_train <- mean_train + mean(train_results == train_predictions_enet)
  }
  
  print("mean_train:")
  mean_train <- mean_train / 500
  print(mean_train)
  
  # 在测试集上进行预测
  threshold <- 0.5
  test_predictions <- predict(model_enet, newx = test_matrix, type = "response")
  test_predictions_enet <- as.factor(ifelse(test_predictions >= threshold, 1, 0))
  
  # 查看模型预测准确率
  print("test:")
  print(mean(test_results == test_predictions_enet))
  
  
  # 绘制ROC曲线
  ROC <- roc(response = test_results, predictor = as.numeric(test_predictions_enet))
  plot(ROC,
       legacy.axes = TRUE,
       main = "Elastic-net test ROC curve",
       type = "l",
       col = "red",
       lty = 1,
       print.auc = TRUE,
       thresholders = "best",
       print.thres = "best"
  )
  
  # 混淆矩阵
  confusion_matrix <- table(actual = test_results, test_predictions_enet)
  
  return(confusion_matrix)
  
}