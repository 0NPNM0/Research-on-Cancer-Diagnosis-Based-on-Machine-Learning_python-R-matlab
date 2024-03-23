
#Ridge惩罚逻辑回归拟合模型

RidgeBinaryModel <- function(train_data, train_results, test_data, test_results){
  
  train_matrix <- as.matrix(train_data[, -1])  
  test_matrix <- as.matrix(test_data[, -1])  
  
  
  train_results <- as.numeric(as.character(train_results))
  test_results <- as.numeric(as.character(test_results))
  
  
  library(glmnet)
  library(pROC)
  
  mean_train <- 0.0000
  
  for(i in 1:500){#重复100次
    
    model <- cv.glmnet(x = train_matrix, y = train_results, family = "binomial", alpha = 0)
    
    model_ridge <- glmnet(x = train_matrix, y = train_results, family = "binomial", alpha = 0, lambda = model$lambda.min, standardize = TRUE, type.measure = "class")

    # 获取模型在训练集上的预测概率
    threshold <- 0.5
    train_predictions <- predict(model_ridge, newx = train_matrix, type = "response")
    train_predictions_ridge <- as.factor(ifelse(train_predictions >= threshold, 1, 0))
    
    # 查看模型预测准确率
    print("train:")
    print(mean(train_results == train_predictions_ridge))
    mean_train <- mean_train + mean(train_results == train_predictions_ridge)
  }
  
  # 查看模型预测准确率
  print("mean_train:")
  mean_train <- mean_train / 500
  print(mean_train)
  
  # 在测试集上进行预测
  threshold <- 0.5
  test_predictions <- predict(model_ridge, newx = test_matrix, type = "response")
  test_predictions
  test_predictions_ridge <- as.factor(ifelse(test_predictions >= threshold, 1, 0))
  test_predictions_ridge
  
  # 查看模型预测准确率
  print("test:")
  print(mean(test_results == test_predictions_ridge))
  
  # 绘制ROC曲线
  ROC <- roc(response = test_results, predictor = as.numeric(test_predictions_ridge))
  plot(ROC,
       legacy.axes = TRUE,
       main = "Ridge test ROC curve",
       type = "l",
       col = "red",
       lty = 1,
       print.auc = TRUE,
       thresholders = "best",
       print.thres = "best"
  )
  
  # 混淆矩阵
  confusion_matrix <- table(actual = test_results, test_predictions_ridge)
  
  return(confusion_matrix)
  
}