
#人工神经网络拟合模型(二元分类)

ANNBinaryModel <- function(lasso_data, split_number){
  
  train_index <- sample(1:nrow(lasso_data), nrow(lasso_data) * split_number)
  train_data <- lasso_data[train_index, ]
  train_results <- train_data$results
  
  test_data <- lasso_data[-train_index, ]
  test_results <- test_data$results

  
  library(nnet)
  library(pROC)
  
  mean_train <- 0.0000
  
  for(i in 1:500){#重复100次
    
    # 构建神经网络模型
    model_ANN <- nnet(train_results ~ ., data = train_data, method = "nnet",
                      maxit = 1000, size = 6, decay = 0.01, trace = F)
    
    # 获取模型在训练集上的预测概率
    threshold <- 0.5
    train_predictions <- predict(model_ANN, train_data, type = "raw")
    train_predictions_ann <- as.factor(ifelse(train_predictions >= threshold, 1, 0))
    
    # 查看模型预测准确率
    mean_train <- mean_train + mean(train_results == train_predictions_ann)
    print(mean(train_results == train_predictions_ann))
  }
  
  #查看模型预测准确率
  print("mean_train:")
  mean_train <- mean_train / 500
  print(mean_train)
  
  # 在测试集上进行预测
  threshold <- 0.5
  test_predictions <- predict(model_ANN, test_data, type = "raw")
  test_predictions_ann <- as.factor(ifelse(test_predictions >= threshold, 1, 0))
  
  # 查看模型预测准确率
  print("mean_test:")
  print(mean(test_results == test_predictions_ann))
  
  test_results
  test_predictions_ann
  
  # 绘制ROC曲线
  ROC <- roc(response = test_results, predictor = as.numeric(test_predictions_ann))
  plot(ROC,
       legacy.axes = TRUE,
       main = "ANN test ROC curve",
       type = "l",
       col = "red",
       lty = 1,
       print.auc = TRUE,
       thresholders = "best",
       print.thres = "best"
  )
  
  # 混淆矩阵
  confusion_matrix <- table(actual = test_results, test_predictions_ann)
  
  return(confusion_matrix)
}