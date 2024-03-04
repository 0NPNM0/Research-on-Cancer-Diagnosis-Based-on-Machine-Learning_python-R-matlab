
#人工神经网络拟合模型

ANNModel <- function(lasso_data, split_number){
  
  #将数据集分成训练集和测试集
  set.seed(12345)#保证每次跑的结果都一样
  train_index <- sample(1:nrow(lasso_data), nrow(lasso_data) * split_number)
  train_data <- lasso_data[train_index, ]
  test_data <- lasso_data[-train_index, ]
  
  
  # 构建神经网络模型
  model_ANN <- nnet(results ~ ., data = train_data, method = "nnet",
                    maxit = 1000, size = 6, decay = 0.01, trace = F)
  
  
  # 获取模型预测概率
  threshold <- 0.5
  train_predictions <- predict(model_ANN, train_data, type = "raw")
  train_predictions_ann <- as.factor(ifelse(train_predictions >= threshold, 1, 0))
  print(train_predictions_ann)
  # 查看模型预测准确率
  mean(train_data[,1] == train_predictions_ann)
  
  # 查看混淆矩阵
  table(actual = train_data[,1], train_predictions_ann)
  # 获取模型预测概率
  prob_ann <- predict(model_ANN, newdata = train_data, type = "class")
  ROC <- roc(response = train_data$results, predictor = as.numeric(prob_ann))
  
  # 绘制ROC曲线
  plot(ROC,
       legacy.axes = TRUE,
       main = "ANN train ROC curve",
       type = "l",
       col = "red",
       lty = 1,
       print.auc = TRUE,
       thresholders = "best",
       print.thres = "best"
  )
  
  # 在测试集上进行预测
  test_predictions <- predict(model_ANN, test_data, type = "class")
  test_predictions_ann <- as.factor(ifelse(test_predictions >= threshold, 1, 0))
  print(test_predictions_ann)
  # 查看模型预测准确率
  mean(test_data[,1] == test_predictions_ann)
  
  # 查看混淆矩阵
  table(actual = test_data[,1], test_predictions_ann)
  # 获取模型预测概率
  prob_ann <- predict(model_ANN, newdata = test_data, type = "class")
  ROC <- roc(response = test_data$results, predictor = as.numeric(prob_ann))
  
  
  # 绘制ROC曲线
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
  
  # 计算混淆矩阵
  confusion_matrix <- table(actual = test_data[,1], test_predictions_ann)
  
  
  # 计算准确率
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

  # 计算精确率
  precision <- diag(confusion_matrix) / colSums(confusion_matrix)

  # 计算特异度
  special <- diag(confusion_matrix)[2] / sum(confusion_matrix[, 2])

  # 计算召回率
  recall <- diag(confusion_matrix) / rowSums(confusion_matrix)

  
  return_sets <- list(plot_train = plot_train, plot_test = plot_test, accuracy = accuracy, precision = precision, special = special, recall = recall)
  
  return(return_sets)
}