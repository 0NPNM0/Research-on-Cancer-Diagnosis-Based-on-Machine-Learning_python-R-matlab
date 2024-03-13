
#Elastic-Net惩罚逻辑回归拟合模型

ElasticNetModel <- function(lasso_data, split_number){
  
  #set.seed(12345) #保证每次跑的结果都一样
  train_index <- sample(1:nrow(lasso_data), nrow(lasso_data) * split_number) 
  train_data <- lasso_data[train_index, ]
  test_data <- lasso_data[-train_index, ]
  
  
  train_matrix <- as.matrix(train_data[, -1])  
  test_matrix <- as.matrix(test_data[, -1])  
  
  
  train_results <- as.numeric(as.character(train_data[, 1]))
  test_results <- as.numeric(as.character(test_data[, 1]))
  
  
  
  library(glmnet)
  library(pROC)
  
  mean_train <- 0.0000
  
  for(i in 1:500){#重复100次
    
    # 构建Elastic-net惩罚逻辑回归模型
    model_enet <- cv.glmnet(x = train_matrix, y = train_results, family = "binomial", alpha = 0.5, standardize = TRUE, type.measure = "class")
    
    # 获取模型在训练集上的预测概率
    threshold <- 0.5
    train_predictions <- predict(model_enet, newx = train_matrix, type = "response")
    train_predictions_enet <- as.factor(ifelse(train_predictions >= threshold, 1, 0))
    
    # 查看模型预测准确率
    mean_train <- mean_train + mean(train_results == train_predictions_enet)
    print(mean(train_results == train_predictions_enet))
  }
  
  #查看模型预测准确率
  print("mean_train:")
  mean_train <- mean_train / 500
  print(mean_train)
  
  # 在测试集上进行预测
  threshold <- 0.5
  test_predictions <- predict(model_enet, newx = test_matrix, type = "response")
  test_predictions_enet <- as.factor(ifelse(test_predictions >= threshold, 1, 0))
  
  # 查看模型预测准确率
  print("mean_test:")
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