
#人工神经网络拟合模型(二元分类)

ANNBinaryModel <- function(lasso_data, split_number){
  
  #将数据集分成训练集和测试集
  #set.seed(12345)#保证每次跑的结果都一样
  
  train_index <- sample(1:nrow(lasso_data), nrow(lasso_data) * split_number)
  train_data <- lasso_data[train_index, ]
  train_results <- train_data$results
  
  test_data <- lasso_data[-train_index, ]
  test_results <- test_data$results
  
  library(nnet)
  library(pROC)
  
  final_confusion_matrix <- matrix(0, nrow = 2, ncol = 2)
  
  for(i in 1:500){#重复500次
    
    # 构建神经网络模型
    model_ANN <- nnet(results ~ ., data = train_data, method = "nnet",
                      maxit = 1000, size = 6, decay = 0.01, trace = F)
    print(coef(model_ANN))
    
    # 获取模型预测概率
    threshold <- 0.5
    train_predictions <- predict(model_ANN, train_data, type = "raw")
    train_predictions_ann <- as.factor(ifelse(train_predictions >= threshold, 1, 0))
    
    # 查看模型预测准确率
    mean(train_data[,1] == train_predictions_ann)
    
    # 查看混淆矩阵
    table(actual = train_data[,1], train_predictions_ann)
    # 获取模型预测概率
    prob_ann <- predict(model_ANN, data = train_data, type = "class")

    ROC <- roc(response = train_results, predictor = as.numeric(prob_ann))
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
    test_predictions <- predict(model_ANN, test_data, type = "raw")
    test_predictions_ann <- as.factor(ifelse(test_predictions >= threshold, 1, 0))
    
    # 查看模型预测准确率
    mean(test_data[,1] == test_predictions_ann)
    
    # 计算混淆矩阵
    table(actual = test_data[,1], test_predictions_ann)
    # 获取模型预测概率
    prob_ann <- predict(model_ANN, newdata = test_data, type = "class")
    ROC <- roc(response = test_results, predictor = as.numeric(prob_ann))
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
    
    
    # 合并混淆矩阵
    final_confusion_matrix <- final_confusion_matrix + table(actual = test_data[,1], test_predictions_ann)
    
    
  }

  
  return(final_confusion_matrix)
}