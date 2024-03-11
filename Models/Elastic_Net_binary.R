
#Elastic-Net惩罚逻辑回归拟合模型

ElasticNetModel <- function(lasso_data, split_number){
  #将数据集分成训练集和测试集
  set.seed(12345) #保证每次跑的结果都一样
  train_index <- sample(1:nrow(lasso_data), nrow(lasso_data) * 0.2) 
  train_data <- lasso_data[train_index, ]
  test_data <- lasso_data[-train_index, ]
  
  # 将数据转换为矩阵形式
  train_matrix <- as.matrix(train_data[, -1])  # 去除目标变量列
  test_matrix <- as.matrix(test_data[, -1])  # 去除目标变量列
  
  # 将目标变量转换为numeric
  train_results <- as.numeric(as.character(train_data$results))
  test_results <- as.numeric(as.character(test_data$results))
  
  library(nnet)
  library(pROC)
  
  final_confusion_matrix <- matrix(0, nrow = 2, ncol = 2)
  
  for(i in 1:500){#重复500次
    
    # 构建Elastic-Net惩罚逻辑回归模型
    model_Elastic_Net <- cv.glmnet(x = train_matrix, y = train_results, family = "binomial", alpha = 0.5, standardize = TRUE, type.measure = "class")
    print(coef(model_Elastic_Net))
    
    # 在训练集上进行预测
    train_predictions <- predict(model_Elastic_Net, newx = train_matrix, type = "response")
    threshold <- 0.5
    train_predictions_Elastic_Net <- as.factor(ifelse(train_predictions >= threshold, 1, 0))
    
    # 查看模型预测准确率
    mean(train_data[,1] == train_predictions_Elastic_Net)
    
    # 查看混淆矩阵
    table(actual = train_data[,1], train_predictions_Elastic_Net)
    
    # 获取模型预测概率
    prob_Elastic_Net <- predict(model_Elastic_Net, newx = train_matrix, type = "class")
    ROC <- roc(response = train_results, predictor = as.numeric(prob_Elastic_Net))
    
    # 绘制ROC曲线
    plot(ROC,
         legacy.axes = TRUE,
         main = "Elastic-Net train ROC curve",
         type = "l",
         col = "red",
         lty = 1,
         print.auc = TRUE,
         thresholders = "best",
         print.thres = "best"
    )
    
    # 在测试集上进行预测
    test_predictions <- predict(model_Elastic_Net, newx = test_matrix, type = "response")
    test_predictions_Elastic_Net <- as.factor(ifelse(test_predictions >= threshold, 1, 0))
    
    # 查看模型预测准确率
    mean(test_data[,1] == test_predictions_Elastic_Net)
    
    # 查看混淆矩阵
    table(actual = test_data[,1], test_predictions_Elastic_Net)
    
    # 获取模型预测概率
    prob_Elastic_Net <- predict(model_Elastic_Net, newx = test_matrix, type = "class")
    ROC <- roc(response = test_results, predictor = as.numeric(prob_Elastic_Net))
    
    # 绘制ROC曲线
    plot(ROC,
         legacy.axes = TRUE,
         main = "Elastic-Net test ROC curve",
         type = "l",
         col = "red",
         lty = 1,
         print.auc = TRUE,
         thresholders = "best",
         print.thres = "best"
    )
    
    # 合并混淆矩阵
    final_confusion_matrix <- final_confusion_matrix + table(actual = test_data[,1], test_predictions_Elastic_Net)
    
  }
  
  
  return(final_confusion_matrix)
  
}