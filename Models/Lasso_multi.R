
#Lasso惩罚逻辑回归拟合模型(多元分类)

LassoMultiModel <- function(lasso_data, split_number){
  
  library(glmnet)
  results <- show$class
  # 删除show数据框中的class列
  show <- show[, -which(names(show) == "class")]
  lasso_data <- cbind(results, show)
  #library(skimr)
  #skim(lasso_data)
  
  #将数据集分成训练集和测试集
  set.seed(12345) #保证每次跑的结果都一样
  train_index <- sample(1:nrow(lasso_data), nrow(lasso_data) * 0.8) 
  train_data <- lasso_data[train_index, ]
  test_data <- lasso_data[-train_index, ]
  
  # 将数据转换为矩阵形式
  train_matrix <- as.matrix(train_data[, -1])  # 去除目标变量列
  
  test_matrix <- as.matrix(test_data[, -1])  # 去除目标变量列
  
  # 将目标变量转换为numeric
  train_results <- as.numeric(as.character(train_data$results))
  test_results <- as.numeric(as.character(test_data$results))
  
  table(train_data$results)
  
  binary_data <- as.matrix(train_data[,-c(1)])
  binary_test_data <- as.matrix(test_data[,-c(1)])
  
  library(nnet)
  library(pROC)
  
  final_confusion_matrix <- matrix(0, nrow = 2, ncol = 2)
  
  #对1预测(0.8)
  class_label <- 1
  
  binary_results <- as.numeric(as.character(ifelse(train_results == class_label, 1, 0)))
  table(binary_results)
  binary_test_results <- as.numeric(as.character(ifelse(test_results == class_label, 1, 0)))
  binary_test_results
  model_lasso_1 <- cv.glmnet(x = binary_data, y = binary_results, family = "binomial", alpha = 1, standardize = TRUE, type.measure = "class")
  
  train_predictions <- predict(model_lasso_1, newx = binary_test_data, type = "response")
  train_predictions
  
  max_value <- max(train_predictions)
  max_value
  results <- as.numeric(ifelse(train_predictions >0.5, 1, 0))
  results
  
  # 查看模型预测准确率
  mean(binary_test_results == results)
  # 查看混淆矩阵
  table(actual = binary_test_results, results)
  
  binary_test_results
  results
  
  
  #对2预测
  class_label <- 2
  
  binary_results <- as.numeric(as.character(ifelse(train_results == class_label, 1, 0)))
  table(binary_results)
  binary_test_results <- as.numeric(as.character(ifelse(test_results == class_label, 1, 0)))
  binary_test_results
  model_lasso_1 <- cv.glmnet(x = binary_data, y = binary_results, family = "binomial", alpha = 1, standardize = TRUE, type.measure = "class")
  
  train_predictions <- predict(model_lasso_1, newx = binary_test_data, type = "response")
  train_predictions
  
  max_value <- max(train_predictions)
  max_value
  results <- as.numeric(ifelse(train_predictions >0.5, 1, 0))
  results
  
  # 查看模型预测准确率
  mean(binary_test_results == results)
  # 查看混淆矩阵
  table(actual = binary_test_results, results)
  
  binary_test_results
  results
  
  
  #对3预测
  class_label <- 3
  
  binary_results <- as.numeric(as.character(ifelse(train_results == class_label, 1, 0)))
  table(binary_results)
  binary_test_results <- as.numeric(as.character(ifelse(test_results == class_label, 1, 0)))
  binary_test_results
  model_lasso_1 <- cv.glmnet(x = binary_data, y = binary_results, family = "binomial", alpha = 1, standardize = TRUE, type.measure = "class")
  
  train_predictions <- predict(model_lasso_1, newx = binary_test_data, type = "response")
  train_predictions
  
  max_value <- max(train_predictions)
  max_value
  results <- as.numeric(ifelse(train_predictions >0.5, 1, 0))
  results
  
  # 查看模型预测准确率
  mean(binary_test_results == results)
  # 查看混淆矩阵
  table(actual = binary_test_results, results)
  
  binary_test_results
  results
  
  
  #对4预测
  class_label <- 4
  
  binary_results <- as.numeric(as.character(ifelse(train_results == class_label, 1, 0)))
  
  test_results <- as.numeric(as.character(ifelse(test_results == class_label, 1, 0)))
  
  model_lasso_4 <- cv.glmnet(x = binary_data, y = binary_results, family = "binomial", alpha = 1, standardize = TRUE, type.measure = "class")
  
  
  train_predictions <- predict(model_lasso_4, newx = test_data, type = "response")
  train_predictions
  
  
  #对5预测
  class_label <- 5
  
  binary_results <- as.numeric(as.character(ifelse(train_results == class_label, 1, 0)))
  
  test_results <- as.numeric(as.character(ifelse(test_results == class_label, 1, 0)))
  
  model_lasso_5 <- cv.glmnet(x = binary_data, y = binary_results, family = "binomial", alpha = 1, standardize = TRUE, type.measure = "class")
  
  
  train_predictions <- predict(model_lasso_5, newx = test_data, type = "response")
  train_predictions
  
  
  #对6预测
  class_label <- 6
  
  binary_results <- as.numeric(as.character(ifelse(train_results == class_label, 1, 0)))
  
  test_results <- as.numeric(as.character(ifelse(test_results == class_label, 1, 0)))
  
  model_lasso_6 <- cv.glmnet(x = binary_data, y = binary_results, family = "binomial", alpha = 1, standardize = TRUE, type.measure = "class")
  
  
  train_predictions <- predict(model_lasso_6, newx = test_data, type = "response")
  train_predictions
  
  
  # 创建一个空的列表来存储每个二元分类器
  binary_classifiers <- list()
  
  # 对于每个类别，训练一个二元分类器
  for (class_label in unique(train_data$results)) {
    # 创建一个副本数据集，将当前类别设置为正类别，其他类别设置为负类别
    class_label 
    binary_data <- as.matrix(train_data[,-c(1)])
    binary_test <- as.numeric(as.character(ifelse(train_results == class_label, 1, 0)))
  
    # 训练二元分类器，例如逻辑回归
    model_lasso <- cv.glmnet(x = binary_data, y = binary_test, family = "binomial", alpha = 1, standardize = TRUE, type.measure = "class")
    coef(model_lasso)
    
    # 将训练好的二元分类器添加到列表中
    binary_classifiers[[class_label]] <- model_lasso
  }
  binary_classifiers
  
  # 初始化预测概率向量
  prediction_probs <- rep(0, length(unique(train_data$results)))
  prediction_probs
  test_data <- as.matrix(test_data[,-1])
  test_data
  
  # 对于每个二元分类器，获取预测概率
  for (i in seq_along(binary_classifiers)) {
    class_label <- names(binary_classifiers)[i]
    model_lasso <- binary_classifiers[[class_label]]
    
    # 对新样本进行预测，并获取正类别的预测概率
    pred <- predict(model_lasso, newx = test_data, type = "response")
    pred
    pred <- as.data.frame(pred)
    positive_prob <- pred$lambda.1se
    positive_prob
    
    # 将正类别的预测概率存储在相应的位置上
    prediction_probs[i] <- positive_prob
  }
  
  
  # 找到具有最高预测概率的类别
  predicted_class <- unique(train_data$results)[which.max(prediction_probs)]
  predicted_class
  
  
  
  
  # 初始化预测结果矩阵
  predictions <- matrix("", nrow = nrow(test_data), ncol = length(binary_classifiers))
  
  # 外部循环遍历每个样本
  for (i in 1:nrow(test_data)) {
    new_sample <- test_data[i, ]
    new_sample
    
    # 内部循环遍历每个类别的二元分类器
    for (j in seq_along(binary_classifiers)) {
      class_label <- names(binary_classifiers)[j]
      model_lasso <- binary_classifiers[[class_label]]
      
      # 对当前样本进行预测，并获取正类别的预测概率
      pred <- predict(model_lasso, newx = new_sample, type = "response")
      pred <- as.data.frame(pred)
      positive_prob <- pred$lambda.1se
      positive_prob
      
      # 将预测结果存储在相应的位置上
      predictions[i, j] <- positive_prob
    }
  }
  
  # 输出预测结果矩阵
  print(predictions)
  
  
  
  
  
  
  
  
  for(i in 1:500){#重复500次
    
    # 构建Lasso惩罚逻辑回归模型
    model_lasso <- cv.glmnet(x = train_matrix, y = train_results, family = "multinomial", alpha = 1, standardize = TRUE, type.measure = "class")
    print(coef(model_lasso))
    
    # 在训练集上进行预测
    train_predictions <- predict(model_lasso, newx = train_matrix, type = "response")
    train_predictions
    
    
    threshold <- 0.5
    train_predictions_lasso <- as.factor(ifelse(train_predictions >= threshold, 1, 0))
    
    # 查看模型预测准确率
    mean(train_data[,1] == train_predictions_lasso)
    # 查看混淆矩阵
    table(actual = train_data[,1], train_predictions_lasso)
    
    # 获取模型预测概率
    prob_lasso <- predict(model_lasso, newx = train_matrix, type = "class")
    ROC <- roc(response = train_results, predictor = as.numeric(prob_lasso))
    # 绘制ROC曲线
    plot(ROC,
         legacy.axes = TRUE,
         main = "Lasso train ROC curve",
         type = "l",
         col = "red",
         lty = 1,
         print.auc = TRUE,
         thresholders = "best",
         print.thres = "best"
    )
    
    
    
    # 在测试集上进行预测
    test_predictions <- predict(model_lasso, newx = test_matrix, type = "response")
    test_predictions
    
    threshold <- 0.5
    test_predictions_lasso <- as.factor(ifelse(test_predictions >= threshold, 1, 0))
    
    # 查看模型预测准确率
    mean(test_data[,1] == test_predictions_lasso)
    # 查看混淆矩阵
    table(actual = test_data[,1], test_predictions_lasso)
    # 获取模型预测概率
    prob_lasso <- predict(model_lasso, newx = test_matrix, type = "class")
    ROC <- roc(response = test_results, predictor = as.numeric(prob_lasso))
    # 绘制ROC曲线
    plot(ROC,
         legacy.axes = TRUE,
         main = "Lasso test ROC curve",
         type = "l",
         col = "red",
         lty = 1,
         print.auc = TRUE,
         thresholders = "best",
         print.thres = "best"
    )
    
    
    # 合并混淆矩阵
    final_confusion_matrix <- final_confusion_matrix + table(actual = test_data[,1], test_predictions_lasso)
  }
  
  
  
  return(final_confusion_matrix)
  
}
