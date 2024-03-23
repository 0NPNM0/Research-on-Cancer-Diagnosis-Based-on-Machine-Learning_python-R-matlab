
#朴素贝叶斯分类器模型(二元分类)

NaiveBayesBinaryModel <- function(train_data, train_results, test_data, test_results){
  
  library(naivebayes)
  
  train_results <- as.factor(train_results)
  test_results <- as.factor(test_results)
  
  for(i in 1:500){
    # 创建并训练朴素贝叶斯模型
    model <- naive_bayes(train_results ~ ., data = train_data) 
    
    # 在测试集上进行预测
    predicted_classes <- predict(model, newdata = train_data)
    print("train:")
    print(mean(predicted_classes == train_results))
  }
  
  # 在测试集上进行预测
  predicted_classes <- predict(model, newdata = test_data)
  print("test:")
  print(mean(predicted_classes == test_results))  
  
  # 绘制ROC曲线
  ROC <- roc(response = test_results, predictor = as.numeric(predicted_classes))
  plot(ROC,
       legacy.axes = TRUE,
       main = "Naive Bayes test ROC curve",
       type = "l",
       col = "red",
       lty = 1,
       print.auc = TRUE,
       thresholders = "best",
       print.thres = "best"
  )
  
  # 混淆矩阵
  confusion_matrix <- table(actual = test_results, predicted_classes)
  return(confusion_matrix)
}


