
#朴素贝叶斯分类器模型(多元分类)

NaiveBayesMultiModel <- function(nbc_train_data,nbc_test_data){
  
  library(naivebayes)
  
  train_data <- as.data.frame(nbc_train_data[,-1])
  train_results <- as.factor(nbc_train_data$results)
  
  test_data <- as.data.frame(nbc_test_data[,-1])
  test_results <- as.factor(nbc_test_data$results)
  
  
  for(i in 1:500){
    # 创建并训练朴素贝叶斯模型
    model <- naive_bayes(train_results ~ ., data = train_data)  # 使用Species作为目标变量
  
    # 在测试集上进行预测
    predicted_classes <- predict(model, newdata = train_data)
    print("train:")
    print(mean(predicted_classes == train_results))  # 打印预测的类别标签
  }
  
  # 在测试集上进行预测
  predicted_classes <- predict(model, newdata = test_data)
  print("test:")
  print(mean(predicted_classes == test_results))  # 打印预测的类别标签

  confusion_matrix <- table(actual = test_results, predicted_classes)
  return(confusion_matrix)
}


