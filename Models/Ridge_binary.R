
#Ridge惩罚逻辑回归拟合模型

RidgeModel <- function(train_data, train_results, test_data,  test_results){
  
  
  train_matrix <- as.matrix(train_data[, -1])  
  test_matrix <- as.matrix(test_data[, -1])  
  
  
  train_results <- as.numeric(as.character(train_data[, 1]))
  test_results <- as.numeric(as.character(test_data[, 1]))
  
  
  library(glmnet)
  library(pROC)
  
  for(i in 1:500){#重复100次
    
    lambdas <- seq(0,5,length.out = 200)
    
    model <- cv.glmnet(x = train_matrix, y = train_results, family = "binomial", alpha = 0, lambda = lambdas)
    
    #查看lambda对均方误差的影响
    plot(model)
    #可视化ridge模型回归系数的轨迹线
    plot(model$glmnet.fit,"lambda",label = T)
    
    # 获取最佳正则化参数
    best_lambda <- model$lambda.min
    
    # 使用最佳正则化参数重新拟合模型
    model_ridge <- glmnet(x = train_matrix, y = train_results, family = "binomial", alpha = 0, lambda = best_lambda, standardize = TRUE, type.measure = "class")

    summary(model_ridge)
    coef(model_ridge)
  }
  
  # 获取模型在训练集上的预测概率
  threshold <- 0.5
  train_predictions <- predict(model_ridge, newx = train_matrix, type = "response")
  train_predictions_ridge <- as.factor(ifelse(train_predictions >= threshold, 1, 0))
  
  # 查看模型预测准确率
  print("train:")
  print(mean(train_results == train_predictions_ridge))
  
  # 在测试集上进行预测
  threshold <- 0.5
  test_predictions <- predict(model_ridge, newx = test_matrix, type = "response")
  test_predictions_ridge <- as.factor(ifelse(test_predictions >= threshold, 1, 0))
  
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