
#svm进行特征选择

SVMSelectionFunction <- function(dataset, dataset_length, select_feature_number, family){
  library(caret)
  
  X <- as.matrix(dataset[,-c(1,2)]) 
  Y <- as.factor(dataset$results) 
  
  control <- rfeControl(functions = caretFuncs, method = "cv", number = 5)  #cv 交叉验证次数5
  
  # 执行SVM-RFE算法
  results <- rfe(X,
                 Y,
                 sizes = c(1:8),  
                 rfeControl = control,
                 method = "svmRadial") # method = "svmRadial" specifies that the SVM model should use a radial kernel
  
  
  # 结果分析
  print(results)
  # 列出选择的变量集
  predictors(results)
  
}