
#使用Lasso回归进行特征选择(用于多元分类)

LassoRegressionFunction <- function(dataset, dataset_length,select_feature_number){

  y <- as.numeric(dataset[,2])#因变量
  x <- as.matrix(dataset[,3:dataset_length])#自变量
  
  set.seed(12345)#保持结果不变
  library(glmnet)
 
  # 使用交叉验证选择最佳的lambda值
  cv_model <- cv.glmnet(x,
                        y,
                        alpha = 1,
                        nfolds = 10)
  
  # 根据选择的lambda值构建最终的Lasso模型
  lasso_model <- glmnet(x, 
                        y, 
                        alpha = 1, 
                        lambda = cv_model$lambda.min)
  
  coef_cv <- coef(lasso_model)
  exp(coef_cv)
  
  coef_cv <- as.matrix((coef_cv))
  coef_cv <- data.frame(coef_cv)
  
  coef_cv$OR <- exp(coef_cv$s0)#计算每一个变量的OR值
  nonzero_vars <- coef_cv[coef_cv$OR != 1,]#提取OR不为1的特征
  nonzero_vars
  nonzero_vars_sorted <- rownames(nonzero_vars[order(nonzero_vars$OR, decreasing = TRUE), ])# 按照 OR 大小从大到小排序
  nonzero_vars_sorted
  final_nonzero_vars <- nonzero_vars_sorted[2:select_feature_number]#去除第一个无关的变量
  final_nonzero_vars
  
  lasso_data <- dataset[,final_nonzero_vars]
  lasso_data <- cbind(dataset$results, lasso_data)
  colnames(lasso_data)[1] <- "results"
  
  return(lasso_data)
}
