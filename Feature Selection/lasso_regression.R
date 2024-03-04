
#使用Lasso回归进行特征选择

lassoRegressionFeatureSelectionFunction <- function(dataset_length,select_feature_number){
  y <- as.matrix(dap[,1])#因变量
  x <- as.matrix(dap[,2:dataset_length])#自变量
  
  set.seed(12345)#保持结果不变
  lasso_model <- glmnet(x,
                        y,
                        family = "binomial",#表示变量为二元分类
                        alpha = 1#采用L1正则化
  )
  
  print(lasso_model) #DF:选择的自变量个数
  #%Dev:拟合优度，越接近1越好
  #Lambda:正则化参数，通过交叉验证来选取
  
  plot(lasso_model,
       xvar = "lambda",#将正则化参数lambda作为横坐标
       label = F#不在图中显示变量名标签
  )
  
  coef_lasso <- coef(lasso_model,
                     s = 0.31220#lambda大小:选择特征个数
  )
  
  coef_lasso
  
  cv_model <- cv.glmnet(x,
                        y,
                        family = "binomial",
                        alpha = 1,
                        nfolds = 10)
  plot(cv_model)
  
  lambda_min <- cv_model$lambda.min
  lambda_min
  lambda_1se <- cv_model$lambda.1se# 1个标准差
  lambda_1se
  
  coef_cv <- coef(lasso_model, s = lambda_min)#根据lambda值，确定哪些变量应该被保留
  coef_cv
  
  exp(coef_cv)#根据回归系数计算OR值,不为1就是被筛选出来的特征
  
  coef_cv <- as.matrix((coef_cv))
  coef_cv <- data.frame(coef_cv)
  
  coef_cv$OR <- exp(coef_cv$s1)#计算每一个变量的OR值
  nonzero_vars <- rownames(coef_cv[coef_cv$OR != 1,])
  nonzero_vars
  nonzero_vars <- nonzero_vars[2:select_feature_number]#去除第一个无关的变量
  nonzero_vars
  
  
  lasso_data <- dap[,nonzero_vars]
  lasso_data <- cbind(dap$results, lasso_data)
  colnames(lasso_data)[1] <- "results"
  
  return(return_list)
}
