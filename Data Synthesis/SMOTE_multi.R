
#数据合成（解决样本量不足的问题）

SMOTEMultiFunction <- function(dap_class){

  library(smotefamily)
  
  #获取类别为1的样本数据(原始样本量为78)
  origin_data_1 <- dap[dap$results == 1, ]
  
  original_X1 <- origin_data_1[,-c(1,2)]#获取自变量
  original_Y1 <- origin_data_1[,2]#获取因变量
  
  syn_data_1 <- SMOTE(original_X1, original_Y1, K = 5, dup_size = 3)#使用SMOTE算法合成数据
  new_data_1 <- syn_data_1$syn_data#取出合成的数据
  results <- new_data_1$class#(16~18行)将class列移到第一列
  new_data_1 <- new_data_1[, -which(names(new_data_1) == "class")]
  new_data_1 <- cbind(results,new_data_1)
  new_data_1 <- transform(type = "new",new_data_1)#为新合成的数据添加type列，值为new
  types <- new_data_1$type#(20~22行)将type列移到第一列，type用于区分新旧数据
  new_data_1 <- new_data_1[, -which(names(new_data_1) == "type")]
  new_data_1 <- cbind(types,new_data_1)
  
  
  origin_data_1 <- origin_data_1[,-1]
  origin_data_1 <- transform(type = "old",origin_data_1)#为原始数据添加type列,值为old
  types <- origin_data_1$type#(27~29行)将type列移到第一列
  origin_data_1 <- origin_data_1[, -which(names(origin_data_1) == "type")]
  origin_data_1 <- cbind(types,origin_data_1)
  
  
  class_data_1 <- rbind(origin_data_1, new_data_1)#合并新旧样本
  class_data_1 <- class_data_1[1:200,]#只拿200个样本
  
  
  
  
  #获取类别为2的样本数据（原始样本量为26）
  origin_data_2 <- dap[dap$results == 2, ]
  
  original_X2 <- origin_data_2[,-c(1,2)]#获取自变量
  original_Y2 <- origin_data_2[,2]#获取因变量
  
  syn_data_2 <- SMOTE(original_X2, original_Y2, K = 5, dup_size = 8)#使用SMOTE算法合成数据
  new_data_2 <- syn_data_2$syn_data#取出合成的数据
  results <- new_data_2$class#(16~18行)将class列移到第一列
  new_data_2 <- new_data_2[, -which(names(new_data_2) == "class")]
  new_data_2 <- cbind(results,new_data_2)
  new_data_2 <- transform(type = "new",new_data_2)#为新合成的数据添加type列，值为new
  types <- new_data_2$type#(20~22行)将type列移到第一列，type用于区分新旧数据
  new_data_2 <- new_data_2[, -which(names(new_data_2) == "type")]
  new_data_2 <- cbind(types,new_data_2)
  
  
  origin_data_2 <- origin_data_2[,-1]
  origin_data_2 <- transform(type = "old",origin_data_2)#为原始数据添加type列,值为old
  types <- origin_data_2$type#(27~29行)将type列移到第一列
  origin_data_2 <- origin_data_2[, -which(names(origin_data_2) == "type")]
  origin_data_2 <- cbind(types,origin_data_2)
  
  
  class_data_2 <- rbind(origin_data_2, new_data_2)#合并新旧样本
  class_data_2 <- class_data_2[1:200,]#只拿200个样本
  
  
  
  
  
  #获取类别为3的样本数据（原始样本量为45）
  origin_data_3 <- dap[dap$results == 3, ]
  
  original_X3 <- origin_data_3[,-c(1,2)]#获取自变量
  original_Y3 <- origin_data_3[,2]#获取因变量
  
  syn_data_3 <- SMOTE(original_X3, original_Y3, K = 5, dup_size = 5)#使用SMOTE算法合成数据
  new_data_3 <- syn_data_3$syn_data#取出合成的数据
  results <- new_data_3$class#(16~18行)将class列移到第一列
  new_data_3 <- new_data_3[, -which(names(new_data_3) == "class")]
  new_data_3 <- cbind(results,new_data_3)
  new_data_3 <- transform(type = "new",new_data_3)#为新合成的数据添加type列，值为new
  types <- new_data_3$type#(20~22行)将type列移到第一列，type用于区分新旧数据
  new_data_3 <- new_data_3[, -which(names(new_data_3) == "type")]
  new_data_3 <- cbind(types,new_data_3)
  
  
  origin_data_3 <- origin_data_3[,-1]
  origin_data_3 <- transform(type = "old",origin_data_3)#为原始数据添加type列,值为old
  types <- origin_data_3$type#(27~29行)将type列移到第一列
  origin_data_3 <- origin_data_3[, -which(names(origin_data_3) == "type")]
  origin_data_3 <- cbind(types,origin_data_3)
  
  
  class_data_3 <- rbind(origin_data_3, new_data_3)#合并新旧样本
  class_data_3 <- class_data_3[1:200,]#只拿200个样本
  
  
  
  return_list <- list(class_data_1 = class_data_1, class_data_2 = class_data_2, class_data_3 = class_data_3)
  return(return_list)
  
}

