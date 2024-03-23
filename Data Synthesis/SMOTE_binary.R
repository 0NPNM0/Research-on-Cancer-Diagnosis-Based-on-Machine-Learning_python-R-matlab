
#数据合成（解决样本量不足的问题）

SMOTEBinaryFunction <- function(dap_class){
  
  library(smotefamily)
  
  #获取类别为1的样本数据(原始样本量为83)
  origin_data_1 <- dap[dap$results == 1, ]
  
  original_X1 <- origin_data_1[,-c(1,2)]#获取自变量
  original_Y1 <- as.numeric(as.character(origin_data_1[,2]))#获取因变量
  
  syn_data_1 <- SMOTE(original_X1, original_Y1, K = 5, dup_size = 2)#使用SMOTE算法合成数据
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
  class_data_1 <- class_data_1[1:140,]#只拿70个样本
  
  
  
  
  #获取类别为0的样本数据（原始样本量为83）
  origin_data_0 <- dap[dap$results == 0, ]
  
  original_X0 <- origin_data_0[,-c(1,2)]#获取自变量
  original_Y0 <- as.numeric(as.character(origin_data_0[,2]))#获取因变量
  
  syn_data_0 <- SMOTE(original_X0, original_Y0, K = 5, dup_size = 6)#使用SMOTE算法合成数据
  new_data_0 <- syn_data_0$syn_data#取出合成的数据
  results <- new_data_0$class#(16~18行)将class列移到第一列
  new_data_0 <- new_data_0[, -which(names(new_data_0) == "class")]
  new_data_0 <- cbind(results,new_data_0)
  new_data_0 <- transform(type = "new",new_data_0)#为新合成的数据添加type列，值为new
  types <- new_data_0$type#(20~22行)将type列移到第一列，type用于区分新旧数据
  new_data_0 <- new_data_0[, -which(names(new_data_0) == "type")]
  new_data_0 <- cbind(types,new_data_0)
  
  
  origin_data_0 <- origin_data_0[,-1]
  origin_data_0 <- transform(type = "old",origin_data_0)#为原始数据添加type列,值为old
  types <- origin_data_0$type#(27~29行)将type列移到第一列
  origin_data_0 <- origin_data_0[, -which(names(origin_data_0) == "type")]
  origin_data_0 <- cbind(types,origin_data_0)
  
  
  class_data_0 <- rbind(origin_data_0, new_data_0)#合并新旧样本
  class_data_0 <- class_data_0[1:420,]#只拿210个样本
  
  return_list <- list(class_data_1 = class_data_1, class_data_0 = class_data_0)
  return(return_list)
  
}