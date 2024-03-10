#GSE11969(肺腺癌的表达谱定义分类)
#进行肺癌恶性程度诊断研究


# 1.导入相关的脚本(从本地导入)
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Import Package\\packages.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Data Load\\data_load.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Data Processing\\data_processing.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Feature Selection\\lasso_regression.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Models\\ANN_multi.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Models\\Lasso_multi.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Models\\Ridge.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Models\\Elastic_net.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Evaluation\\Evaluation.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Plots\\pca_3d.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Data Synthesis\\SMOTE.R")


# 2.导入相关的包
loadPackagesFunction

# 3.加载对应数据集,获取对应返回值
dataset_name <- 11969
result_list <- DataLoadFunction(dataset_name)
exp <- result_list$exp
plate <- result_list$plate
clinical <- result_list$clinical
table(clinical$characteristics_ch1.7)


# 4.数据处理
result_col_name <- clinical$characteristics_ch1.7
symbol_col_name <- plate$`Gene symbol`
dap <- DataProcessingFunction(dataset_name,exp,plate,clinical,result_col_name,symbol_col_name)#dap:data_after_processing

dap$results <- trimws(sapply(strsplit(dap$results, ":"), function(x) x[2]))#去除原数据中包含的pStage无关信息
dap <- as.data.frame(dap)
dap <- dap[!dap$results=="NA",, drop=FALSE]#删除值为NA的行

filename <- paste("exp_", dataset_name, ".csv", sep = "")
write.csv(dap, filename)#保存为csv格式
savepath <- paste("F:/Graduation thesis/data/exp_", dataset_name, ".csv", sep = "")
write.csv(dap, file = savepath, row.names = FALSE) 

csvname <- "exp_11969.csv"
dap <- read.csv(csvname)#已经有该文件的情况下使用，避免重复添加

# 5.多元分类
for (i in seq_along(dap$results)) {
  stage <- dap$results[i]
  if (stage == "IA") dap$results[i] <- 1
  else if (stage == "IB") dap$results[i] <- 1
  else if (stage == "IIA") dap$results[i] <- 2
  else if (stage == "IIB") dap$results[i] <- 2
  else if (stage == "IIIA") dap$results[i] <- 3
  else if (stage == "IIIB") dap$results[i] <- 3
}

table(dap$results)

# 6.数据合成
return_data <- SMOTEFunction(dap)#使用smote算法合成数据
data_all_1 <- return_data$class_data_1
data_all_2 <- return_data$class_data_2
data_all_3 <- return_data$class_data_3
PCA3DFunction(data_all_1)#类别1合成前后图像比较
PCA3DFunction(data_all_2)#类别2合成前后图像比较
PCA3DFunction(data_all_3)#类别3合成前后图像比较


# 7.使用Lasso回归进行特征选择
dataset_length <- 17069
select_feature_number <- 50#从2开始算第一个,这里参数含义是选到第几个
dataset <- rbind(data_all_1, data_all_2, data_all_3)
lasso_data <- LassoRegressionFunction(dataset, dataset_length, select_feature_number)

data_for_class_1 <- lasso_data[lasso_data$results == 1, ]
data_for_class_2 <- lasso_data[lasso_data$results == 2, ]
data_for_class_3 <- lasso_data[lasso_data$results == 3, ]

data_for_class_1_train <- rbind(data_for_class_1[1:180,],data_for_class_2[1:90,],data_for_class_3[1:90,])
data_for_class_1_validate <- rbind(data_for_class_1[181:210,],data_for_class_2[91:105,],data_for_class_3[91:105,])

data_for_class_2_train <- rbind(data_for_class_2[1:140,],data_for_class_1[1:70,],data_for_class_3[1:70,])
data_for_class_2_validate <- rbind(data_for_class_2[141:170,],data_for_class_1[71:85,],data_for_class_3[71:85,])

data_for_class_3_train <- rbind(data_for_class_3[1:140,],data_for_class_1[1:70,],data_for_class_2[1:70,])
data_for_class_3_validate <- rbind(data_for_class_3[141:170,],data_for_class_1[71:85,],data_for_class_2[71:85,])

data_for_class_test <- rbind(data_for_class_1[211:240,],data_for_class_2[171:200,],data_for_class_3[171:200,])
  
# 8.训练模型,用测试数据进行评估

# （1）人工神经网络拟合模型
split_number <- 0.3 #训练集:测试集 3:7
confusion_matrix_ann <- ANNMultiModel(lasso_data, split_number)
EvaluationFunction(confusion_matrix_ann)

# （2）Lasso惩罚逻辑回归拟合模型
confusion_matrix_lasso <- LassoMultiModel(data_for_class_1_train, 
                                          data_for_class_1_validate,
                                          data_for_class_2_train, 
                                          data_for_class_2_validate,
                                          data_for_class_3_train, 
                                          data_for_class_3_validate,
                                          data_for_class_test)
EvaluationFunction(confusion_matrix_lasso)

# （3）Ridge惩罚逻辑回归拟合模型
split_number <- 0.3 #训练集:测试集 3:7
confusion_matrix_ridge <- RidgeModel(lasso_data, split_number)
EvaluationFunction(confusion_matrix_ridge)

# （4）Elastic-Net惩罚逻辑回归拟合模型
split_number <- 0.3 #训练集:测试集 3:7
confusion_matrix_elastic_net <- ElasticNetModel(lasso_data, split_number)
EvaluationFunction(confusion_matrix_elastic_net)

# （5）HLR惩罚逻辑回归拟合模型
split_number <- 0.3 #训练集:测试集 3:7
confusion_matrix_hlr <- HLRModel(lasso_data, split_number)
EvaluationFunction(confusion_matrix_hlr)









