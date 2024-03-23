#GSE11969(肺腺癌的表达谱定义分类)
#进行肺癌恶性程度诊断研究


# 1.导入相关的脚本(从本地导入)
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Import Package\\packages.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Data Load\\data_load.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Data Processing\\data_processing.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Feature Selection\\lasso_regression.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Models\\ANN_multi.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Models\\Lasso_multi.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Models\\Ridge_multi.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Models\\Elastic_net_multi.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Models\\NaiveBayes_multi.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Evaluation\\Evaluation.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Plots\\pca_3d.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Data Synthesis\\SMOTE_multi.R")



# 2.导入相关的包
loadPackagesFunction



# 3.加载对应数据集,获取对应返回值
dataset_name <- 11969
result_list <- DataLoadFunction(dataset_name)
exp <- result_list$exp
plate <- result_list$plate
clinical <- result_list$clinical



# 4.数据处理
result_col_name <- clinical$characteristics_ch1.7
symbol_col_name <- plate$`Gene symbol`
dap <- DataProcessingFunction(dataset_name,exp,plate,clinical,result_col_name,symbol_col_name)#dap:data_after_processing

dap$results <- trimws(sapply(strsplit(dap$results, ":"), function(x) x[2]))#去除原数据中包含的pStage无关信息
dap <- as.data.frame(dap)
dap <- dap[!dap$results=="NA",, drop=FALSE]#删除值为NA的行

for (i in seq_along(dap$results)) {#多元分类
  stage <- dap$results[i]
  if (stage == "IA") dap$results[i] <- 1
  else if (stage == "IB") dap$results[i] <- 1
  else if (stage == "IIA") dap$results[i] <- 2
  else if (stage == "IIB") dap$results[i] <- 2
  else if (stage == "IIIA") dap$results[i] <- 3
  else if (stage == "IIIB") dap$results[i] <- 3
}

table(dap$results)

filename <- paste("F:/Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python/Data/exp_", dataset_name, ".csv", sep = "")
write.csv(dap, filename)#保存为csv格式

csvname <- "F:/Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python/Data/exp_11969.csv"
dap <- read.csv(csvname)#已经有该文件的情况下使用，避免重复添加



# 5.数据合成
return_data <- SMOTEMultiFunction(dap)#使用smote算法合成数据
data_all_1 <- return_data$class_data_1
data_all_2 <- return_data$class_data_2
data_all_3 <- return_data$class_data_3
PCA3DFunction(data_all_1, 20)#类别1合成前后图像比较
PCA3DFunction(data_all_2, 20)#类别2合成前后图像比较
PCA3DFunction(data_all_3, 20)#类别3合成前后图像比较



# 6.使用svm进行特征选择
dataset_length <- 17069
select_feature_number <- 31#从2开始算第一个,这里参数含义是选到第几个
dataset <- rbind(data_all_1, data_all_2, data_all_3)
family <- "multinomial" 
svm_data <- SVMSelectionFunction(dataset, dataset_length, select_feature_number, family)



# 6.使用Lasso回归进行特征选择
dataset_length <- 17069
select_feature_number <- 31#从2开始算第一个,这里参数含义是选到第几个
dataset <- rbind(data_all_1, data_all_2, data_all_3)
family <- "multinomial" 
lasso_data <- LassoRegressionFunction(dataset, dataset_length, select_feature_number, family)


#查看数值类型,防止有不是数值的数据
str(dataset)

results <- dataset[,2]
dataset_data <- dataset[,-c(1,2)]
noise_sd <- 0.2
noisy_data <- dataset_data + rnorm(length(dataset_data), mean = 0, sd = noise_sd)

data_1 <- rbind(dataset_data[1:100,],noisy_data[101:200,])
data_2 <- rbind(dataset_data[201:300,],noisy_data[301:400,])
data_3 <- rbind(dataset_data[401:500,],noisy_data[501:600,])

dataset <- cbind(results,rbind(data_1,data_2,data_3))



# 7.dataset数据划分 训练:验证:测试-> 7:2:1
data_for_class_1_d <- dataset[dataset$results == 1, ]
data_for_class_2_d <- dataset[dataset$results == 2, ]
data_for_class_3_d <- dataset[dataset$results == 3, ]

data_for_class_1_train_d <- rbind(data_for_class_1_d[1:140,],data_for_class_2_d[1:70,],data_for_class_3_d[1:70,])
data_for_class_1_validate_d <- rbind(data_for_class_1_d[141:180,],data_for_class_2_d[71:90,],data_for_class_3_d[71:90,])

data_for_class_2_train_d <- rbind(data_for_class_2_d[1:140,],data_for_class_1_d[1:70,],data_for_class_3_d[1:70,])
data_for_class_2_validate_d <- rbind(data_for_class_2_d[141:180,],data_for_class_1_d[71:90,],data_for_class_3_d[71:90,])

data_for_class_3_train_d <- rbind(data_for_class_3_d[1:140,],data_for_class_1_d[1:70,],data_for_class_2_d[1:70,])
data_for_class_3_validate_d <- rbind(data_for_class_3_d[141:180,],data_for_class_1_d[71:90,],data_for_class_2_d[71:90,])

data_for_class_test_d <- rbind(data_for_class_1_d[181:200,],data_for_class_2_d[181:200,],data_for_class_3_d[181:200,])



# 8.训练模型,用测试数据进行评估(自带特征选择模型)（使用dataset作为数据集）
# （1）Lasso惩罚逻辑回归拟合模型
confusion_matrix_lasso <- LassoMultiModel(data_for_class_1_train_d, 
                                          data_for_class_1_validate_d,
                                          data_for_class_2_train_d, 
                                          data_for_class_2_validate_d,
                                          data_for_class_3_train_d, 
                                          data_for_class_3_validate_d,
                                          data_for_class_test_d)
EvaluationFunction(confusion_matrix_lasso)

# （2）Elastic-Net惩罚逻辑回归拟合模型
confusion_matrix_elastic_net <- ElasticNetMultiModel(data_for_class_1_train_d, 
                                                     data_for_class_1_validate_d,
                                                     data_for_class_2_train_d, 
                                                     data_for_class_2_validate_d,
                                                     data_for_class_3_train_d, 
                                                     data_for_class_3_validate_d,
                                                     data_for_class_test_d)
EvaluationFunction(confusion_matrix_elastic_net)

# （3）HLR惩罚逻辑回归拟合模型
confusion_matrix_hlr <- HLRModel()
EvaluationFunction(confusion_matrix_hlr)



# 9.lasso_data数据划分 训练:验证:测试-> 7:2:1
data_for_class_1_l <- lasso_data[lasso_data$results == 1, ]
data_for_class_2_l <- lasso_data[lasso_data$results == 2, ]
data_for_class_3_l <- lasso_data[lasso_data$results == 3, ]

data_for_class_1_train_l <- rbind(data_for_class_1_l[1:140,],data_for_class_2_l[1:70,],data_for_class_3_l[1:70,])
data_for_class_1_validate_l <- rbind(data_for_class_1_l[141:180,],data_for_class_2_l[71:90,],data_for_class_3_l[71:90,])

data_for_class_2_train_l <- rbind(data_for_class_2_l[1:140,],data_for_class_1_l[1:70,],data_for_class_3_l[1:70,])
data_for_class_2_validate_l <- rbind(data_for_class_2_l[141:180,],data_for_class_1_l[71:90,],data_for_class_3_l[71:90,])

data_for_class_3_train_l <- rbind(data_for_class_3_l[1:140,],data_for_class_1_l[1:70,],data_for_class_2_l[1:70,])
data_for_class_3_validate_l <- rbind(data_for_class_3_l[141:180,],data_for_class_1_l[71:90,],data_for_class_2_l[71:90,])

data_for_class_test_l <- rbind(data_for_class_1_l[181:200,],data_for_class_2_l[181:200,],data_for_class_3_l[181:200,])
 

 
# 10.训练模型,用测试数据进行评估(没有特征选择模型)（使用lasso_data作为数据集）

# （1）人工神经网络拟合模型
confusion_matrix_ann <- ANNMultiModel(data_for_class_1_train_l, 
                                      data_for_class_1_validate_l,
                                      data_for_class_2_train_l, 
                                      data_for_class_2_validate_l,
                                      data_for_class_3_train_l, 
                                      data_for_class_3_validate_l,
                                      data_for_class_test_l)
EvaluationFunction(confusion_matrix_ann)

# （2）Ridge惩罚逻辑回归拟合模型
confusion_matrix_ridge <- RidgeMultiModel(data_for_class_1_train_l, 
                                          data_for_class_1_validate_l,
                                          data_for_class_2_train_l, 
                                          data_for_class_2_validate_l,
                                          data_for_class_3_train_l, 
                                          data_for_class_3_validate_l,
                                          data_for_class_test_l)

EvaluationFunction(confusion_matrix_ridge)

# （3）朴素贝叶斯分类器模型
nbc_train_data <- rbind(data_for_class_1_l[1:180,],data_for_class_2_l[1:180,],data_for_class_3_l[1:180,])
nbc_test_data <- rbind(data_for_class_1_l[181:200,],data_for_class_2_l[181:200,],data_for_class_3_l[181:200,])
confusion_matrix_nbc <- NaiveBayesMultiModel(nbc_train_data,nbc_test_data)
EvaluationFunction(confusion_matrix_nbc)









