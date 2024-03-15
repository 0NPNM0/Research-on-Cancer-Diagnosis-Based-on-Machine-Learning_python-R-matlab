#GSE75037(83对匹配的肺腺癌和非恶性邻近组织的表达谱)
#进行肺部组织良恶性判断研究

# 1.导入相关的脚本(从本地导入)
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Import Package\\packages.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Data Load\\data_load.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Data Processing\\data_processing.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Feature Selection\\lasso_regression.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Models\\ANN_binary.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Models\\Lasso_binary.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Models\\Ridge_binary.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Models\\Elastic_net_binary.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Evaluation\\Evaluation.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Plots\\pca_3d.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Data Synthesis\\SMOTE_binary.R")

# 2.导入相关的包
loadPackagesFunction

# 3.加载对应数据集,获取对应返回值
dataset_name <- 75037
result_list <- DataLoadFunction(dataset_name)
exp <- result_list$exp
plate <- result_list$plate
clinical <- result_list$clinical

# 4.数据处理
result_col_name <- clinical$source_name_ch1
symbol_col_name <- plate$Symbol
dap <- DataProcessingFunction(dataset_name,exp,plate,clinical,result_col_name,symbol_col_name)#dap:data_after_processing

dap$results <- factor(ifelse(dap$results == "Lung cancer",1,0))#将因变量变为数值，0：Non-malignant lung，1：Lung cancer

filename <- paste("F:/Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python/Data/exp_", dataset_name, ".csv", sep = "")
write.csv(dap, filename)#保存为csv格式

csvname <- "F:/Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python/Data/exp_75037.csv"
dap <- read.csv(csvname)#已经有该文件的情况下使用，避免重复添加



# 5.数据合成
return_data <- SMOTEBinaryFunction(dap)
data_all_1 <- return_data$class_data_1
data_all_0 <- return_data$class_data_0
PCA3DFunction(data_all_1,100)#类别1合成前后图像比较
PCA3DFunction(data_all_0,105)#类别0合成前后图像比较

# 6.使用Lasso回归进行特征选择
dataset_length <- 25444
select_feature_number <- 10#这里参数含义是选几个
dataset <- rbind(data_all_1, data_all_0)
lasso_data <- LassoRegressionFunction(dataset, dataset_length, select_feature_number)


# 7.数据划分 训练:测试 7:3
split_number <- 0.8
train_index <- sample(1:nrow(lasso_data), nrow(lasso_data) * split_number)
train_data <- lasso_data[train_index, ]
train_results <- as.numeric(train_data$results)

test_data <- lasso_data[-train_index, ]
test_results <- as.numeric(test_data$results)

library(caret)

scal <- preProcess(train_data, method = c("center","scale"))#数据标准化
train_data <- predict(scal, train_data)
test_data <- predict(scal, test_data)
scal$mean
scal$std


# 8.训练模型,用测试数据进行评估

# （1）人工神经网络拟合模型
confusion_matrix_ann <- ANNBinaryModel(train_data, train_results, test_data,  test_results)
EvaluationFunction(confusion_matrix_ann)

# （2）Lasso惩罚逻辑回归拟合模型
confusion_matrix_lasso <- LassoBinaryModel(train_data, train_results, test_data,  test_results)
EvaluationFunction(confusion_matrix_lasso)

# （3）Ridge惩罚逻辑回归拟合模型
confusion_matrix_ridge <- RidgeModel(train_data, train_results, test_data,  test_results)
EvaluationFunction(confusion_matrix_ridge)

# （4）Elastic-Net惩罚逻辑回归拟合模型
confusion_matrix_elastic_net <- ElasticNetModel(train_data, train_results, test_data,  test_results)
EvaluationFunction(confusion_matrix_elastic_net)

# （5）HLR惩罚逻辑回归拟合模型
confusion_matrix_hlr <- HLRModel(lasso_data, split_number)
EvaluationFunction(confusion_matrix_hlr)









