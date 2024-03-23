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
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Models\\NaiveBayes_binary.R")
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
select_feature_number <- 2#这里参数含义是选几个
dataset <- rbind(data_all_1, data_all_0)
family <- "binomial"
lasso_data <- LassoRegressionFunction(dataset, dataset_length, select_feature_number, family)



# 7.dataset(原数据集)数据划分 训练:测试 7:3
split_number <- 0.7
train_index_d <- sample(1:nrow(dataset), nrow(dataset) * split_number)
train_data_d <- dataset[train_index_d, ]
train_results_d <- as.numeric(train_data_d$results)

test_data_d <- dataset[-train_index_d, ]
test_results_d <- as.numeric(test_data_d$results)



# 8.训练模型,用测试数据进行评估(自带特征选择模型)（使用dataset作为数据集）
# （1）Lasso惩罚逻辑回归拟合模型
confusion_matrix_lasso <- LassoBinaryModel(train_data_d, train_results_d, test_data_d, test_results_d)
EvaluationFunction(confusion_matrix_lasso)

# （2）Elastic-Net惩罚逻辑回归拟合模型
confusion_matrix_elastic_net <- ElasticNetBinaryModel(train_data_d, train_results_d, test_data_d, test_results_d)
EvaluationFunction(confusion_matrix_elastic_net)

# （3）HLR惩罚逻辑回归拟合模型
confusion_matrix_hlr <- HLRModel()
EvaluationFunction(confusion_matrix_hlr)



# 9.lasso_data(特征选择出的数据集)数据划分 训练:测试 7:3
split_number <- 0.7
train_index_l <- sample(1:nrow(lasso_data), nrow(lasso_data) * split_number)
train_data_l <- lasso_data[train_index_l, ]
train_results_l <- as.numeric(train_data_l$results)

test_data_l <- lasso_data[-train_index_l, ]
test_results_l <- as.numeric(test_data_l$results)

filename <- paste("F:/Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python/Data/75037_train_data_l.csv", sep = "")
write.csv(train_data_l, filename)#保存为csv格式

filename <- paste("F:/Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python/Data/75037_test_data_l.csv", sep = "")
write.csv(test_data_l, filename)#保存为csv格式

# 10.训练模型,用测试数据进行评估(没有特征选择模型)（使用lasso_data作为数据集）
# （1）人工神经网络拟合模型
confusion_matrix_ann <- ANNBinaryModel(train_data_l, train_results_l, test_data_l, test_results_l)
EvaluationFunction(confusion_matrix_ann)

# （2）Ridge惩罚逻辑回归拟合模型
confusion_matrix_ridge <- RidgeBinaryModel(train_data_l, train_results_l, test_data_l, test_results_l)
EvaluationFunction(confusion_matrix_ridge)

# （3）朴素贝叶斯分类器模型
confusion_matrix_nbc <- NaiveBayesBinaryModel(train_data_l, train_results_l, test_data_l, test_results_l)
EvaluationFunction(confusion_matrix_nbc)






