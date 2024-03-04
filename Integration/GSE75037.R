#GSE75037(83对匹配的肺腺癌和非恶性邻近组织的表达谱)
#进行肺部组织良恶性判断研究

# 1.导入相关的脚本
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Import Package\\packages.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Data Load\\data_load.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Data Processing\\data_processing.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Feature Selection\\lasso_regression.R")
source("F:\\Research-on-Cancer-Diagnosis-Based-on-Machine-Learning_python\\Models\\ANN.R")

# 2.导入相关的包
loadPackagesFunction

# 3.加载对应数据集,获取对应返回值
dataset_name <- 75037
result_list <- dataLoadFunction(dataset_name)
exp <- result_list$exp
plate <- result_list$plate
clinical <- result_list$clinical

# 4.数据处理
result_col_name <- clinical$source_name_ch1
symbol_col_name <- plate$Symbol
dap <- dataProcessingFunction(dataset_name,exp,plate,clinical,result_col_name,symbol_col_name)#dap:data_after_processing
csvname <- "exp_75037.csv"
dap <- read.csv(csvname)#已经有该文件的情况下使用，避免重复添加

# 5.二元分类
dap$results <- factor(ifelse(dap$results == "Lung cancer",1,0))#将因变量变为数值，0：Non-malignant lung，1：Lung cancer

# 6.使用Lasso回归进行特征选择
dataset_length <- 25441
select_feature_number <- 3#从2开始算第一个,这里参数含义是选到第几个
lasso_data <- lassoRegressionFeatureSelectionFunction(dataset_length, select_feature_number)

# 7.训练模型

# （1）人工神经网络拟合模型
split_number <- 0.2 #训练集:测试集 2:8
return_sets <- ANNModel(lasso_data, split_number)
plot_train <- return_sets$plot_train
plot_test <- return_sets$plot_test
accuracy <- return_sets$accuracy
precision <- return_sets$precision
special <- return_sets$special
recall <- return_sets$recall

















