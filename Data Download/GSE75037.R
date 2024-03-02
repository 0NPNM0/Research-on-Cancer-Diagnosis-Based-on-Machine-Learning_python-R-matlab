#导入相关的包
if(!requireNamespace("BiocManager",quietly = T))
  
  install.packages("BiocManager")

if(!requireNamespace("GEOquery",quietly = T))
  
  BiocManager::install("GEOquery")

if(!requireNamespace("limma",quietly = T))
  
  BiocManager::install("limma",force = TRUE)

if(!requireNamespace("tidyverse",quietly = T))
  
  install.packages("tidyverse",force = TRUE)



library(tidyverse)

library(limma)

library(GEOquery)





#下载数据
if (!file.exists("GSE75037_eSet.Rdata")) {
  
  GEO_file <- getGEO('GSE75037',#需要下载的series
                     
                     destdir = 'F:/Graduation thesis/data',#设置文件保存路径
                     
                     getGPL = T #下载平台文件
                     
  )
  
  save(GEO_file, file = "GSE75037_eSet.Rdata")#将下载下来的文件保存为我们可以处理的格式
  
}


#加载数据
load("GSE75037_eSet.Rdata")#将更改好格式并保存好的的数据加载到这里


GEO_file[[1]]#提取GEO_file中第一个数据

exp <- exprs(GEO_file[[1]])#提取数据中的样本基因表达矩阵

plate <- fData(GEO_file[[1]])#提取数据中的平台信息

clinical <- pData(GEO_file[[1]])#提取数据中的样本临床信息（比如：年龄、性别等等）







#数据处理
exp <- as.data.frame(exp)

#提取出指定列
symbol_column <- plate$Symbol

# 将 results 列添加到 exp 数据框中
exp <- cbind( Symbol = symbol_column, exp)


#去除数据头尾空格
exp[, grep("Symbol", colnames(exp))] <-  trimws(exp[, grep("Symbol", colnames(exp))])

#将空白负值NA
exp[exp==""] <- NA

#从数据框中删除包含缺失值（NA）的行
exp <- na.omit(exp)

exp <- as.data.frame(exp)

#将 exp 数据框中小于 0 的值替换为 0
exp[exp < 0] <- 0

#查看列名是否有重复值
table(duplicated(exp$Symbol))

#将重复基因取平均值
exp1 <- avereps(exp, ID = exp$Symbol)
exp1 <- as.data.frame(exp1)
rownames(exp1) <- exp1$Symbol#加上行名
exp1 <- exp1[,-1]#删除第一列，选择所有行，但只选择除了第一列之外的所有列


#将列的值和行的值转置，横轴：探针名，纵轴：样本名
exp1 <- t(exp1)

#获取因变量
results <- clinical$source_name_ch1

results <- as.data.frame(results)

# 将 results 列添加到 exp1 数据框中
exp1 <- cbind( Results = results, exp1)

write.csv(exp1, "exp75037.csv")#保存为csv格式

write.csv(exp1, file = "F:/Graduation thesis/data/exp75037.csv", row.names = FALSE) 





#特征选择
if(!requireNamespace("e1071",quietly = T))
  
  install.packages("e1071")

if(!requireNamespace("caret",quietly = T))
  
  install.packages("caret")

if(!requireNamespace("pROC",quietly = T))
  
  install.packages("pROC")

if(!requireNamespace("mlrMBO",quietly = T))
  
  install.packages("mlrMBO")




library(e1071)#支持向量机
library(caret)#集成了上百种分类和回归算法
library(pROC)#ROC曲线可视化
library(ggplot2)#绘图
library(mlrMBO)#贝叶斯优化：根据之前的模型训练结果来选择下一个参数组合



#导入数据
mydata <- read.csv("exp75037.csv")

#将因变量变为数值，0：Non-malignant lung，1：Lung cancer
#原因：SVM对数值敏感
mydata$results <- factor(ifelse(mydata$results == "Lung cancer",1,0))

#把第一列列名改成samples
colnames(mydata)[1] <- "samples"

#SVM算法变量筛选
table(mydata$results)

#定义自变量
X <- mydata[,3:26]
#定义因变量
Y <- as.numeric(as.factor(mydata$results))


#设置重复交叉验证
control <- trainControl(method = "repeatedcv",
                        number = 5,#将数据分成10份
                        repeats = 1,#重复次数
                        search = "random"
                        )

#使用svm-rfe特征选择方法
set.seed(12345)
svm_rfe <- rfe(X,
               Y,
               sizes = 1:20,#考虑10个特征
               rfeControl = rfeControl(functions = caretFuncs,
                                       method = "repeatedcv",
                                       number = 5,
                                       repeats = 1,
                                       verbose = FALSE),
               method = "svmLinear",#使用SVM分类器
               trControl = control,#trainControl设置
               preProc = c("center", "scale")#特征预处理，标准化
               )

#提取前10个最相关的变量
svm_rfe_ranking <- svm_rfe$variables
head(svm_rfe_ranking)
#overall:特征重要性评分，越高贡献越大
#var:特征的名称
#variables:特征数量
#Resample:交叉验证的fold信息

#查看变量重要性评分
varImp(svm_rfe)

varImp_dataframe <- data.frame(Gene = row.names(varImp(svm_rfe))[1:20],
                               importance = varImp(svm_rfe)[1:20,1])

#删除NA行
varImp_dataframe <- na.omit(varImp_dataframe)


#绘制柱状图颜色
mycolors <- c('#88D7A4','#98CEDD','#88D7A4','#98CEDD','#88D7A4',
              '#98CEDD','#88D7A4','#98CEDD','#88D7A4','#98CEDD',
              '#88D7A4','#98CEDD','#88D7A4','#98CEDD','#88D7A4',
              '#98CEDD','#88D7A4','#98CEDD','#88D7A4','#98CEDD',
              '#88D7A4','#98CEDD'
              )

#绘图
ggplot(varImp_dataframe,aes(x = reorder(Gene, -importance), y = importance, fill = Gene))+
  geom_col()+#指定geom_col()为柱状图
  ggtitle("Hub Genes")+#设置标题
  theme(panel.border = element_blank(),
        axis.text.x = element_text(size = 12, color = "black"),#修改x轴和y轴的文本大小和颜色
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(margin = margin(t = 0, r = 20,b = 0,l = 0)),#移动x轴标题，增加距离
        plot.title = element_text(margin = margin(b = 20)),#移动图标题，增加距离
        panel.grid.major = element_line(color = "grey", size = 0.2)#增加灰色网格
        )+
  xlab("Gene") + ylab("Importance")+#修改x轴名称为"Gene",y轴名称为"Importance"
  scale_fill_manual(values = mycolors)

#查看重要性前10的基因
top_10_vars <- svm_rfe_ranking$var[1:10]
top_10_vars

#提取前10变量的表达矩阵
top_10_SVM_data <- mydata[,top_10_vars]

#提取最优子集中的变量
X_plot = svm_rfe$results$Variables
Y_plot = svm_rfe$results$RMSE
plot(X_plot, Y_plot,
     xlab = "Variable Number",
     ylab = "RMSE(Cross-Validation)",
     col = "#7DEE44",#点的颜色
     pch = 16,#点的形状
     cex = 1.5,#点的大小
     lwd = 2,#线的宽度
     type = "b",#同时绘制点与线
     ylim = c(0.24,0.33)#y轴范围
     )
lines(X_plot, Y_plot, col="#DF294c", lwd = 2)#额外绘制一条红色粗线

abline(h = min(Y_plot), col = "skyblue")#添加一条水平参考线
grid(col = "gray", lwd = 1, lty = 3)#添加网格线

legend("topright", c("Traning RMSE","Cross-Validation RMSE"),
       col = c("#7DEE44","#DF294c"),pch = c(16,NA), lwd = 2, bg = "white" 
       )

#找到RMSE最小的点
#RMSE是评价回归模型效果的指标，通过预测值和实际值之间的差距来评价模型的准确性，值越小表示模型越准确
wmin <- which.min(svm_rfe$results$RMSE)
wmin

#在图上标记RMSE最小的点
points(wmin, svm_rfe$results$RMSE[wmin], col = "orange", pch = 16, cex = 2)
text(wmin, svm_rfe$results$RMSE[wmin],
     paste0("N=", wmin), pos = 2, col = "orange", cex = 2)

#提取最优子集中变量的名称
Target_Genes <- svm_rfe$optVariables
Target_Genes

#提取最优子集的表达矩阵
best_SVM_data <- mydata[,Target_Genes]






#训练模型

#准备数据
View(best_SVM_data)


data <- cbind(mydata$results, best_SVM_data)

colnames(data)[1] <- "results"


#将数据集分成训练集和测试集
set.seed(12345)#保证每次跑的结果都一样
train_index <- sample(1:nrow(data), nrow(data) * 0.8)#训:测 8:2
train_data <- data[train_index, ]
test_data <- data[-train_index, ]



#支持向量机拟合模型
model <-svm(results~., data = train_data,kernel = "linear",probability = TRUE)

#查看模型预测准确表
#Accuracy = (TP + TN) / (TP + FP + FN + TN)
#TP : 真正例，预测为正，实际也正
#FP : 假正例，预测为正，实际为负
#FN : 假负例，预测为负，实际为正
#TN : 真负例，预测为负，实际为负
mean(train_data[,1] == model$fitted)

#查看混淆矩阵
table(actual = train_data[,1], model$fitted)

#获取模型预测概率
prob <- predict(model, train_data, probability = TRUE)
prob_use <- attr(prob, "probabilities")
ROC <- roc(response = train_data$results, predictor = prob_use[,2])

#plot函数绘制ROC曲线
plot(ROC,
     legacy.axes = TRUE,
     main = "SVM ROC curve",
     type = "l",
     col = "red",
     lty = 1,
     print.auc = T,
     thresholders = "best",#基于youden指数选择roc曲线最佳阈值点
     print.thres = "best"#在roc曲线上显示最佳阈值点
     )





#人工神经网络拟合模型

if(!requireNamespace("nnet",quietly = T))
  
  install.packages("nnet")

library(nnet)

#将数据集分成训练集和测试集
set.seed(12345)#保证每次跑的结果都一样
train_index <- sample(1:nrow(data), nrow(data) * 0.8)#训:测 8:2
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# 构建神经网络模型
model_ANN <- nnet(results ~ ., data = train_data, method = "nnet",
                  maxit = 1000, size = 6, decay = 0.01, trace = F)

# 将预测概率转换为二元的预测类别
threshold <- 0.5
binary_predictions_ann <- as.factor(ifelse(model_ANN$fitted >= threshold, 1, 0))
print(binary_predictions_ann)

# 查看模型预测准确率
mean(train_data[,1] == binary_predictions_ann)

# 检查标签列和模型预测结果的取值范围
unique(train_data[, 1])
unique(model_ANN$fitted)

# 检查标签列和模型预测结果的数据类型
class(train_data[, 1])
class(model_ANN$fitted)

# 检查标签列和模型预测结果的顺序
head(train_data[, 1])
head(model_ANN$fitted)

# 查看混淆矩阵
table(actual = train_data[,1], binary_predictions_ann)

# 获取模型预测概率
prob <- predict(model_ANN, train_data, type = "raw")
ROC <- roc(response = train_data$results, predictor = as.numeric(prob))

# 绘制ROC曲线
plot(ROC,
     legacy.axes = TRUE,
     main = "ANN ROC curve",
     type = "l",
     col = "red",
     lty = 1,
     print.auc = TRUE,
     thresholders = "best",
     print.thres = "best"
)






#Lasso惩罚逻辑回归拟合模型
if(!requireNamespace("glmnet",quietly = T))
  
  install.packages("glmnet")

library(glmnet)


#将数据集分成训练集和测试集
set.seed(12345)#保证每次跑的结果都一样
train_index <- sample(1:nrow(data), nrow(data) * 0.8)#训:测 8:2
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# 将数据转换为矩阵形式
train_matrix <- as.matrix(train_data[, -1])  # 去除目标变量列

# 将目标变量转换为numeric
train_results <- as.numeric(as.character(train_data$results))


# 构建Lasso惩罚逻辑回归模型
model_lasso <- cv.glmnet(x = train_matrix, y = train_results, family = "binomial", alpha = 1, standardize = TRUE, type.measure = "class")

# 使用predict函数生成模型的预测结果
model_lasso$fitted <- predict(model_lasso, newx = train_matrix, type = "class")


threshold <- 0.5
binary_predictions_lasso <- as.factor(ifelse(model_lasso$fitted >= threshold, 1, 0))
print(binary_predictions_lasso)

# 检查标签列和模型预测结果的取值范围
unique(train_data[, 1])
unique(model_lasso$fitted)

# 检查标签列和模型预测结果的数据类型
class(train_data[, 1])
class(model_lasso$fitted)

# 检查标签列和模型预测结果的顺序
head(train_data[, 1])
head(model_lasso$fitted)

# 查看模型预测准确率
mean(train_data[,1] == binary_predictions_lasso)

# 查看混淆矩阵
table(actual = train_data[,1], binary_predictions_lasso)

# 获取模型预测概率
prob_lasso <- predict(model_lasso, newx = train_matrix, type = "response")
ROC <- roc(response = train_results, predictor = as.numeric(prob_lasso))

# 绘制ROC曲线
plot(ROC,
     legacy.axes = TRUE,
     main = "Lasso ROC curve",
     type = "l",
     col = "red",
     lty = 1,
     print.auc = TRUE,
     thresholders = "best",
     print.thres = "best"
)







#Ridge惩罚逻辑回归拟合模型
if(!requireNamespace("glmnet",quietly = T))
  
  install.packages("glmnet")

library(glmnet)


#将数据集分成训练集和测试集
set.seed(12345)#保证每次跑的结果都一样
train_index <- sample(1:nrow(data), nrow(data) * 0.8)#训:测 8:2
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# 将数据转换为矩阵形式
train_matrix <- as.matrix(train_data[, -1])  # 去除目标变量列

# 将目标变量转换为numeric
train_results <- as.numeric(as.character(train_data$results))


# 构建Ridge惩罚逻辑回归模型
model_ridge <- cv.glmnet(x = train_matrix, y = train_results, family = "binomial", alpha = 0, standardize = TRUE)

# 使用predict函数生成模型的预测结果
model_ridge$fitted <- predict(model_ridge, newx = train_matrix, type = "response")

threshold <- 0.5
binary_predictions_ridge <- as.factor(ifelse(model_ridge$fitted >= threshold, 1, 0))
print(binary_predictions_ridge)

# 检查标签列和模型预测结果的取值范围
unique(train_data[, 1])
unique(model_ridge$fitted)

# 检查标签列和模型预测结果的数据类型
class(train_data[, 1])
class(model_ridge$fitted)

# 检查标签列和模型预测结果的顺序
head(train_data[, 1])
head(model_ridge$fitted)

# 查看模型预测准确率
mean(train_data[, 1] == binary_predictions_ridge)

# 查看混淆矩阵
table(actual = train_data[, 1], binary_predictions_ridge)

# 获取模型预测概率
prob_ridge <- predict(model_ridge, newx = train_matrix, type = "response")
ROC <- roc(response = train_results, predictor = as.numeric(prob_ridge))

# 绘制ROC曲线
plot(ROC,
     legacy.axes = TRUE,
     main = "Ridge ROC curve",
     type = "l",
     col = "red",
     lty = 1,
     print.auc = TRUE,
     thresholders = "best",
     print.thres = "best"
)






#Elastic-Net惩罚逻辑回归拟合模型
if(!requireNamespace("glmnet",quietly = T))
  
  install.packages("glmnet")

library(glmnet)


#将数据集分成训练集和测试集
set.seed(12345)#保证每次跑的结果都一样
train_index <- sample(1:nrow(data), nrow(data) * 0.8)#训:测 8:2
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# 将数据转换为矩阵形式
train_matrix <- as.matrix(train_data[, -1])  # 去除目标变量列

# 将目标变量转换为numeric
train_results <- as.numeric(as.character(train_data$results))

# 构建Elastic Net惩罚逻辑回归模型
model_enet <- cv.glmnet(x = train_matrix, y = train_results, family = "binomial", alpha = 0.5, standardize = TRUE)

# 使用predict函数生成模型的预测结果
model_enet$fitted <- predict(model_enet, newx = train_matrix, type = "response")

threshold <- 0.5
binary_predictions_enet <- as.factor(ifelse(model_enet$fitted >= threshold, 1, 0))
print(binary_predictions_enet)

# 查看模型预测准确率
mean(train_data[, 1] == binary_predictions_enet)

# 查看混淆矩阵
table(actual = train_data[, 1], binary_predictions_enet)

# 获取模型预测概率
prob_enet <- predict(model_enet, newx = train_matrix, type = "response")
ROC_enet <- roc(response = train_results, predictor = as.numeric(prob_enet))

# 绘制ROC曲线
plot(ROC_enet,
     legacy.axes = TRUE,
     main = "Elastic Net ROC curve",
     type = "l",
     col = "red",
     lty = 1,
     print.auc = TRUE,
     thresholders = "best",
     print.thres = "best"
)






