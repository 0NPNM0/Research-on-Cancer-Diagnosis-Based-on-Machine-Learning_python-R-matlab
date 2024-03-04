
#各种乱七八糟的测试








#特征选择






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






#使用Lasso回归进行特征选择
y <- as.matrix(mydata[,2])
x <- as.matrix(mydata[,3:25443])

set.seed(12345)
lasso_model <- glmnet(x,
                      y,
                      family = "binomial",#表示变量为二元分类
                      alpha = 1#采用L1正则化
)
print(lasso_model)
#DF:选择的自变量个数
#%Dev:拟合优度，越接近1越好
#Lambda:正则化参数，通过交叉验证来选取

plot(lasso_model,
     xvar = "lambda",#将正则化参数lambda作为横坐标
     label = F#不在图中显示变量名标签
)

coef_lasso <- coef(lasso_model,
                   s = 0.45300#lambda大小
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
lambda_1se <- cv_model$lambda.1se#1个标准差
lambda_1se

#根据lambda值，确定哪些变量应该被保留
coef_cv <- coef(lasso_model, s = lambda_min)
coef_cv

#根据回归系数计算OR值
exp(coef_cv)#不为1就是被筛选出来的特征

#提取有意义的变量
coef_cv <- as.matrix(coef_cv)
coef_cv <- data.frame(coef_cv)

coef_cv$OR <- exp(coef_cv$s1)#计算每一个变量的OR值
nonzero_vars <- rownames(coef_cv[coef_cv$OR != 1,])
nonzero_vars
nonzero_vars <- nonzero_vars[2:3]#去除第一个无关的变量
nonzero_vars


lasso_data <- mydata[,nonzero_vars]
lasso_data <- cbind(mydata$results, lasso_data)
colnames(lasso_data)[1] <- "results"

#训练模型


#准备数据
View(best_SVM_data)


data <- cbind(mydata$results, best_SVM_data)

colnames(data)[1] <- "results"


#将数据集分成训练集和测试集
set.seed(12345)#保证每次跑的结果都一样
train_index <- sample(1:nrow(data), nrow(data) * 0.8)
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



# 检查标签列和模型预测结果的取值范围
unique(train_data[, 1])
unique(model_lasso$fitted)

# 检查标签列和模型预测结果的数据类型
class(train_data[, 1])
class(model_lasso$fitted)

# 检查标签列和模型预测结果的顺序
head(train_data[, 1])
head(model_lasso$fitted)





#人工神经网络拟合模型

if(!requireNamespace("nnet",quietly = T))
  
  install.packages("nnet")

library(nnet)

#将数据集分成训练集和测试集
set.seed(12345)#保证每次跑的结果都一样
train_index <- sample(1:nrow(lasso_data), nrow(lasso_data) * 0.2)
train_data <- lasso_data[train_index, ]
test_data <- lasso_data[-train_index, ]


# 构建神经网络模型
model_ANN <- nnet(results ~ ., data = train_data, method = "nnet",
                  maxit = 1000, size = 6, decay = 0.01, trace = F)


# 获取模型预测概率
threshold <- 0.5
train_predictions <- predict(model_ANN, train_data, type = "raw")
train_predictions_ann <- as.factor(ifelse(train_predictions >= threshold, 1, 0))
print(train_predictions_ann)
# 查看模型预测准确率
mean(train_data[,1] == train_predictions_ann)

# 查看混淆矩阵
table(actual = train_data[,1], train_predictions_ann)
# 获取模型预测概率
prob_ann <- predict(model_ANN, newdata = train_data, type = "class")
ROC <- roc(response = train_data$results, predictor = as.numeric(prob_ann))

# 绘制ROC曲线
plot(ROC,
     legacy.axes = TRUE,
     main = "ANN train ROC curve",
     type = "l",
     col = "red",
     lty = 1,
     print.auc = TRUE,
     thresholders = "best",
     print.thres = "best"
)

# 在测试集上进行预测
test_predictions <- predict(model_ANN, test_data, type = "raw")
test_predictions_ann <- as.factor(ifelse(test_predictions >= threshold, 1, 0))
print(test_predictions_ann)
# 查看模型预测准确率
mean(test_data[,1] == test_predictions_ann)

# 查看混淆矩阵
table(actual = test_data[,1], test_predictions_ann)
# 获取模型预测概率
prob_ann <- predict(model_ANN, newdata = test_data, type = "class")
ROC <- roc(response = test_data$results, predictor = as.numeric(prob_ann))


# 绘制ROC曲线
plot(ROC,
     legacy.axes = TRUE,
     main = "ANN test ROC curve",
     type = "l",
     col = "red",
     lty = 1,
     print.auc = TRUE,
     thresholders = "best",
     print.thres = "best"
)

# 计算混淆矩阵
confusion_matrix <- table(actual = test_data[,1], test_predictions_ann)


# 计算准确率
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)
# 计算精确率
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
print(precision)
# 计算特异度
special <- diag(confusion_matrix)[2] / sum(confusion_matrix[, 2])
print(special)
# 计算召回率
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
print(recall)











#Lasso惩罚逻辑回归拟合模型


if(!requireNamespace("glmnet",quietly = T))
  
  install.packages("glmnet")

library(glmnet)


#将数据集分成训练集和测试集
set.seed(12345) #保证每次跑的结果都一样
train_index <- sample(1:nrow(lasso_data), nrow(lasso_data) * 0.2) 
train_data <- lasso_data[train_index, ]
test_data <- lasso_data[-train_index, ]

# 将数据转换为矩阵形式
train_matrix <- as.matrix(train_data[, -1])  # 去除目标变量列
test_matrix <- as.matrix(test_data[, -1])  # 去除目标变量列

# 将目标变量转换为numeric
train_results <- as.numeric(as.character(train_data$results))
test_results <- as.numeric(as.character(test_data$results))

# 构建Lasso惩罚逻辑回归模型
model_lasso <- cv.glmnet(x = train_matrix, y = train_results, family = "binomial", alpha = 1, standardize = TRUE, type.measure = "class")

# 在训练集上进行预测
train_predictions <- predict(model_lasso, newx = train_matrix, type = "response")

threshold <- 0.5
train_predictions_lasso <- as.factor(ifelse(train_predictions >= threshold, 1, 0))

# 查看模型预测准确率
mean(train_data[,1] == train_predictions_lasso)
# 查看混淆矩阵
table(actual = train_data[,1], train_predictions_lasso)
# 获取模型预测概率
prob_lasso <- predict(model_lasso, newx = train_matrix, type = "class")
ROC <- roc(response = train_results, predictor = as.numeric(prob_lasso))
# 绘制ROC曲线
plot(ROC,
     legacy.axes = TRUE,
     main = "Lasso train ROC curve",
     type = "l",
     col = "red",
     lty = 1,
     print.auc = TRUE,
     thresholders = "best",
     print.thres = "best"
)



# 在测试集上进行预测
test_predictions <- predict(model_lasso, newx = test_matrix, type = "response")

threshold <- 0.5
test_predictions_lasso <- as.factor(ifelse(test_predictions >= threshold, 1, 0))

# 查看模型预测准确率
mean(test_data[,1] == test_predictions_lasso)
# 查看混淆矩阵
table(actual = test_data[,1], test_predictions_lasso)
# 获取模型预测概率
prob_lasso <- predict(model_lasso, newx = test_matrix, type = "class")
ROC <- roc(response = test_results, predictor = as.numeric(prob_lasso))
# 绘制ROC曲线
plot(ROC,
     legacy.axes = TRUE,
     main = "Lasso test ROC curve",
     type = "l",
     col = "red",
     lty = 1,
     print.auc = TRUE,
     thresholders = "best",
     print.thres = "best"
)


# 计算混淆矩阵
confusion_matrix <- table(actual = test_data[,1], test_predictions_lasso)


# 计算准确率
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)
# 计算精确率
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
print(precision)
# 计算特异度
special <- diag(confusion_matrix)[2] / sum(confusion_matrix[, 2])
print(special)
# 计算召回率
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
print(recall)










#Ridge惩罚逻辑回归拟合模型


if(!requireNamespace("glmnet",quietly = TRUE))
  install.packages("glmnet")
library(glmnet)

#将数据集分成训练集和测试集
set.seed(12345) #保证每次跑的结果都一样
train_index <- sample(1:nrow(lasso_data), nrow(lasso_data) * 0.2) 
train_data <- lasso_data[train_index, ]
test_data <- lasso_data[-train_index, ]


# 将数据转换为矩阵形式
train_matrix <- as.matrix(train_data[, -1])  # 去除目标变量列
test_matrix <- as.matrix(test_data[, -1])  # 去除目标变量列

# 将目标变量转换为numeric
train_results <- as.numeric(as.character(train_data$results))
test_results <- as.numeric(as.character(test_data$results))

# 构建Ridge惩罚逻辑回归模型
model_ridge <- cv.glmnet(x = train_matrix, y = train_results, family = "binomial", alpha = 0, standardize = TRUE, type.measure = "class")

# 在训练集上进行预测
train_predictions <- predict(model_ridge, newx = train_matrix, type = "response")
threshold <- 0.5
train_predictions_ridge <- as.factor(ifelse(train_predictions >= threshold, 1, 0))

# 查看模型预测准确率
mean(train_data[,1] == train_predictions_ridge)

# 查看混淆矩阵
table(actual = train_data[,1], train_predictions_ridge)

# 获取模型预测概率
prob_ridge <- predict(model_ridge, newx = train_matrix, type = "class")
ROC <- roc(response = train_results, predictor = as.numeric(prob_ridge))

# 绘制ROC曲线
plot(ROC,
     legacy.axes = TRUE,
     main = "Ridge train ROC curve",
     type = "l",
     col = "red",
     lty = 1,
     print.auc = TRUE,
     thresholders = "best",
     print.thres = "best"
)

# 在测试集上进行预测
test_predictions <- predict(model_ridge, newx = test_matrix, type = "response")
test_predictions_ridge <- as.factor(ifelse(test_predictions >= threshold, 1, 0))

# 查看模型预测准确率
mean(test_data[,1] == test_predictions_ridge)

# 查看混淆矩阵
table(actual = test_data[,1], test_predictions_ridge)

# 获取模型预测概率
prob_ridge <- predict(model_ridge, newx = test_matrix, type = "class")
ROC <- roc(response = test_results, predictor = as.numeric(prob_ridge))

# 绘制ROC曲线
plot(ROC,
     legacy.axes = TRUE,
     main = "Ridge test ROC curve",
     type = "l",
     col = "red",
     lty = 1,
     print.auc = TRUE,
     thresholders = "best",
     print.thres = "best"
)


# 计算混淆矩阵
confusion_matrix <- table(actual = test_data[,1], test_predictions_ridge)


# 计算准确率
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)
# 计算精确率
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
print(precision)
# 计算特异度
special <- diag(confusion_matrix)[2] / sum(confusion_matrix[, 2])
print(special)
# 计算召回率
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
print(recall)













#Elastic-Net惩罚逻辑回归拟合模型



if(!requireNamespace("glmnet",quietly = TRUE))
  install.packages("glmnet")
library(glmnet)

#将数据集分成训练集和测试集
set.seed(12345) #保证每次跑的结果都一样
train_index <- sample(1:nrow(lasso_data), nrow(lasso_data) * 0.2) 
train_data <- lasso_data[train_index, ]
test_data <- lasso_data[-train_index, ]

# 将数据转换为矩阵形式
train_matrix <- as.matrix(train_data[, -1])  # 去除目标变量列
test_matrix <- as.matrix(test_data[, -1])  # 去除目标变量列

# 将目标变量转换为numeric
train_results <- as.numeric(as.character(train_data$results))
test_results <- as.numeric(as.character(test_data$results))

# 构建Elastic-Net惩罚逻辑回归模型
model_Elastic_Net <- cv.glmnet(x = train_matrix, y = train_results, family = "binomial", alpha = 0.5, standardize = TRUE, type.measure = "class")

# 在训练集上进行预测
train_predictions <- predict(model_Elastic_Net, newx = train_matrix, type = "response")
threshold <- 0.5
train_predictions_Elastic_Net <- as.factor(ifelse(train_predictions >= threshold, 1, 0))

# 查看模型预测准确率
mean(train_data[,1] == train_predictions_Elastic_Net)

# 查看混淆矩阵
table(actual = train_data[,1], train_predictions_Elastic_Net)

# 获取模型预测概率
prob_Elastic_Net <- predict(model_Elastic_Net, newx = train_matrix, type = "class")
ROC <- roc(response = train_results, predictor = as.numeric(prob_Elastic_Net))

# 绘制ROC曲线
plot(ROC,
     legacy.axes = TRUE,
     main = "Elastic-Net train ROC curve",
     type = "l",
     col = "red",
     lty = 1,
     print.auc = TRUE,
     thresholders = "best",
     print.thres = "best"
)

# 在测试集上进行预测
test_predictions <- predict(model_Elastic_Net, newx = test_matrix, type = "response")
test_predictions_Elastic_Net <- as.factor(ifelse(test_predictions >= threshold, 1, 0))

# 查看模型预测准确率
mean(test_data[,1] == test_predictions_Elastic_Net)

# 查看混淆矩阵
table(actual = test_data[,1], test_predictions_Elastic_Net)

# 获取模型预测概率
prob_Elastic_Net <- predict(model_Elastic_Net, newx = test_matrix, type = "class")
ROC <- roc(response = test_results, predictor = as.numeric(prob_Elastic_Net))

# 绘制ROC曲线
plot(ROC,
     legacy.axes = TRUE,
     main = "Elastic-Net test ROC curve",
     type = "l",
     col = "red",
     lty = 1,
     print.auc = TRUE,
     thresholders = "best",
     print.thres = "best"
)

# 计算混淆矩阵
confusion_matrix <- table(actual = test_data[,1], test_predictions_Elastic_Net)


# 计算准确率
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)
# 计算精确率
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
print(precision)
# 计算特异度
special <- diag(confusion_matrix)[2] / sum(confusion_matrix[, 2])
print(special)
# 计算召回率
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
print(recall)











#HLR拟合模型
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
