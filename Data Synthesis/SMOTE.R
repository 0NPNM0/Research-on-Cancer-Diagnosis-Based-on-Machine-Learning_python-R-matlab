
#数据合成（解决样本量不足的问题）

# 安装并加载ROSE包
install.packages("smotefamily")
library(smotefamily)

#获取类别为1的样本数据

#获取类别为2的样本数据

#获取类别为3的样本数据


summary(dap)
library(ggplot2)
ggplot(dap, aes(x = results))+geom_histogram()
# 使用base绘图系统绘制直方图
hist(dap$results)
table(dap$results)


show <- data_add$data
summary(show)
library(ggplot2)
ggplot(show, aes(x = class)) + geom_bar(stat = "count")
# 使用base绘图系统绘制直方图
show$class <- as.numeric(as.character(show$class))
hist(show$class)
table(show$class)


original_X <- dap[,-c(1,2)]
original_Y <- dap[,2]
  
data_add <- SMOTE(original_X, original_Y, K = 5, dup_size = 0)

syn_data <- data_add$syn_data

show <- syn_data$class
show
savepath <- paste("F:/Graduation thesis/data/syn1.csv", sep = "")
write.csv(syn_data, file = savepath, row.names = FALSE) 

plot(data_add$data[,1:2],main="SMOTE处理后的观测点分布",xlab="x1",ylab="x2",pch=as.integer(as.vector(dap[,3]))+1,cex=0.8)






