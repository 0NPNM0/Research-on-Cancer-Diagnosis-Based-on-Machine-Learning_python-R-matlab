#导入相关的包


loadPackagesFunction <- function(){
  if(!requireNamespace("BiocManager",quietly = T))
    
    install.packages("BiocManager")
  
  if(!requireNamespace("GEOquery",quietly = T))
    
    BiocManager::install("GEOquery")
  
  if(!requireNamespace("limma",quietly = T))
    
    BiocManager::install("limma",force = TRUE)
  
  if(!requireNamespace("tidyverse",quietly = T))
    
    install.packages("tidyverse",force = TRUE)
  
  if(!requireNamespace("Biobase",quietly = T))
    
    install.packages("Biobase",force = TRUE)
  
  if(!requireNamespace("e1071",quietly = T))
    
    install.packages("e1071")
  
  if(!requireNamespace("caret",quietly = T))
    
    install.packages("caret")
  
  if(!requireNamespace("pROC",quietly = T))
    
    install.packages("pROC")
  
  if(!requireNamespace("mlrMBO",quietly = T))
    
    install.packages("mlrMBO")
  
  if(!requireNamespace("nnet",quietly = T))
    
    install.packages("nnet")
  
  if(!requireNamespace("glmnet",quietly = T))
    
    install.packages("glmnet")
  
  if(!requireNamespace("skimr",quietly = T))
    
    install.packages("skimr")
  
  
  
  
  library(tidyverse)
  
  library(limma)
  
  library(GEOquery)
  
  library(Biobase)
  
  library(e1071)#支持向量机
  
  library(caret)#集成了上百种分类和回归算法
  
  library(pROC)#ROC曲线可视化
  
  library(ggplot2)#绘图
  
  library(mlrMBO)#贝叶斯优化：根据之前的模型训练结果来选择下一个参数组合
  
  library(nnet)
  
  library(glmnet)
  
  library(skimr)
  
}
