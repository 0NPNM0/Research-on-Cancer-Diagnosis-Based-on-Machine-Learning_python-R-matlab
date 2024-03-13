#导入相关的包


loadPackagesFunction <- function(){
  
  if(!requireNamespace("BiocManager",quietly = T))
    
    install.packages("BiocManager")  #用于安装和管理Bioconductor项目中的软件包
                                     #Bioconductor:专门用于生物学和生物信息学的开源软件包和工具的存储库和平台
  if(!requireNamespace("GEOquery",quietly = T))
    
    BiocManager::install("GEOquery")  #R语言包，用于从NCBI的Gene Expression Omnibus（GEO）数据库中获取基因表达数据
  
  if(!requireNamespace("limma",quietly = T))
    
    BiocManager::install("limma",force = TRUE)  #R语言包，用于分析基因表达谱数据
  
  if(!requireNamespace("tidyverse",quietly = T))
    
    install.packages("tidyverse",force = TRUE)  #R语言中的数据科学工具集合,包括ggplot2等
  
  if(!requireNamespace("Biobase",quietly = T))
    
    install.packages("Biobase",force = TRUE)  #提供处理和分析生物数据的核心功能和工具
  
  if(!requireNamespace("e1071",quietly = T))
    
    install.packages("e1071")  #R语言包，提供了用于机器学习和统计模型的功能和工具集，包括SVM,KNN,Naive Bayes,Decision Trees,Clustering Analysis
  
  if(!requireNamespace("caret",quietly = T))
    
    install.packages("caret")  #R语言包，集成了上百种分类和回归算法
  
  if(!requireNamespace("pROC",quietly = T))
    
    install.packages("pROC")  #R语言包，ROC曲线可视化
  
  if(!requireNamespace("mlrMBO",quietly = T))
    
    install.packages("mlrMBO")  #R语言包，用于基于模型的优化，特别是用于搜索和优化黑盒函数的参数配置
  
  if(!requireNamespace("nnet",quietly = T))
    
    install.packages("nnet")  #R语言包，用于构建和训练人工神经网络（Artificial Neural Networks）
  
  if(!requireNamespace("glmnet",quietly = T))
    
    install.packages("glmnet")  #R语言包，用于拟合和调优广义线性模型（Generalized Linear Models，GLMs）的弹性网络（Elastic Net）
  
  if(!requireNamespace("skimr",quietly = T))
    
    install.packages("skimr")  #R语言包，用于提供数据摘要和描述性统计的快速概览
  
  if(!requireNamespace("smotefamily",quietly = T))
    
    install.packages("smotefamily")  #R语言包，包含SMOTE算法
  
  if(!requireNamespace("rgl",quietly = T))
    
    install.packages("rgl")  #R语言包，用于在三维环境中进行交互式的数据可视化和图形展示
  
  
  
  
  library(tidyverse)
  
  library(limma)
  
  library(GEOquery)
  
  library(Biobase)
  
  library(e1071)
  
  library(caret)
  
  library(pROC)
  
  library(ggplot2)
  
  library(mlrMBO)
  
  library(nnet)
  
  library(glmnet)
  
  library(skimr)
  
  library(smotefamily)
  
  library(rgl)
  
}
