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
if (!file.exists("GSE40419_eSet.Rdata")) {
  
  GEO_file <- getGEO('GSE40419',#需要下载的series
                     
                     destdir = 'F:/Graduation thesis',#设置文件保存路径
                     
                     getGPL = T #下载平台文件

  )

  save(GEO_file, file = "GSE40419_eSet.Rdata")#将下载下来的文件保存为我们可以处理的格式

}



load("GSE40419_eSet.Rdata")#将更改好格式并保存好的的数据加载到这里


GEO_file[[1]]#提取GEO_file中第一个数据

exp <- exprs(GEO_file[[1]])#提取数据中的样本基因表达矩阵

plate <- fData(GEO_file[[1]])#提取数据中的平台信息

clinical <- pData(GEO_file[[1]])#提取数据中的样本临床信息（比如：年龄、性别、是否存活等等）

#看一下数据分布

exp <- as.matrix(exp)

boxplot(exp)
