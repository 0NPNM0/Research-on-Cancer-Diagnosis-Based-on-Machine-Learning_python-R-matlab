
#数据加载

DataLoadFunction <- function(param){
  
  
  filename <- paste("GSE", param, "_eSet.Rdata", sep = "")#sep:无空格
  load(filename)#将更改好格式并保存好的的数据加载到这里
  
  
  GEO_file[[1]]#提取GEO_file中第一个数据
  
  exp <- exprs(GEO_file[[1]])#提取数据中的样本基因表达矩阵
  
  plate <- fData(GEO_file[[1]])#提取数据中的平台信息
  
  clinical <- pData(GEO_file[[1]])#提取数据中的样本临床信息（比如：年龄、性别等等）
  
  return_list <- list(exp = exp, plate = plate, clinical = clinical)
  
  return(return_list)
}

