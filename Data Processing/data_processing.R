
#数据处理

DataProcessingFunction <- function(dataset_name,exp,plate,clinical,result_col_name,symbol_col_name){
  
  exp <- as.data.frame(exp)
  
  #提取出指定列
  symbol_column <- plate$symbol_col_name
  
  # 将 symbol 列添加到 exp 数据框中
  exp <- cbind( Symbol = symbol_col_name, exp)
  
  
  #去除数据头尾空格（trimws）
  exp[, grep("Symbol", colnames(exp))] <- trimws(exp[, grep("Symbol", colnames(exp))])#grep 函数用于在 colnames(exp) 中查找包含字符串 "Symbol" 的列名
  
  #将空白负值NA
  exp[exp==""] <- NA
  
  #从数据框中删除包含缺失值（NA）的行
  exp <- na.omit(exp)
  
  exp <- as.data.frame(exp)
  
  #将 exp 数据框中小于 0 的值替换为 0
  #exp[exp < 0] <- 0
  
  #查看是否有重复值
  table(duplicated(exp$Symbol))
  
  library(limma)
  
  #将重复基因取平均值
  exp1 <- avereps(exp, ID = exp$Symbol)
  exp1 <- as.data.frame(exp1)
  rownames(exp1) <- exp1$Symbol#加上行名
  exp1 <- exp1[,-1]#删除第一列，选择所有行，但只选择除了第一列之外的所有列
  
  
  #将列的值和行的值转置，横轴：探针名，纵轴：样本名
  exp1 <- t(exp1)
  
  #获取因变量
  results <- result_col_name
  
  results <- as.data.frame(results)
  
  # 将 results 列添加到 exp1 数据框中
  exp1 <- cbind( Results = results, exp1)
  
  #处理“基因1/基因2”情况
  two_genes <- grep("/", colnames(exp1), fixed = TRUE, value = TRUE)#匹配包含"/"符号的列名
  print("contains two genes:")
  print(length(two_genes))
  
  for(col_name in two_genes){
    
    #提取“/”符号前后的列名
    parts <- strsplit(col_name,"/",fixed = TRUE)[[1]]
    before_slash <- parts[1]
    after_slash <- parts[2]
    
    #复制列并命名
    new_col <- subset(exp1, select = col_name)
    colnames(new_col) <- after_slash
    
    #将新列添加到原数据框中
    exp1 <- cbind(exp1, new_col)
    
    #重新命名旧列
    colnames(exp1)[colnames(exp1) == col_name] <- before_slash
    
  }
  
  #查看列名是否有重复值
  table(duplicated(exp1$Symbol))
  
  
  return(exp1)
}

