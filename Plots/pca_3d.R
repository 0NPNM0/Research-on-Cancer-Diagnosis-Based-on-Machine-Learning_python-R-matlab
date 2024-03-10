
# PCA 3D scatterplot

PCA3DFunction <- function(data_syn){
  
  library(rgl)
  
  #处理合成后的数据
  
  data_all <- data_syn
  
  do <- as.matrix(data_all[,3:17069])
  
  pca <- prcomp(do)
  
  group <- ifelse(data_all$types == "old", "white",
                  ifelse(data_all$types == "new", "yellow", NA))
  
  
  # 绘制3D图形
  plot3d(pca$x[, 1:3],
         xlab = "comp1", ylab = "comp2", zlab = "comp3",
         col = group,
         type = "s",
         size = 1,
         lwd = 2,
         box = TRUE,
         xlim = c(-20, 20),
         ylim = c(-20, 20),
         zlim = c(-20, 20))
  
}






