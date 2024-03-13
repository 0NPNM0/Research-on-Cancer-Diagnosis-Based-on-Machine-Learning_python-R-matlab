
#下载数据

library(GEOquery)

if (!file.exists("GSE11969_eSet.Rdata")) {
  
  GEO_file <- getGEO('GSE11969',#需要下载的series
                     
                     destdir = 'F:/Graduation thesis/data',#设置文件保存路径
                     
                     getGPL = T #下载平台文件
                     
  )
  
  save(GEO_file, file = "GSE11969_eSet.Rdata")#将下载下来的文件保存为可以处理的格式
  
}
