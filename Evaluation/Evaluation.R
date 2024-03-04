
#评估(当前仅包含对测试数据的评估)

EvaluationFunction <- function(confusion_matrix){
  
  # 计算准确率
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  print("accuracy:")
  print(accuracy)
  # 计算精确率
  precision <- diag(confusion_matrix) / colSums(confusion_matrix)
  print("precision:")
  print(precision)
  # 计算特异度
  special <- diag(confusion_matrix)[2] / sum(confusion_matrix[, 2])
  print("special:")
  print(special)
  # 计算召回率
  recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
  print("recall:")
  print(recall)
  
}
