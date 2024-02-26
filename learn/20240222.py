# 坐标下降
# import numpy as np
#
# def sigmoid(x):
#     return 1 / (1 + np.exp(-x))
#
# def coordinate_descent(X, y, lambd, max_iter=100, tol=1e-4):
#     n_samples, n_features = X.shape
#     beta = np.zeros(n_features)
#     prev_beta = beta.copy()
#
#     for iteration in range(max_iter):
#         for j in range(n_features):
#             X_j = X[:, j]
#             r = y - sigmoid(X.dot(beta))
#             c = X_j.dot(r)
#             a = X_j.dot(X_j)
#
#             if j == 0:
#                 beta_0 = np.sum(r) / n_samples
#                 beta[j] = beta_0
#             else:
#                 if c < -lambd / 2:
#                     beta_j = (c + lambd / 2) / a
#                 elif c > lambd / 2:
#                     beta_j = (c - lambd / 2) / a
#                 else:
#                     beta_j = 0
#
#                 beta[j] = beta_j
#
#                 r += X_j * (prev_beta[j] - beta_j)
#
#         if np.linalg.norm(beta - prev_beta) < tol:
#             break
#
#         prev_beta = beta.copy()
#
#     return beta
#
#
# # 示例数据
# X = np.array([[1, 2], [2, 3], [3, 4], [4, 5]])
# y = np.array([0, 0, 1, 1])
#
# # 添加截距项
# X = np.concatenate((np.ones((X.shape[0], 1)), X), axis=1)
#
# # 设置正则化参数
# lambd = 1
#
# # 使用坐标下降算法拟合模型
# beta = coordinate_descent(X, y, lambd)
#
# print("参数估计结果：", beta)


# 解决类别不平衡问题的数据合成方法
from imblearn.over_sampling import SMOTE
from sklearn.datasets import make_classification
from collections import Counter

# n_samples：总样本数量。
# n_features：每个样本的特征数量。
# n_informative：类别数量。
# n_redundant：冗余特征数量。
# n_clusters_per_class：每个类别中的簇数量。
# weights：每个类别的样本权重。


# 创建一个不平衡数据集
X, y = make_classification(n_samples=1000, n_features=2, n_informative=2,
                           n_redundant=0, n_clusters_per_class=1,
                           weights=[0.9], random_state=42)
print(X)
print("原始数据集类别分布：", Counter(y))

# 使用 SMOTE 进行过采样
smote = SMOTE(random_state=42)
X_resampled, y_resampled = smote.fit_resample(X, y)

print("过采样后数据集类别分布：", Counter(y_resampled))
