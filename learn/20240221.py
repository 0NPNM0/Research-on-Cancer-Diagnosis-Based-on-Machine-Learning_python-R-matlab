# 线性回归
# import numpy as np
# from sklearn.linear_model import LinearRegression
#
# X = np.array([
#     [1],
#     [2],
#     [3],
#     [4],
#     [5]
# ])
# y = np.array([1, 3, 5, 7, 9])
# lr = LinearRegression()
# lr.fit(X, y)
# y_predict = lr.predict(np.array([[6]]))
# print(y_predict)


# 逻辑回归
# import numpy as np
# from sklearn.linear_model import LogisticRegression
#
# # 准备X的数据
# X = np.array([
#     [60],
#     [20],
#     [30],
#     [80],
#     [59],
#     [90]
# ])
# # 准备y的数据
# y = np.array([1, 0, 0, 1, 0, 1])
# # 创建逻辑回归模型
# lr = LogisticRegression()
# # 填充数据并训练
# lr.fit(X, y)
# # 准备用于测试的数据
# X_test = np.array([
#     [62],
#     [87],
#     [39],
#     [99]
# ])
# # 判断测试数据是否及格
# y_predict = lr.predict(X_test)
# print(y_predict)


# 神经网络
from sklearn.datasets import load_iris
from sklearn.neural_network import MLPClassifier
from sklearn.model_selection import train_test_split

# 加载数据集
iris_data = load_iris()
print(iris_data)
# 拆分数据集
X_train, X_test, y_train, y_test = train_test_split(iris_data['data'], iris_data['target'], test_size=0.25,
                                                    random_state=1)
# 创建神经网络模型
mlp = MLPClassifier(solver='lbfgs', hidden_layer_sizes=[4, 2], random_state=0)
# 填充数据并训练
mlp.fit(X_train, y_train)
# 评估模型
score = mlp.score(X_test, y_test)
print(score)
