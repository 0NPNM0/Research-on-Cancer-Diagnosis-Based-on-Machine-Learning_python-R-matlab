import numpy as np
from keras.models import Sequential
from keras.layers import Conv2D, MaxPooling2D, Flatten, Dense
from keras.datasets import mnist
from keras.utils import to_categorical
from keras.models import Model

# 加载MNIST数据集
(x_train, y_train), (x_test, y_test) = mnist.load_data()

# 将图像数据进行预处理和归一化
x_train = np.expand_dims(x_train / 255.0, axis=-1)
x_test = np.expand_dims(x_test / 255.0, axis=-1)

# 对标签进行独热编码
y_train = to_categorical(y_train, num_classes=10)
y_test = to_categorical(y_test, num_classes=10)

# 构建CNN模型
model = Sequential()
model.add(Conv2D(32, (3, 3), activation='relu', input_shape=(28, 28, 1)))
model.add(MaxPooling2D((2, 2)))
model.add(Conv2D(64, (3, 3), activation='relu'))
model.add(MaxPooling2D((2, 2)))
model.add(Flatten())
model.add(Dense(64, activation='relu'))
model.add(Dense(10, activation='softmax'))

# 编译模型
model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])

# 训练模型
model.fit(x_train, y_train, batch_size=128, epochs=10)

# 获取中间层的特征向量
intermediate_layer_model = Model(inputs=model.input, outputs=model.layers[-2].output)
features_train = intermediate_layer_model.predict(x_train)
features_test = intermediate_layer_model.predict(x_test)

# 输出特征向量的示例
print('Train Features Example:')
print(features_train[0])