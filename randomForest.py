import numpy as np
import pandas as pd
import numpy as np
import tensorflow as tf
from sklearn.ensemble import RandomForestClassifier

print(tf.config.experimental.list_physical_devices('GPU'))

big = pd.read_csv("big.csv")
big = big.drop(columns = "Unnamed: 0")
num_features = big.shape[1]

x_train = big.iloc[0:250, 1:num_features]
y_train = big.iloc[0:250, 0]

x_test = big.iloc[250:500, 1:num_features]
y_test = big.iloc[250:500, 0]

rf = RandomForestClassifier()
rf.fit(x_train, y_train)
print(rf.score(x_test, y_test))
