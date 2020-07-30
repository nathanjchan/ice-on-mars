# Libraries ----

library(randomForest)
library(e1071)
library(class)



# Random Forest ----

big = read.csv("bigClassification.csv")
big = big[, !(colnames(big) %in% "X")]
big$ice = as.factor(big$ice)

# big2 = read.csv("bigClassification2.csv")
# big2 = big2[, !(colnames(big2) %in% "X")]
# big = cbind(big, big2)
# big = big[-c(242, 337, 341, 418),] # TODO: DROP THEM FROM THE DF

num_features = ncol(big)
n = nrow(big)
half1 = 1:(n/2)
half2 = (n/2 + 1):n

# x = big[,2:num_features]
x = big[,31:num_features] # use only GLCM features
y = big[,1]
  
x_train = x[half1,]
y_train = y[half1]
x_test = x[half2,]
y_test = y[half2]

set.seed(23*3)
rf_model = randomForest(x = x_train, y = y_train, xtest = x_test, ytest = y_test, importance = TRUE, proximity = TRUE)
print(rf_model)
round(importance(rf_model), 2)

# cross validation
rf_cv = rfcv(trainx = x, trainy = y, cv.fold = 10)
print(rf_cv$error.cv)
rf_tune = tune.randomForest(x, y)
print(rf_tune)

# testing with fake labels
rf_fake = randomForest(x = x_train, y = y_test, xtest = x_test, ytest = y_train, importance = TRUE, proximity = TRUE)
print(rf_fake)








# Support Vector Machines ----

svm_model = svm(x_train, y_train)
svm_predict = predict(svm_model, x_test)
table(svm_predict, y_test)

# cross validation
svm_tune = tune.svm(x, y)
print(svm_tune)


# K-Nearest Neighbors ----
knn_model = knn(train = x_train, test = x_test, cl = y_train, k = 3, prob = TRUE)
table(knn_model, y_test) # does pretty poorly
