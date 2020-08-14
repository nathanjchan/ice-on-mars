# Libraries ----

library(randomForest)
library(e1071)
library(class)









# Load/subset data ----
big = read.csv("bigClassification196.csv")
big = big[, !(colnames(big) %in% "X")]
big$ice = as.factor(big$ice)

big2 = read.csv("bigClassificationFractal.csv")
big2 = big2[, !(colnames(big2) %in% "X")]
big = cbind(big, big2)
big = big[-c(22, 26, 42, 54, 104, 123, 138, 139, 160, 180, 190, 205, 215, 242, 252, 257, 304, 333, 357,
             385, 386, 413, 415, 429, 431, 444, 455, 456, 467, 493),]

num_features = ncol(big)
n = nrow(big)
half1 = 1:(n/2)
half2 = (n/2 + 1):n

x = big[,2:num_features]
y = big[,1]
  
x_train = x[half1,]
y_train = y[half1]
x_test = x[half2,]
y_test = y[half2]



# Random Forest ----
set.seed(23*3)
rf_model = randomForest(x = x_train, y = y_train, xtest = x_test, ytest = y_test, importance = TRUE, proximity = TRUE)
print(rf_model)
importance = as.data.frame(round(importance(rf_model), 2))
rf_cv = rfcv(trainx = x, trainy = y, cv.fold = 10)
print(rf_cv$error.cv)
rf_tune = tune.randomForest(x, y)
print(rf_tune)

# indices of samples labeled incorrectly
# wrong = which(rf_cv$predicted$`270` != big$ice)
wrong = which(rf_cv$predicted$`222` != big$ice)

# choose variables with highest importance
importance4 = importance[importance$MeanDecreaseAccuracy > 4,]
print(nrow(importance4))

# Load/subset data again
x = big[,rownames(importance4)]
y = big[,1]

x_train = x[half1,]
y_train = y[half1]
x_test = x[half2,]
y_test = y[half2]



# Random Forest again ----
set.seed(23*3)
rf_model = randomForest(x = x_train, y = y_train, xtest = x_test, ytest = y_test, importance = TRUE, proximity = TRUE)
print(rf_model)
rf_cv = rfcv(trainx = x, trainy = y, cv.fold = 10)
print(rf_cv$error.cv)
rf_tune = tune.randomForest(x, y)
print(rf_tune)

# indices of samples labeled incorrectly
# wrong4 = which(rf_cv$predicted$`52` != big$ice)
wrong4 = which(rf_cv$predicted$`51` != big$ice)



# RF with fake labels ----
rf_fake = randomForest(x = x_train, y = y_test, xtest = x_test, ytest = y_train, importance = TRUE, proximity = TRUE)
print(rf_fake)








# Support Vector Machines ----

svm_model = svm(x_train, y_train)
svm_predict = predict(svm_model, x_test)
table(svm_predict, y_test)

# cross validation
svm_tune = tune.svm(x, y)
print(svm_tune)

wrong_svm = which(svm_tune$best.model$fitted != big$ice)


# K-Nearest Neighbors ----
knn_model = knn(train = x_train, test = x_test, cl = y_train, k = 3, prob = TRUE)
table(knn_model, y_test) # does pretty poorly
