# Libraries ----

library(randomForest)
library(e1071)
library(class)
library(caret)



# Load/subset data ----
big = read.csv("bigColorGLCM64-45-23650new.csv")
big = big[, !(colnames(big) %in% "X")]
big$ice = as.factor(big$ice)

# big2 = read.csv("bigClassificationFractal.csv")
# big2 = big2[, !(colnames(big2) %in% "X")]
# big = cbind(big, big2)
# big = big[-c(22, 26, 42, 54, 104, 123, 138, 139, 160, 180, 190, 205, 215, 242, 252, 257, 304, 333, 357,
#              385, 386, 413, 415, 429, 431, 444, 455, 456, 467, 493),]

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
rf_model = randomForest(x, y, importance = TRUE, ntree = 500)
# rf_model = randomForest(x = x_train, y = y_train, xtest = x_test, ytest = y_test, importance = TRUE, proximity = TRUE)
# rf_model = randomForest(x = x_test, y = y_test, xtest = x_train, ytest = y_train, importance = TRUE, proximity = TRUE)
print(rf_model)
importance = as.data.frame(round(importance(rf_model), 2))

# # cross validation
set.seed(23*3)
rf_cv = rfcv(trainx = x, trainy = y, cv.fold = 10)
print(rf_cv$error.cv)
table(rf_cv$predicted$`228`, y)

# 
# # tune?
set.seed(23*3)
rf_tune = tune.randomForest(x, y)
print(rf_tune)



run_prediction = function(seed, file_name) {
  big = read.csv(file_name)
  big = big[, !(colnames(big) %in% "X")]
  big$ice = as.factor(big$ice)
  
  big = big[-c(22, 26, 42, 54, 104, 123, 138, 139, 160, 180, 190, 205, 215, 242, 252, 257, 304, 333, 357,
               385, 386, 413, 415, 429, 431, 444, 455, 456, 467, 493),]
  rownames(big) = NULL
  
  num_features = ncol(big)
  n = nrow(big)
  half1 = 1:(n/2)
  half2 = (n/2 + 1):n
  
  x = big[,2:num_features]
  y = big[,1]
  
  set.seed(seed)
  folds = createFolds(y, k = 470)
  cm = 0
  misclassified = c()
  for (fold in folds) {
    x_train_fold = x[-fold, ]
    y_train_fold = y[-fold]
    x_test_fold = x[fold, ]
    y_test_fold = y[fold]
    
    rf_model = randomForest(x_train_fold, y_train_fold)
    rf_predict = predict(rf_model, x_test_fold)
    cm = cm + table(rf_predict, y_test_fold)
    
    if (rf_predict != y_test_fold) {
      misclassified = append(misclassified, fold)
    }
  }
  print(file_name)
  print(seed)
  print(misclassified)
  print(cm)
  print(cm[1, 1] + cm[1, 2]); e1 = cm[1, 2] / (cm[1, 1] + cm[1, 2]); print(e1); print(1 - e1)
  print(cm[2, 1] + cm[2, 2]); e2 = cm[2, 1] / (cm[2, 1] + cm[2, 2]); print(e2); print(1 - e2)
  print(cm[1, 1] + cm[2, 1]); print(cm[1, 2] + cm[2, 2]); e3 = (cm[1, 2] + cm[2, 1]) / 470; print(e3); print(1 - e3)
  results = c(seed, 1 - e3)
  return(results)
}

run_prediction(71, "bigColorGLCM64-45-10000.csv")

run_prediction(71, "bigColorGLCM3000-45.csv")
run_prediction(71, "bigColorGLCM3000-90.csv")

run_prediction(71, "bigColorGLCM750-90.csv")
run_prediction(71, "bigColorGLCM750-45.csv")

run_prediction(71, "bigColorGLCM300-90.csv")
run_prediction(71, "bigColorGLCM300-45.csv")

# num_cores = detectCores()
# cl = makeCluster(num_cores, outfile = "output.txt")
# # clusterExport(cl)
# clusterEvalQ(cl, {
#   library(randomForest)
#   library(caret)
# })
# accuracies = parLapply(cl, 1:100, run_prediction, file_name = "bigColorGLCM3000-45.csv")
# stopCluster(cl)

# indices of samples labeled incorrectly
# wrong = which(rf_cv$predicted$`228` != big$ice)

# choose variables with highest importance
importance15 = importance[importance$MeanDecreaseAccuracy > 15,]
print(nrow(importance15))

# Load/subset data again
x = big[,rownames(importance15)]
y = big[,1]

x_train = x[half1,]
y_train = y[half1]
x_test = x[half2,]
y_test = y[half2]



# Random Forest again ----
set.seed(23*3)
rf_model15 = randomForest(x, y, importance = TRUE)
# rf_model = randomForest(x = x_train, y = y_train, xtest = x_test, ytest = y_test, importance = TRUE, proximity = TRUE)
# rf_model = randomForest(x = x_test, y = y_test, xtest = x_train, ytest = y_train, importance = TRUE, proximity = TRUE)
print(rf_model15)
rf_cv = rfcv(trainx = x, trainy = y, cv.fold = 10)
print(rf_cv$error.cv)
rf_tune = tune.randomForest(x, y)
print(rf_tune)

# indices of samples labeled incorrectly
# wrong4 = which(rf_cv$predicted$`45` != big$ice)



# RF with fake labels ----
set.seed(23*3)
y_train_fake = as.factor(sample(0:1, 235, replace = TRUE))
y_test_fake = as.factor(sample(0:1, 235, replace = TRUE))
rf_fake = randomForest(x = x_train, y = y_train_fake, xtest = x_test, ytest = y_test_fake, importance = TRUE, proximity = TRUE)
print(rf_fake)



# Support Vector Machines ----

# svm_model = svm(x_train, y_train)
# svm_predict = predict(svm_model, x_test)
# table(svm_predict, y_test)

# # cross validation
# set.seed(23*3)
# svm_tune = tune.svm(x, y)
# print(svm_tune)

# cross validation again
set.seed(23*3)
folds = createFolds(y, k = 10)
cm = 0
for (fold in folds) {
  x_train_fold = x[-fold, ]
  y_train_fold = y[-fold]
  x_test_fold = x[fold, ]
  y_test_fold = y[fold]
  svm_model = svm(x_train_fold, y_train_fold, tolerance = 0.1)
  
  svm_predict = predict(svm_model, x_test_fold)
  cm = cm + table(svm_predict, y_test_fold)
  print(cm)
}

#wrong_svm = which(svm_tune$best.model$fitted != big$ice)



# K-Nearest Neighbors ----
knn_model = knn(train = x_train, test = x_test, cl = y_train, k = 3, prob = TRUE)
table(knn_model, y_test) # does pretty poorly
