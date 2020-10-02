# Libraries ----

library(raster)
library(e1071)
library(glcm)
library(parallel)
library(randomForest)
source("functions.R")

# Global Variables ----
df = read.csv("radar.csv", stringsAsFactors = FALSE)
df = df[, !(colnames(df) %in% "X")]

# Functions ----
extractFeatures = function(tif_path) {
  # Given file path to a .tif file, return a list of features
  
  # set up
  print(tif_path)
  start_time = Sys.time()
  
  # read and crop radar
  radar = raster(tif_path)
  n = 128
  relevant = relevantColumns(ncol(radar), n)
  e = extent(relevant[1] - 1, relevant[n], 0, nrow(radar))
  radar_crop = crop(radar, e)
  radar_mat = as.matrix(radar_crop)
  
  # change resolution
  # radar_smol2 = aggregate(radar_crop, fact=2)
  radar_smol2 = radar_crop
  
  # vector for return
  features = c()
  
  # color statistics
  features = append(features, getStatistics(radar_mat))
  
  # density
  features = append(features, getDensity(radar_mat))
  
  # color histogram
  color_hist = hist(radar_mat, breaks = seq(0, 255, l = 26))
  features = append(features, color_hist$density) # density instead of counts
  
  # gray level co-occurrance matrix
  features = append(features, getGLCM(radar_smol2, window_size = 3))
  features = append(features, getGLCM(radar_smol2, window_size = 7))
  features = append(features, getGLCM(radar_smol2, window_size = 15))
  features = append(features, getGLCM(radar_smol2, window_size = 31))
  features = append(features, getGLCM(radar_smol2, window_size = 63))
  features = append(features, getGLCM(radar_smol2, window_size = 127))
  
  # fractal dimension
  fractal3 = fd.estimate(as.matrix(radar_crop), window.size = 3)
  fractal7 = fd.estimate(as.matrix(radar_crop), window.size = 7)
  fractal15 = fd.estimate(as.matrix(radar_crop), window.size = 15)
  fractal31 = fd.estimate(as.matrix(radar_crop), window.size = 31)
  fractal63 = fd.estimate(as.matrix(radar_crop), window.size = 63)
  fractal127 = fd.estimate(as.matrix(radar_crop), window.size = 127)
  features = append(features, getStatistics(fractal3$fd))
  features = append(features, getStatistics(fractal3$scale))
  features = append(features, getStatistics(fractal7$fd))
  features = append(features, getStatistics(fractal7$scale))
  features = append(features, getStatistics(fractal15$fd))
  features = append(features, getStatistics(fractal15$scale))
  features = append(features, getStatistics(fractal31$fd))
  features = append(features, getStatistics(fractal31$scale))
  features = append(features, getStatistics(fractal63$fd))
  features = append(features, getStatistics(fractal63$scale))
  features = append(features, getStatistics(fractal127$fd))
  features = append(features, getStatistics(fractal127$scale))
  
  # end
  end_time = Sys.time()
  time_taken = end_time - start_time
  total_time_taken = end_time - global_start_time
  #global_i <<- global_i + 1
  print(time_taken)
  print(total_time_taken)
  #print(paste0(global_i, "/", global_n))
  return(features)
}

# Random Forest ----

n = 500
global_i = 0
global_n = n
global_start_time = Sys.time()
set.seed(23*3)
sample = sampleTifClassification(n)
half1 = 1:(n/2)
half2 = (n/2 + 1):n



# rasterOptions(tmpdir = "D:/Mars_Data/__raster__/")
# features = lapply(sample$tif, extractFeatures)

num_cores = detectCores()
cl = makeCluster(num_cores, outfile = "output.txt")
clusterExport(cl, varlist = c("relevantColumns", "getStatistics", "replaceInf",
                              "getGLCM", "getDensity", "global_start_time"))
clusterEvalQ(cl, {
  library(raster)
  library(e1071)
  library(glcm)
  library(fractaldim)
  rasterOptions(tmpdir = "D:/Mars_Data/__raster__/")
})
features = parLapply(cl, sample$tif, extractFeatures)
stopCluster(cl)

features_df = as.data.frame(do.call(rbind, features))

ice = lapply(sample$tif, extractIceClassification)
ice_df = as.data.frame(do.call(rbind, ice))
features_df = cbind(ice_df, features_df)

feature_names = c("ice", "mean", "sd", "skew", "kurt", "density", paste0("color_hist", 1:25), paste0("glcm", 1:192), paste0("fractal", 1:48))
num_features = length(feature_names)
if (num_features != ncol(features_df)) {
  stop("Number of feature names and number of features don't match!")
}
colnames(features_df) = feature_names
big = features_df[, colnames(features_df) %in% feature_names]

big$ice = as.factor(big$ice)
write.csv(big, "bigClassificationTest128.csv")



big = read.csv("bigClassificationTest128.csv")
big = big[, !(colnames(big) %in% "X")]
big = big[-c(22, 26, 42, 54, 104, 123, 138, 139, 160, 180, 190, 205, 215, 242, 252, 257, 304, 333, 357,
             385, 386, 413, 415, 429, 431, 444, 455, 456, 467, 493),]
big$ice = as.factor(big$ice)
x = big[,2:num_features]
y = big[,1]
x_train = x[half1,]
y_train = y[half1]
x_test = x[half2,]
y_test = y[half2]
set.seed(23*3)
big_rf = randomForest(x=x_train, y=y_train, xtest=x_test, ytest=y_test, importance=TRUE, proximity=TRUE)
print(big_rf)
importance = as.data.frame(round(importance(big_rf), 2))
rf_cv = rfcv(trainx = x, trainy = y, cv.fold = 10)
print(rf_cv$error.cv)
rf_tune = tune.randomForest(x, y)
print(rf_tune)
wrongTest = which(rf_cv$predicted$`270` != big$ice)

importance4 = importance[importance$MeanDecreaseAccuracy > 4,]
print(nrow(importance4))
x = big[,rownames(importance4)]
y = big[,1]
x_train = x[half1,]
y_train = y[half1]
x_test = x[half2,]
y_test = y[half2]
set.seed(23*3)
rf_model = randomForest(x = x_train, y = y_train, xtest = x_test, ytest = y_test, importance = TRUE, proximity = TRUE)
print(rf_model)
rf_cv = rfcv(trainx = x, trainy = y, cv.fold = 10)
print(rf_cv$error.cv)
rf_tune = tune.randomForest(x, y)
print(rf_tune)
wrongTest4 = which(rf_cv$predicted$`42` != big$ice)

wrong[which(wrong %in% wrong4 & wrong %in% wrongTest & wrong %in% wrongTest4)]