# Set Up ----
df = read.csv("radar.csv", stringsAsFactors = FALSE)
df = df[, !(colnames(df) %in% "X")]
set.seed(23*3)

library(raster)
library(e1071)
library(randomForest)
#library(corrgram)
#library(spatial)
#library(glcm)


# Functions ----
skewness2 = function(radar_mat) {
  # Given an image matrix, returns third color moment
  # NOTE: doesn't work sometimes because it exceeds color capacity
  mu = mean(radar_mat)
  skewness = 0
  for (f in radar_mat) {
    skewness = skewness + ((f - mu) ^ 3)
  }
  skewness = (skewness / length(radar_mat)) ^ (1/3)
  return(skewness)
}


relevantColumns = function(ncol, nwant) {
  # Given the number of columns and the number of wanted center columns,
  # return a vector with the indices of the wanted columns
  cut = (ncol - nwant) / 2
  top_cut = floor(cut)
  bottom_cut = ceiling(cut)
  selected = (bottom_cut + 1):(ncol - top_cut)
  return(selected)
}


extractFeatures = function(tif_path) {
  # Given file path to a .tif file, return a list of features
  print(tif_path)
  radar = raster(tif_path)
  radar_mat = as.matrix(radar)
  radar_mat = radar_mat[,relevantColumns(ncol(radar_mat), 3000)]
  
  features = c()
  
  # FEATURE 1 FOR REGRESSION ----
  # REGRESSION: shallow ice depth
  features[1] = df$depth[df$tif == tif_path]
  
  # FEATURE 2 ----
  # mean of intensity of radargram
  features[2] = mean(radar_mat)
  
  # FEATURE 3 ----
  # standard deviation of intensity of radargram
  features[3] = sd(radar_mat)
  
  # FEATURE 4 ----
  # skewness of intensity of radargram
  features[4] = skewness(radar_mat)
  #features[4] = skewness2(radar_mat)
  
  # FEATURE 5 ----
  # color histogram
  color_hist = hist(radar_mat, breaks = seq(0, 255, l = 26))
  features = append(features, color_hist$density) # density instead of counts
  
  # FEATURE 6:
  
  return(features)
}


sampleTIF = function(n) {
  # Given sample size n, return a sample of radargrams from GLOBAL VARIABLE df
  
  # NORMAL ----
  # Sample from all radargrams
  #num = nrow(df)
  #sample_indices = sample(num, n)
  #sampled = df[sample_indices,]
  
  # REGRESSION ----
  # Exclude areas with no shallow ice 
  df3000 = df[df$width > 3000 & df$depth != -32768,]
  num = nrow(df3000)
  sample_indices = sample(num, n)
  sampled = df3000[sample_indices,]
  
  return(sampled)
}


# Sample data frame ----
n = 500
sample = sampleTIF(n)
half1 = 1:(n/2)
half2 = (n/2 + 1):n

# create data frame
features = lapply(sample$tif, extractFeatures)
features_df = as.data.frame(do.call(rbind, features))

# rename features
feature_names = c("ice", "mean", "sd", "skew", paste0("color_hist", 1:25))
colnames(features_df) = feature_names
big = features_df[, colnames(features_df) %in% feature_names]


# Random Forest ----
#big_rf = randomForest(ice ~ ., data=big, importance=TRUE, proximity=TRUE)
big_rf = randomForest(x=big[half1,2:29], y=big[half1,1], xtest=big[half2,2:29], ytest=big[half2,1],
                      importance=TRUE, proximity=TRUE)
print(big_rf)
round(importance(big_rf), 2)
