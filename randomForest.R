# Set Up ----
df = read.csv("radar.csv", stringsAsFactors = FALSE)
df = df[, !(colnames(df) %in% "X")]
df = df[df$tif != "Radar_Images/tiff/s_0240xx/s_02407601_tiff.tif",]
set.seed(23*3)

library(raster)
library(e1071)
library(randomForest)
#library(spatial)
library(glcm)


# Functions ----
# CHANGE FOR REGRESSION
sampleTIF = function(n) {
  # Given sample size n, return a sample of radargrams from GLOBAL VARIABLE df
  
  # NORMAL ----
  # Sample from all radargrams
  #num = nrow(df)
  #sample_indices = sample(num, n)
  #sampled = df[sample_indices,]
  
  # CLASSIFICATION ----
  # Include radargrams with width > 3000
  df3000 = df[df$width > 3000,]
  num = nrow(df3000)
  sample_indices = sample(num, n)
  sampled = df3000[sample_indices,]
  
  # REGRESSION ----
  # Exclude areas with no shallow ice 
  #df3000 = df[df$width > 3000 & df$depth != -32768,]
  #num = nrow(df3000)
  #sample_indices = sample(num, n)
  #sampled = df3000[sample_indices,]
  
  return(sampled)
}


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


getStatistics = function(radar_mat) {
  # Given a matrix, calculate mean, standard deviation, skewness, and kurtosis
  # and return them in a list
  features = c()
  features[1] = mean(radar_mat, na.rm=TRUE)
  features[2] = sd(radar_mat, na.rm=TRUE)
  features[3] = skewness(radar_mat, na.rm=TRUE)
  features[4] = kurtosis(radar_mat, na.rm=TRUE)
  return(features)
}


replaceInf = function(x) {
  # Given an object, if it is infinite, turn it into NA
  if (is.infinite(x)) {
    x = NA
  }
  return(x)
}


getGLCM = function(radar_smol) {
  # Given a radargram, calculate the GLCM and return the feature statistics
  gray = glcm(radar_smol)
  correlation = sapply(as.matrix(gray$glcm_correlation), replaceInf)
  stats = c(getStatistics(as.matrix(gray$glcm_mean)),
            getStatistics(as.matrix(gray$glcm_variance)),
            getStatistics(as.matrix(gray$glcm_homogeneity)),
            getStatistics(as.matrix(gray$glcm_contrast)),
            getStatistics(as.matrix(gray$glcm_dissimilarity)),
            getStatistics(as.matrix(gray$glcm_entropy)),
            getStatistics(as.matrix(gray$glcm_second_moment)),
            getStatistics(correlation)
  )
  return(stats)
}


# CHANGE FOR REGRESSION
extractFeatures = function(tif_path) {
  # Given file path to a .tif file, return a list of features
  print(tif_path)
  start_time = Sys.time()
  radar = raster(tif_path)
  relevant = relevantColumns(ncol(radar), 3000)
  e = extent(relevant[1] - 1, relevant[3000], 0, nrow(radar))
  radar_crop = crop(radar, e)
  radar_mat = as.matrix(radar_crop)
  
  # CHANGE RESOLUTION
  radar_smol = aggregate(radar_crop, fact=2)
  radar_smol2 = aggregate(radar_crop, fact=200)
  
  features = c()
  
  # CLASSIFICATION ----
  # ice or not
  if (df$depth[df$tif == tif_path] == -32768) {
    features[1] = "no"
  } else {
    features[1] = "yes"
  }
  
  # REGRESSION ----
  # shallow ice depth
  #features[1] = df$depth[df$tif == tif_path]
  
  # STATISTICS ----
  features = append(features, getStatistics(radar_mat))
  
  
  # GRAY LEVEL CO-OCCURRANCE MATRIX ----
  stats = getGLCM(radar_smol)
  stats2 = getGLCM(radar_smol2)
  features = append(features, stats)
  features = append(features, stats2)
  
  # COLOR HISTOGRAM ----
  color_hist = hist(radar_mat, breaks = seq(0, 255, l = 26))
  features = append(features, color_hist$density) # density instead of counts
  
  color_hist2 = hist(radar_mat, breaks = seq(0, 255, l = 26))
  features = append(features, color_hist2$density) # density instead of counts
  
  color_hist3 = hist(radar_mat, breaks = seq(0, 255, l = 26))
  features = append(features, color_hist3$density) # density instead of counts
  
  # END ----
  end_time = Sys.time()
  time_taken = end_time - start_time
  total_time_taken = end_time - global_start_time
  global_i <<- global_i + 1
  print(time_taken)
  print(total_time_taken)
  print(paste0(global_i, "/", global_n))
  return(features)
}


# Sample data frame ----
n = 500
global_i = 0
global_n = n
global_start_time = Sys.time()
sample = sampleTIF(n)
half1 = 1:(n/2)
half2 = (n/2 + 1):n

# create data frame
features = lapply(sample$tif, extractFeatures)
features_df = as.data.frame(do.call(rbind, features))

# rename features
feature_names = c("ice", "mean", "sd", "skew", "kurtosis",
                  paste0("glcm_mean", 1:4),
                  paste0("glcm_variance", 1:4),
                  paste0("glcm_homogeneity", 1:4),
                  paste0("glcm_contrast", 1:4),
                  paste0("glcm_dissimilarity", 1:4),
                  paste0("glcm_entropy", 1:4),
                  paste0("glcm_second_moment", 1:4),
                  paste0("glcm_correlation", 1:4),
                  paste0("glcm_mean2", 1:4),
                  paste0("glcm_variance2", 1:4),
                  paste0("glcm_homogeneity2", 1:4),
                  paste0("glcm_contrast2", 1:4),
                  paste0("glcm_dissimilarity2", 1:4),
                  paste0("glcm_entropy2", 1:4),
                  paste0("glcm_second_moment2", 1:4),
                  paste0("glcm_correlation2", 1:4),
                  paste0("color_hist", 1:25),
                  paste0("color_hist2", 1:25),
                  paste0("color_hist3", 1:25))
num_features = length(feature_names)
if (num_features != ncol(features_df)) {
  stop("Number of feature names and number of features don't match!")
}
colnames(features_df) = feature_names
big = features_df[, colnames(features_df) %in% feature_names]

# convert to numeric
# NOTE: REMOVE WHEN DOING REGRESSION
for (name in feature_names) {
  if (name == "ice") {
    big[, colnames(big) %in% name] = as.factor(big[, colnames(big) %in% name])
    next
  }
  print(name)
  #big[, colnames(big) %in% name] = as.numeric(levels(big[, colnames(big) %in% name]))[big[, colnames(big) %in% name]]
  big[, colnames(big) %in% name] = as.numeric(big[, colnames(big) %in% name])
}


# Random Forest ----
#big_rf = randomForest(ice ~ ., data=big, importance=TRUE, proximity=TRUE)
big_rf = randomForest(x=big[half1,2:num_features], y=big[half1,1], xtest=big[half2,2:num_features], ytest=big[half2,1],
                      importance=TRUE, proximity=TRUE)
print(big_rf)
round(importance(big_rf), 2)





# Testing ----
start.time <- Sys.time()
###
radar = raster("Radar_Images/tiff/s_0357xx/s_03576301_tiff.tif")
# crop by selecting middle 3000
relevant = relevantColumns(ncol(radar), 3000)
e = extent(relevant[1] - 1, relevant[3000], 0, nrow(radar))
radar_crop = crop(radar, e)
# shrink the resolution by x20
radar_smol = aggregate(radar_crop, fact=100)
gray = glcm(radar_smol)
plot(gray)
###
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



# random stuff
radar_mat = as.matrix(radar)
plot(radar)
color_hist = hist(radar_mat)


extractFeatures("Radar_Images/tiff/s_0357xx/s_03576301_tiff.tif")
