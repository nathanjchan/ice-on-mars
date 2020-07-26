# Set Up ----
df = read.csv("radar.csv", stringsAsFactors = FALSE)
df = df[, !(colnames(df) %in% "X")]
bad = c("Radar_Images/tiff/s_0240xx/s_02407601_tiff.tif", "Radar_Images/tiff/s_0564xx/s_05645702_tiff.tif")
df = df[!(df$tif %in% bad),]

library(raster)
library(e1071)
library(glcm)
library(parallel)
library(randomForest)

library(corrgram)









# Functions ----
sampleTifClassification = function(n) {
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
  
  return(sampled)
}

sampleTifRegression = function(n) {
  # Given sample size n, return a sample of radargrams from GLOBAL VARIABLE df
  
  # REGRESSION ----
  # Exclude areas with no shallow ice 
  df3000 = df[df$width > 3000 & df$depth != -32768,]
  num = nrow(df3000)
  sample_indices = sample(num, n)
  sampled = df3000[sample_indices,]
  
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

getGLCM = function(radar_smol, window_size) {
  # Given a radargram, calculate the GLCM and return the feature statistics
  gray = glcm(radar_smol, window = c(window_size, window_size))
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
  #if (df$depth[df$tif == tif_path] == -32768) {
  #  features[1] = "no"
  #} else {
  #  features[1] = "yes"
  #}
  
  # REGRESSION ----
  # shallow ice depth
  #features[1] = df$depth[df$tif == tif_path]
  
  # COLOR STATISTICS ----
  features = append(features, getStatistics(radar_mat))
  
  # COLOR HISTOGRAM ----
  color_hist = hist(radar_mat, breaks = seq(0, 255, l = 26))
  features = append(features, color_hist$density) # density instead of counts
  
  # GRAY LEVEL CO-OCCURRANCE MATRIX ----
  stats1 = getGLCM(radar_smol, window_size = 3)
  stats2 = getGLCM(radar_smol2, window_size = 3)
  stats3 = getGLCM(radar_smol, window_size = 9)
  features = append(features, stats1)
  features = append(features, stats2)
  features = append(features, stats3)
  
  # END ----
  end_time = Sys.time()
  time_taken = end_time - start_time
  total_time_taken = end_time - global_start_time
  #global_i <<- global_i + 1
  print(time_taken)
  print(total_time_taken)
  #print(paste0(global_i, "/", global_n))
  return(features)
}

extractIceClassification = function(tif_path) {
  # CLASSIFICATION
  # ice or not
  if (df$depth[df$tif == tif_path] == -32768) {
    ice_or_not = "no"
  } else {
    ice_or_not = "yes"
  }
  return(ice_or_not)
}

extractIceRegression = function(tif_path) {
  # REGRESSION
  # shallow ice depth
  ice_depth = df$depth[df$tif == tif_path]
  return(ice_depth)
}










# Sample radargrams----
n = 500
global_i = 0
global_n = n
global_start_time = Sys.time()
set.seed(23*3)
#sample = sampleTifClassification(n)
sample = sampleTifRegression(n)
half1 = 1:(n/2)
half2 = (n/2 + 1):n



# PARALLEL???
num_cores = detectCores()
cl = makeCluster(num_cores, outfile = "output.txt")
clusterExport(cl, varlist = c("relevantColumns", "getStatistics", "replaceInf", "getGLCM", "global_start_time"))
clusterEvalQ(cl, {
  library(raster)
  library(e1071)
  library(glcm)
})
features = parLapply(cl, sample$tif, extractFeatures)
stopCluster(cl)

#features = lapply(sample$tif, extractFeatures)
features_df = as.data.frame(do.call(rbind, features))



#ice = lapply(sample$tif, extractIceClassification)
ice = lapply(sample$tif, extractIceRegression)
ice_df = as.data.frame(do.call(rbind, ice))
features_df = cbind(ice_df, features_df)


# rename features
feature_names = c("ice", "mean", "sd", "skew", "kurt", paste0("color_hist", 1:25), paste0("glcm", 1:96))
num_features = length(feature_names)
if (num_features != ncol(features_df)) {
  stop("Number of feature names and number of features don't match!")
}
colnames(features_df) = feature_names
big = features_df[, colnames(features_df) %in% feature_names]
#write.csv(big, "big.csv")







# TODO: NOTE: MAY OR MAY NOT BE NECESSARY DEPENDING ON HOW FEATURES LOOKS AFTERWARDS

# convert to numeric
#for (name in feature_names) {
#  if (name == "ice") {
#    # REMOVE THIS LINE IF ICE COLUMN IS NUMERIC
#    #big[, colnames(big) %in% name] = as.factor(big[, colnames(big) %in% name])
#    next
#  }
#  print(name)
#  #big[, colnames(big) %in% name] = as.numeric(levels(big[, colnames(big) %in% name]))[big[, colnames(big) %in% name]]
#  big[, colnames(big) %in% name] = as.numeric(big[, colnames(big) %in% name])
#}
#write.csv(big, "bigClassification.csv")
write.csv(big, "bigRegression.csv")




# Random Forest ----
#big_rf = randomForest(ice ~ ., data=big, importance=TRUE, proximity=TRUE)

x_train = big[half1,2:num_features]
y_train = big[half1,1]
x_test = big[half2,2:num_features]
y_test = big[half2,1]

big_rf = randomForest(x=x_train, y=y_train, xtest=x_test, ytest=y_test, importance=TRUE, proximity=TRUE)
print(big_rf)
round(importance(big_rf), 2)






# Testing ----
start.time <- Sys.time()

radar = raster("Radar_Images/tiff/s_0357xx/s_03576301_tiff.tif")
# crop by selecting middle 3000
relevant = relevantColumns(ncol(radar), 3000)
e = extent(relevant[1] - 1, relevant[3000], 0, nrow(radar))
radar_crop = crop(radar, e)
# shrink the resolution by x20
radar_smol = aggregate(radar_crop, fact=2)
#gray = glcm(radar_smol, window = c(3, 3))
#plot(gray)
corrgram(as.matrix(radar_smol))

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



# random stuff
radar_mat = as.matrix(radar)
plot(radar)
color_hist = hist(radar_mat)


test = extractFeatures("Radar_Images/tiff/s_0357xx/s_03576301_tiff.tif")
