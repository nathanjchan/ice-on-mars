# Libraries ----

library(raster)
library(e1071)
library(glcm)



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



extractFeatures2 = function(tif_path) {
  print(tif_path)
  start_time = Sys.time()
  radar = raster(tif_path)
  relevant = relevantColumns(ncol(radar), 3000)
  e = extent(relevant[1] - 1, relevant[3000], 0, nrow(radar))
  radar_crop = crop(radar, e)
  radar_mat = as.matrix(radar_crop)
  
  features = c()
  
  # CHANGE RESOLUTION ----
  radar_smol = aggregate(radar_crop, fact=50)
  
  # CORRGRAM ----
  # corr = corrgram(as.matrix(radar_smol))
  # for (i in 1:nrow(corr)) {
  #   for (j in 1:i) {
  #     features = append(features, corr[i, j])
  #   }
  # }
  
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