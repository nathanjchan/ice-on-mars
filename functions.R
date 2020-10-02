# Libraries ----

library(raster)
library(e1071)
library(glcm)
# library(radiomics)



# Functions ----
sampleTifClassification = function(n) {
  # Given sample size n, return a sample of radargrams from GLOBAL VARIABLE df
  
  # classification: include radargrams with width > 3000
  # df3000 = df[df$width > 3000 & df$corrupt != "bad",]
  # num = nrow(df3000)
  # sample_indices = sample(num, n)
  # sampled = df3000[sample_indices,]
  
  df_good = df[df$corrupt != "bad",]
  num = nrow(df_good)
  sample_indices = sample(num, n)
  sampled = df_good[sample_indices,]
  
  return(sampled)
}



sampleTifRegression = function(n) {
  # Given sample size n, return a sample of radargrams from GLOBAL VARIABLE df
  
  # regression: exclude areas with no shallow ice 
  # df3000 = df[df$width > 3000 & df$depth != -32768 & df$corrupt != "bad",]
  # num = nrow(df3000)
  # sample_indices = sample(num, n)
  # sampled = df3000[sample_indices,]
  
  df_good = df[df$depth != -32768 & df$corrupt != "bad",]
  num = nrow(df_good)
  sample_indices = sample(num, n)
  sampled = df_good[sample_indices,]
  
  return(sampled)
}



skewness2 = function(radar_mat) {
  # Given an image matrix, returns third color moment
  # NOTE: doesn't work sometimes because it exceeds integer capacity
  mu = mean(radar_mat)
  skewness = 0
  for (f in radar_mat) {
    skewness = skewness + ((f - mu) ^ 3)
  }
  skewness = (skewness / length(radar_mat)) ^ (1/3)
  return(skewness)
}



relevantColumns = function(ncol, nwant) {
  # Given the # columns and # wanted center columns, return a vector with the indices of the wanted columns
  cut = (ncol - nwant) / 2
  top_cut = floor(cut)
  bottom_cut = ceiling(cut)
  selected = (bottom_cut + 1):(ncol - top_cut)
  return(selected)
}



getStatistics = function(radar_mat) {
  # Given a matrix, calculate statistics, and return them in a list
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
  gray = glcm(radar_smol, window = c(window_size, window_size), shift = c(1, 1))
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
  
  # set up
  print(tif_path)
  start_time = Sys.time()
  
  # read and crop radar
  width = 64
  radar = raster(tif_path)
  relevant = relevantColumns(ncol(radar), width)
  e = extent(relevant[1] - 1, relevant[width], 0, nrow(radar))
  radar_crop = crop(radar, e)
  radar_mat = as.matrix(radar_crop)
  
  # change resolution
  # radar_smol2 = aggregate(radar_crop, fact=2)
  radar_smol2 = radar_crop
  
  # vector for return
  features = c()
  
  # color statistics
  features = append(features, getStatistics(radar_mat))

  # color histogram
  color_hist = hist(radar_mat, breaks = seq(0, 255, l = 33))
  features = append(features, color_hist$density) # density instead of counts
  
  # gray level co-occurrance matrix
  features = append(features, getGLCM(radar_smol2, window_size = 3))
  features = append(features, getGLCM(radar_smol2, window_size = 5))
  features = append(features, getGLCM(radar_smol2, window_size = 7))
  features = append(features, getGLCM(radar_smol2, window_size = 11))
  features = append(features, getGLCM(radar_smol2, window_size = 15))
  features = append(features, getGLCM(radar_smol2, window_size = 23))
  features = append(features, getGLCM(radar_smol2, window_size = 31))
  features = append(features, getGLCM(radar_smol2, window_size = 47))
  features = append(features, getGLCM(radar_smol2, window_size = 63))
  # features = append(features, getGLCM(radar_smol2, window_size = 127))
  
  # fractal dimension
  # fractal7 = fd.estimate(as.matrix(radar_crop), window.size = 7)
  # fractal15 = fd.estimate(as.matrix(radar_crop), window.size = 15)
  # fractal31 = fd.estimate(as.matrix(radar_crop), window.size = 31)
  # fractal63 = fd.estimate(as.matrix(radar_crop), window.size = 63)
  # fractal127 = fd.estimate(as.matrix(radar_crop), window.size = 127)
  # features = append(features, getStatistics(fractal7$fd))
  # features = append(features, getStatistics(fractal15$fd))
  # features = append(features, getStatistics(fractal31$fd))
  # features = append(features, getStatistics(fractal63$fd))
  # features = append(features, getStatistics(fractal127$fd))
  
  # end
  end_time = Sys.time()
  time_taken = end_time - start_time
  total_time_taken = end_time - global_start_time
  # global_i <<- global_i + 1
  print(time_taken)
  print(total_time_taken)
  # print(paste0(global_i, "/", global_n))
  return(features)
}



extractIceClassification = function(tif_path) {
  # classification: ice or not
  if (df$depth[df$tif == tif_path] == -32768) {
    ice_or_not = "no"
  } else {
    ice_or_not = "yes"
  }
  return(ice_or_not)
}



extractIceRegression = function(tif_path) {
  # regression: shallow ice depth
  ice_depth = df$depth[df$tif == tif_path]
  return(ice_depth)
}



# Work in progress function ----
extractFeatures2 = function(tif_path) {
  # set up
  print(tif_path)
  start_time = Sys.time()
  rasterOptions(tmpdir = "D:/Mars_Data/__raster__/", tmptime = 2)
  
  # read and crop radar
  radar = raster(tif_path)
  relevant = relevantColumns(ncol(radar), 3000)
  e = extent(relevant[1] - 1, relevant[3000], 0, nrow(radar))
  radar_crop = crop(radar, e)
  radar_mat = as.matrix(radar_crop)
  
  # list for return
  features = c()

  # color histogram
  color_hist = hist(radar_mat, breaks = seq(0, 255, l = 33))
  features = append(features, color_hist$density) # density instead of counts
  
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



getDensity = function(radar_mat) {
  mass = sum(radar_mat[radar_mat > 128])
  volume = length(radar_mat[radar_mat > 128])
  if (volume == 0) {
    return(0)
  }
  density = mass / volume
  return(density)
}
