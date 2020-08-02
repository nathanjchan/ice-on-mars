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






# Sample ----
n = 500
global_i = 0
global_n = n
global_start_time = Sys.time()
set.seed(23*3)
sample = sampleTifClassification(n)
#sample = sampleTifRegression(n)
half1 = 1:(n/2)
half2 = (n/2 + 1):n



# Feature Extraction ----
# parallel computing
num_cores = detectCores()
cl = makeCluster(num_cores, outfile = "output.txt")
clusterExport(cl, varlist = c("relevantColumns", "getStatistics", "replaceInf",
                              "getGLCM", "getDensity", "global_start_time"))
clusterEvalQ(cl, {
  library(raster)
  library(e1071)
  library(glcm)
  rasterOptions(tmpdir = "D:/Mars_Data/__raster__/")
})
features = parLapply(cl, sample$tif, extractFeatures)
stopCluster(cl)

# create data frame
#features = lapply(sample$tif, extractFeatures)
features_df = as.data.frame(do.call(rbind, features))

# get labels
ice = lapply(sample$tif, extractIceClassification)
# ice = lapply(sample$tif, extractIceRegression)
ice_df = as.data.frame(do.call(rbind, ice))
features_df = cbind(ice_df, features_df)

# rename features
feature_names = c("ice", "mean", "sd", "skew", "kurt", "density", paste0("color_hist", 1:25), paste0("glcm", 1:192))
num_features = length(feature_names)
if (num_features != ncol(features_df)) {
  stop("Number of feature names and number of features don't match!")
}
colnames(features_df) = feature_names
big = features_df[, colnames(features_df) %in% feature_names]

big$ice = as.factor(big$ice)
write.csv(big, "bigClassification.csv")







# TODO: NOTE: MAY OR MAY NOT BE NECESSARY DEPENDING ON HOW FEATURES LOOKS AFTERWARDS

# convert to numeric
#for (name in feature_names) {
#  if (name == "ice") {
#    # REMOVE THIS LINE IF ICE COLUMN IS NUMERIC
#    # Need this format because can't do big$name or something
#    #big[, colnames(big) %in% name] = as.factor(big[, colnames(big) %in% name])
#    next
#  }
#  print(name)
#  #big[, colnames(big) %in% name] = as.numeric(levels(big[, colnames(big) %in% name]))[big[, colnames(big) %in% name]]
#  big[, colnames(big) %in% name] = as.numeric(big[, colnames(big) %in% name])
#}
#write.csv(big, "bigRegression.csv")




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

# shrink the resolution by x2 and x200
radar_smol2 = aggregate(radar_crop, fact = 2)
# radar_smol200 = aggregate(radar_crop, fact = 200)

# three types
gray1 = glcm(radar_smol2, window = c(3, 3))
gray2 = glcm(radar_smol2, window = c(7, 7))
gray3 = glcm(radar_smol2, window = c(15, 15))
gray4 = glcm(radar_smol2, window = c(31, 31))
gray5 = glcm(radar_smol2, window = c(63, 63))
gray6 = glcm(radar_smol2, window = c(127, 127))
# gray4 = glcm(radar_smol200, window = c(3, 3), shift = list(c(0,1), c(1,1), c(1,0), c(1,-1)))
# gray5 = glcm(radar_smol200, window = c(7, 7), shift = list(c(0,1), c(1,1), c(1,0), c(1,-1)))
# gray6 = glcm(radar_smol200, window = c(15, 15), shift = list(c(0,1), c(1,1), c(1,0), c(1,-1)))
plot(gray1)
plot(gray2)
plot(gray3)
plot(gray4)
plot(gray5)
plot(gray6)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken









# random stuff
radar_mat = as.matrix(radar)
plot(radar)
color_hist = hist(radar_mat)


test = extractFeatures("Radar_Images/tiff/s_0357xx/s_03576301_tiff.tif")
