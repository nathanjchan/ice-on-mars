# Libraries ----

library(parallel)
source("functions.R")

# Global Variables ----
df = read.csv("radar.csv", stringsAsFactors = FALSE)
df = df[, !(colnames(df) %in% "X")]









# Sample ----
n = 1000
global_i = 0
global_n = n
global_start_time = Sys.time()
set.seed(23*3)
sample = sampleTifClassification(n)
#sample = sampleTifRegression(n)
half1 = 1:(n/2)
half2 = (n/2 + 1):n



# Parallelization
num_cores = detectCores()
cl = makeCluster(num_cores, outfile = "output.txt")
clusterExport(cl, varlist = c("relevantColumns", "getDensity", "global_start_time"))
clusterEvalQ(cl, {
  library(raster)
})
features2 = parLapply(cl, sample$tif, extractFeatures2)
stopCluster(cl)

features_df2 = as.data.frame(do.call(rbind, features2))

feature_names2 = c("density")
num_features2 = length(feature_names2)
if (num_features2 != ncol(features_df2)) {
  stop("Number of feature names and number of features don't match!")
}
colnames(features_df2) = feature_names2
big2 = features_df2

write.csv(big2, "bigClassification2.csv")









# Testing ----
start.time <- Sys.time()

radar = raster("Radar_Images/tiff/s_0055xx/s_00553001_tiff.tif")

# crop by selecting middle 3000
relevant = relevantColumns(ncol(radar), 3000)
e = extent(relevant[1] - 1, relevant[3000], 0, nrow(radar))
radar_crop = crop(radar, e)


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
