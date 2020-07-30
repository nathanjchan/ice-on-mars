library(raster)
library(corrgram)
source("functions.R")

df = read.csv("radar.csv", stringsAsFactors = FALSE)
df = df[, !(colnames(df) %in% "X")]

corrupted = function(tif_path) {
  # Given a .tif file path, return whether the radargram is damaged based on the correlogram
  print(tif_path)
  start_time = Sys.time()
  
  # radar = raster(tif_path)
  # if (ncol(radar) <= 3000) {
  #   return("short")
  # }
  # relevant = relevantColumns(ncol(radar), 3000)
  # e = extent(relevant[1] - 1, relevant[3000], 0, nrow(radar))
  # radar_crop = crop(radar, e)
  # radar_mat = as.matrix(radar_crop)
  
  # CHANGE RESOLUTION
  # radar_smol = aggregate(radar_crop, fact=50)
  # radar_smol = aggregate(radar, fact=50)
  
  # CORRGRAM
  corr = corrgram(as.matrix(aggregate(raster(tif_path), fact=50)))
  for (i in 1:nrow(corr)) {
    for (j in 1:i) {
      if (is.na(corr[i, j])) {
        return("bad")
      }
    }
  }
  
  # END ----
  end_time = Sys.time()
  time_taken = end_time - start_time
  total_time_taken = end_time - global_start_time
  #global_i <<- global_i + 1
  print(time_taken)
  print(total_time_taken)
  #print(paste0(global_i, "/", global_n))
  return("good")
}


global_start_time = Sys.time()
num_cores = detectCores()
cl = makeCluster(num_cores, outfile = "output.txt")
clusterExport(cl, varlist = c("relevantColumns", "global_start_time"))
clusterEvalQ(cl, {
  library(raster)
  library(corrgram)
})
corrupt = parLapply(cl, df$tif, corrupted)
stopCluster(cl)



corrupt_df = do.call(rbind, corrupt)
colnames(corrupt_df) = "corrupt"
df = cbind(df, corrupt_df)
write.csv(df, "radar.csv")
