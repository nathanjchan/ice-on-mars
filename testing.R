library(randomForest)
library(raster)


# Classification ----
data(iris)
set.seed(71)
iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        proximity=TRUE)
print(iris.rf)

# Importance of each variable
round(importance(iris.rf), 2)

# Do MDS on 1 - proximity
iris.mds <- cmdscale(1 - iris.rf$proximity, eig=TRUE)
op <- par(pty="s")
pairs(cbind(iris[,1:4], iris.mds$points), cex=0.6, gap=0,
      col=c("red", "green", "blue")[as.numeric(iris$Species)],
      main="Iris Data: Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(iris.mds$GOF)

# Unsupervised case:
set.seed(17)
iris.urf <- randomForest(iris[, -5])
MDSplot(iris.urf, iris$Species)

# Stratified sampling: draw 20, 30, and 20 of the species to grow each tree
(iris.rf2 <- randomForest(iris[1:4], iris$Species, 
                          sampsize=c(20, 30, 20)))


# Regression ----
data(airquality)
set.seed(131)
ozone.rf <- randomForest(Ozone ~ ., data=airquality, mtry=3,
                         importance=TRUE, na.action=na.omit)
print(ozone.rf)

# Important of each variable
round(importance(ozone.rf), 2)

# "x" can be a matrix instead of a data frame
set.seed(17)
x <- matrix(runif(5e2), 100)
y <- gl(2, 50)
(myrf <- randomForest(x, y))
(predict(myrf, x))



# KNN ----
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
iris.knn = knn(train, test, cl, k = 3, prob=TRUE)
attributes(.Last.value)

table(iris.knn)


train <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
cl <- factor(c(rep("s",50), rep("c",50), rep("v",50)))
knn.cv(train, cl, k = 3, prob = TRUE)
attributes(.Last.value)



# NORMAL ----
# Sample from all radargrams
#num = nrow(df)
#sample_indices = sample(num, n)
#sampled = df[sample_indices,]



# CORRGRAM ----
# use 50x shrinked resolution
# corr = corrgram(as.matrix(radar_smol))
# for (i in 1:nrow(corr)) {
#   for (j in 1:i) {
#     features = append(features, corr[i, j])
#   }
# }



# Density ----

# crop by selecting middle 3000
radar = raster("Radar_Images/tiff/s_0357xx/s_03576301_tiff.tif")
relevant = relevantColumns(ncol(radar), 3000)
e = extent(relevant[1] - 1, relevant[3000], 0, nrow(radar))
radar_crop = crop(radar, e)
radar_mat = as.matrix(radar_crop)

test = length(radar_mat[radar_mat > 128])

