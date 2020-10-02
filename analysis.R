library(ggplot2)
library(ggmap)

sample = read.csv("sample23650.csv")




# Map with actual labels for all radar images
# map.png
Actual = c()
for (i in 1:nrow(sample)) {
    if (sample$depth[i] != -32768) {
      Actual = append(Actual, "Yes ice")
    } else if (sample$depth[i] == -32768) {
      Actual = append(Actual, "No ice")
    } else {
      Actual = append(Actual, "What")
    }
}

sample = cbind(sample, Actual)

ggplot(sample, aes(center_long, center_lat, color = Actual)) +
  geom_point(size = 1, alpha = 0.5) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12), text = element_text(family = "sans"))








# NOTE: assumes random forest model object is in current environment
# hist.png
misclassified = which(rf_model$predicted != y)

Result = c()
for (i in 1:nrow(sample)) {
  if (i %in% misclassified) {
    Result = append(Result, "Misclassified")
  } else {
    Result = append(Result, "Correctly classified")
  }
}


ggplot(sample, aes(center_lat, fill = Result)) +
  geom_histogram(alpha = 0.5, binwidth = 10) +
  labs(x = "Latitude at center of radar image", y = "Number of radar images", title = "Radar images correctly classified or misclassifed at certain latitudes on Mars") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12), text = element_text(family = "sans"))







sample = read.csv("sample23650.csv")

Actual = c()
for (i in 1:nrow(sample)) {
  if (i %in% misclassified) {
    if (sample$depth[i] != -32768) {
      Actual = append(Actual, "Misclassified (actual: yes ice)")
    } else if (sample$depth[i] == -32768) {
      Actual = append(Actual, "Misclassified (actual: no ice)")
    }
  } else {
    if (sample$depth[i] != -32768) {
      Actual = append(Actual, "Correctly classified (yes ice)")
    } else if (sample$depth[i] == -32768) {
      Actual = append(Actual, "Correctly classified (no ice)")
    }
  }
}

sample = cbind(sample, Actual)
Result = Actual
sample = cbind(sample, Result)

# map2.png
ggplot(sample[sample$Result != "Correctly classified (yes ice)" & sample$Result != "Correctly classified (no ice)",],
       aes(center_long, center_lat, color = Result)) +
  geom_point(size = 1, alpha = 0.5) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12), text = element_text(family = "sans")) +
  scale_color_manual(values = c("Misclassified (actual: no ice)" = "red", "Misclassified (actual: yes ice)" = "blue"))

# ggplot(sample, aes(center_lat, fill = Result)) +
#   geom_histogram(alpha = 0.5, binwidth = 10) +
#   labs(x = "Latitude", y = "Number of radar images") +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 12), text = element_text(family = "sans")) +
#   scale_fill_manual("Result", values = c("Correctly classified (no ice)" = "orange", "Correctly classified (yes ice)" = "purple",
#                                          "Misclassified (actual: no ice)" = "red", "Misclassified (actual: yes ice)" = "blue"))

# map3.png
ggplot(sample, aes(center_long, center_lat, color = Result)) +
  geom_point(size = 1, alpha = 0.5) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12), text = element_text(family = "sans")) +
  scale_color_manual(values = c("Correctly classified (no ice)" = "orange", "Correctly classified (yes ice)" = "purple",
                                         "Misclassified (actual: no ice)" = "red", "Misclassified (actual: yes ice)" = "blue"))


ggplot(importance, aes(x = rownames(importance), y = MeanDecreaseAccuracy)) + geom_point() + theme(axis.text.x=element_text(angle=90,hjust=1))
