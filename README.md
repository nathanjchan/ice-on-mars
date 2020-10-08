# Ice on Mars
Full research paper: [Finding Near-Surface Ice on Mars From Radar Images](https://github.com/nathanjchan/ice-on-mars/blob/master/Ice%20on%20Mars.pdf)

## Intro
Finding near-surface ice on Mars from radar images using computer vision and machine learning
- Predicts the presence of near-surface ice on Mars from the radar image with 87% accuracy
- Built software in R to extract texture from 23,650 images and applied random forest algorithm
- Learned and applied the latest research in computer vision, machine learning, and Mars science
- Presents a novel application of machine learning and has huge implications for the field of radar

## Purpose
Demonstrates
- Proficiency in computer vision techniques like image feature extraction and feature engineering
- Theoretical understanding of machine learning techniques and how to apply them
- Fluency in R to wrangle and analyze giant datasets (23,650 images, 336 GB)
- Ability to ask questions, do research, learn, and solve challenging and novel problems

Presents new research on
- A new application of computer vision and machine learning: quantifying speckle-like interference in ground-penetrating radar images using texture features, which has huge implications for the study of ground-penetrating radar and its applications
- A new image feature: using the mean, standard deviation, skewness, and kurtosis of the distribution of texture descriptors from many sliding windows as image features, which creates new ways of using features for image retrieval and classification

## Contents
- [exploratory.R](https://github.com/nathanjchan/ice-on-mars/blob/master/exploratory.R): find radar image coordinates and the depth of ice at that location and create [radar2.csv](https://github.com/nathanjchan/ice-on-mars/blob/master/radar2.csv)
- [corrupted.R](https://github.com/nathanjchan/ice-on-mars/blob/master/corrupted.R): find images with processing errors
- [functions.R](https://github.com/nathanjchan/ice-on-mars/blob/master/functions.R): functions to calculate texture features from each image and create [sample23650.csv](https://github.com/nathanjchan/ice-on-mars/blob/master/sample23650.csv)
- [features.R](https://github.com/nathanjchan/ice-on-mars/blob/master/features.R): extract texture features from gray-level co-occurance matrix
- [prediction.R](https://github.com/nathanjchan/ice-on-mars/blob/master/predictions.R): apply random forest to predict locations of ice from image
- [analysis.R](https://github.com/nathanjchan/ice-on-mars/blob/master/analysis.R): create figures for locations of correctly classified and misclassifed radar images

# Research Paper
Full research paper: [Finding Near-Surface Ice on Mars From Radar Images](https://github.com/nathanjchan/ice-on-mars/blob/master/Ice%20on%20Mars.pdf)

## Abstract
Utilizing the near-surface ice on Mars just centimeters under the Martial soil is essential to human exploration of the planet. SHARAD, a ground-penetrating radar in orbit around Mars, is too low resolution to differentiate between the soil and ice. Radar reflections from these two indistinguishable subsurface layers create speckle-like interference, which is the background noise in processed SHARAD radar images. I quantify this speckle-like interference in each radar image using texture features from gray-level co-occurrence matrices. I label each image with whether or not near-surface ice exists at the location of the image based on an ice depth map of Mars from previous research. I find that machine learning with random forest predicts the presence of near-surface ice from the radar image texture with 87% accuracy, suggesting it is possible to measure speckle-like interference and use it to find near-surface ice on Mars and beyond.

## Hypothesis
The near-surface ice may be causing *speckle-like interference* in the radar data, which are present in processed radar images as different textures. Texture features can quantify the radar image’s texture, and machine learning can find whether near-surface ice exists at the location of an image, because the background noise (the texture) is different when there is ice.

## Results
Random forest out-of-bag confusion matrix
|                 | Predict: no ice | Predict: yes ice | Total |  Error | Accuracy |
|----------------:|----------------:|-----------------:|------:|-------:|---------:|
|  Actual: no ice |           12903 |             1380 | 14283 |  9.66% |   90.34% |
| Actual: yes ice |            1527 |             7840 |  9367 | 16.30% |   83.70% |
|           Total |           14430 |             9220 | 23650 | 12.29% |   87.71% |

## Conclusion
Near-surface ice influences the ground-penetrating radar returns through speckle-like interference. Texture features can quantify the speckle-like interference because it is present in processed radar images, and machine learning can use texture features to distinguish whether there is near-surface ice or not at the location of the image. The ability to measure speckle-like interference opens up many new opportunities to how we use ground-penetrating radar to study Mars and other celestial bodies in continuing humanity’s never-ending quest to live among the stars.
