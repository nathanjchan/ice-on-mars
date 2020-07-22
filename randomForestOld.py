import numpy as np
import pandas as pd
from PIL import Image
from scipy.stats import skew, kurtosis
from sklearn.ensemble import RandomForestClassifier
from save_load import save_load
save_load = save_load()

#radar = Image.open("Radar_Images/tiff/s_0025xx/s_00258001_tiff.tif")
#radar_mat = np.array(radar)
#print(radar_mat.shape)

df = pd.read_csv("radar.csv")
df = df.drop(columns = "Unnamed: 0")
#print(df)


def relevantColumns(ncol, nwant):
    """Given the number of columns and the number of wanted center columns,
    return a vector with the indices of the wanted columns"""
    cut = (ncol - nwant) / 2
    top_cut = np.floor(cut)
    bottom_cut = np.ceil(cut)
    selected = list(range(int(bottom_cut), int(ncol - top_cut)))
    return(selected)


def extractFeatures(tif_path):
    """Given file path to a .tif file, return a list of features"""
    print(tif_path)
    radar = Image.open(tif_path)
    radar_mat = np.array(radar)
    radar_mat = radar_mat[:,relevantColumns(radar_mat.shape[1], 3000)]
    features = []
    
    # CLASSIFICATION
    # ice or not
    if float(df["depth"][df["tif"] == tif_path]) == -32768:
        features.append("no")
    else:
        features.append("yes")
        
    # STATISTICS
    features.append(np.mean(radar_mat))
    features.append(np.std(radar_mat))
    features.append(skew(radar_mat.flatten()))
    features.append(kurtosis(radar_mat.flatter()))
    
    # COLOR HISTOGRAM
    color_hist, bin_edges = np.histogram(radar_mat, bins=25)
    features.extend(color_hist)
    
    return(features)


def sampleTIF(n):
    """Given sample size n, return a sample of radargrams from GLOBAL VARIABLE df"""
    # CLASSIFICATION
    # Include radargrams with width > 3000
    df3000 = df[df["width"] > 3000]
    num = df3000.shape[0]
    sample_indices = np.random.choice(num, n)
    sampled = df3000.iloc[sample_indices,:]
    return(sampled)


# Sample
n = 500
sample = sampleTIF(n)
half1 = list(range(0, int(n/2)))
half2 = list(range(int(n/2), n-1))
features = sample["tif"].apply(extractFeatures)

features_df = pd.DataFrame(features.tolist())
feature_names = ["ice", "mean", "sd", "skew", "kurtosis"]
feature_names.extend(["".join(["color_hist", str(i)]) for i in range(1, 26)])
features_df.columns = feature_names
big = features_df


# Random Forest
x = big.iloc[0:250, 1:30]
y = big.iloc[0:250, 0]

x_test = big.iloc[250:500, 1:30]
y_test = big.iloc[250:500, 0]

rf = RandomForestClassifier()
rf.fit(x, y)
rf.score(x_test, y_test)
