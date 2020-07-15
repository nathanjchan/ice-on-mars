# Global variables ----
depth_df = read.table("Subsurface_Ice/d_m.ascii")
long = read.table("Subsurface_Ice/Lon.ascii")
lat = read.table("Subsurface_Ice/Lat.ascii")


# Functions ----
parseCoords = function(file_path) {
  # Given path to a .lbl file of a radargram, return the coordinates of the area
  text = readLines(file_path)
  long_start = as.numeric(strsplit(strsplit(text[20], " = ")[[1]][2], " <")[[1]][1])
  lat_start = as.numeric(strsplit(strsplit(text[21], " = ")[[1]][2], " <")[[1]][1])
  long_end = as.numeric(strsplit(strsplit(text[22], " = ")[[1]][2], " <")[[1]][1])
  lat_end = as.numeric(strsplit(strsplit(text[23], " = ")[[1]][2], " <")[[1]][1])
  return(c(long_start, long_end, lat_start, lat_end))
}


meanCoords = function(coords) {
  # Given coordinates to a rectangular area of a radargram, return the center of the area
  long_start = coords[1]
  long_end = coords[2]
  lat_start = coords[3]
  lat_end = coords[4]
  # LONGITUDE: always decreasing
  # If longitude is decreasing yet the ending coord is larger, then it crossed the coordinate plane
  # Adjust by 360 depending on the sum
  if (long_end > long_start) {
    if (long_start + long_end > 360) {
      long_coord = (long_start + long_end - 360) / 2
    } else {
      long_coord = (long_start + long_end + 360) / 2
    }
  } else {
    long_coord = (long_start + long_end) / 2
  }
  # LATITUDE
  # If latitude is decreasing yet the ending coord is larger, then it crossed the coordinate plane
  # If latitude is increasing yet the ending coord is smaller, then it crossed the coordinate plane
  # Adjust by 180 depending on the sum
  inc_or_dec = df$lat_dir[df$long_start == long_start & df$long_end == long_end &
                            df$lat_start == lat_start & df$lat_end == lat_end]
  if ((inc_or_dec == "increasing" & lat_end < lat_start) |
      (inc_or_dec == "decreasing" & lat_end > lat_start)) {
    if (lat_start + lat_end > 0) {
      lat_coord = (lat_start + lat_end - 180) / 2
    } else {
      lat_coord = (lat_start + lat_end + 180) / 2
    }
  } else {
    lat_coord = (lat_start + lat_end) / 2
  }
  return(c(long_coord, lat_coord))
}


areaCoords = function(coords) {
  # Given coordinates to a rectangular area of a radargram, return the area of the area
  # NOTE: uses global df
  long_start = coords[1]
  long_end = coords[2]
  lat_start = coords[3]
  lat_end = coords[4]
  # longitude always decreases
  width = abs(long_start - long_end)
  if (long_end > long_start) {
    width = 360 - width
  }
  # latitude: check if crosses coordinate plane
  inc_or_dec = df$lat_dir[df$long_start == long_start & df$long_end == long_end &
                            df$lat_start == lat_start & df$lat_end == lat_end]
  length = abs(lat_end - lat_start)
  if ((inc_or_dec == "increasing" & lat_end < lat_start) |
      (inc_or_dec == "decreasing" & lat_end > lat_start)) {
    length = 180 - length
  }
  area = width * length
  return(area)
}


getDepthCoords = function(coords) {
  # Given coordinates to a rectangular area of a radargram, return the depth based on Piqueux et al 2019
  # Note: uses global variable depth_df
  long_start = coords[1]
  long_end = coords[2]
  lat_start = coords[3]
  lat_end = coords[4]
  
  # Find center
  long_coord = mean(c(long_start, long_end))
  lat_coord = mean(c(lat_start, lat_end))
  
  # Round to nearest .25 or .75
  long_coord = round((long_coord + 0.25) * 2) / 2 - 0.25
  lat_coord = round((lat_coord + 0.25) * 2) / 2 - 0.25
  
  # Check edge cases when rounding breaks
  if (long_coord < 0) {
    long_coord = 0
  }
  if (lat_coord < -89.75) {
    lat_coord = -89.75
  } else if (lat_coord > 89.75) {
    lat_coord = 89.75
  }
  #print(paste0("Longitude: ", long_coord))
  #print(paste0("Latitude: ", lat_coord))
  
  # Locate in depth_df
  i = which(lat$V1 == lat_coord)
  j = which(long[1,] == long_coord)
  depth = depth_df[i, j]
  return(depth)
}


getDepthFile = function(file_path) {
  # Given a path to a .lbl file of a radargram, return the depth of the subsurface ice
  coords = parseCoords(file_path)
  d = getDepthCoords(coords)
  #print(paste0("Depth: ", d))
  return(d)
}


numFiles = function(folder, extension) {
  # Given a file extension,
  # Find the number of files with that extension one level down the specified file tree
  file_path = paste0("Radar_Images/", folder)
  first_layer = list.files(file_path)
  count = 0
  for (first_item in first_layer) {
    second_path = paste0(file_path, "/", first_item)
    second_layer = list.files(second_path)
    for (second_item in second_layer) {
      if (strsplit(second_item, "\\.")[[1]][2] == extension) {
        count = count + 1
      }
    }
  }
  return(count)
}


allFiles = function(folder, extension) {
  # Given a file extension,
  # return a list of all files with that extension one level down the file tree
  files = c()
  index = 1
  file_path = paste0("Radar_Images/", folder)
  first_layer = list.files(file_path)
  for (first_item in first_layer) {
    second_path = paste0(file_path, "/", first_item)
    second_layer = list.files(second_path)
    for (second_item in second_layer) {
      if (strsplit(second_item, "\\.")[[1]][2] == extension) {
        files[index] = paste0(second_path, "/", second_item)
        index = index + 1
      }
    }
  }
  return(files)
}


sampleFiles = function(n, extension) {
  # Given sample size n, return a simple random sample of file paths with given extension
  num = numFiles("tiff", extension)
  sample_i = sample(num, n)
  population = allFiles("tiff", extension)
  sampled = population[sample_i]
  return(sampled)
}


interTabLong = function(file_path) {
  # Given a path to a .tab file, return whether the longitude is increasing or decreasing
  file_lines = readLines(file_path)
  long_one = as.numeric(strsplit(file_lines[1], ",")[[1]][4])
  long_two = as.numeric(strsplit(file_lines[2], ",")[[1]][4])
  if (long_two > long_one) {
    return("increasing")
  }
  return("decreasing")
}


interTabLat = function(file_path) {
  file_lines = readLines(file_path)
  lat_one = as.numeric(strsplit(file_lines[1], ",")[[1]][3])
  lat_two = as.numeric(strsplit(file_lines[2], ",")[[1]][3])
  if (lat_two > lat_one) {
    return("increasing")
  }
  return("decreasing")
}


radarWidth = function(tif_path) {
  # Given a file path to a .tif file, return the number of columns of the raster
  print(tif_path)
  # radar = raster(tif_path)
  # radar_mat = as.matrix(radar)
  # length = ncol(radar_mat)
  return(ncol(as.matrix(raster(tif_path))))
}


# Testing ----
getDepthFile("Radar_Images/tiff/s_0025xx/s_00258001_tiff.lbl")
sample = sampleFiles(100, "tif")
#sample_files = sampleFiles(100, "lbl")

interTabLong("Radar_Images/geom/s_0016xx/s_00168901_geom.tab")
interTabLat("Radar_Images/geom/s_0016xx/s_00168901_geom.tab")


# Giant data frame ----
# Direction of satellite
long_dir = lapply(tab_files, interTabLong)
lat_dir = lapply(tab_files, interTabLat)

# File paths
lbl_files = allFiles("tiff", "lbl")
tif_files = allFiles("tiff", "tif")
tab_files = allFiles("geom", "tab")

# Coordinates
coords = lapply(lbl_files, parseCoords)
coords_df = do.call(rbind, coords)
#coords_df = t(data.frame(coords))

# Midpoint of Coordinates
centers = lapply(coords, meanCoords)
centers_df = do.call(rbind, centers)
#centers_df = t(data.frame(centers))

# Area formed by Coordinates
areas = lapply(coords, areaCoords)

# Depth of Shallow Ice
depths = lapply(coords, getDepthCoords)

# Create the giant thing
df = do.call(rbind, Map(data.frame, long_start = coords_df[,1], long_end = coords_df[,2],
                        lat_start = coords_df[,3], lat_end = coords_df[,4], center_long = centers_df[,1],
                        center_lat = centers_df[,2], area = areas, depth = depths, 
                        lbl = lbl_files, tif = tif_files, tab = tab_files,
                        long_dir = long_dir, lat_dir = lat_dir))
write.csv(df, "radar.csv")

# Width of Radargram
widths = lapply(df$tif, radarWidth)
widths_df = do.call(rbind, widths)
colnames(widths_df) = "width"
df = cbind(df, widths_df)
write.csv(df, "radar.csv")


# Global coverage ----
# Kernal density
plot(density(centers_df[,1]))
plot(density(centers_df[,2]))
