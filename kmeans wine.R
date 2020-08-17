#uploading the data
library(readr)
wine <- read_csv("C:/Users/Desktop/ExcelR/Assignment/PCA/wine.csv")

attach(wine)

##kmeans clustering##
#random k#
km <- kmeans(wine, 12)
str(km)
km$cluster
km$centers


##for animation##
#how initial centroid are formed
#how the data points are moving
#install.packages("animation")
library(animation)

windows()
km <- kmeans.ani(wine, 12)
