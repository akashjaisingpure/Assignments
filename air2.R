#importing the data
library(readxl)
air <- read_excel("C:/Users/Desktop/ExcelR/Assignment/Clustering/EastWestAirlines.xlsx", 
                               sheet = "data")

# Normalizing continuous columns to bring them under same scale
normalized_data <- scale(air[,2:11])

# distance matrix
d <- dist(normalized_data, method = "euclidean")

# fitting the data
fit <- hclust(d, method="complete")

##display dendrogram##
plot(fit) 
plot(fit, hang=-1)

#making no sense as data point are more

#k means clustering
km <- kmeans(air, 5)
str(km)
km$cluster
km$centers

# groups or cluster numbers
groups <- km$cluster
membership<-as.matrix(groups)

# groups or cluster numbers
final <- data.frame(air, membership)

write.csv(final, file="final.csv",row.names = F)
getwd()

#clubing them together#
aggregate(air[,-1],by=list(final$membership),mean)
##giving the passengers name accoundingly