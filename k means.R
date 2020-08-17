# creating random variable

#install.packages("plyr")
library("plyr")

?runif()
x <- runif(50,0,100)
y <- runif(50,100,200)

x<- range(50)

#plotting both
plot(x)
barplot(x)
barplot(y)

data <- cbind(x,y)
View(data)
hist(data)

# scaling the above data point
data_scale <- scale(data[])
View(data_scale)

hist(data_scale)

# writing a function for normalization

normalize <- function(i)
{
(i-min(data))/(max(data)-min(data))
}

data_normalize <- normalize(data)
View(data_normalize)
max(data_normalize)
max(data_scale)

hist(data_scale)
hist(data_normalize)
