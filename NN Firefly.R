#uploading the file
firefly <- read.csv(file.choose())
str(firefly)

#converting factor into number
table(firefly$month)
firefly$month <- as.numeric(firefly$month)
firefly$day<- as.numeric(firefly$day)
firefly$size_category <- as.numeric(firefly$size_category)

#normalizing the concrete data
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

firefly_norm <- as.data.frame(lapply(firefly[,-c(1,2,31)], normalize))

#checking the summary
summary(firefly_norm)

#binding the month, day and size
firefly_norm <- cbind(firefly_norm, firefly$month,firefly$day,firefly$size_category)

#renaming the binded columns
colnames(firefly_norm)[29] <- "month"
colnames(firefly_norm)[30] <- "day"
colnames(firefly_norm)[31] <- "size"

#spliting into train and test
firefly_train <- firefly_norm[1:450,]
firefly_test <- firefly_norm[451:517,]

#building a regression model
library(neuralnet)
firefly_model <- neuralnet(firefly_train$size~., data = firefly_train)

model_result <- compute(firefly_model, firefly_test)
str(model_result)

cor(model_result$net.result, firefly_test$size)

#different value of neural network
firefly_model2 <- neuralnet(firefly_train$size~., data = firefly_train, hidden = c(1,5))

model_result2 <- compute(firefly_model2,firefly_test)

cor(model_result2$net.result, firefly_test$size)

