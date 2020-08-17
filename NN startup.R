#uploading the file
startup <- read.csv(file.choose())
View(startup)
str(startup)
attach(startup)

#converting factor into number
table(startup$State)
startup$State <- as.numeric(startup$State)

#normalizing the concrete data
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

startup_norm <- as.data.frame(lapply(startup[,-c(4,5)], normalize))

#checking the summary
summary(startup_norm)

#binding the state and profit
startup_norm <- cbind(startup_norm, startup$State,startup$Profit)

#renaming the state and profit column
colnames(startup_norm)[4] <- "state"
colnames(startup_norm)[5] <- "profit"

#spliting into train and test
startup_train <- startup_norm[1:40,]
startup_test <- startup_norm[41:50,]

#building a regression model
library(neuralnet)
startup_model <- neuralnet(startup_train$profit~., data = startup_train, stepmax = 1e7)

windows()
plot(startup_model)

model_result <- compute(startup_model, startup_test)
str(model_result)

predicted_profit <- model_result$net.result

cor(predicted_profit, startup_test$profit)

#giving different values of nodes to neural network
startup_model2 <- neuralnet(startup_train$profit~., data = startup_train, hidden = c(4,4), stepmax = 1e7)
plot(startup_model2)

