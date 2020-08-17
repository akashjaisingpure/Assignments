#uploading the file
concrete <- read.csv(file.choose())
View(concrete)
str(concrete)
attach(concrete)

#normalizing the concrete data
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

concrete_norm<-as.data.frame(lapply(concrete[,-9],FUN=normalize))

summary(concrete_norm)

concrete_norm <- cbind(concrete_norm,concrete$strength)
colnames(concrete_norm)[9] <- "strength"

#spliting into train and test
concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]

#building a regression model
library(neuralnet)  
?neuralnet
concrete_model <- neuralnet(strength~., data = concrete_train)

plot(concrete_model)

model_result <- compute(concrete_model,concrete_test[1:8])
str(model_result)

predicted_strength <- model_result$net.result
cor(predicted_strength, concrete_test$strength)

#changing the neurans and making different models
concrete_model2 <- neuralnet(strength~., data = concrete_train, hidden = c(4,4),stepmax = 1e6)

windows()
plot(concrete_model2)
model_result2 <- compute(concrete_model2,concrete_test[1:8])
str(model_result2)

predicted_strength2 <- model_result2$net.result
cor(predicted_strength2, concrete_test$strength)
