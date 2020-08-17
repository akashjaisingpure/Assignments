#uploading the file
firefly <- read.csv(file.choose())
str(firefly)
attach(firefly)

#converting into numeric
firefly$month<- as.numeric(firefly$month)
firefly$day <- as.numeric(firefly$day)

#spliting into train and test
firefly_train <- firefly[1:460,]
firefly_test <- firefly[461:517,]

#writing the svm model 
library(kernlab)
library(caret)

model1 <- ksvm(size_category~., data= firefly_train, kernel= "vanilladot")

model1

#predicitng on ksvm
predict_ksvm <- predict(model1, newdata = firefly_test)

predict_ksvm <- as.numeric(predict_ksvm)
firefly_test$size_category <- as.numeric(firefly_test$size_category)

cor(predict_ksvm, firefly_test$size_category)

#trying different kernels
?ksvm
model2 <- ksvm(size_category~., data= firefly_train, kernel= "rbfdot")

model2

#predicting on rbfdot
predict_rbfdot <- predict(model2, newdata = firefly_test)

predict_rbfdot <- as.numeric(predict_rbfdot)

cor(predict_rbfdot, firefly_test$size_category)
#clearly ksvm gives us more accuracy