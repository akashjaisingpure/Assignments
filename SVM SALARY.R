#uploading the file
salary_train <- read.csv(file.choose())
salary_test <- read.csv(file.choose())

#converting into numeric

salary_train[, c(2,3,5,6,7,8,9,13,14)] <- as.numeric(salary_train[,c(2,3,5,6,7,8,9,13,14)]
salary_test[,c(2,3,5,6,7,8,9,13,14)] <- as.numeric(salary_test[,c(2,3,5,6,7,8,9,13,14)])

#writing the svm model 
library(kernlab)
library(caret)

model1 <- ksvm(salary_train$Salary~., data= salary_train, kernel= "vanilladot")

model1

#predicitng on ksvm
predict_ksvm <- predict(model1, newdata = salary_test)

cor(predict_ksvm, salary_test$Salary)

#trying different kernals
#laplacedot 58%
#besseldot 37.5%
#
?ksvm
model2 <- ksvm(salary_train$Salary~., data= salary_train,kernel= "anovadot")

#prediciting on rbfdot
predict_rbfdot <- predict(model2, newdata = salary_test)

predict_rbfdot <- as.numeric(predict_rbfdot)

cor(predict_rbfdot, salary_test$Salary)
