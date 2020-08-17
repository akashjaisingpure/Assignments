#importing the data
library(readr)
fraud <- read_csv("Fraud_check.csv")
View(fraud)
str(fraud)

#converting into factors
fraud$Undergrad <- as.factor(fraud$Undergrad)
fraud$Marital.Status <- as.factor(fraud$Marital.Status)
fraud$Urban <- as.factor(fraud$Urban)

summary(fraud$Taxable.Income)
tax_income <- cut(fraud$Taxable.Income, br= c(0,30000,100000), labels = c("risky","good"))
table(tax_income)
fraud$Taxable.Income<- tax_income

is.data.frame(fraud)

#splitting into train and test
fraud_split <- round(nrow(fraud)*0.40)
fraud_train <- fraud[1:fraud_split,]
fraud_test <- fraud[(fraud_split+1):nrow(fraud),]

# we neeed to install C50 package to use decision tree
install.packages("C50")
#install.packages("tree")
library("C50")
fraud_model_5 <- C5.0(fraud_train[,-3], fraud_train$Taxable.Income)
#plotting
windows()
plot(fraud_model_5)
# Training accuracy
pred_train <- predict(fraud_model_5, fraud_train)
#Accuracy
mean(fraud_train$Taxable.Income==pred_train)
#77.5%

#predicting on test data
pred5_test <- predict(fraud_model_5, newdata = fraud_test)

mean(pred5_test==fraud_test$Taxable.Income)
#82%
#due to serial sampeling the results are not that good
#other sampling technique may give good results