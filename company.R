#importing the data
library(readr)
company <- read_csv("C:/Users/Desktop/ExcelR/Assignment/12. Decision Tree/Company_Data.csv")
View(company)
attach(company)
summary(company)
hist(company$Sales)

#discrating the data
(sales <- cut(company$Sales, 3))
table(company$Sales==8.9)
sales<- cut(company$Sales,br= c(-1,6,8.9,17), labels= c("<6","6-9",">8.9"))
table(sales)
company$Sales <- sales

#checking weather it is factor or not
is.factor(company$Sales)
is.data.frame(company)
is.data.frame(company$Sales)
str(company)
#splitting the data into similar catogries
company_less_6 <- company[company$Sales== "<6",]
company_bet6_9 <- company[company$Sales=="6-9",]
company_grt_8.9 <- company[company$Sales== ">8.9",]

#splitting into train and testing sets
company_train <- rbind(company_less_6[1:80,], company_bet6_9[1:100,],company_grt_8.9[1:98,])
company_test <- rbind(company_less_6[81:130,], company_bet6_9[101:154,], company_grt_8.9[99:116,])

# we neeed to install C50 package to use decision tree
install.packages("C50")
#install.packages("tree")
library(C50)
company_train_5 <- C5.0(company_train[,-1], company_train$Sales)
#plotting
windows()
plot(company_train_5)
# Training accuracy
pred_train <- predict(company_train_5, company_train)
#Accuracy
mean(company_train$Sales==pred_train) 
#93%

#predicting on test data
pred5_test <- predict(company_train_5, newdata = company_test)
#Accuracy
mean(company_test$Sales==pred5_test)

#the sampling method may change the accuracy

# Using tree function 
library(tree)
# Building a model on training data 
company_tree <- tree(Sales~.,data=company_train)

#plotting the tree
windows()
plot(company_tree)
text(company_tree,pretty = 0)

#Predicting the test data using the model
pred_tree <- as.data.frame(predict(company_tree, newdata = company_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(company_tree, newdata = company_test)

#pred_tree_final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]

mean(pred_tree_final == company_test$Sales)
