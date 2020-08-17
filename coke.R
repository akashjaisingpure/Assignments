#Forcasting on coke sale data
#calling data in R enviro
library(readxl)
coke <- read_excel("C:/Users/Desktop/ExcelR/Assignment/17. Forcasting/CocaCola_Sales_Rawdata.xlsx")
#checking the data
View(coke)
#checking for null values
table(is.na(coke))
#attaching the file for further use
attach(coke)

#plotting the sales data for trend and sesonlity 
windows()
plot(coke$Sales,type="o")
#upward trend and additive seasonlity
#anyway we are going to check all the models for given data

#prerequsit for model making
#creating dummy variable
x <- 0:1
#x1<-ifelse(grepl('Q1', coke$Quarter),1,0)
#x <- matrix(rep(c(1,0,0,0), length=42),nrow = 42, ncol = 4)

q1 <- matrix(rep( c(1,0,0,0), length=42) , ncol=1)
q2 <- matrix(rep(c(0,1,0,0), length= 42), ncol=1)
q3 <- matrix(rep(c(0,0,1,0), length= 42), ncol=1)
q4 <- matrix(rep(c(0,0,0,1), length= 42), ncol=1)
x <- cbind(q1,q2,q3,q4)
colnames(x) <- c("q1","q2","q3","q4")
View(x)

#combinning the dummy and our data
newcoke <- cbind(coke, x)
#reviewing the data
View(newcoke)
#creating time series data
#creating t
newcoke["t"]<- 1:42
View(trakdata)
#creating t square
newcoke["t_sq"] <- ((newcoke["t"])^2)

#creating log of y
newcoke["log_sales"] <- (log(newcoke$Sales))

#attaching the new data
attach(newcoke)

#splitting into train and test
traincoke <- newcoke[1:38,]
testcoke <- newcoke[39:42,]

#creating different models for prediction

############################### LINEAR MODEL ######################################

linearcoke <- lm(Sales~t, data = traincoke)
summary(linearcoke)
#r squ = 81.17%

#Predicting the test data
linear_pred <- data.frame(predict(linearcoke,interval='predict',newdata = testcoke))
View(linear_pred)
#checking the test data RMSE
rmse_linear<-sqrt(mean((testcoke$Sales -linear_pred$fit)^2,na.rm = T))
rmse_linear
#RMSE = 591.5

############################## EXPONENTIAL MODEL ###################################

expocoke <- lm(log(Sales)~t, data = traincoke)
summary(expocoke)
#r squ = 82.91%

#Predicting the test data
expo_pred <- data.frame(predict(expocoke, interval = "predict", newdata = testcoke))
View(expo_pred)
#checking the test data rmse
rmse_expo <- sqrt(mean((testcoke$Sales-exp(expo_pred$fit))^2))
rmse_expo                  
#RMSE = 466.2

############################# Quadratic MODEL #######################################

quadcoke <- lm(Sales~ t + t_sq, data = traincoke)
summary(quadcoke)
#r squ = 87.99
# Predicting the test data
quad_pred <- data.frame(predict(quadcoke, interval = "predict", newdata = testcoke))
View(quad_pred)
#checking the test data rmse
rmse_quad <- sqrt(mean((testcoke$Sales-quad_pred$fit)^2))
rmse_quad
# RMSE = 475.56

########################### Additive Seasonality ####################################

addseacoke <- lm(Sales~(q1+q2+q3+q4), data = traincoke)
summary(addseacoke)
#r squ = 9.68%
#predicting the test data
addseas_pred <- data.frame(predict(addseacoke, interval = "predict", newdata= testcoke))
View(addseas_pred)
#checking the test data rmse
rmse_addsea <- sqrt(mean((testcoke$Sales- addseas_pred$fit)^2))
rmse_addsea
#RMSE = 1860.0

########################## Additive seasonality with linear trend ###################

addsealincoke <- lm(Sales~(t+q1+q2+q3+q4), data = traincoke)
summary(addsealincoke)
#r squ = 89.78
#predicting the test data
addsealinpred <- data.frame(predict(addsealincoke,interval = "predict", newdata = testcoke ))
View(addsealinpred)
#checking the test data rmse
rmse_addsealin <- sqrt(mean((testcoke$Sales- addsealinpred$fit)^2))
rmse_addsealin
#RMSE = 464.98
######################### Additive Seasonality with Quadratic #######################

addquadcoke <- lm(Sales~(t+t_sq+q1+q2+q3+q4),data = traincoke)
summary(addquadcoke)
#r squ = 96.76
#checking for test data
addquadpred <- data.frame(predict(addquadcoke, interval = "predict", newdata = testcoke))
View(addquadpred)
#checking the test rmse
rmse_addquad <- sqrt (mean((testcoke$Sales-addquadpred$fit)^2))
rmse_addquad
#301.74

###################### Multiplicative Seasonality ##################################

multicoke <- lm(log_sales~(q1+q2+q3+q4), data = traincoke)
summary(multicoke)
# r sq = 10.41
#checking for test data
multicoke_pred <-data.frame(predict(multicoke, interval = "predict", newdata = testcoke))
View(multicoke_pred)
#checking rmse of test data
rmse_multicoke <- sqrt(mean((testcoke$Sales- multicoke_pred$fit)^2))
rmse_multicoke
#4680.394

##################### Multiplicative Seasonality exponentional trend ######################

multilincoke <- lm(log_sales~(t+q1+q2+q3+q4), data = traincoke)
summary(multicoke)
#r sq = 10.41
#checking for test data
multilincoke_pred <- data.frame(predict(multilincoke, interval = "predict", newdata = testcoke))
View(multilincoke_pred)
#checking rmse of test data
rmse_multilincoke <- sqrt(mean((testcoke$Sales-exp(multilincoke_pred$fit))^2))
rmse_multilincoke
#255.52

# Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_quad","rmse_seaquad","rmse_addquad","rmse_addsealin","rmse_expo","rmse_linear","rmse_multicoke","rmse_multilincoke"), c(rmse_addquad,rmse_addsea,rmse_addsealin,rmse_expo,rmse_linear,rmse_multicoke,rmse_multilincoke,rmse_quad))
View(table_rmse)                
#it is clearly visible that RMSE of multiplicative seasonlity and linear trend have best RMSE

##################### FINALIZING THE MODEL ##################################

finalcoke <- lm(log_sales~(t+q2+q3+q4), data = newcoke)
summary(finalcoke)
exp(finalcoke$residuals)
#now we will forcast for next 4 quators
#creatign dummeies for next 4 quator
x2 <- 0:1 
Q1 <- matrix(rep( c(0,0,1,0), length=4) , ncol=1)
Q2 <- matrix(rep(c(0,0,0,1), length= 4), ncol=1)
Q3 <- matrix(rep(c(1,0,0,0), length= 4), ncol=1)
Q4 <- matrix(rep(c(0,1,0,0), length= 4), ncol=1)
#binding them together
x2 <- cbind(Q1,Q2,Q3,Q4)
#preview of the formed data
View(x2)
#creating the new time series
t2<- 39:42
#binding them all togethe
newtest_coke <- cbind(x2, t2)
#renaming it as per our data
colnames(newtest_coke) <- c("q1","q2","q3","q4","t")
#saving as data frame
newtest_coke <- as.data.frame(newtest_coke)  
#previewing the new formed data
View(newtest_coke)

#predicting on the new test data
pred_new  <- predict(finalcoke,newdata = newtest_coke)

#back transforming and saving it in the new test data
newtest_coke['predicted'] <- exp(pred_new)
View(newtest_coke)

#saving the residuals of the new model
#to check the relaionship bet them
resid <- exp(residuals(finalcoke))
resid[1:10]
#plotting the acf cure to find the corelaiton between errors
windows()
acf(resid,lag.max = 5)
#as per graph our error have corelation problem

# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 
# basic ARIMA model

k <- arima(resid, order=c(1,0,0))
str(k)
summary(k)
windows()
acf(k$residuals, lag.max = 4)
#creating data frame of previous error and new errors
View(data.frame(res= resid, newresid= k$residuals))
#predicting the next 4 error from our model
pred_res <- predict(k, ahead = 4)
str(pred_res)
#transforming and saving the errors
newtest_coke["forcasted error"] <- exp(pred_res$pred )
newtest_coke["Final Forcast"] <- newtest_coke$predicted+newtest_coke$`forcasted error`
View(newtest_coke)

#ARIMA MODEL, takes p,d and q input respectively for AR, I & MA

#I value can be 0 and 1

#Finding q value for arima 
acf(coke$Sales, lag.max = 12)
#q=9

#Finding p value for arima
pacf(coke$Sales, lag.max = 12)
# p= 1

#model without splitting
?arima

arima_k <- arima(coke$Sales,order = c(1,1,9))
pred_arima <- predict(arima_k, n.ahead = 4)
View(data.frame(pred_arima))
#using on splitted data
#training on training data
arimatrain <- arima(traincoke$Sales, order = c(1,1,9))
#predicting the test output
pred_test <- predict(arimatrain, n.ahead = 4)
#checking the RMSE for ARIMA model
rmsearima <- sqrt(mean((testcoke$Sales - pred_test$pred )^2))
#rmse = 174.58
#which is the lowest

