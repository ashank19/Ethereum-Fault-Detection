library(data.table)
library(kernlab)
library(caret)
library(ggplot2)

df=fread('DATA_FINAL.csv')

summary(df)

var(df$Feature1)

var(df$Feature2)

var(df$Feature3)

var(df$Feature4)

var(df$Feature5)

var(df$Feature6)

var(df$Feature7)

var(df$Feature8)

var(df$Feature9)

var(df$Feature10)

var(df$Feature11)

var(df$Feature12)

var(df$Feature13)

var(df$Feature14)

var(df$Feature15)

var(df$Feature16)

var(df$Feature17)

var(df$Feature18)

var(df$Feature19)

var(df$Feature20)

library(corrgram)

corrgram(df[,1:21], order = F,upper.panel=panel.cor,text.panel=panel.txt, main = "Correlation Plot")

k=df[,9:21]

k$Property1=df$Property1

corrgram(k[,1:12], order = F,upper.panel=panel.cor,text.panel=panel.txt, main = "Correlation Plot")

k=k[,-c('Feature12')]

corrgram(k[,1:12], order = F,upper.panel=panel.cor,text.panel=panel.txt, main = "Correlation Plot")

multiple_reg=lm(formula = Property1~.,data=k)

summary(multiple_reg)

multiple_reg=lm(formula = Property1~.,data=df)

summary(multiple_reg)

k=df[,c('Feature1','Feature3','Feature5','Feature13','Feature15','Feature17','Property1')]

corrgram(k[,1:6], order = F,upper.panel=panel.cor,text.panel=panel.txt, main = "Correlation Plot")

multiple_reg=lm(formula = Property1~.,data=k)

summary(multiple_reg)

multiple_reg=lm(formula = Property1~ Feature5+Feature13,data=k)

summary(multiple_reg)

training=createDataPartition(y=k$Property1,p=0.8,list=FALSE)

train_set=k[training, ]
test_set=k[-training, ]

reg = lm(formula = Property1 ~.,
         data = train_set)

a=summary(reg)

mean(a$residuals^2)

y_pred_train=predict(reg, newdata = train_set)

y_pred = predict(reg, newdata = test_set)

test_set$Property1

plot(y_pred, test_set$Property1)


sqrt(mean((train_set$Property1 - y_pred_train)^2))

sqrt(mean((test_set$Property1 - y_pred)^2))

(mean((train_set$Property1 - y_pred_train)^2))

(mean((test_set$Property1 - y_pred)^2))

(mean(abs(train_set$Property1 - y_pred_train)))

(mean(abs(test_set$Property1 - y_pred)))

install.packages('h2o')

library(h2o)

h2o.init(nthreads=-1)

data=k

data$Feature1=scale(k$Feature1)
data$Feature3=scale(k$Feature3)
data$Feature5=scale(k$Feature5)
data$Feature13=scale(k$Feature13)
data$Feature15=scale(k$Feature15)
data$Feature17=scale(k$Feature17)

data=as.h2o(data)

splits=h2o.splitFrame(data=data,ratios=0.8,seed=123)

train_set=splits[[1]]

test_set=splits[[2]]

model_h2o=h2o.deeplearning(training_frame=train_set,x=1:6,y=7,activation="Rectifier",hidden=c(5),seed=123,mini_batch_size=5,epochs=100)

h2o.performance(model_h2o,newdata=train_set)

h2o.r2(model_h2o)

h2o.performance(model_h2o,newdata=test_set)

h2o.r2(model_h2o)

library(usdm)

vif(k[,-7])

vif(df[,-1])

library(rpart)

library(MASS)

training=createDataPartition(y=k$Property1,p=0.8,list=FALSE)

train_set=k[training, ]
test_set=k[-training, ]

fit=rpart(Property1 ~ .,data=train_set)

# Predicting the output on training and testing set.
pred_DT_train=predict(fit,train_set[,-7])

pred_DT_test=predict(fit,test_set[,-7])

pred_DT_testdata=predict(fit,test_set[-7])

summary(fit)

# Writing rules to disk

write(capture.output(summary(fit)), "Rules.txt")

#  Average of actual data
avr_y_actual <- mean(test_set$Property1)

#  Total sum of squares
ss_total <- sum((test_set$Property1 - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((test_set$Property1 - pred_DT_testdata)^2)

#  R2 Score
r2 <- 1 - (ss_residuals / ss_total)

r2

library(DMwR)

regr.eval(train_set$Property1,pred_DT_train,stats='rmse')
# RMSE = 61

regr.eval(test_set$Property1,pred_DT_test,stats='rmse')
# RMSE = 66


library(randomForest)

# Training the model
fit_RF=randomForest(Property1 ~ .,data=train_set,importance=TRUE)


# Predicting the output on testing and training set.
pred_RF_train=predict(fit_RF,train_set[,-7])

pred_RF_test=predict(fit_RF,test_set[,-7])

# Evaluating the error metrics.
regr.eval(train_set$Property1,pred_RF_train,stats='rmse')

# RMSE = 54.88

regr.eval(test_set$Property1,pred_RF_test,stats='rmse')
# RMSE = 72.64

# Calculaitng the r squared value of this model

#  Average of actual data
avr_y_actual <- mean(test_set$Property1)

#  Total sum of squares
ss_total <- sum((test_set$Property1 - avr_y_actual)^2)

#  Regression sum of squares
ss_regression <- sum((pred_RF_test - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((test_set$Property1 - pred_RF_test)^2)

#  R2 Score
r2 <- 1 - ss_residuals / ss_total

library(e1071)


#Regression with SVM
fit_svm = svm(Property1 ~ .,data=train_set)

#Predict using SVM regression
pred_svm_train = predict(fit_svm, train_set[,-7])

pred_svm_test=predict(fit_svm, test_set[,-7])

pred_svm_testdata=predict(fit_svm,X1)

test$fare_amount_SVM=pred_svm_testdata

regr.eval(train_set$Property1,pred_svm_train,stats='rmse')

# RMSE = 62.88

regr.eval(test_set$Property1,pred_svm_test,stats='rmse')

# RMSE = 73.28

# Calculaitng the r squared value of this model

#  Average of actual data
avr_y_actual <- mean(test_set$Property1)

#  Total sum of squares
ss_total <- sum((test_set$Property1 - avr_y_actual)^2)

#  Regression sum of squares
ss_regression <- sum((pred_svm_test - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((test_set$Property1 - pred_svm_test)^2)

#  R2 Score
r2 <- 1 - ss_residuals / ss_total

library(gbm)

#Develop Model on training data
fit_XGB = gbm(Property1~., data = train_set, n.trees = 1000, interaction.depth = 2)

#Lets predict for training data
pred_XGB_train = predict(fit_XGB, train_set[,-7], n.trees = 1000)

#Lets predict for testing data
pred_XGB_test = predict(fit_XGB, test_set[,-7], n.trees = 1000)


regr.eval(train_set$Property1,pred_XGB_train,stats='rmse')

# RMSE = 54

regr.eval(test_set$Property1,pred_XGB_test,stats='rmse')

# RMSE = 77.00
#  Average of actual data
avr_y_actual <- mean(test_set$Property1)

#  Total sum of squares
ss_total <- sum((test_set$Property1 - avr_y_actual)^2)

#  Regression sum of squares
ss_regression <- sum((pred_XGB_test - avr_y_actual)^2)

#  Residual sum of squares
ss_residuals <- sum((test_set$Property1 - pred_XGB_test)^2)

#  R2 Score
r2 <- 1 - ss_residuals / ss_total

data=k

data$Feature7=scale(k$Feature7)
data$Feature12=scale(k$Feature12)
data$Feature5=scale(k$Feature5)
data$Feature15=scale(k$Feature15)
data$Feature6=scale(k$Feature6)

data=as.h2o(data)

splits=h2o.splitFrame(data=data,ratios=0.6,seed=123)

train_set=splits[[1]]

test_set=splits[[2]]

#Two hidden layers
model_h2o=h2o.deeplearning(training_frame=train_set,x=1:5,y=6,activation="Rectifier",hidden=c(5),seed=123,mini_batch_size=10,epochs=100000)

h2o.performance(model_h2o,newdata=train_set)

h2o.r2(model_h2o)

h2o.performance(model_h2o,newdata=test_set)

h2o.r2(model_h2o)

#One hidden layer 10 nodes
model_h2o=h2o.deeplearning(training_frame=train_set,x=1:6,y=7,activation="Rectifier",hidden=c(10),seed=123,mini_batch_size=3,epochs=100)

h2o.performance(model_h2o,newdata=train_set)

h2o.r2(model_h2o)

h2o.performance(model_h2o,newdata=test_set)

h2o.r2(model_h2o)

#Two hidden layer 10 nodes each
model_h2o=h2o.deeplearning(training_frame=train_set,x=1:6,y=7,activation="Rectifier",hidden=c(10,10),seed=123,mini_batch_size=3,epochs=100)

h2o.performance(model_h2o,newdata=train_set)

h2o.r2(model_h2o)

h2o.performance(model_h2o,newdata=test_set)

h2o.r2(model_h2o)

#3 hidden layer 10 nodes each
model_h2o=h2o.deeplearning(training_frame=train_set,x=1:6,y=7,activation="Rectifier",hidden=c(10,10,10),seed=123,mini_batch_size=3,epochs=100)

h2o.performance(model_h2o,newdata=train_set)

h2o.r2(model_h2o)

h2o.performance(model_h2o,newdata=test_set)

h2o.r2(model_h2o)

#3 hidden layer nodes each
model_h2o=h2o.deeplearning(training_frame=train_set,x=1:6,y=7,activation="Rectifier",hidden=c(10,10,5),seed=123,mini_batch_size=3,epochs=100)

h2o.performance(model_h2o,newdata=train_set)

h2o.r2(model_h2o)

h2o.performance(model_h2o,newdata=test_set)

h2o.r2(model_h2o)

#4 hidden layer nodes each
model_h2o=h2o.deeplearning(training_frame=train_set,x=1:6,y=7,activation="Rectifier",hidden=c(10,10,10,5),seed=123,mini_batch_size=3,epochs=100)

h2o.performance(model_h2o,newdata=train_set)

h2o.r2(model_h2o)

h2o.performance(model_h2o,newdata=test_set)

h2o.r2(model_h2o)

#4 hidden layer nodes each
model_h2o=h2o.deeplearning(training_frame=train_set,x=1:6,y=7,activation="Rectifier",hidden=c(20,20,10,5),seed=123,mini_batch_size=3,epochs=100)

h2o.performance(model_h2o,newdata=train_set)

h2o.r2(model_h2o)

h2o.performance(model_h2o,newdata=test_set)

h2o.r2(model_h2o)

#4 hidden layer nodes each
model_h2o=h2o.deeplearning(training_frame=train_set,x=1:6,y=7,activation="Rectifier",hidden=c(20,20,20,5),seed=123,mini_batch_size=3,epochs=100)

h2o.performance(model_h2o,newdata=train_set)

h2o.r2(model_h2o)

h2o.performance(model_h2o,newdata=test_set)

h2o.r2(model_h2o)

library(caTools)

RFE=rfe(train_set[,1:6],train_set$Property1,sizes=c(1:6),rfeControl = rfeControl(functions=rfFuncs,method='cv',number=5))
#It uses 10 fld cross validation using random forest

RFE

ggplot(RFE)

RFE=rfe(df[,2:21],df$Property1,sizes=c(1:20),rfeControl = rfeControl(functions=rfFuncs,method='cv',number=20))
#It uses 10 fld cross validation using random forest

RFE

set.seed(123)
ggplot(RFE)

REF= rfe(df[,2:21],df$Property1,
         sizes = c(2:21),rfeControl = rfeControl
         (functions=rfFuncs,method ="rf",number=2))

REF

ggplot(REF)


k=df[,c('Feature7', 'Feature6', 'Feature12', 'Feature5', 'Feature15','Property1')]

training=createDataPartition(y=k$Property1,p=0.6,list=FALSE)

train_set=k[training, ]
test_set=k[-training, ]

reg = lm(formula = Property1 ~.,
         data = train_set)

a=summary(reg)

mean(a$residuals^2)

y_pred_train=predict(reg, newdata = train_set)

y_pred = predict(reg, newdata = test_set)

test_set$Property1

plot(y_pred, test_set$Property1)

p=test_set
p$pred=y_pred

ggplot(data=p)+geom_point(aes(x=y_pred,y=test_set$Property1),color='red')



sqrt(mean((train_set$Property1 - y_pred_train)^2))

sqrt(mean((test_set$Property1 - y_pred)^2))

(mean((train_set$Property1 - y_pred_train)^2))

(mean((test_set$Property1 - y_pred)^2))

(mean(abs(train_set$Property1 - y_pred_train)))

(mean(abs(test_set$Property1 - y_pred)))

