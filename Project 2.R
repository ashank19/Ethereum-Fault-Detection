install.packages('DMwR')
install.packages('pROC')
library(kernlab)
library(DiscriMiner)
library(mlbench)
library(ggplot2)
library(caret)
library(dplyr)
library(GGally)
library(lattice)
library(caret)
library(reshape)
library(mice)
library(data.table)
library(DMwR)
library(pROC)
data_in=read.csv("/Users/aekanshgupta/Desktop/Sakib/Datasets/median.csv")
data=data_in[, -1]
str(data)
df=subset(data,select= -c(total_transactions_including_tnx_to_create_contract,total_ether_received,ERC20_total_Ether_received,ERC20_uniq_rec_contract_addr,ERC20_max_val_rec,ERC20_uniq_sent_token_name,Total_ERC20_tnxs))
flag=df$FLAG
df=scale(df[2:24])
fin=cbind(flag,df)
fin[,1] <- fin[,1] - 1
x <- as.data.frame(fin)
x$flag = as.factor(x$flag)
old <- table(x$flag)
#prop.table(table(x$temp))
newdata <- SMOTE(flag~., x, perc.over = 100)
prop.table(table(newdata$flag))
newfd <- table(newdata$flag)
training_r= createDataPartition(y= newdata$flag, p=0.80, list=FALSE)
train_set_r= newdata[training_r, ]
test_set_r= newdata[-training_r, ]
#confusionMatrix(predict(model_cart, newdata= train_set), train_set$type)
#confusionMatrix(predict(model_cart, newdata= test_set), test_set$type)
#Bagging
model_bagging_r= train(data=train_set_r, flag~., method="treebag")
confusionMatrix(predict(model_bagging_r, newdata= train_set_r), train_set_r$flag)
confusionMatrix(predict(model_bagging_r, newdata= test_set_r), test_set_r$flag)
#Random_FOREST
model_forest= train(data=train_set_r, temp~., method="rf", prox=TRUE)
confusionMatrix(predict(model_forest, newdata= test_set_r), test_set_r$temp)
#Boosting
model_boosting= train(data=train_set_r, flag~., method="gbm", verbose=FALSE)
confusionMatrix(predict(model_boosting, newdata= train_set_r), train_set_r$flag)
confusionMatrix(predict(model_boosting, newdata= test_set_r), test_set_r$flag)
#CART
model_cart=train(data=train_set_r, temp~., method="rpart")
confusionMatrix(predict(model_cart, newdata= test_set_r), test_set_r$temp)
###################
f=predict(model_boosting,newdata=test_set_r,type = "prob")
f_roc=roc(test_set_r$flag, f[,"0"])
f_roc
plot(f_roc)







