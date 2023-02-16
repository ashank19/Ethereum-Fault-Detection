library(data.table)
library(kernlab)

library(caret)
library(ggplot2)
df=fread('transaction_dataset.csv')

colnames(df)

summary(df)

boxplot(df$`ERC20 avg val sent contract`)

boxplot(df$`ERC20 max val sent contract`)

boxplot(df$`ERC20 min val sent contract`)

boxplot(df$`ERC20 avg val sent`)

boxplot(df$`ERC20 max val sent`)

boxplot(df$`ERC20 min val sent`)

boxplot(df$`ERC20 max val rec`)

boxplot(df$`ERC20 avg time between contract tnx`)

boxplot(df$`ERC20 avg time between rec 2 tnx`)

boxplot(df$`ERC20 avg time between rec tnx`)

boxplot(df$`ERC20 avg time between sent tnx`)

boxplot(df$`ERC20 uniq rec contract addr`)

boxplot(df$`ERC20 uniq sent addr.1`)

boxplot(df$`ERC20 uniq sent addr`)

boxplot(df$`ERC20 total Ether received`)

boxplot(df$`Total ERC20 tnxs`)

boxplot(df$`total ether balance`)

boxplot(df$`total ether sent contracts`)

boxplot(df$`total ether received`)

boxplot(df$`total Ether sent`)

boxplot(df$`total transactions (including tnx to create contract`)

boxplot(df$`avg value sent to contract`)

boxplot(df$`max val sent to contract`)

boxplot(df$`min val sent to contract`)

boxplot(df$`min value sent to contract`)

boxplot(df$`min value sent to contract`)

boxplot(df$`avg val sent`)

boxplot(df$`max val sent`)

boxplot(df$`min val sent`)

boxplot(df$`avg val received`)

boxplot(df$`max value received`)

boxplot(df$`min value received`)

boxplot(df$`Unique Sent To Addresses`)

boxplot(df$`Unique Received From Addresses`)

boxplot(df$`Number of Created Contracts`)

boxplot(df$`Received Tnx`)

boxplot(df$`Sent tnx`)

boxplot(df$`Time Diff between first and last (Mins)`)

boxplot(df$`Avg min between received tnx`)

boxplot(df$`Avg min between sent tnx`)

a=colnames(df)

for (i in 1:length(a))
{
  a[i]=gsub(" ", "_", a[i])
}

a[7]="Time_Diff_between_first_and_last_Mins"
a[22]="total_transactions_including_tnx_to_create_contract"

colnames(df)=a

df1=df[,-c('ERC20_min_val_sent_contract','ERC20_max_val_sent_contract',
        'ERC20_avg_val_sent_contract','ERC20_avg_val_sent','ERC20_min_val_sent',
        'ERC20_max_val_sent','ERC20_avg_time_between_contract_tnx',
        'ERC20_avg_time_between_rec_2_tnx','ERC20_avg_time_between_rec_tnx',
        'ERC20_avg_time_between_sent_tnx','ERC20_uniq_sent_addr.1','total_ether_sent_contracts',
        'avg_value_sent_to_contract','max_val_sent_to_contract','min_value_sent_to_contract')]



data=df1[,4:34]

summary(data)

data$ERC20_uniq_rec_token_name[is.na(data$ERC20_uniq_rec_token_name)]=median(data$ERC20_uniq_rec_token_name,na.rm=T)

data$ERC20_uniq_sent_token_name[is.na(data$ERC20_uniq_sent_token_name)]=median(data$ERC20_uniq_sent_token_name,na.rm=T)

data$ERC20_avg_val_rec[is.na(data$ERC20_avg_val_rec)]=median(data$ERC20_avg_val_rec,na.rm=T)

data$ERC20_max_val_rec[is.na(data$ERC20_max_val_rec)]=median(data$ERC20_max_val_rec,na.rm=T)

data$ERC20_min_val_rec[is.na(data$ERC20_min_val_rec)]=median(data$ERC20_min_val_rec,na.rm=T)

data$ERC20_uniq_rec_contract_addr[is.na(data$ERC20_uniq_rec_contract_addr)]=median(data$ERC20_uniq_rec_contract_addr,na.rm=T)

data$ERC20_uniq_rec_addr[is.na(data$ERC20_uniq_rec_addr)]=median(data$ERC20_uniq_rec_addr,na.rm=T)

data$ERC20_uniq_sent_addr[is.na(data$ERC20_uniq_sent_addr)]=median(data$ERC20_uniq_sent_addr,na.rm=T)

data$ERC20_total_Ether_sent_contract[is.na(data$ERC20_total_Ether_sent_contract)]=median(data$ERC20_total_Ether_sent_contract,na.rm=T)

data$ERC20_total_ether_sent[is.na(data$ERC20_total_ether_sent)]=median(data$ERC20_total_ether_sent,na.rm=T)

data$ERC20_total_Ether_received[is.na(data$ERC20_total_Ether_received)]=median(data$ERC20_total_Ether_received,na.rm=T)

data$Total_ERC20_tnxs[is.na(data$Total_ERC20_tnxs)]=median(data$Total_ERC20_tnxs,na.rm=T)

summary(data)

write.csv(data,file='median imputed and cleaned.csv')

install.packages('DMwR')

install.packages('xts')

install.packages('quantmod')

install.packages('zoo')

install.packages('ROCR')

install.packages('pROC')

library(zoo)
library(xts)
library(ROCR)
library(quantmod)
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

data=data[,-c('total_transactions_including_tnx_to_create_contract','total_ether_received','ERC20_total_Ether_received','ERC20_uniq_rec_contract_addr','ERC20_max_val_rec','ERC20_uniq_sent_token_name','Total_ERC20_tnxs')]

flag=data$FLAG

data=scale(data[,2:24])
fin=cbind(flag,data)
x<-as.data.frame(fin)
x$flag=as.factor(x$flag)
old<-table(x$flag) # To check how many zeros and ones

newdata <- SMOTE(flag~., x, perc.over = 100)
prop.table(table(newdata$flag))

library(corrgram)

newdata=newdata[,-10]

corrgram(newdata[,2:23], order = F,upper.panel=panel.cor,text.panel=panel.txt, main = "Correlation Plot")

newdata=newdata[,-13]

corrgram(newdata[,2:22], order = F,upper.panel=panel.cor,text.panel=panel.txt, main = "Correlation Plot")

newfd <- table(newdata$flag)
training_r= createDataPartition(y= newdata$flag, p=0.80, list=FALSE)
train_set_r= newdata[training_r, ]
test_set_r= newdata[-training_r, ]

#Bagging
model_bagging_r= train(data=train_set_r, flag~., method="treebag")
confusionMatrix(predict(model_bagging_r, newdata= train_set_r), train_set_r$flag)
confusionMatrix(predict(model_bagging_r, newdata= test_set_r), test_set_r$flag)
#Random_FOREST
model_forest= train(data=train_set_r, flag~., method="rf", prox=TRUE)
confusionMatrix(predict(model_forest, newdata= test_set_r), test_set_r$flag)
#Boosting
model_boosting= train(data=train_set_r, flag~., method="gbm", verbose=FALSE)
confusionMatrix(predict(model_boosting, newdata= train_set_r), train_set_r$flag)
confusionMatrix(predict(model_boosting, newdata= test_set_r), test_set_r$flag)
#CART
model_cart=train(data=train_set_r, flag~., method="rpart")
confusionMatrix(predict(model_cart, newdata= test_set_r), test_set_r$flag)
###################
f=predict(model_boosting,newdata=test_set_r,type = "prob")
f_roc=roc(test_set_r$flag, f[,"0"])
f_roc
plot(f_roc)

b=colnames(newdata[,2:24])

install.packages('corrgram')

library(corrgram)

corrgram(newdata[,2:24], order = F,upper.panel=panel.cor,text.panel=panel.txt, main = "Correlation Plot")
