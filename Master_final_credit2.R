#Separated Primary and Secondary


setwd("C:/Users/jhkjhk/Desktop/New folder/Spring 2023/Master Final/credit")

library(tidyverse)
library(e1071)
library(ICS)
library(mvtnorm)
library(MASS)
library(eeptools)
library(randomForest)
library(xgboost)
library(class)
library(ROCR)
library(pROC)
library(caret)
library(rfvimptest)
library(PRROC)
library(MLmetrics)
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Data Cleaning


train <- read_csv("train.csv")

dim(train)
head(train)


colnames(train) # 1,5,6,7,8,11,12,13,14,15,16,17,18,19,21,

# -----------------------------------------------------------------------------------------------


corrplot::corrplot(cor(train[,22:33])) # correlation between primary accounts and between secondary accounts
# no correlation between primary and secondary 


#Let's combine primary and secondary account
train[,22:27]
train[,28:33]
#column names are same
colnames(train[,22:27])
colnames(train[,28:33])

summary(train[,22:27]) 
#figuring out why col # = 25 has neg values

uniqid1  <- which(train[,25] <0  | train[,31] <0)# removing them. Logically cannot be below zero. Could have been a mistake w/ -
# Yet not sure, hence removing them. # need to remove such observations for secondary loans as well
train2 <- train[-uniqid1,] # After 

#combine col= 34,35

cor(train[,22:33])

#Combining all primary and secondary loans


train3 <- train2[,1:21] 
train3<- train3[,-c(1,5,6,7,8,11,12,13,14,15,16,17,18,19,21)]

#train4 <- cbind(train3,loan_amt,install_amt,train2$CREDIT.HISTORY.LENGTH) ######
train4 <- cbind(train3,train2[,22:33],train2$CREDIT.HISTORY.LENGTH) ####
train4 <- na.omit(train4);train2 <- na.omit(train2) ;train <- na.omit(train)

#colnames(train4)[14] <- "credit.history.length"
colnames(train4)[19] <- "credit.history.length" ####

creditlength <- cbind(as.numeric(str_split_fixed(train4$credit.history.length,"yrs",2)[,1]),
                      as.numeric(str_split_fixed(str_split_fixed(train4$credit.history.length,"yrs",2)[,2],"mon",2)[,1]))

train4[,which(colnames(train4) =="credit.history.length")] <- creditlength[,1]*12+creditlength[,2] # credit histroy in months

# Converting date of birth to age

dates <- str_split_fixed(train4$Date.of.Birth,"-",3)
# Supposition: 00 indicates 2000
dates[(as.numeric(dates[,3]) <40),3] <- paste(20,(dates[(as.numeric(dates[,3]) <40),3]),sep="") 
dates[(as.numeric(dates[,3]) >=40),3] <- paste(19,(dates[(as.numeric(dates[,3]) >=40),3]),sep="")

train4$Date.of.Birth <-paste(paste(dates[,3],dates[,2],sep="-"),dates[,1],sep="-")
train4$Date.of.Birth <- as.Date(train4$Date.of.Birth)
train4$Date.of.Birth


train4$Date.of.Birth <-floor(age_calc(train4$Date.of.Birth,as.Date("2019-04-14"),unit="years"))
colnames(train4)[which(colnames(train4)=="Date.of.Birth")] <- "age"

unlist(lapply(1:ncol(train4), function(x) is.numeric(train4[,x])))

#x <- c(1,2,3,4,6:14)
#train4[,x] <- scale(train4[,x]) ##############

x <- c(1:4,6:19)
train4[,x] <- scale(train4[,x])#################

#The Freedman-Diaconis rule = â=2ÃIQRÃðâ1/3



#### Modelling
set.seed(45454)
index <- sample(1:nrow(train4), nrow(train4)*0.8)
train4 <- cbind(train4,train2$loan_default)
#colnames(train4)[15] <- "default" ###
colnames(train4)[20] <- "default" #####
train4 <- as.data.frame(train4)
train4$default <- as.factor(train4$default)

#car::leveneTest(as.formula(paste(colnames(train4)[2],"~ default")),data=train4[index,])
#as.formula(paste(colnames(train4)[2],"~ default"))


train4 <- na.omit(train4) # removing all NA 224997 x 15
# Checking for multivariate normality




train[,2:4]

mvnorm.kur.test(na.omit(train4[train4$default==1,c(1:4,6:14)]))
mvnorm.kur.test(na.omit(train4[train4$default==0,c(1:4,6:14)]))
mvnorm.skew.test(na.omit(train4[train4$default==1,c(1:4,6:14)]))
mvnorm.skew.test(na.omit(train4[train4$default==0,c(1:4,6:14)]))
#LDA
#lda.m <- lda(default~., data= train4[index,c(1:4,6:13,14,15)])
lda.m <- lda(default~.,data=train4[index,-5])

lda.m
pred_l <- predict(lda.m, newdata=train4[-index,])
mean(pred_l$class == train4[-index,]$default) # Accuracy 78.17%
rates.l <- prediction(pred_l$posterior[,2],train4$default[-index])
auc.m<-ROCR::performance(rates.l, measure = "auc")
auc.m@y.values #0.6063219
PRAUC(pred_l$posterior[,2],train4$default[-index]) #0.2832568


# Equal variance test
table(train4$default) #1=48897

index_0 <- sample(which(train$loan_default==0), 48897)
index_bal <-c(index_0,which(train$loan_default==1))
pvals <- c()

a <- var.test(as.formula(paste(colnames(train4)[2],"~ default")), data=train4[index_bal,])$p.value
# equal variance test is rejected for a lot of them.
sapply(x, function(y) {var.test(as.formula(paste(colnames(train4)[y],"~ default")), data=train4[index_bal,])$p.value}) 

#qda.m <- qda(default~., data= train4[index,c(1:4,6:15)])
qda.m <- qda(default~., data= train4[index,-5])
pred_q <- predict(qda.m, newdata=train4[-index,])
rates.q <- prediction(pred_q$posterior[,2],train4$default[-index])
auc.q<-ROCR::performance(rates.q, measure = "auc")
auc.q@y.values #0.5933839
PRAUC(pred_q$posterior[,2],train4$default[-index]) #0.2709822



corrplot::corrplot(cor(train4[,x])) # high correlation Sanction Amt. & Disbursed.Amt / asset_cost & ltv


# Logistic Regression

log_reg <- glm(default~.,data=train4[index,], family="binomial")
summary(log_reg)
pred_log <- predict(log_reg,newdata=train4[-index,],type="response")
rates.log <- prediction(pred_log,train4$default[-index])
auc.log<-ROCR::performance(rates.log, measure = "auc")
auc.log@y.values #0.6083304
PRAUC(pred_log,train4$default[-index]) #0.2845929

# Random Forest
rf <- randomForest(default~.,data=train4[index,],mtry=sqrt(ncol(train4)-1), maxnodes=500,importance=T)
importance(rf)
varImpPlot(rf)
rf_pred <- predict(rf,newdata=train4[-index,], type="prob")
rates.rf <- prediction(rf_pred[,2],train4$default[-index])
auc.rf<-ROCR::performance(rates.rf, measure = "auc")
auc.rf@y.values #0.5188709
PRAUC(rf_pred[,2],train4$default[-index]) #0.244587

# XGBoost

dtrain <- xgb.DMatrix(data = model.matrix(default~.-1,data=train4[index,]), label=as.numeric(train4$default[index])-1)
dtest <- xgb.DMatrix(data = model.matrix(default~.-1,data=train4[-index,]), label=as.numeric(train4$default[-index])-1)
xgb <- xgboost(dtrain,max.depth=10,objective= "binary:logistic", nrounds = 10)
mean(ifelse(predict(xgb,newdata=dtest)>0.5,1,0)==train4$default[-index]) #78.21%
xgb_pred <- predict(xgb,newdata=dtest, type="prob")
rates.xgb <- prediction(xgb_pred,train4$default[-index])
auc.xgb<-ROCR::performance(rates.xgb, measure = "auc")
auc.xgb@y.values #0.6207343
PRAUC(xgb_pred,train4$default[-index]) #0.2916367

xgb.ggplot.importance(importance_matrix=xgb.importance(feature_names=colnames(dtrain),model=xgb))


xgb2 <- xgboost(dtrain,max.depth=50,objective= "binary:logistic", nrounds = 10,
                eval_metric="auc")
xgb2_pred <- predict(xgb2,newdata = dtest,type="prob")
xgb2_auc <- sapply(threshold, function(x) { auc(train4$default[-index], ifelse(xgb2_pred>=x,1,0))})
xgb2_auc <- cbind(threshold,xgb2_auc)
xgb2_auc

#lambda tuning
lambda <- seq(0,50,length.out=30)

xgb3 <- function(x){
  xgb_result <- xgboost(dtrain,max.depth=50,objective= "binary:logistic", nrounds = 5,
                        params= list(lambda=x),
                        eval_metric="auc")
  xgb_pred <- predict(xgb_result, newdata=dtest,type="prob")
  rates.xgb <- prediction(xgb_pred,train4$default[-index])
  auc.xgb<-ROCR::performance(rates.xgb, measure = "auc")
  auc.xgb@y.values #0.6091
}
lambda <- cbind(lambda,sapply(lambda, function(z){
  xgb3(z)
}))

#alpha tuning

alpha = seq(0,50,length.out=30)
xgb3 <- function(x){
  xgb_result <- xgboost(dtrain,max.depth=50,objective= "binary:logistic", nrounds = 5,
                        params= list(alpha=x),
                        eval_metric="auc")
  xgb_pred <- predict(xgb_result, newdata=dtest,type="prob")
  xgb_auc <- sapply(threshold, function(x) { auc(train4$default[-index], ifelse(xgb2_pred>=x,1,0))})
  xgb_auc <- cbind(threshold,xgb_auc)
  xgb_auc.max <- xgb_auc[which.max(xgb_auc[,2]),]
  return(xgb_auc.max)
}
sapply(alpha, function(z){
  xgb3(z)
})

#eta tuning
eta= seq(0,50,length.out=30)
xgb3 <- function(x){
  xgb_result <- xgboost(dtrain,max.depth=50,objective= "binary:logistic", nrounds = 5,
                        params= list(eta=x),
                        eval_metric="auc")
  xgb_pred <- predict(xgb_result, newdata=dtest,type="prob")
  xgb_auc <- sapply(threshold, function(x) { auc(train4$default[-index], ifelse(xgb2_pred>=x,1,0))})
  xgb_auc <- cbind(threshold,xgb_auc)
  xgb_auc.max <- xgb_auc[which.max(xgb_auc[,2]),]
  return(xgb_auc.max)
}
sapply(eta, function(z){
  xgb3(z)
})