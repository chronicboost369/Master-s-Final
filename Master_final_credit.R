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
library(carData)
library("effects")
library(car)
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

loan_amt <- train2[,22:27] + train2[,28:33] # of loan balance and accounts

colnames(loan_amt) <- c("No.Accts", "Active.Accts", "Overdue.Accts", "Current.Balance", "Sanctioned.Amt", "Disbursed.Amt")

install_amt <- train2[,34] + train2[,35]
colnames(install_amt) <- "Install.Amt"
train3 <- train2[,1:21] 
train3<- train3[,-c(1,5,6,7,8,11,12,13,14,15,16,17,18,19,21)]

train4 <- cbind(train3,loan_amt,install_amt,train2$CREDIT.HISTORY.LENGTH) ######
train4 <- na.omit(train4);train2 <- na.omit(train2) ;train <- na.omit(train)

colnames(train4)[14] <- "credit.history.length"


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



train4$Date.of.Birth <-floor(age_calc(train4$Date.of.Birth,as.Date("2019-04-14"),unit="years"))
colnames(train4)[which(colnames(train4)=="Date.of.Birth")] <- "age"

unlist(lapply(1:ncol(train4), function(x) is.numeric(train4[,x])))

x <- c(1,2,3,4,6:14)

train_a <- train4
asdta <- cbind(train4,train2$loan_default)
colnames(asdta)[15] <- "default"
asdta<- asdta[,-2]
#boxTidwell(default~.,data=asdta[,-4]+0.1, type="numeric")

?boxTidwell

train4[,x] <- scale(train4[,x]) ##############


#The Freedman-Diaconis rule = ℎ=2×IQR×𝑛−1/3


####################################################################################################################
#More dropping

for(i in x){
  hist(train4[,i], main=colnames(train4)[i])
}

table(train4[,7])

#### Modelling
set.seed(45454)
index <- sample(1:nrow(train4), nrow(train4)*0.8)
train4 <- cbind(train4,train2$loan_default)
colnames(train4)[15] <- "default" ###
train4 <- as.data.frame(train4)
train4$default <- as.factor(train4$default)
train4 <- train4[,-which(colnames(train4)=="asset_cost")]

#car::leveneTest(as.formula(paste(colnames(train4)[2],"~ default")),data=train4[index,])
#as.formula(paste(colnames(train4)[2],"~ default"))


train4 <- na.omit(train4) # removing all NA 224997 x 15
# Checking for multivariate normality

#mvnorm.kur.test(na.omit(train4[train4$default==1,c(1:4,6:14)]))
#mvnorm.kur.test(na.omit(train4[train4$default==0,c(1:4,6:14)]))
#mvnorm.skew.test(na.omit(train4[train4$default==1,c(1:4,6:14)]))
#mvnorm.skew.test(na.omit(train4[train4$default==0,c(1:4,6:14)]))
#LDA
lda.m <- lda(default~., data= train4[index,])


lda.m
pred_l <- predict(lda.m, newdata=train4[-index,])
mean(pred_l$class == train4[-index,]$default) # Accuracy 78.17%


rates.l <- prediction(pred_l$posterior[,2],train4$default[-index])
auc.m<-ROCR::performance(rates.l, measure = "auc")
auc.m@y.values #0.606339

x <- c(1:4,6:14)



hist(log(train$disbursed_amount))
hist((train$ltv),main="Histogram of Ltv", xlab='Ltv')
hist(train$disbursed_amount)

summary(train$disbursed_amount)

ggplot(train,aes(x=disbursed_amount))+geom_histogram(binwidth = 2*IQR(train$disbursed_amount)*nrow(train)^(-1/3))
ggplot(train,aes(x=disbursed_amount))+geom_histogram(bins = 250)+theme_bw()+xlab("Loan Amount")

hist(train$asset_cost)

table(train$PRI.OVERDUE.ACCTS)

# Equal variance test
#table(train4$default) #1=48897

#index_0 <- sample(which(train$loan_default==0), 48897)
#index_bal <-c(index_0,which(train$loan_default==1))
#pvals <- c()

#a <- var.test(as.formula(paste(colnames(train4)[2],"~ default")), data=train4[index_bal,])$p.value
# equal variance test is rejected for a lot of them.
#sapply(x, function(y) {var.test(as.formula(paste(colnames(train4)[y],"~ default")), data=train4[index_bal,])$p.value}) 

#qda.m <- qda(default~., data= train4[index,c(1:4,6:15)])
#pred_q <- predict(qda.m, newdata=train4[-index,])
#pred_q$posterior[,1]
#mean(pred_q$class == train4[-index,]$default) # Accuracy 77.54%

#rates.q <- prediction(pred_q$posterior[,2],train4$default[-index])
#auc.q<-ROCR::performance(rates.q, measure = "auc")
#auc.q@y.values #0.5972699



#corrplot::corrplot(cor(train4[,x])) # high correlation Sanction Amt. & Disbursed.Amt / asset_cost & ltv


# Logistic Regression

log_reg <- glm(default~.,data=train4[index,], family="binomial")
summary(log_reg)
pred_log <- predict(log_reg,newdata=train4[-index,],type="response")
mean(ifelse(pred_log>=0.5,1,0) == train4$default[-index]) #78.17%

Expected Loss
results_log <- data.frame("predicted"=ifelse(pred_log>=0.5,1,0),"real"=train4$default[-index])

#index3 <- which(results_log$predicted==0& results_log$real==1)

#sum(train$disbursed_amount[index3]) #$522 mil
#sum(train$disbursed_amount) #12.2Bilion


1-pchisq(log_reg$null.deviance-log_reg$deviance,13)


cutoffs <- seq(0.6,0.9,by=0.1)

sapply(1:length(cutoffs), function(x) {mean(ifelse(pred_log>=cutoffs[x],1,0) == train4$default[-index])}) #78.17%)

train5 <- train4[,-2]
train5 <- cbind(train5,poly(train4$ltv,5))
colnames(train5)[14:18] <- c("ltv_1", "ltv_2", "ltv_3", "ltv_4","ltv5")

log_reg2 <- glm(default~.,data=train4[index,c(2,5,8,14)], family="binomial")
summary(log_reg2)
pred_log2 <- predict(log_reg2,newdata=train4[-index,],type="response")
mean(ifelse(pred_log2>=0.5,1,0) == train4$default[-index]) #78.17%


colnames(train4)
log_reg2 <- glm(default~ + .,data=train5[index,3,5,9], family="binomial")


sort(abs(coefficients(log_reg)),decreasing = T)

sapply(1:length(cutoffs), function(x) {mean(ifelse(pred_log2>=cutoffs[x],1,0) == train4$default[-index])}) #78.17%)

set.seed(12345)
ras <- sample((1:nrow(train4))[-index],3000)


plot(x=train4[ras,"ltv"], y=pred_log[ras], col=train4$default[ras])

plot(allEffects(log_reg))
?allEffects

rates.log <- prediction(pred_log,train4$default[-index])
plot(rates.log)






barplot(abs(coef(log_reg))[-1][order(abs(coef(log_reg))[-1],decreasing = T)],
        names.arg=c("ltv","Self Emp.", "Overdue.Act", "Disbur.Amt", "Sanct.Amt", "Loan Amt", "Current Bal", "Age", "Credit.Hist",
                    "Active.Act", "Credit Score", "No.Accts","Install.Amt"),cex.names=0.95,las=2,
        main="Variable Importance", ylab="Absolute Value of Coef")




names(coef(log_reg))[-1][order(abs(coef(log_reg))[-1],decreasing = T)]



auc.log<-ROCR::performance(rates.log,measure="tpr", x.measure="fpr")
plot(auc.log, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")
auc.log<-ROCR::performance(rates.log,measure="auc")
auc.log@y.values



# Random Forest
#rf <- randomForest(default~.,data=train4[index,],mtry=sqrt(ncol(train4)-1), maxnodes=500,importance=T)
#importance(rf)
#varImpPlot(rf)
#rf_pred <- predict(rf,newdata=train4[-index,], type="prob")
#mean(rf_pred == train4$default[-index]) # 78.21%

#rates.rf <- prediction(rf_pred[,2],train4$default[-index])
#auc.rf<-ROCR::performance(rates.rf, measure = "auc")
#auc.rf@y.values #0.52

# XGBoost

dtrain <- xgb.DMatrix(data = model.matrix(default~.-1,data=train4[index,]), label=as.numeric(train4$default[index])-1)
dtest <- xgb.DMatrix(data = model.matrix(default~.-1,data=train4[-index,]), label=as.numeric(train4$default[-index])-1)
xgb <- xgboost(dtrain,max.depth=3,objective= "binary:logistic", nrounds = 50)
mean(ifelse(predict(xgb,newdata=dtest)>0.5,1,0)==train4$default[-index]) #78.21%
xgb_pred <- predict(xgb,newdata = dtest,type="prob")
rates.xgb <- prediction(xgb_pred,train4$default[-index])
auc.xgb<-ROCR::performance(rates.xgb, measure = "auc")
auc.xgb@y.values #0.6091

xgb.plot.importance(importance_matrix=xgb.importance(feature_names=colnames(dtrain),model=xgb), xlab="Information Gain")


#Tuning
xgb_grid_1 <- expand.grid(
  nrounds= 30,
  eta=seq(1,10,length.out=6),
  lambda = seq(1,10,length.out=6),
  alpha = 0
)
xgb_grid_1 = expand.grid(
  nrounds = 200,
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1,
  colsample_bytree = c(0.3,0.4,0.5),
  subsample = 0.8,
  min_child_weight=1
)

xgb_trcontrol <- trainControl(
  method="cv",
  number = 3,
  verboseIter = TRUE,
  returnData=FALSE,
  returnResamp = "all",
  allowParallel = TRUE,
  classProbs = T,
  summaryFunction = twoClassSummary
  
)

#xgb_train_1 <- train(
  #x =  model.matrix(default~.-1,data=train4[index,]),
  #y = make.names(train4$default[index]),
  #trControl = xgb_trcontrol,
  #tuneGrid = xgb_grid_1,
  #metric = "ROC",
  #method="xgbTree"
#)

#xgb_train_1$bestTune






xgb <- xgboost(dtrain,max.depth=3,objective= "binary:logistic", nrounds = 30,lambda=4.6,eta=1)
mean(ifelse(predict(xgb,newdata=dtest)>0.5,1,0)==train5$default[-index]) #78.21%
sapply(1:length(cutoffs), function(x) {mean(ifelse(predict(xgb,newdata=dtest)>cutoffs[x],1,0)==train5$default[-index])}) #78.17%)
xgb.importance(feature_names=colnames(dtrain),model=xgb)




table(ifelse(predict(xgb,newdata=dtest)>0.5,1,0),train5$default[-index]) 

asd <- xgb.importance(feature_names=colnames(dtrain),model=xgb)

asd2 <- data.frame("Gain" = asd$Gain[order(asd$Gain,decreasing = T)])
asd2$Variable <- unlist(asd$Feature[order(asd$Gain,decreasing = T)])
asd2$Variable[c(2,3,4,7,13,14)] <- c("Loan_Amount", "Credit Score","Sanct.Amt", "Cred.Hist","Salaried", "Self.Emp")

#Expected Loss
results_xgb <- data.frame("predicted"=ifelse(predict(xgb,newdata=dtest)>0.5,1,0),"real"=train4$default[-index])

index3 <- which(results_xgb$predicted==0& results_xgb$real==1)

sum(train$disbursed_amount[index3]) #$522 mil
sum(train$disbursed_amount) #12.2Bilion





barplot(asd2$Gain,
        names.arg=asd2$Variable,cex.names=0.85,las=2,
        main="Variable Importance", ylab="Information Gain")

#From xgbTree
param <- list( booster="gbtree",gamma=1,colsample_bytree=0.5, min_child_weight=1, max_depth=10, eta=0.01,
              objective= "binary:logistic")
xgb <- xgboost(dtrain,nrounds=200, params=param)
mean(ifelse(predict(xgb,newdata=dtest)>0.5,1,0)==train5$default[-index]) #78.21%

summary(xgb)





train6 <- train4[,-5]
train6 <- cbind(train6, poly(train4$PERFORM_CNS.SCORE,2))
colnames(train6)[14:15] <- c("credit_sc_1","credit_sc_2")

dtrain2 <- xgb.DMatrix(data = model.matrix(default~.-1,data=train6[index,]), label=as.numeric(train4$default[index])-1)
dtest2 <- xgb.DMatrix(data = model.matrix(default~.-1,data=train6[-index,]), label=as.numeric(train4$default[-index])-1)

xgb_train_2$bestTune
xgb2 <- xgboost(dtrain2,max.depth=3,objective= "binary:logistic", nrounds = 30,lambda=4.6,eta=1)
mean(ifelse(predict(xgb2,newdata=dtest2)>0.5,1,0)==train5$default[-index]) #78.21%
xgb_pred <- predict(xgb,newdata = dtest,type="prob")
rates.xgb <- prediction(xgb_pred,train4$default[-index])
auc.xgb<-ROCR::performance(rates.xgb, measure = "auc")
auc.xgb@y.values #0.6284.521


sapply(1:length(cutoffs), function(x) {mean(ifelse(predict(xgb2,newdata=dtest2)>cutoffs[x],1,0)==train5$default[-index])}) #78.17%)

xgb.plot.importance(importance_matrix=xgb.importance(feature_names=colnames(dtrain),model=xgb), xlab="Information Gain",
                    main="Variable Importance")
auc.xgb<-ROCR::performance(rates.xgb, measure = "tpr",x.measure = "fpr")
plot(auc.xgb, main="ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")


barplot(unlist(xgb.importance(feature_names=colnames(dtrain),model=xgb)[,2]),
        names.arg=c("Credit Score","Disbur.Amt","ltv","Sanct.Amt","Install.Amt","Overdue.Act","Age","Credit.Hist","Employed","No.Accts","Current Bal",
                    "Disbur.Amt","Active.Act"),cex.names=0.95,las=2,
        main="Variable Importance", ylab="Information Gain")




c("Credit Score","Disbur.Amt","ltv","Sanct.Amt","Install.Amt","Overdue.Act","Age","Credit.Hist","Employed","No.Accts","Current Bal",
  "Disbur.Amt","Active.Act")







###########################
#Higher Order Terms
train5 <-cbind(poly(train4$ltv,2),poly(train4$Overdue.Accts,2),poly(train4$PERFORM_CNS.SCORE,2), poly(train4$asset_cost,2),
      train4[,!colnames(train4)%in%c("ltv","Overdue.Accts","PERFORM_CNS.SCORE","asset_cost" )]      )
colnames(train5)[1:8] <- c("ltv_1","ltv_2","Overdue.Accts_1","Overdue.Accts_2",
                           "PERFORM_CNS.SCORE_1","PERFORM_CNS.SCORE_2",
                           "asset_cost_1","asset_cost_2")
train5[,1:8] <- scale(train5[,1:8])
#LDA
lda.m2 <- lda(default~.,data=train5[index,-11])
pred_l2 <- predict(lda.m2,newdata=train5[-index,-11])

rates.l2 <- prediction(pred_l2$posterior[,2],train4$default[-index])
auc.m2<-ROCR::performance(rates.l2, measure = "auc")
auc.m2@y.values #0.571527

#Logistic
log_reg2 <- glm(default~.,data=train5[index,], family="binomial")
summary(log_reg2)
pred_log2 <- predict(log_reg2,newdata=train5[-index,],type="response")
mean(ifelse(pred_log2>=0.5,1,0) == train4$default[-index]) #78.17%

sort(abs(coefficients(log_reg2)),decreasing = T)


rates.log2 <- prediction(pred_log2,train4$default[-index])
auc.log2<-ROCR::performance(rates.log2, measure = "auc")
auc.log2@y.values #0.5725127



