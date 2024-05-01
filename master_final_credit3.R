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




# Logistic Regression

train_a$default <- train2$loan_default
train_a <- train_a[,-which(colnames(train_a)=="asset_cost")]

train_a[,-c(4,14)]<- log((train_a[,-c(4,14)])+1)

log_reg <- glm(default~.,data=train_a[index,], family="binomial")
summary(log_reg)
pred_log <- predict(log_reg ,newdata=train_a[-index,],type="response")
mean(ifelse(pred_log>=0.5,1,0) == train_a$default[-index]) #78.17%



#Expected Loss
results_log <- data.frame("predicted"=ifelse(pred_log>=0.5,1,0),"real"=train_a$default[-index])
index3 <- which(results_log$predicted==0& results_log$real==1)
sum(train$disbursed_amount[index3]) #$522 mil
sum(train$disbursed_amount) #12.2Bilion


cutoffs <- seq(0.6,0.9,by=0.1)

asd <- lapply(1:length(cutoffs), function(x) {mean(ifelse(pred_log>=cutoffs[x],1,0)==train_a$default[-index])}) #78.17%)
asd

NPR <- c()
for( i in 1:length(cutoffs)){
  NPR[i] <- asd[[i]][1,2] / sum(asd[[i]][1,])
}

NPR



train5 <- train4[,-2]
train5 <- cbind(train5,poly(train4$ltv,5))
colnames(train5)[14:18] <- c("ltv_1", "ltv_2", "ltv_3", "ltv_4","ltv5")

log_reg2 <- glm(default~.,data=train5[index,], family="binomial")
summary(log_reg2)
pred_log2 <- predict(log_reg2,newdata=train5[-index,],type="response")
mean(ifelse(pred_log2>=0.5,1,0) == train4$default[-index]) #78.17%







