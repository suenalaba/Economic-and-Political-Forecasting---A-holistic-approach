library(data.table)
         # Install psych package
library("psych")




### set working directory to HDI Datasets Cleaned
##setwd("C:/NTU/Business/Y2 S1/Analytics/AY21 Team Assignment and Project/HDI Datasets Cleaned")

#####################################################################exploration
library(ggplot2)
wdata = fread("Indexdata.csv")
summary(wdata)
wdata

##boxplot

ggplot(data=wdata,aes(x=year,y=Education_Index))+geom_point()+labs(title="Year vs Education_Index")
cor(wdata$year, wdata$Income_Index)
ggplot(data=wdata,aes(x=year,y=Income_Index))+geom_point()+labs(title="Year vs Income_Index")
cor(wdata$year, wdata$Income_Index)
ggplot(data=wdata,aes(x=year,y=Life_Expectency_Index))+geom_point()+labs(title="Year vs Life_Expectency_Index")
cor(wdata$year, wdata$Income_Index)
ggplot(data=wdata,aes(x=year,y=HDI))+geom_point()+labs(title="Year vs HDI")
cor(wdata$year, wdata$Income_Index)

##correlation of indexes to HDI in 5 years
ggplot(data=wdata,aes(x = Education_Index,y=HDI))+geom_point()+labs(title="HDI vs Education_Index")
cor(wdata$HDI, wdata$Education_Index)

ggplot(data=wdata,aes(x = Income_Index,y=HDI))+geom_point()+labs(title="HDI vs Income_Index")
cor(wdata$HDI, wdata$Income_Index)

ggplot(data=wdata,aes(x = Life_Expectency_Index,y=HDI))+geom_point()+labs(title="HDI vs Life_Expectency_Index")
cor(wdata$HDI, wdata$Life_Expectency_Index)

library(corrplot)
corrplot(cor(wdata), type = "upper")




#############################################################Regression

#uses all data in set to generate regression, aim to predict 2015-2019

wdata
wdata1= wdata[c(6:25),] #use date before 2010
wdata2 = wdata[c(1:5),] #values for 2010-2014 to predict 2015-2019
wdata1
wdata2


m1 <- lm(HDI ~ Education_Index+Income_Index+Life_Expectency_Index, data = wdata1)
summary(m1)

library(car)
vif(m1) #colinearity present between life expectancy and education, hence remove education index

m2 <- lm(HDI ~ Life_Expectency_Index+Income_Index, data = wdata1)
summary(m2) #only life expectancy index significant, so only use that as predictor
vif(m2)


m3 <- lm(HDI ~ Life_Expectency_Index, data = wdata1)
summary(m3)

par(mfrow = c(2,2))
plot(m3)
par(mfrow = c(1,1))


test.hdi=predict(m3,newdata=wdata2) #using 2010-2014 data
error=wdata2$HDI-test.hdi

RMSE <- sqrt(mean(error^2))
summary(abs(error))

RMSE
##Only 1 variable, but we learn that health is the most signifacnt factor in predicting HDI in 5 years, hence now we find predictos
##that might affect LE and use that to predict HDI

#################################################using LE factors as predictors
data = fread("HDIdata.csv")
names(data)[4]="year"
dataLE = data
dataLE$Education_Index=NULL
dataLE$Income_Index=NULL
dataLE$Life_Expectency_Index=NULL
dataLE

LE = fread("LE factors.csv")
LE$hdi=NULL

LE=merge(x = LE, y = dataLE, by = "year", all = TRUE)
LE

LE1 = LE[!is.na(LE$HDI), ]   
LE2=LE1
LE2$HDI= shift(LE1$HDI, 5)
LE2

LE3=LE2[order(-LE2$year)]


LE4=LE3[year<2014,]
LE4 = LE4[!is.na(LE4$HDI), ]  
LE4



LE4


#####################################################full data 5 years project(Argentina only)

library(rpart)
library(rpart.plot)	
LE4
var = LE4[c(1:5),] #to be used as variables to predict 2014-2018
var

LE5=LE4[c(6:24),]#remainding years to be used in model
LE5

#use cart to predict
cart11 <- rpart(HDI ~ .-year, data = LE5, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))

plotcp(cart11)

print(cart11)

CVerror.cap <- cart11$cptable[which.min(cart11$cptable[,"xerror"]), "xerror"] + cart11$cptable[which.min(cart11$cptable[,"xerror"]), "xstd"]

i <- 1; j<- 4
while (cart11$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(cart11$cptable[i,1] * cart11$cptable[i-1,1]), 1)

cart22 <- prune(cart11, cp = cp.opt)
printcp(cart22, digits = 3)


print(cart22)

rpart.plot(cart22, nn = T, main = "Optimal Tree in predicting HDI")
## The number inside each node represent the mean value of Y.

cart22$variable.importance

summary(cart22)


test.predict = predict(cart22, newdata = var)
test.predict
testset.error <- var$HDI - test.predict
testset.error

# Testset Errors
RMSE.m5.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.m5.test



######################################################################### 1 year project
LE1y=LE1

LE1y$HDI= shift(LE1$HDI, 1)
LE1y

LE1y1=LE1y[order(-LE1y$year)]
LE1y1

LE1y2=LE1y1[year<2014,]
LE1y2 = LE1y2[!is.na(LE4$HDI), ]  
LE1y2


LE6=LE1y2[c(2:24),]
LE6
var1 = LE1y2[1,]
var1

cart111 <- rpart(HDI ~ .-year, data = LE6, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))

plotcp(cart111)

print(cart111)

CVerror.cap <- cart111$cptable[which.min(cart111$cptable[,"xerror"]), "xerror"] + cart111$cptable[which.min(cart111$cptable[,"xerror"]), "xstd"]

i <- 1; j<- 4
while (cart111$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(cart111$cptable[i,1] * cart111$cptable[i-1,1]), 1)

cart222 <- prune(cart111, cp = cp.opt)
printcp(cart222, digits = 3)


print(cart222)

rpart.plot(cart222, nn = T, main = "Optimal Tree in predicting HDI")
## The number inside each node represent the mean value of Y.

cart222$variable.importance

summary(cart222)




test.predict1 = predict(cart222, newdata = var1)
testset.error1 <- var1$HDI - test.predict1

# Testset Errors
RMSE.m5.test1 <- sqrt(mean(testset.error1^2))
summary(abs(testset.error1))

RMSE.m5.test1



#####################################comparing the 3 projections

RMSE
RMSE.m5.test
RMSE.m5.test1





######################################################################### All countries

AC1 = fread("All countries LE factors.csv")
library(caTools)
set.seed(2004)
AC1



AC=AC1
AC$country=NULL
AC$year=NULL
AC

ggplot(data=AC1,aes(x=greenhouse_gas_per_capita,y=hdi))+geom_point()+labs(title="Year vs Education_Index")

summary(AC)

corrplot(cor(AC), type = "upper")




# 70% trainset. Stratify on Y = mpg. Caution: Sample size only 32 in this example.
train <- sample.split(Y = AC$hdi, SplitRatio = 0.7)
trainset <- subset(AC, train == T)
testset <- subset(AC, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$hdi)
summary(testset$hdi)

# Develop model on trainset
mAC <- lm(hdi~.,data = trainset)
summary(mAC)
residuals(mAC) 

vif(mAC)


par(mfrow = c(2,2))
plot(mAC)
par(mfrow = c(1,1))



# Residuals = Error = Actual mpg - Model Predicted mpg
RMSE.mAC.train <- sqrt(mean(residuals(mAC)^2))  # RMSE on trainset based on mAC model.
summary(abs(residuals(mAC)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.mAC.test <- predict(mAC, newdata = testset)
testset.error <- testset$hdi - predict.mAC.test

# Testset Errors
RMSE.mAC.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.mAC.train 
RMSE.mAC.test



AC1[country=="Argentina"&year>=2008]


pred1=AC1[country=="Argentina"&year>=2010]
pred1 
pred.hdi = predict(mAC,newdata=pred1)
pred.hdi

errorAC=pred1$hdi-pred.hdi
RMSE_p<- sqrt(mean(errorAC^2))
RMSE_p #using all countries vastly disprove accuracy of model




