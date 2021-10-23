library(data.table)
         # Install psych package
library("psych")

###############################################################Cleaning
Htable = fread("2020_Statistical_Annex_Table_1.csv")
Htable

#HDI is the geometric mean (equally-weighted) of life expectancy, education, and GNI per capita

edu = fread("Education index.csv",fill = TRUE,header=TRUE)
inc = fread("Income index.csv",fill = TRUE,header=TRUE)
lif = fread("Life expectancy index.csv",fill = TRUE,header=TRUE)

edu$Country
head(edu)
eduA= edu[Country==" Argentina",]
summary(eduA)
if (!require("tidyverse")) install.packages("tidyverse")
eduA=eduA %>% discard(~all(is.na(.) | . ==""))
eduA$`HDI Rank`="Education Index"
head(eduA)

incA = inc[Country=="Argentina",]
incA=incA %>% discard(~all(is.na(.) | . ==""))
summary(incA)
incA$`HDI Rank`="Income Index"

lifA = lif[Country == "Argentina",]
lifA=lifA %>% discard(~all(is.na(.) | . ==""))
summary(lifA)
lifA$`HDI Rank`="Life Expectency Index"

temp=rbind(eduA,incA)
clean1 = rbind(temp,lifA)
head(clean1)

names(clean1)[1]="Indexes"
class(clean1)

clean1$Country=NULL

clean2 = t(clean1)

head(clean2)

labels(clean2)



data_new <- clean2                         # Duplicate data frame
colnames(data_new) <- clean2[1, ]          # Convert first row to header
head(data_new)              

data_new <- data_new[- 1, ]  



arr = c();
y=1990
for(x in 1:30) 
{
  arr[x]=y
  y=y+1
}

class(data)
data <- as.data.frame(data_new)
setDT(data) 
colnames(data) <- c("Education_Index", "Income_Index", "Life_Expectency_Index")
data$Year = arr

summary(data)
head(data)

sum(is.na(data)) 
## No Nulls

data <- data[ , Education_Index := as.numeric(Education_Index)]
data <- data[ , Income_Index := as.numeric(Income_Index)]
data <- data[ , Life_Expectency_Index := as.numeric(Life_Expectency_Index)]

#data[,HDI:=geometric.mean(c(Education_Index,Income_Index,Life_Expectnecy_Index))]
#c(Education_Index,Income_Index,Life_Expectnecy_Index)
#geometric.mean(data[12,c(Education_Index,Income_Index,Life_Expectnecy_Index)])
#data$HDI=geometric.mean(c(data$Education_Index,data$Income_Index,data$Life_Expectnecy_Index))

for(x in 1:30){
  data$HDI[x]=geometric.mean(data[x,c(Education_Index,Income_Index,Life_Expectency_Index)])
}
colnames(data) <- c("Education_Index", "Income_Index", "Life_Expectency_Index","Year","HDI")
data



data1=data


shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

data1$HDI <- shift(data1$HDI, 4)

data1=data1[order(-data1$Year)]

data1

arr = c();
y=2023
for(x in 1:30) 
{
  arr[x]=y
  y=y-1
}
arr

data1$pred_Year=arr

data1

wdata=data1[c(5:30),]

wdata

#write.csv(data,"C:\\NTU\\Business\\Y2S1\\Analytics\\AY21 Team Assignment and Project\\Argentina HDI.csv", row.names = FALSE)



#####################################################################exploration
library(ggplot2)
summary(wdata)

ggplot(data=wdata,aes(x = Education_Index,y=HDI))+geom_point()+labs(title="HDI vs Education_Index")

ggplot(data=wdata,aes(x = Income_Index,y=HDI))+geom_point()+labs(title="HDI vs Income_Index")

ggplot(data=wdata,aes(x = Life_Expectency_Index,y=HDI))+geom_point()+labs(title="HDI vs Life_Expectency_Index")

library(corrplot)
corrplot(cor(wdata), type = "upper")

plot(wdata$HDI, wdata$Education_Index)
cor(wdata$HDI, wdata$Education_Index)

plot(wdata$HDI, wdata$Income_Index)
cor(wdata$HDI, wdata$Income_Index)

plot(wdata$HDI, wdata$Life_Expectency_Index)
cor(wdata$HDI, wdata$Life_Expectency_Index)
#############################################################Regression

#uses all data in set to generate regression

m1 <- lm(HDI ~ Education_Index+Income_Index+Life_Expectency_Index, data = wdata)
summary(m1)
#only life expectency index significant, so only use that as predictor

m2 <- lm(HDI ~ Life_Expectency_Index, data = wdata)
summary(m2)

#m3 <- lm(HDI ~ Income_Index, data = wdata)
#summary(m3)
#m4 <- lm(HDI ~ Education_Index, data = wdata)
#summary(m4)
#m5 <- lm(HDI ~ Education_Index+Life_Expectency_Index, data = wdata)
#summary(m5)
#m6 <- lm(HDI ~ Income_Index+Life_Expectency_Index, data = wdata)
#summary(m6)
#m7 <- lm(HDI ~ Education_Index+Income_Index, data = wdata)
#summary(m7)
#d1 <- lm(HDI ~ Education_Index+Income_Index+Life_Expectency_Index, data = data)
#summary(d1)

data
class(data)

test.hdi=predict(m2,newdata=wdata)
error=wdata$HDI-test.hdi

RMSE <- sqrt(mean(error^2))
summary(abs(error))

RMSE



pred=data[27:30]
pred

pred.hdi = predict(m2,newdata=pred)
pred$pred_HDI=pred.hdi

pred$HDI=NULL
pred$pred_year = c(2020,2021,2022,2023)
pred #pred HDI for 2020 to 2023

###############################################testing accuracy of t-4 method using train-test
#seperate data into train and test to check if t-4 method is valid
wdata
library(caTools)
set.seed(2004)

# 70% trainset. Stratify on Y = mpg. Caution: Sample size only 32 in this example.
train <- sample.split(Y = wdata$HDI, SplitRatio = 0.7)
trainset <- subset(wdata, train == T)
testset <- subset(wdata, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$HDI)
summary(testset$HDI)

# Develop model on trainset
m5 <- lm(HDI ~ Life_Expectency_Index,data = trainset)
summary(m5)
residuals(m5) 

# Residuals = Error = Actual mpg - Model Predicted mpg
RMSE.m5.train <- sqrt(mean(residuals(m5)^2))  # RMSE on trainset based on m5 model.
summary(abs(residuals(m5)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.m5.test <- predict(m5, newdata = testset)
testset.error <- testset$HDI - predict.m5.test

# Testset Errors
RMSE.m5.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.m5.train 
RMSE.m5.test ##predicts well enough


##Using trainset to predict 2020 - 2023

pred1=data[27:30]
pred.hdi1 = predict(m5,newdata=pred1)
pred1$pred_HDI=pred.hdi1

pred1$HDI=NULL
pred1$pred_year = c(2020,2021,2022,2023)
pred1 #pred HDI for 2020 to 2023



################################################################CART
library(rpart)
library(rpart.plot)	


cart1 <- rpart(HDI ~ Education_Index+Life_Expectency_Index+Income_Index, data = wdata, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))

plotcp(cart1)

print(cart1)

CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]

i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)

cart2 <- prune(cart1, cp = cp.opt)
printcp(cart2, digits = 3)


print(cart2)

rpart.plot(cart2, nn = T, main = "Optimal Tree in mtcars")
## The number inside each node represent the mean value of Y.

cart2$variable.importance

summary(cart2)
##Doesn't make sense to use cart in this case as we restrict possible values of
## HDI to a few averages

