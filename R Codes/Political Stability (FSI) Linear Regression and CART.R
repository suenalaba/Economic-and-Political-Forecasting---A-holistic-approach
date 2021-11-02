#run relevant libraries to be updated as we go
library(ggplot2)
library(data.table)
library(caTools)
library(rpart)
library(rpart.plot)
library(car)
library(dplyr)
library(corrplot)
library(vtable)
library(Hmisc)
library(xtable)
library(MLmetrics)

#set working directory here to import files...
setwd("C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Cleaned")

#===============================================================================

#Importing datasets for FSI for regression analysis on political stability

#===============================================================================

#Linear regression to forecast FSI(Y+1)
developed_countries_linreg1 <- fread("developed_countries_linreg1.csv")
developing_countries_linreg1 <- fread("developing_countries_linreg1.csv")
leastdeveloped_linreg1 <- fread("leastdeveloped_linreg1.csv")
transitioning_linreg1 <- fread("transitioning_linreg1.csv")

#Linear regression to forecast FSI(Y+4)
developed_countries_linreg4 <- fread("developed_countries_linreg4.csv")
leastdeveloped_linreg4 <- fread("leastdeveloped_linreg4.csv")
developing_countries_linreg4 <- fread("developing_countries_linreg4.csv")
transitioning_linreg4 <- fread("transitioning_linreg4.csv")

#CART to forecast FSI(Y+1)
developed_countries_cart1 <- fread("developed_countries_cart1.csv",stringsAsFactors = TRUE)
developing_countries_cart1 <- fread("developing_countries_cart1.csv",stringsAsFactors = TRUE)
leastdeveloped_cart1 <- fread("leastdeveloped_cart1.csv",stringsAsFactors = TRUE)
transitioning_cart1 <- fread("transitioning_cart1.csv",stringsAsFactors = TRUE)

#CART to forecast FSI(Y+4)
FSI_cart4 <- fread("FSI_cart4.csv",stringsAsFactors = TRUE)



#===============================================================================

#Data Visualisation 
#Exploring the relationship of FSI(Y+4) ~ FSI(Y) + Other factors(Y)
#Exploring the relationship of FSI(Y+1) ~ FSI(Y) + Other factors(Y)

#===============================================================================
#summary statistics
st(developed_countries_cart1[,-c(1,2)],title="Developed Economies FSI(Year+1)")
st(developing_countries_cart1[,-c(1,2)],title="Developing Economies FSI(Year+1)")
st(leastdeveloped_cart1[,-c(1,2)],title="Least Developed Economies FSI(Year+1)")
st(transitioning_cart1[,-c(1,2)],title="Transitioning Economies FSI(Year+1)")

st(developed_countries_linreg4[,-c(1,2)],title="Developed Economies FSI(Year+4)")
st(leastdeveloped_linreg4[,-c(1,2)],title="Least developed Economies FSI(Year+4)")
st(developing_countries_linreg4[,-c(1,2)],title="Developing Economies FSI(Year+4)")
st(transitioning_linreg4[,-c(1,2)],title="Transitioning Economies FSI(Year+4)")

st(FSI_cart4[,-c(1,2)],title="Fragile State Index for all economies(Year+4)")

#function for plotting pairwise correlation table 
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 
#end of function

#printing pairwise correlation table, will open in localhost browser, for different economy types
print(htmlTable::htmlTable((corstars(developed_countries_linreg1[,-c(1,2)],result = "html")),useViewer=utils::browseURL))
print(htmlTable::htmlTable((corstars(developing_countries_linreg1[,-c(1,2)],result = "html")),useViewer=utils::browseURL))
print(htmlTable::htmlTable((corstars(leastdeveloped_linreg1[,-c(1,2)],result = "html")),useViewer=utils::browseURL))
print(htmlTable::htmlTable((corstars(transitioning_linreg1[,-c(1,2)],result = "html")),useViewer=utils::browseURL))


#Visualise the plots, to explore the relationship between forecasting FSI 1 year ahead
#and the independent variables of the current year
corrplot(cor(developed_countries_linreg1[,3:13]),  
         main = "Developed Countries Correlation Plot, 1 year ahead forecast",cex.main = 0.9,
         mar = c(0,0,2,0))
corrplot(cor(developing_countries_linreg1[,3:13]), 
         main = "Developing Countries Correlation Plot, 1 year ahead forecast",cex.main = 0.9,
         mar = c(0,0,2,0))
corrplot(cor(leastdeveloped_linreg1[,3:13]),  
         main = "Least Developed Countries Correlation Plot, 1 year ahead forecast",cex.main = 0.9,
         mar = c(0,0,2,0))
corrplot(cor(transitioning_linreg1[,3:13]), 
         main = "Transitioning Countries Correlation Plot, 1 year ahead forecast",cex.main = 0.9,
         mar = c(0,0,2,0))

#Visualise the plots, to explore the relationship between forecasting FSI 4 years ahead,
#with the independent variables at current year.
corrplot(cor(developed_countries_linreg4[,3:13]), 
         main = "Developed Countries Correlation Plot, 4 years ahead forecast",cex.main = 0.9,
         mar = c(0,0,2,0))
corrplot(cor(developing_countries_linreg4[,3:13]),  
         main = "Developing Countries Correlation Plot, 4 years ahead forecast",cex.main = 0.9,
         mar = c(0,0,2,0))
corrplot(cor(leastdeveloped_linreg4[,3:13]), 
         main = "Least Developed Countries Correlation Plot, 4 years ahead forecast",cex.main = 0.9,
         mar = c(0,0,2,0))
corrplot(cor(transitioning_linreg4[,3:13]),
         main = "Transitioning Countries Correlation Plot, 4 years ahead forecast",cex.main = 0.9,
         mar = c(0,0,2,0))


#=======================================================================================

#Developing a model for developed countries using linear regression 

#Using data: developed_countries_linreg1
#=======================================================================================

#Step 1: Identify potential outliers and remove them
#In this step, we use diagnostic plots to identify potential outliers


m1 <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year` , data = developed_countries_linreg1)
summary(m1)
#From the initial mod, R-squared and Adjusted R-squared are approximately 98%.
#Can we do better?
#Lets explore whether there are any outliers.
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))
#From residuals vs fitted, there is no pattern in the residual plot, with the red line almost horizontal
#We can assume a linear relationship between FSI(Year+1) and the predictors

#From scale-location, the residuals are spread equally along the range of predictors
#Hence, we can verify that the variances in errors are constant.

#From normal Q-Q, we can see the residuals, all fall approximately on th reference line
#Some points are bit far off, but not a major cause of concern

#From residuals vs leverage, all points lie safely within the Cook's distance lines, 
#No significant influential outliers

#No outliers need to be removed, after examining the residual plots.

#Step 2: Train-test split
#We will now do train-test split on the dataset, after checking for outliers
#Data to be used here: #developed_countries_linreg1

set.seed(2004)
train <- sample.split(Y = developed_countries_linreg1$`FSI (Year+1)`, SplitRatio = 0.7)
trainset <- subset(developed_countries_linreg1, train == T)
testset <- subset(developed_countries_linreg1, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$`FSI (Year+1)`)
summary(testset$`FSI (Year+1)`)
#distribution are relatively similar, we can proceed.

#Step 3: Feature selection using backward elimination, VIF & Domain Knowledge

#Training our model & feature selection
m2 <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year`, data = trainset)
summary(m2) #initial train model

#Perform backward elimination remove unimportant variables
m.full <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year`, data = trainset)
m3 <- step(m.full)
summary(m3)


#what if we remove FSI of the current year? 
m.full4 <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year` - FSI, data = trainset)
m4 <- step(m.full4)
summary(m4)
#If we exclude FSI of the current year and generate a model, the Adjusted R-squared falls by about 18%.
#This shows that when forecasting FSI, the FSI is heavily influenced by previous year's FSI value.
#This is true because FSI changes with time and demonstrates a time-series relationship.
#Therefore, when forecasting FSI, it is important to factor in previous year's value.

#Without previous year's FSI, the importance of variables can be ranked as:
# 1: No violence Index
# 2: Human Development Index
# 3: Rule of Law
# 4: Voice & Accountability

#Check diagnostic plots
par(mfrow = c(2,2))
plot(m3)
par(mfrow = c(1,1))
#No major issues in the diagnostic plots here, lets see if there is multicollinearity issue

#Check VIF
vif(m3)
#No obvious multicollinearity problem here as all the vif values are relatively small.
#Some form of multicollinearity will be present, but not much in this case.

#The final model chosen will be:
trainmodel <- lm(formula = `FSI (Year+1)` ~ FSI + `No violence Index` + CorruptionPI, 
                 data = trainset)

#Regression Equation:
#`FSI (Year+1)` ~ FSI + `No violence Index` + CorruptionPI, 
summary(trainmodel)
#Our model now has 98.8% R-squared and Adjusted R-squared of 98.7% which indicates
#a strong model.
#If we consider present year's FSI to predict the next year's FSI in developed countries:
#1: FSI
#2: Corruption Perception Index
#3: No violence Index

# Residuals = Error = Actual FSI(t+1) - Model Predicted FSI(t+1)
RMSE.trainmodel <- sqrt(mean(residuals(trainmodel)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(trainmodel)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(trainmodel, newdata = testset)
testset.error <- testset$`FSI (Year+1)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 

#Visualise our predictions

developed_prediction <- data.frame(Predicted = predict.test,  # Create data for ggplot2
                                  Actual = testset$`FSI (Year+1)`)
# Draw plot using ggplot2 package
developed_plot <- ggplot(developed_prediction,                                     
       aes(x = Predicted,
           y = Actual)) +
  ggtitle("Developed Countries FSI(Year+1)") +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
developed_plot
#RMSE for train is 1.393297, while RMSE for test is 1.887529
#Given the range of FSI of 0-120, there is approximately 1.16% error on the trainset and 1.57% error on testset on average
#In general however, given any random set of data, actual FSI(Y+1) will differ from forecasted FSI(Y+1) by 1.887529
#This shows that the model to forecast FSI 1 year into the future is quite strong.
#This is supported by the visualisation plots, which show that the actual values lie closely
#to the forecasted FSI(Y+1) represented by the red line.

#===============================================================================

#Developing a model for developed countries using Categorical CART

#Data: developed_countries_cart1
#===============================================================================
#For CART, we want to find out, how well our predictions can be in the absence of
#previous year's FSI data, that is what truly affects a political stability of a country
#In the absence of FSI data, what is the best prediction CART can come up with?
#We will be using categorical CART, by grouping the FSI indexes according to their categories.

#For CART, missing values are handled via surrogates & outliers will tend towards the leaf node
#Therefore, we can skip the step of removing outliers/NA
cart1_developed <- developed_countries_cart1[,c(3:13,15)]
#Step 1: Train-test split for CART

set.seed(2004)
train <- sample.split(Y = cart1_developed$`Fragility_Category (Year+1)`, SplitRatio = 0.7)
trainset <- subset(cart1_developed, train == T)
testset <- subset(cart1_developed, train == F)

#we are trying to find the optimal tree based on 1SE rule
set.seed(2004)
cart1 <- rpart(`Fragility_Category (Year+1)` ~ . -FSI ,data = trainset,
               method = 'class', control = rpart.control(minsplit = 5, cp = 0))
par(mfrow = c(1,1))
rpart.plot(cart1, nn=T, main = "Maximal Tree for Fragility Category (Year+1) for developed countries")

print(cart1)

printcp(cart1)

plotcp(cart1)

#Extract the Optimal Tree 
# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1) #calculate geometric mean here

cart2 <- prune(cart1, cp = cp.opt)

rpart.plot(cart2, nn=T, tweak=2.1, main = "Optimal Tree for Fragility Category (Year+1) for developed countries")

print(cart2)

printcp(cart2)

summary(cart2)

scaledVarImpt <- round(100*cart2$variable.importance/sum(cart2$variable.importance))
scaledVarImpt[scaledVarImpt > 0]
#CART identifies the most important variables to predict FSI(Year+1) as:
# 1: Rule of Law 19%
# 2: Human Development Index 16%
# 3: GDP Per Capita 15%
# 4: Voice & Accountability 14%
# 5: Life Satisfaction 13%

#Lets see how well our model performs
cart.predict <- predict(cart2, newdata = testset, type = "class")

table <- table(Testset.Actual = testset$`Fragility_Category (Year+1)`, cart.predict, deparse.level = 2)
table
#From the table, we aim to identify if there is any serious misclassification.
# 2 data points are heavily misclassified, 1 predicts a stable country to be "warning" and
#the other predicts a country that is supposed to be "warning" as stable.

# Overall Accuracy
mean(cart.predict == testset$`Fragility_Category (Year+1)`)

#Overall model accuracy is 66.4%. On average, 33.6% data will be misclassified.


#=================================================================================

#Developing a linear regression model for developing countries

#Data: developing_countries_linreg1
#=================================================================================

#Step 1: Identify potential outliers and remove them
#In this step, we use diagnostic plots to identify potential outliers

#In this step, we use diagnostic plots to identify potential outliers

m1 <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year`, data = developing_countries_linreg1)
summary(m1)
#From the initial mod, R-squared and Adjusted R-squared are approximately 98%.
#Can we do better?
#Lets explore whether there are any outliers.
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))
#From residuals vs fitted, there is no pattern in the residual plot, with the red line almost horizontal
#We can assume a linear relationship between FSI(Year+1) and the predictors

#From scale-location, the residuals are spread equally along the range of predictors
#Hence, we can verify that the variances in errors are constant.

#From normal Q-Q, we can see the residuals, all fall approximately on th reference line

#From residuals vs leverage, all points lie safely within the Cook's distance lines, 
#No significant influential outliers

#Although there are no major issues, it appears that point #170 appears to be far off in 4 diagnostic plots.
#Lets try removing this point 170, to see if our model accuracy will improve.
developing_countries_linreg2 <- developing_countries_linreg1[-c(170)]
m2 <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year`, data = developing_countries_linreg2)
summary(m2)
#Wow, model accuracy actually increased by 1% after removing point 170.
#Lets double check our diagnostic plots
par(mfrow = c(2,2))
plot(m2)
par(mfrow = c(1,1))
#No outliers need to be removed, after examining the residual plots. 
#All the residuals seem to fit nicely, satisying the linear regression assumptions

#Step 2: Train-test split
#We will now do train-test split on the dataset, after checking for outliers
#Data to be used here: #developing_countries_linreg2

set.seed(2004)
train <- sample.split(Y = developing_countries_linreg2$`FSI (Year+1)`, SplitRatio = 0.7)
trainset <- subset(developing_countries_linreg2, train == T)
testset <- subset(developing_countries_linreg2, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$`FSI (Year+1)`)
summary(testset$`FSI (Year+1)`)
#distribution are relatively similar, we can proceed.

#Step 3: Feature selection using backward elimination, VIF & Domain Knowledge

#Training our model & feature selection
m3 <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year`, data = trainset)
summary(m3) #initial train model

#Perform backward elimination remove unimportant variables
m.full <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year`, data = trainset)
m4 <- step(m.full)
summary(m4)
#Our model accuracy remains strong at 99% after backward elimination.
#From which, can find out the more important variables to predict FSI(Y+1)

#what if we remove FSI of the current year? 
m.full5 <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year` - FSI, data = trainset)
m5 <- step(m.full5)
summary(m5)

#Without previous year's FSI, the importance of variables can be ranked as:
# 1: No-violence index
# 2: Voice & Accountability
# 3: Human Development Index
# 4: Rule of Law
# 5: Aid per capita
#If we exclude FSI of the current year and generate a model, the Adjusted R-squared falls by about 11%.
#This shows that when forecasting FSI, the FSI is heavily influenced by previous year's FSI value.
#This is true because FSI changes with time and demonstrates a time-series relationship.
#Therefore, when forecasting FSI, it is important to factor in previous year's value.

#Check diagnostic plots
par(mfrow = c(2,2))
plot(m4)
par(mfrow = c(1,1))
#No major issues in the diagnostic plots here, lets see if there is multicollinearity issue

#Check VIF
vif(m4)
#No obvious multicollinearity problem here as all the vif values are relatively small.
#Some form of multicollinearity will be present, but not much in this case.

#The final model chosen will be:
trainmodel <- lm(formula = `FSI (Year+1)` ~ FSI + Voice.Accountability + `No violence Index` + 
                   `Human Development Index` + Life_Satisfaction + `Unemployment Rate`, 
                 data = trainset)
#Regression Equation:
# `FSI (Year+1)` ~ FSI + Voice.Accountability + `No violence Index` + 
#`Human Development Index` + Life_Satisfaction + `Unemployment Rate`
summary(trainmodel)
#Our model now has 99% R-squared and Adjusted R-squared of 99%, a relatively strong number.
#If we consider present year's FSI to predict the next year's FSI in developed countries:
#1: FSI
#2: Human Development Index
#3: No violence Index
#4: Voice & Accountability
#5: Unemployment Rate

# Residuals = Error = Actual FSI(Y+1) - Model Predicted FSI(Y+1)
RMSE.trainmodel <- sqrt(mean(residuals(trainmodel)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(trainmodel)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(trainmodel, newdata = testset)
testset.error <- testset$`FSI (Year+1)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 

#Visualise our predictions

developing_prediction <- data.frame(Predicted = predict.test,  # Create data for ggplot2
                                   Actual = testset$`FSI (Year+1)`)
# Draw plot using ggplot2 package
developing_plot <- ggplot(developing_prediction,                                     
                         aes(x = Predicted,
                             y = Actual)) +
  ggtitle("Developing Countries FSI(t+1)") +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
developing_plot
#RMSE for train is 1.509893, while RMSE for test is 1.383143
#Given the range of FSI of 0-120, there is approximately 1.26% error on the trainset and 1.15% error on testset on average
#In general however, given any random set of data, actual FSI(Y+1) will differ from forecasted FSI(Y+1) by 1.383143
#This shows that the model to forecast FSI 1 year into the future is quite strong.
#This is supported by the visualisation plots, which show that the actual values lie closely
#to the forecasted FSI(Y+1) represented by the red line.

#===============================================================================

#verifying results with a CART model for developing countries

#Data: developing_countries_cart1
#===============================================================================

#For CART, we want to find out, how well our predictions can be in the absence of
#previous year's FSI data, that is what truly affects a political stability of a country
#In the absence of FSI data, what is the best prediction CART can come up with?

#For CART, missing values are handled via surrogates & outliers will tend towards the leaf node
#Therefore, we can skip the step of removing outliers/NA
cart1_developing <- developing_countries_cart1[,c(3:13,15)]
#Step 1: Train-test split for CART

set.seed(2004)
train <- sample.split(Y = cart1_developing$`Fragility_Category (Year+1)`, SplitRatio = 0.7)
trainset <- subset(cart1_developing, train == T)
testset <- subset(cart1_developing, train == F)

set.seed(2004)
cart1 <- rpart(`Fragility_Category (Year+1)` ~ . -FSI ,data = trainset,
               method = 'class', control = rpart.control(minsplit = 7, cp = 0))
par(mfrow = c(1,1))
rpart.plot(cart1, nn=T, main = "Maximal Tree for Fragility_Category (Year+1) for developing countries")

print(cart1)

printcp(cart1)

plotcp(cart1)

#Extract the Optimal Tree 
# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1) #calculate geometric mean here

cart2 <- prune(cart1, cp = cp.opt)

rpart.plot(cart2, nn=T, tweak = 1.5, fallen.leaves=FALSE, box.palette = "RdYlGn", main = "Optimal Tree for Fragility_Category (Year+1) for developing countries")
prp(cart2) #to get better visual of decision making

print(cart2)

printcp(cart2)

summary(cart2)

scaledVarImpt <- round(100*cart2$variable.importance/sum(cart2$variable.importance))
scaledVarImpt[scaledVarImpt > 0]
#CART identifies the most important variables to predict FSI(Year+1) as:
# 1: Rule of Law 16%
# 2: Voice and Accountability 14%
# 3: No violence Index 14%
# 4: Human Development Index 13%
# 5: GDP Per Capita 10%

#Lets see how well our model performs
cart.predict <- predict(cart2, newdata = testset, type = "class")

table <- table(Testset.Actual = testset$`Fragility_Category (Year+1)`, cart.predict, deparse.level = 2)
table
#There are no concerning misclassifications

# Overall Accuracy
mean(cart.predict == testset$`Fragility_Category (Year+1)`)

#The model by CART gives a 73.9% accuracy.

#===============================================================================

#Developing a linear regression model for least developed countries

#Data: leastdeveloped_linreg1
#===============================================================================


#Step 1: Identify potential outliers and remove them
#In this step, we use diagnostic plots to identify potential outliers

#In this step, we use diagnostic plots to identify potential outliers

m1 <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year`, data = leastdeveloped_linreg1)
summary(m1)
#From the initial mod, R-squared and Adjusted R-squared are approximately 97.7%.
#Can we do better?
#Lets explore whether there are any outliers.
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))
#From residuals vs fitted, there is no pattern in the residual plot, with the red line almost horizontal
#We can assume a linear relationship between FSI(Year+1) and the predictors

#From scale-location, the residuals are spread equally along the range of predictors
#Hence, we can verify that the variances in errors are constant.

#From normal Q-Q, we can see the residuals, all fall approximately on th reference line

#From residuals vs leverage, all points lie safely within the Cook's distance lines, 

#No outliers need to be removed, after examining the residual plots. (No major issues)
#All the residuals seem to fit nicely, satisying the linear regression assumptions

#Step 2: Train-test split
#We will now do train-test split on the dataset, after checking for outliers
#Data to be used here: #leastdeveloped_linreg1

set.seed(2004)
train <- sample.split(Y = leastdeveloped_linreg1$`FSI (Year+1)`, SplitRatio = 0.7)
trainset <- subset(leastdeveloped_linreg1, train == T)
testset <- subset(leastdeveloped_linreg1, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$`FSI (Year+1)`)
summary(testset$`FSI (Year+1)`)
#distribution are relatively similar, we can proceed.

#Step 3: Feature selection using backward elimination, VIF & Domain Knowledge

#Training our model & feature selection
m2 <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year`, data = trainset)
summary(m2) #initial train model

#Perform backward elimination remove unimportant variables
m.full <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year`, data = trainset)
m3 <- step(m.full)
summary(m3)

#what if we remove FSI of the current year? 
m.full4 <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year` - FSI, data = trainset)
m4 <- step(m.full4)
summary(m4)
#Without previous year's FSI, the importance of variables can be ranked as:
# 1: Voice & Accountability & No violence Index
# 2: Life Satisfaction
# 3: Human Development Index
# 4: Aid per capita
#If we exclude FSI of the current year and generate a model, the Adjusted R-squared falls by about 17%.
#This shows that when forecasting FSI, the FSI is heavily influenced by previous year's FSI value.
#This is true because FSI changes with time and demonstrates a time-series relationship.
#Therefore, when forecasting FSI, it is important to factor in previous year's value.

#Check diagnostic plots
par(mfrow = c(2,2))
plot(m3)
par(mfrow = c(1,1))
#No major issues in the diagnostic plots here, lets see if there is multicollinearity issue

#Check VIF
vif(m3)
#No obvious multicollinearity problem here as all the vif values are relatively small.
#Some form of multicollinearity will be present, but not much in this case.

#The final model chosen will be:
trainmodel <- lm(formula = `FSI (Year+1)` ~ FSI + Voice.Accountability + `No violence Index` + 
                   CorruptionPI + `Human Development Index` + Life_Satisfaction, 
                 data = trainset)
#Regression Equation:
#`FSI (Year+1)` ~ FSI + Voice.Accountability + `No violence Index` + 
 # CorruptionPI + `Human Development Index` + Life_Satisfaction
summary(trainmodel)
#Our model now has 97.9% R-squared and Adjusted R-squared of 97.9%, a relatively strong number.
#If we consider present year's FSI to predict the next year's FSI in developed countries:
#1: FSI
#2: Human Development Index
#3: No violence Index


# Residuals = Error = Actual FSI(t+1) - Model Predicted FSI(t+1)
RMSE.trainmodel <- sqrt(mean(residuals(trainmodel)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(trainmodel)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(trainmodel, newdata = testset)
testset.error <- testset$`FSI (Year+1)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 

#Visualise our predictions

least_developed_prediction <- data.frame(Predicted = predict.test,  # Create data for ggplot2
                                   Actual = testset$`FSI (Year+1)`)
# Draw plot using ggplot2 package
least_developed_plot <- ggplot(least_developed_prediction,                                     
                         aes(x = Predicted,
                             y = Actual)) +
  ggtitle("Least developed Countries FSI(t+1)") +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
least_developed_plot
#RMSE for train is 1.665759, while RMSE for test is 1.897411
#Given the range of FSI of 0-120, there is approximately 1.39% error on the trainset and 1.58% error on testset on average
#In general however, given any random set of data, actual FSI(Y+1) will differ from forecasted FSI(Y+1) by 1.90
#for least developed countries.
#This shows that the model to forecast FSI 1 year into the future is quite strong.
#This is supported by the visualisation plots, which show that the actual values lie closely
#to the forecasted FSI(Y+1) represented by the red line.

#===============================================================================

#Verify the model for least developed countries using CART

#Data: leastdeveloped_cart1
#===============================================================================

#For CART, we want to find out, how well our predictions can be in the absence of
#previous year's FSI data, that is what truly affects a political stability of undeveloped countries
#In the absence of FSI data, what is the best prediction CART can come up with?

#For CART, missing values are handled via surrogates & outliers will tend towards the leaf node
#Therefore, we can skip the step of removing outliers/NA
cart1_leastdeveloped <- leastdeveloped_cart1[,c(3:13,15)]
#Step 1: Train-test split for CART

set.seed(2004)
train <- sample.split(Y = cart1_leastdeveloped$`Fragility_Category (Year+1)`, SplitRatio = 0.7)
trainset <- subset(cart1_leastdeveloped, train == T)
testset <- subset(cart1_leastdeveloped, train == F)

set.seed(2004)
cart1 <- rpart(`Fragility_Category (Year+1)` ~ . -FSI ,data = trainset,
               method = 'class', control = rpart.control(minsplit = 7, cp = 0))
par(mfrow = c(1,1))
rpart.plot(cart1, nn=T, tweak=1.5, main = "Maximal Tree for Fragility_Category (Year+1) for least developed countries")

print(cart1)

printcp(cart1)

plotcp(cart1)

#Extract the Optimal Tree 
# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1) #calculate geometric mean here

cart2 <- prune(cart1, cp = cp.opt)

rpart.plot(cart2, nn=T, fallen.leaves = FALSE, box.palette = "RdYlGn", tweak= 1.3, main = "Optimal Tree for Fragility_Category (Year+1) for least developed countries")

prp(cart2) #to get a better view of the decision tree

print(cart2)

printcp(cart2)

summary(cart2)

scaledVarImpt <- round(100*cart2$variable.importance/sum(cart2$variable.importance))
scaledVarImpt[scaledVarImpt > 0]
#CART identifies the most important variables to predict FSI(Year+1) as:
# 1: Voice and Accountability 19%
# 2: No violence Index 16%
# 3: Rule of Law 14%
# 4: Human Development Index 12%
# 5: GDP Per Capita & Aid Per Capita 10%

#Lets see how well our model performs
cart.predict <- predict(cart2, newdata = testset, type = "class")

table <- table(Testset.Actual = testset$`Fragility_Category (Year+1)`, cart.predict, deparse.level = 2)
table

# Overall Accuracy
mean(cart.predict == testset$`Fragility_Category (Year+1)`)

#Model accuracy is 64.8%, on average there will be 35.2% classification error.



#====================================================================================

#Develop a model to forecast FSI 1 year ahead for transitioning economies using linear regression

#Data: transitioning_linreg1
#====================================================================================

#Step 1: Identify potential outliers and remove them
#In this step, we use diagnostic plots to identify potential outliers


#In this step, we use diagnostic plots to identify potential outliers

m1 <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year`, data = transitioning_linreg1)
summary(m1)
#From the initial mod, R-squared and Adjusted R-squared are approximately 95%.
#Can we do better?
#Lets explore whether there are any outliers.
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))
#From residuals vs fitted, there is no pattern in the residual plot, with the red line almost horizontal
#We can assume a linear relationship between FSI(Year+1) and the predictors

#From scale-location, the residuals are spread equally along the range of predictors
#Hence, we can verify that the variances in errors are constant.

#From normal Q-Q, we can see the residuals, all fall approximately on th reference line

#From residuals vs leverage, all points lie safely within the Cook's distance lines, 

#All the residuals seem to fit nicely, satisying the linear regression assumptions
#Some minor issues with points 128,64 but not major, lets see if removing them does improve our model
transitioning_linreg2 <- transitioning_linreg1[-c(64,128),]
m2 <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year`, data = transitioning_linreg2)
summary(m2)

#Lets re-examine the diagnostic plots
par(mfrow = c(2,2))
plot(m2)
par(mfrow = c(1,1))
#our model accuracy improved and the minor issues appearing in the previous model are resolved.

#Step 2: Train-test split
#We will now do train-test split on the dataset, after checking for outliers
#Data to be used here: transitioning_linreg2

set.seed(2004)
train <- sample.split(Y = transitioning_linreg2$`FSI (Year+1)`, SplitRatio = 0.7)
trainset <- subset(transitioning_linreg2, train == T)
testset <- subset(transitioning_linreg2, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$`FSI (Year+1)`)
summary(testset$`FSI (Year+1)`)
#distribution are relatively similar, we can proceed.

#Step 3: Feature selection using backward elimination, VIF & Domain Knowledge

#Training our model & feature selection
m3 <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year`, data = trainset)
summary(m3) #initial train model 

#Perform backward elimination remove unimportant variables
m.full <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year`, data = trainset)
m4 <- step(m.full)
summary(m4)

#what if we remove FSI of the current year? 
m.full5 <- lm(`FSI (Year+1)` ~ . - `Country Name` - `Year` - FSI, data = trainset)
m5 <- step(m.full5)
summary(m5)
#Without previous year's FSI, the importance of variables to predict FSI for transitioning economies can be ranked as:
# 1: Voice & Accountability
# 2: No violence index
# 3: Human Development Index
# 4: Corruption Perception Index

#If we exclude FSI of the current year and generate a model, the Adjusted R-squared falls by about 35%.
#This shows that when forecasting FSI, the FSI is heavily influenced by previous year's FSI value.
#This is true because FSI changes with time and demonstrates a time-series relationship.
#Therefore, when forecasting FSI, it is important to factor in previous year's value.

#Check diagnostic plots
par(mfrow = c(2,2))
plot(m4)
par(mfrow = c(1,1))
#No major issues in the diagnostic plots here, lets see if there is multicollinearity issue

#Check VIF
vif(m4)
#No obvious multicollinearity problem here as all the vif values are relatively small.
#Some form of multicollinearity will be present, but not much in this case.

#The final model chosen will be:
trainmodel <- lm(formula = `FSI (Year+1)` ~ FSI + Voice.Accountability + `Human Development Index` + 
                   Life_Satisfaction + `Unemployment Rate` + `Aid per capita`, 
                 data = trainset)

#Regression Equation:
# `FSI (Year+1)` ~ FSI + Voice.Accountability + `Human Development Index` + 
#                 Life_Satisfaction + `Unemployment Rate` + `Aid per capita`
summary(trainmodel)
#Our model now has 97.5% R-squared and Adjusted R-squared of 97.3%, a relatively strong number.
#If we consider present year's FSI to predict the next year's FSI in transitioning economies:
#1: FSI
#2: Life Satisfaction

# Residuals = Error = Actual FSI(t+1) - Model Predicted FSI(t+1)
RMSE.trainmodel <- sqrt(mean(residuals(trainmodel)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(trainmodel)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(trainmodel, newdata = testset)
testset.error <- testset$`FSI (Year+1)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 

#Visualise our predictions

transitioning_prediction <- data.frame(Predicted = predict.test,  # Create data for ggplot2
                                   Actual = testset$`FSI (Year+1)`)
# Draw plot using ggplot2 package
transitioning_plot <- ggplot(transitioning_prediction,                                     
                         aes(x = Predicted,
                             y = Actual)) +
  ggtitle("Transitioning Economies FSI(t+1)") +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
transitioning_plot
#RMSE for train is 1.167937, while RMSE for test is 1.407306
#Given the range of FSI of 0-120, there is approximately 0.97% error on the trainset and 1.17% error on testset on average
#In general however, given any random set of data, actual FSI(Y+1) will differ from forecasted FSI(Y+1) by 1.407306
#This shows that the model to forecast FSI 1 year into the future is quite strong for transitioning economies.
#This is supported by the visualisation plots, which show that the actual values lie closely
#to the forecasted FSI(Y+1) represented by the red line.

#===============================================================================

#verify our results using CART for transitioning economies

#Data: transitioning_cart1
#===============================================================================

#For CART, we want to find out, how well our predictions can be in the absence of
#previous year's FSI data, that is what truly affects a political stability of transitioning economies
#In the absence of FSI data, what is the best prediction CART can come up with?

#For CART, missing values are handled via surrogates & outliers will tend towards the leaf node
#Therefore, we can skip the step of removing outliers/NA
cart1_transitioning <- transitioning_cart1[,c(3:13,15)]
#Step 1: Train-test split for CART

set.seed(2004)
train <- sample.split(Y = cart1_transitioning$`Fragility_Category (Year+1)`, SplitRatio = 0.7)
trainset <- subset(cart1_transitioning, train == T)
testset <- subset(cart1_transitioning, train == F)

set.seed(2004)
cart1 <- rpart(`Fragility_Category (Year+1)` ~ . -FSI ,data = trainset,
               method = 'class', control = rpart.control(minsplit = 3, cp = 0))
par(mfrow = c(1,1))
rpart.plot(cart1, nn=T, tweak=1.5, main = "Maximal Tree for Fragility_Category (Year+1) for transitioning countries")

print(cart1)

printcp(cart1)

plotcp(cart1)

#Extract the Optimal Tree 
# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1) #calculate geometric mean here

cart2 <- prune(cart1, cp = cp.opt)

rpart.plot(cart2, nn=T, tweak = 1, main = "Optimal Tree for Fragility_Category (Year+1) for transitioning countries")

print(cart2)

printcp(cart2)

summary(cart2)

scaledVarImpt <- round(100*cart2$variable.importance/sum(cart2$variable.importance))
scaledVarImpt[scaledVarImpt > 0]
#CART identifies the most important variables to predict FSI(Year+1) for transitioning economies as:
# 1: Rule of Law 17%
# 2: Human Development Index & Voice.Accountability: 16%
# 3: GDP Per Capita 15%
# 4: No violence index 14%


#Lets see how well our model performs
cart.predict <- predict(cart2, newdata = testset, type = "class")

table <- table(Testset.Actual = testset$`Fragility_Category (Year+1)`, cart.predict, deparse.level = 2)
table
#There is a data point that is high warning but predicted as less stable,
#Another 2 data points is at elevated warning but predicted only as less stable.
#There are 3 serious misclassifications.

# Overall Accuracy
mean(cart.predict == testset$`Fragility_Category (Year+1)`)

#For transitioning economies, the model accuracy is only 60% which is not the best.
#CART may not be suitable for this particular economy type.

#===============================================================================

#For forecasting 4 years ahead, we decide to go with linear regression
#As seen in the linear regressions model, the linear regressions assumptions are
#mostly satisfied and a strong model can be observed and be used to make predictions

#===============================================================================

#===============================================================================

#Developing a linear regression model for developed economies to predict FSI(Y+4)

#Data: developed_countries_linreg4
#===============================================================================

#Step 1: Identify potential outliers and remove them

#In this step, we use diagnostic plots to identify potential outliers

m1 <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year` , data = developed_countries_linreg4)
summary(m1)
#From the initial mod, R-squared and Adjusted R-squared are approximately 98%.
#Can we do better?
#Lets explore whether there are any outliers.
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))
#From residuals vs fitted, there is no pattern in the residual plot, with the red line almost horizontal
#We can assume a linear relationship between FSI(Year+1) and the predictors

#From scale-location, the residuals are spread equally along the range of predictors
#Hence, we can verify that the variances in errors are constant.

#From normal Q-Q, we can see the residuals, all fall approximately on the reference line
#Some points are bit far off, but not a major cause of concern

#From residuals vs leverage, all points lie safely within the Cook's distance lines, 
#No significant influential outliers

#No outliers need to be removed, after examining the residual plots.
#However point 139 seems a bit far off from the trend, lets examine if removing it will help our model.
developed_countries_linreg4b <- developed_countries_linreg4[-c(139),]
m2 <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year` , data = developed_countries_linreg4b)
summary(m2)

#Lets re-examine the diagnostic plots
par(mfrow = c(2,2))
plot(m2)
par(mfrow = c(1,1))
#All the residuals seem to fit nicely now. R-squared and Adjusted R-squared has improved by ~0.5%.

#Step 2: Train-test split
#We will now do train-test split on the dataset, after checking for outliers
#Data to be used here: developed_countries_linreg4b

set.seed(2004)
train <- sample.split(Y = developed_countries_linreg4b$`FSI (Year+4)`, SplitRatio = 0.7)
trainset <- subset(developed_countries_linreg4b, train == T)
testset <- subset(developed_countries_linreg4b, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$`FSI (Year+4)`)
summary(testset$`FSI (Year+4)`)
#distribution are relatively similar, we can proceed.

#Step 3: Feature selection using backward elimination, VIF & Domain Knowledge

#Training our model & feature selection
m3 <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year`, data = trainset)
summary(m3) #initial train model

#Perform backward elimination remove unimportant variables
m.full <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year`, data = trainset)
m4 <- step(m.full)
summary(m4)


#what if we remove FSI of the current year? 
m.full5 <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year` - FSI, data = trainset)
m5 <- step(m.full5)
summary(m5)
#If we exclude FSI of the current year and generate a model, the Adjusted R-squared falls by about 12%
#This shows that when forecasting FSI, the FSI is heavily influenced by previous year's FSI value.
#This is true because FSI changes with time and demonstrates a time-series relationship.
#Therefore, when forecasting FSI, it is important to factor in previous year's value.

#Without previous year's FSI, the importance of variables for developed countries can be ranked as:
# 1: No violence Index
# 2: Rule of Law
# 3: Human Development Index
# 4: Voice & Accountability

#Check diagnostic plots
par(mfrow = c(2,2))
plot(m4)
par(mfrow = c(1,1))
#No major issues in the diagnostic plots here, lets see if there is multicollinearity issue

#Check VIF
vif(m4)
#No obvious multicollinearity problem here as all the vif values are relatively small.
#Some form of multicollinearity will be present, but not much in this case.

#The final model chosen will be:
trainmodel <- lm(formula = `FSI (Year+4)` ~ FSI + `Rule of Law` + Voice.Accountability + 
                   `No violence Index` + CorruptionPI + `Human Development Index` + 
                   Life_Satisfaction + `Unemployment Rate`, data = trainset)


#Regression Equation:
#`FSI (Year+4)` ~ FSI + `Rule of Law` + Voice.Accountability + 
#  `No violence Index` + CorruptionPI + `Human Development Index` + 
#   Life_Satisfaction + `Unemployment Rate`
summary(trainmodel)
#Our model now has 95.0% R-squared and Adjusted R-squared of 94.8% which indicates
#a strong model in forecasting FSI(Y+4) using current year's data
#If we consider present year's FSI to predict the next year's FSI in developed countries:
#1: FSI
#2: No violence Index
#3: Voice & Accountability

# Residuals = Error = Actual FSI(t+1) - Model Predicted FSI(t+1)
RMSE.trainmodel <- sqrt(mean(residuals(trainmodel)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(trainmodel)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(trainmodel, newdata = testset)
testset.error <- testset$`FSI (Year+4)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 

#Visualise our predictions

developed_prediction4 <- data.frame(Predicted = predict.test,  # Create data for ggplot2
                                   Actual = testset$`FSI (Year+4)`)
# Draw plot using ggplot2 package
developed_plot4 <- ggplot(developed_prediction4,                                     
                         aes(x = Predicted,
                             y = Actual)) +
  ggtitle("Developed Economies FSI(t+4)") +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
developed_plot4
#RMSE for train is 2.574661, while RMSE for test is 3.27923
#Given the range of FSI of 0-120, there is approximately 2.15% error on the trainset and 2.73% error on testset on average
#In general however, given any random set of data, actual FSI(Y+4) will differ from forecasted FSI(Y+4) by 3.2793
#This shows that the model to forecast FSI 4 year into the future is quite strong.
#This is supported by the visualisation plots, which show that the actual values lie closely
#to the forecasted FSI(Y+4) represented by the red line.

#===============================================================================

#Developing a linear regression model for developing economies to predict FSI(Y+4)

#Data: developing_countries_linreg4
#===============================================================================

#Step 1: Identify potential outliers and remove them

#In this step, we use diagnostic plots to identify potential outliers

m1 <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year` , data = developing_countries_linreg4)
summary(m1)
#From the initial mod, R-squared and Adjusted R-squared are approximately 95%
#Can we do better?
#Lets explore whether there are any significant outliers.
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))
#From residuals vs fitted, there is no pattern in the residual plot, with the red line almost horizontal
#We can assume a linear relationship between FSI(Year+1) and the predictors

#From scale-location, the residuals are spread equally along the range of predictors
#Hence, we can verify that the variances in errors are constant.

#From normal Q-Q, we can see the residuals, all fall approximately on the reference line

#From residuals vs leverage, all points lie safely within the Cook's distance lines, 
#No significant influential outliers

#No outliers need to be removed, after examining the residual plots.
#The assumptions of linear regression are satisfied.

#Step 2: Train-test split
#We will now do train-test split on the dataset, after checking for outliers
#Data to be used here: developing_countries_linreg4

set.seed(2004)
train <- sample.split(Y = developing_countries_linreg4$`FSI (Year+4)`, SplitRatio = 0.7)
trainset <- subset(developing_countries_linreg4, train == T)
testset <- subset(developing_countries_linreg4, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$`FSI (Year+4)`)
summary(testset$`FSI (Year+4)`)
#distribution are relatively similar, we can proceed.

#Step 3: Feature selection using backward elimination, VIF & Domain Knowledge

#Training our model & feature selection
m2 <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year`, data = trainset)
summary(m2) #initial train model

#Perform backward elimination remove unimportant variables
m.full <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year`, data = trainset)
m3 <- step(m.full)
summary(m3)


#what if we remove FSI of the current year? 
m.full4 <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year` - FSI, data = trainset)
m4 <- step(m.full4)
summary(m4)
#If we exclude FSI of the current year and generate a model, the Adjusted R-squared falls by about 14%
#This shows that when forecasting FSI, the FSI is heavily influenced by previous year's FSI value.
#This is true because FSI changes with time and demonstrates a time-series relationship.
#Therefore, when forecasting FSI, it is important to factor in previous year's value.

#Without previous year's FSI, the importance of variables for developing countries can be ranked as:
# 1: No violence Index
# 2: Human Development Index
# 3: Voice & Accountability
# 4: Aid Per Capita
# 5: GDP Per Capita

#Check diagnostic plots
par(mfrow = c(2,2))
plot(m3)
par(mfrow = c(1,1))
#No major issues in the diagnostic plots here, lets see if there is multicollinearity issue

#Check VIF
vif(m3)
#No obvious multicollinearity problem here as all the vif values are relatively small.
#Some form of multicollinearity will be present, but not much in this case.

#The final model chosen will be:
trainmodel <- lm(formula = `FSI (Year+4)` ~ FSI + Voice.Accountability + `No violence Index` + 
                   `Human Development Index` + `Unemployment Rate`, data = trainset)

#Regression Equation:
#`FSI (Year+4)` ~ FSI + Voice.Accountability + `No violence Index` + 
#  `Human Development Index` + `Unemployment Rate`
summary(trainmodel)
#Our model now has 96.7% R-squared and Adjusted R-squared of 96.5 which indicates
#a strong model in forecasting FSI(Y+4) using current year's data
#If we consider present year's FSI to predict the next year's FSI in developed countries:
#1: FSI
#2: No violence Index
#3: Human Development Index
#4: Voice and Accountability
#5: Unemployment Rate
# Residuals = Error = Actual FSI(Y+4) - Model Predicted FSI(Y+4)
RMSE.trainmodel <- sqrt(mean(residuals(trainmodel)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(trainmodel)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(trainmodel, newdata = testset)
testset.error <- testset$`FSI (Year+4)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 

#Visualise our predictions

developing_prediction4 <- data.frame(Predicted = predict.test,  # Create data for ggplot2
                                    Actual = testset$`FSI (Year+4)`)
# Draw plot using ggplot2 package
developing_plot4 <- ggplot(developing_prediction4,                                     
                          aes(x = Predicted,
                              y = Actual)) +
  ggtitle("Developing Countries FSI(t+4)") +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
developing_plot4
#RMSE for train is 2.998283, while RMSE for test is 2.984019
#Given the range of FSI of 0-120, there is approximately 2.50% error on the trainset and 2.49% error on testset on average
#In general however, given any random set of data, actual FSI(Y+4) will differ from forecasted FSI(Y+4) by 2.98
#This shows that the model to forecast FSI 4 year into the future is quite strong.
#This is supported by the visualisation plots, which show that the actual values lie closely
#to the forecasted FSI(Y+4) represented by the red line.
#Let's try to predict Argentina's FSI in 2020 based on 2016 data. 
#We can do this because our model did not "see" the actual 2020 data.
#Argentina's actual FSI in 2020 is 46.1 which falls in the Stable Range
FSI_data <- fread("FSI_data.csv")
argentina_fsi_2020_linreg <- predict(trainmodel,FSI_data[Year == '2016' & `Country Name` == "Argentina"])
argentina_fsi_2020_linreg
#The model predicts that Argentina's FSI in 2020 to be 48.1 which falls in the stable range as well.
#This forecast is reasonably accurate.

#===============================================================================

#Developing a linear regression model for least developed economies to predict FSI(Y+4)

#Data: leastdeveloped_linreg4
#===============================================================================

#Step 1: Identify potential outliers and remove them

#In this step, we use diagnostic plots to identify potential outliers

m1 <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year` , data = leastdeveloped_linreg4)
summary(m1)
#From the initial mod, R-squared is 92.4% and Adjusted R-squared is approximately 92.2%
#Can we do better?
#Lets explore whether there are any outliers.
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))
#From residuals vs fitted, there is no pattern in the residual plot, with the red line almost horizontal
#We can assume a linear relationship between FSI(Year+1) and the predictors

#From scale-location, the residuals are spread equally along the range of predictors
#Hence, we can verify that the variances in errors are constant.

#From normal Q-Q, we can see the residuals, all fall approximately on the reference line
#Some points are bit far off, but not a major cause of concern

#From residuals vs leverage, all points lie safely within the Cook's distance lines, 
#No significant influential outliers

#No influential outliers need to be removed, after examining the residual plots.


#Step 2: Train-test split
#We will now do train-test split on the dataset, after checking for outliers
#Data to be used here: leastdeveloped_linreg4

set.seed(2004)
train <- sample.split(Y = leastdeveloped_linreg4$`FSI (Year+4)`, SplitRatio = 0.7)
trainset <- subset(leastdeveloped_linreg4, train == T)
testset <- subset(leastdeveloped_linreg4, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$`FSI (Year+4)`)
summary(testset$`FSI (Year+4)`)
#distribution are relatively similar, we can proceed.

#Step 3: Feature selection using backward elimination, VIF & Domain Knowledge

#Training our model & feature selection
m2 <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year`, data = trainset)
summary(m2) #initial train model

#Perform backward elimination remove unimportant variables
m.full <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year`, data = trainset)
m3 <- step(m.full)
summary(m3)


#what if we remove FSI of the current year? 
m.full4 <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year` - FSI, data = trainset)
m4 <- step(m.full4)
summary(m4)
#If we exclude FSI of the current year and generate a model, the Adjusted R-squared falls by about 11%
#This shows that when forecasting FSI, the FSI is heavily influenced by previous year's FSI value.
#This is true because FSI changes with time and demonstrates a time-series relationship.
#Therefore, when forecasting FSI, it is important to factor in previous year's value.

#Without previous year's FSI, the importance of variables for least developed countries can be ranked as:
# 1: Voice & Accountability, No violence Index
# 2: Human Development Index

#Check diagnostic plots
par(mfrow = c(2,2))
plot(m3)
par(mfrow = c(1,1))
#No major issues in the diagnostic plots here, lets see if there is multicollinearity issue

#Check VIF
vif(m3)
#No obvious multicollinearity problem here as all the vif values are relatively small.
#Some form of multicollinearity will be present, but not much in this case.

#The final model chosen will be:
trainmodel <- lm(formula = `FSI (Year+4)` ~ FSI + Voice.Accountability + `No violence Index` + 
                   CorruptionPI + `Human Development Index` + Healthcare_Exp + 
                   `Unemployment Rate`, data = trainset)


#Regression Equation:
#`FSI (Year+4)` ~ FSI + Voice.Accountability + `No violence Index` + 
#      CorruptionPI + `Human Development Index` + Healthcare_Exp + 
#     `Unemployment Rate`
summary(trainmodel)
#Our model now has 93.1% R-squared and Adjusted R-squared of 92.9% which indicates
#a strong model in forecasting FSI(Y+4) using current year's data
#If we consider present year's FSI to predict the next year's FSI in developed countries:
#1: FSI
#2: Human Development Index
#3: No violence Index
#4: Voice & Accountability
#5: Corruption Perception Index

# Residuals = Error = Actual FSI(Y+4) - Model Predicted FSI(Y+4)
RMSE.trainmodel <- sqrt(mean(residuals(trainmodel)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(trainmodel)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(trainmodel, newdata = testset)
testset.error <- testset$`FSI (Year+4)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 

#Visualise our predictions

least_developed_prediction4 <- data.frame(Predicted = predict.test,  # Create data for ggplot2
                                    Actual = testset$`FSI (Year+4)`)
# Draw plot using ggplot2 package
least_developed_plot4 <- ggplot(least_developed_prediction4,                                     
                          aes(x = Predicted,
                              y = Actual)) +
  ggtitle("Least Developed Economies FSI(t+4)") +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
least_developed_plot4
#RMSE for train is 3.007481, while RMSE for test is 3.138622
#Given the range of FSI of 0-120, there is approximately 2.51% error on the trainset and 2.62% error on testset on average
#In general however, given any random set of data, actual FSI(Y+4) will differ from forecasted FSI(Y+4) by 3.138622
#This shows that the model to forecast FSI 4 year into the future is quite strong for least developed countries
#This is supported by the visualisation plots, which show that the actual values lie closely
#to the forecasted FSI(Y+4) represented by the red line.


#===============================================================================

#Developing a linear regression model for transitioning economies to predict FSI(Y+4)

#Data: transitioning_linreg4
#===============================================================================

#Step 1: Identify potential outliers and remove them

#In this step, we use diagnostic plots to identify potential outliers

m1 <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year` , data = transitioning_linreg4)
summary(m1)
#From the initial mod, R-squared is 85.8% and Adjusted R-squared is approximately 84.2%
#Can we do better?
#Lets explore whether there are any outliers.
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))
#From residuals vs fitted, there is no pattern in the residual plot, with the red line almost horizontal
#We can assume a linear relationship between FSI(Year+1) and the predictors

#From scale-location, the residuals are spread equally along the range of predictors
#Hence, we can verify that the variances in errors are constant.

#From normal Q-Q, we can see the residuals, all fall approximately on the reference line
#Some points are bit far off, but not a major cause of concern

#From residuals vs leverage, all points lie safely within the Cook's distance lines, 
#No significant influential outliers

#No influential outliers need to be removed, after examining the residual plots.
#The assumptions of linear regression are met.


#Step 2: Train-test split
#We will now do train-test split on the dataset, after checking for outliers
#Data to be used here: leastdeveloped_linreg4

set.seed(2004)
train <- sample.split(Y = transitioning_linreg4$`FSI (Year+4)`, SplitRatio = 0.7)
trainset <- subset(transitioning_linreg4, train == T)
testset <- subset(transitioning_linreg4, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$`FSI (Year+4)`)
summary(testset$`FSI (Year+4)`)
#distribution are relatively similar, we can proceed.

#Step 3: Feature selection using backward elimination, VIF & Domain Knowledge

#Training our model & feature selection
m2 <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year`, data = trainset)
summary(m2) #initial train model

#Perform backward elimination remove unimportant variables
m.full <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year`, data = trainset)
m3 <- step(m.full)
summary(m3)


#what if we remove FSI of the current year? 
m.full4 <- lm(`FSI (Year+4)` ~ . - `Country Name` - `Year` - FSI, data = trainset)
m4 <- step(m.full4)
summary(m4)
#If we exclude FSI of the current year and generate a model, the Adjusted R-squared falls by about 17%
#This shows that when forecasting FSI, the FSI is heavily influenced by previous year's FSI value.
#This is true because FSI changes with time and demonstrates a time-series relationship.
#Therefore, when forecasting FSI, it is important to factor in previous year's value.

#Without previous year's FSI, the importance of variables for transitioning economies can be ranked as:
# 1: Voice & Accountability
# 2: No violence Index
# 3: Corruption Perception Index


#Check diagnostic plots
par(mfrow = c(2,2))
plot(m3)
par(mfrow = c(1,1))
#No major issues in the diagnostic plots here, lets see if there is multicollinearity issue

#Check VIF
vif(m3)
#No obvious multicollinearity problem here as all the vif values are relatively small.
#Some form of multicollinearity will be present, but not much in this case.

#The final model chosen will be:
trainmodel <- lm(formula = `FSI (Year+4)` ~ FSI + Voice.Accountability + `No violence Index` + 
                   CorruptionPI + Life_Satisfaction, data = trainset)


#Regression Equation:
# `FSI (Year+4)` ~ FSI + Voice.Accountability + `No violence Index` + 
#               CorruptionPI + Life_Satisfaction
summary(trainmodel)
#Our model now has 89.5% R-squared and Adjusted R-squared of 88.8% which indicates
#a strong model in forecasting FSI(Y+4) using current year's data
#If we consider present year's FSI to predict the next year's FSI in developed countries:
#1: FSI
#2: Life Satisfaction
#3: Voice & Accountability

# Residuals = Error = Actual FSI(Y+4) - Model Predicted FSI(Y+4)
RMSE.trainmodel <- sqrt(mean(residuals(trainmodel)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(trainmodel)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(trainmodel, newdata = testset)
testset.error <- testset$`FSI (Year+4)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 
MAPE(predict.test, testset$`FSI (Year+4)`)

#Visualise our predictions

transitioning_prediction4 <- data.frame(Predicted = predict.test,  # Create data for ggplot2
                                          Actual = testset$`FSI (Year+4)`)
# Draw plot using ggplot2 package
transitioning_plot4 <- ggplot(transitioning_prediction4,                                     
                                aes(x = Predicted,
                                    y = Actual)) +
  ggtitle("Transitioning economies FSI (t+4)") +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
transitioning_plot4
#RMSE for train is 2.407132, while RMSE for test is 3.441937
#Given the range of FSI of 0-120, there is approximately 2% error on the trainset and 2.87% error on testset on average
#In general however, given any random set of data, actual FSI(Y+4) will differ from forecasted FSI(Y+4) by 3.44
#This shows that the model to forecast FSI 4 year into the future is quite strong for least developed countries
#This is supported by the visualisation plots, which show that the actual values lie closely
#to the forecasted FSI(Y+4) represented by the red line.

#===============================================================================

#Exploring the possibility of a general CART model for Fragility_Category (Year+4)

#Data: FSI_cart4
#===============================================================================

#We will be using categorical CART, that is grouping the FSI into different categories
#in terms of fragility.

#For CART, missing values are handled via surrogates & outliers will tend towards the leaf node
#Therefore, we can skip the step of removing outliers/NA
cart4_fragility <- FSI_cart4[,c(3:13,15,16)]
#Step 1: Train-test split for CART

set.seed(2004)
train <- sample.split(Y = cart4_fragility$`Fragility_Category (Year+4)`, SplitRatio = 0.7)
trainset <- subset(cart4_fragility, train == T)
testset <- subset(cart4_fragility, train == F)

set.seed(2004)
cart1 <- rpart(`Fragility_Category (Year+4)` ~ . ,data = trainset,
               method = 'class', control = rpart.control(minsplit = 13, cp = 0))
par(mfrow = c(1,1))
rpart.plot(cart1, nn=T, tweak=1.5, main = "Maximal Tree for Fragility_Category (Year+4)")

print(cart1)

printcp(cart1)

plotcp(cart1)

#Extract the Optimal Tree 
# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1) #calculate geometric mean here

cart2 <- prune(cart1, cp = cp.opt)

rpart.plot(cart2, nn=T, tweak = 1.5, fallen.leaves=FALSE, box.palette = "RdYlGn",main = "Optimal Tree for Fragility_Category (Year+4)")

print(cart2)

printcp(cart2)

summary(cart2)

scaledVarImpt <- round(100*cart2$variable.importance/sum(cart2$variable.importance))
scaledVarImpt[scaledVarImpt > 0]
#CART identifies the most important variables to predict FSI(Year+4) as:
# 1: FSI 27%
# 2: Human Development Index 14%
# 3: Rule of Law 13%
# 4: Voice & Accountability 12%
# 5: GDP Per Capita 12%

#Lets see how well our model performs
cart.predict <- predict(cart2, newdata = testset, type = "class")

table <- table(Testset.Actual = testset$`Fragility_Category (Year+4)`, cart.predict, deparse.level = 2)
table
#No serious misclassification error, which improves credibility of the model.

# Overall Accuracy
mean(cart.predict == testset$`Fragility_Category (Year+4)`)

#By adding a control variable for the type of economy, the model predicts reasonably well with 74.7% accuracy.

#Let's try to predict Argentina's FSI in 2020 based on 2016 data. 
#We can do this because our model did not "see" the actual 2020 data.
#Argentina's actual FSI in 2020 is 46.1 which falls in the Stable Range
#extract Argentina and set it to be a Developing Country
argentina_fsi_2016 <- FSI_data[Year == '2016' & `Country Name` == "Argentina"]
argentina_fsi_2016$economy_type = "Developing"
factor(argentina_fsi_2016$economy_type)
argentina_fsi_2020_cart <- predict(cart2,argentina_fsi_2016,type = "class" )
argentina_fsi_2020_cart
#Likewise, CART predicts that 2020 FSI for Argentina will be Stable.

