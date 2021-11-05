# ========================================================================================================
# Purpose:      Linear Regression & CART analysis for GDP per Capita PPP Analysis
# Data Source:  Cleaned CSV data available under "Datasets Cleaned"
# Packages:     ggplot2, data.table, caTools, rpart, rpart.plot, car, dplyr, corrplot, vtable, Hmisc, xtable, MLmetrics
#=========================================================================================================


#run libraries needed for regression analysis, to be updated as we go...
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

#Importing datasets for GDP Per Capita PPP Regression Analysis

#===============================================================================


allcountries.linreg <- fread("allcountries.linreg.csv")

developedcountries.linreg <- fread("developedcountries.linreg.csv")
developingcountries.linreg <- fread("developingcountries.linreg.csv")
leastdeveloped.linreg <- fread("leastdeveloped.linreg.csv")
transitioningcountries.linreg <- fread("transitioningcountries.linreg.csv")

developedcountries.cart <- fread("developedcountries.cart.csv")
developingcountries.cart <- fread("developingcountries.cart.csv")
leastdeveloped.cart <- fread("leastdeveloped.cart.csv")
transitioningcountries.cart <- fread("transitioningcountries.cart.csv")

#convert major.event to a factor variable
allcountries.linreg$major.event <- factor(allcountries.linreg$major.event)

developedcountries.linreg$major.event <- factor(developedcountries.linreg$major.event)
developingcountries.linreg$major.event <- factor(developingcountries.linreg$major.event)
leastdeveloped.linreg$major.event <- factor(leastdeveloped.linreg$major.event)
transitioningcountries.linreg$major.event <- factor(transitioningcountries.linreg$major.event)

developedcountries.cart$major.event <- factor(developedcountries.cart$major.event)
developingcountries.cart$major.event <- factor(developingcountries.cart$major.event)
leastdeveloped.cart$major.event <- factor(leastdeveloped.cart$major.event)
transitioningcountries.cart$major.event <- factor(transitioningcountries.cart$major.event)

#===============================================================================

#Data Visualisation and Exploration
#Exploring the relationship of GDP Per Capita PPP(Y+1) ~ Other Factors(Y)
#Note: need to use zoom function for better view
#===============================================================================
#summary statistics
st(developedcountries.cart[,-c(1,2)],title="Developed Economies GDP Per Capita PPP(Year+1)")
st(developingcountries.cart[,-c(1,2)],title="Developing Economies GDP Per Capita PPP(Year+1)")
st(leastdeveloped.cart[,-c(1,2)],title="Least Developed Economies GDP Per Capita PPP(Year+1)")
st(transitioningcountries.cart[,-c(1,2)],title="Transitioning Economies GDP Per Capita PPP(Year+1)")

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
print(htmlTable::htmlTable((corstars(developedcountries.linreg[,-c(1,2)],result = "html")),useViewer=utils::browseURL))
print(htmlTable::htmlTable((corstars(developingcountries.linreg[,-c(1,2)],result = "html")),useViewer=utils::browseURL))
print(htmlTable::htmlTable((corstars(leastdeveloped.linreg[,-c(1,2)],result = "html")),useViewer=utils::browseURL))
print(htmlTable::htmlTable((corstars(transitioningcountries.linreg[,-c(1,2)],result = "html")),useViewer=utils::browseURL))


#Visualise the plots, to explore the relationship between forecasting GDP Per Capita PPP 1 year ahead
#and the independent variables of the current year
corrplot(cor(allcountries.linreg[,c(4:19,21)]),
         main = "All Countries Correlation Plot, 1 year ahead forecast",cex.main = 0.9,
         mar = c(0,0,2,0))
corrplot(cor(developedcountries.linreg[,c(4:19,21)]),
         main = "Developed Countries Correlation Plot, 1 year ahead forecast",cex.main = 0.9,
         mar = c(0,0,2,0))
corrplot(cor(developingcountries.linreg[,c(4:19,21)]),
         main = "Developing Countries Correlation Plot, 1 year ahead forecast",cex.main = 0.9,
         mar = c(0,0,2,0))
corrplot(cor(leastdeveloped.linreg[,c(4:19,21)]),
         main = "Least Developed Countries Correlation Plot, 1 year ahead forecast",cex.main = 0.9,
         mar = c(0,0,2,0))
corrplot(cor(transitioningcountries.linreg[,c(4:19,21)]),
         main = "Transitioning Countries Correlation Plot, 1 year ahead forecast",cex.main = 0.9,
         mar = c(0,0,2,0))


#===============================================================================

#General case: Build a linear regression model for all countries in general

#Data: allcountries.linreg
#===============================================================================

m1 <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP` , data = allcountries.linreg)
summary(m1)

#Lets explore whether there are any outliers
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))

#In this case, the residuals vs fitted show that, the points have a curvilinear relationship.
#Furthermore, in the Normal Q-Q plot, there are many points that lie far away from the general trend.
#Overall, the linear regression assumptions are not met.
#It is difficult to generalise by ALL countries, as economy sizes differ, hence, we try to segment
#the world into 4 different groups and conduct a separate analysis to draw more meaningful insights.
#There is a need to introduce some CONTROL into the linear regression model.

#===============================================================================

#Building a model for developed countries using linear regression

#Data: developedcountries.linreg
#===============================================================================

#Step 1: Identify potential outliers and remove them

#In this step, we use diagnostic plots to identify potential outliers

m1 <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP` 
         , data = developedcountries.linreg)
summary(m1)
#From the initial model, R-squared is 91.5%, and the Adjusted R-squared is 91%
#Can we do better?
#Lets explore whether there are any outliers and whether the linear regression assumptions are met.
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))

#From residuals vs fitted, there is no pattern in the residual plot, with the red line almost horizontal
#We can assume a linear relationship between GDP Per Capita(Year+1) and the predictors

#From scale-location, the residuals are spread equally along the range of predictors
#Hence, we can verify that the variances in errors are constant.

#From normal Q-Q, we can see the residuals, all fall approximately on the reference line
#Some points are bit far off(Data points 156,157,158)

#From residuals vs leverage, Data point 16, lies outside the 0.5 Cook's distance line
#Lets see whether our model will improve by removing this point

#In general, data points 156,157 and 158 consistently appear out of trend. So Let's see whether removing them
#Will indeed improve our model

developedcountries.linreg_2 <- developedcountries.linreg[-c(16,156,157,158),]

m2 <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP` 
       , data = developedcountries.linreg_2)
summary(m2)
#R-squared value increases to 92.4% and Adjusted R-squared increases to 91.8%. 
#Removing these points do actually improve our model!

#Lets verify the diagnostic plots
par(mfrow = c(2,2))
plot(m2)
par(mfrow = c(1,1))
#The residual plots now satisfy the assumptions of linear regression. 
#After removing outliers, we can proceed to to the next step.

#Step 2: Train-test split
#We will now do train-test split on the dataset, after checking for outliers
#Data to be used here: #developedcountries.linreg_2

#set seed to make results reproducible
set.seed(2004)
train <- sample.split(Y = developedcountries.linreg_2$`GDP Per Capita PPP (Year+1)`, SplitRatio = 0.7)
trainset <- subset(developedcountries.linreg_2, train == T)
testset <- subset(developedcountries.linreg_2, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$`GDP Per Capita PPP (Year+1)`)
summary(testset$`GDP Per Capita PPP (Year+1)`)
#distribution are relatively similar, we can proceed. Mean and Median are approximately similar.

#Step 3: Feature selection using backward elimination, VIF & Domain Knowledge

#Training our model & feature selection
m3 <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP` , data = trainset)
summary(m3) #initial train model

#Perform backward elimination remove unimportant variables
m.full <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP` , data = trainset)
m4 <- step(m.full)
summary(m4)


#Check VIF
vif(m4)
#VIF for exports of G&S and imports of G&S are extremely high, this means they can be predicted
#using the other independent variables, let's try removing 1 of them to see how our model performs.
#We will eliminate Imports of G&S due to multicollinearity as Imports of G&S has the highest VIF.

m5 <- lm(formula = `GDP Per Capita PPP (Year+1)` ~ `Private Expenditure Consumption (% growth)` + 
           `Exports of G&S (% GDP)` + `Human Development Index` + 
           `Gross Capital Formation` + `Net foreign asset` + `Net FDI (BOP)` + 
           `Total Reserves` + `Inflation Rate` + `Govt Effectiveness` + 
           `Male Adult Mortality Rate` + major.event, data = trainset)
summary(m5) 
#Our model's Adjusted R-squared does not change much, it is still a strong model.
vif(m5)
#After verifying the VIF, the issue of multicollinearity is reduced significantly.
#Some form of multicollinearity is acceptable.


#The final model chosen will be:
trainmodel <- lm(formula = `GDP Per Capita PPP (Year+1)` ~ `Private Expenditure Consumption (% growth)` + 
                   `Exports of G&S (% GDP)` + `Human Development Index` + 
                   `Gross Capital Formation` + `Net foreign asset` + `Net FDI (BOP)` + 
                   `Total Reserves` + `Inflation Rate` + `Govt Effectiveness` + 
                   `Male Adult Mortality Rate` + major.event, data = trainset)

summary(trainmodel)
#Regression Equation:
#`GDP Per Capita PPP (Year+1)` ~ `Private Expenditure Consumption (% growth)` + 
#     `Exports of G&S (% GDP)` + `Human Development Index` + `Gross Capital Formation` + 
#     `Net foreign asset` + `Net FDI (BOP)` + `Total Reserves` + 
#     `Inflation Rate` + `Govt Effectiveness` + `Male Adult Mortality Rate` + major.event

#Our model has 93.1% R-Squared and 92.6% Adjusted R-squared which shows that the accuracy based on trainset data is relatively high.
#What can we conclude based on the model?
#The most significant variables in predicting GDP Per Capita PPP for developed countries are:
#1: Exports of G&S (% GDP)
#2: Human Development Index
#3: Total Reserves
#4: Male Adult Mortality Rate
#5: Net foreign asset
#6: Inflation Rate
#All of which are significant at less than 0.1% significance level

# Residuals = Error = `GDP Per Capita PPP (Year+1)` - `GDP Per Capita PPP (Year+1)`
RMSE.trainmodel <- sqrt(mean(residuals(trainmodel)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(trainmodel)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(trainmodel, newdata = testset)
testset.error <- testset$`GDP Per Capita PPP (Year+1)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 
MAPE(predict.test,testset$`GDP Per Capita PPP (Year+1)`)
#Visualise our predictions

developed_prediction <- data.frame(Predicted = predict.test,  # Create data for ggplot2
                                   Actual = testset$`GDP Per Capita PPP (Year+1)`)
# Draw plot using ggplot2 package
developed_plot <- ggplot(developed_prediction,                                     
                         aes(x = Predicted,
                             y = Actual)) +
  ggtitle("Model for GDP Per Capita PPP(Y+1), developed countries") +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
developed_plot
#RMSE for train is 4086.735, while RMSE for test is 5193.523
#In general, given any random set of data, actual GDP Per Capita(Y+1) will differ from forecasted GDP Per Capita(Y+1) by 5193.523
#There is 14% MAPE which falls under the good forecasting category.
#This is supported by the visualisation plots, which show that the actual values lie closely
#to the forecasted GDP Per Capita(Y+1) represented by the red line.

#===============================================================================

#Building a model for developed countries using CART

#Dataset: developedcountries.cart
#===============================================================================

#Removing columns that won't be used in CART
developed_cart <- developedcountries.cart[,c(4:21)]


#CART automatically handles missing values, and outliers will be pushed to the leaf node.
#Therefore, we can proceed straight to step 2.
#Train test split:

#set seed to get reproducible results
set.seed(2004)
train <- sample.split(Y = developed_cart$`GDP Per Capita PPP (Year+1)`, SplitRatio = 0.7)
trainset <- subset(developed_cart, train == T)
testset <- subset(developed_cart, train == F)

#the optimal minsplit that reduces error here is 6.
set.seed(2004)
cart1 <- rpart(`GDP Per Capita PPP (Year+1)` ~ . ,data = trainset,
               method = 'anova', control = rpart.control(minsplit = 6, cp = 0))
par(mfrow = c(1,1))
rpart.plot(cart1, nn=T, tweak = 0.8, main = "Maximal Tree for GDP Per Capita PPP(Y+1) for Developed Countries")

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
options(scipen=999)
rpart.plot(cart2, nn=T, tweak = 1.4, digits= 4, main = "Optimal Tree for GDP Per Capita PPP (Year+1) for Developed Countries")

print(cart2)

printcp(cart2)

summary(cart2)

scaledVarImpt <- round(100*cart2$variable.importance/sum(cart2$variable.importance))
scaledVarImpt[scaledVarImpt > 0]
#CART identifies the most important variables to predict GDP per capita for developed countries as:
#1: Human Development Index: 18%
#2: Government Effectiveness: 17%
#3: Imports and Exports of G&S (% of GDP), Male Adult Mortality Rate: 11%

# Residuals = Error = `GDP Per Capita PPP (Year+1)` - `GDP Per Capita PPP (Year+1)`
RMSE.trainmodel <- sqrt(mean(residuals(cart2)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(cart2)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(cart2, newdata = testset)
testset.error <- testset$`GDP Per Capita PPP (Year+1)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 
MAPE(predict.test,testset$`GDP Per Capita PPP (Year+1)`)
#In this case, the RMSE for train model is 3023.342 while RMSE for test set is 4701.371
#This means when CART is used to predict, on average the actual GDP Per Capita PPP (Year+1)
#will differ from the forecasted GDP Per Capita PPP (year+1) by 4701.371
#There is a 12% MAPE in this case, once again a good forecasting.
#It seems that CART is slightly better model for developed countries... 
#But with the results comparable, both models can be used in the future for comparison
#To validate future forecasts...


#===============================================================================

#Building a model for developing countries using linear regression

# Data: developingcountries.linreg
#===============================================================================

#Step 1: Identify potential outliers and remove them


#In this step, we use diagnostic plots to identify potential outliers

m1 <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP` 
         , data = developingcountries.linreg)
summary(m1)

#From the initial model, R-squared is 75%, and the Adjusted R-squared is 73.8%.
#Can we do better?
#Lets explore whether there are any outliers
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))

#There are a huge number of points that is causing the residuals to be out of shape
#Here we try to find them.
sort(abs(residuals(m1)), decreasing = T)[1:13]
which(rownames(developingcountries.linreg) %in% names(sort(abs(rstandard(m1)),  decreasing = T)[1:13]))
#We realise that these data points belong to Brunei, Brunei, therefore might not belong to this group of country
#Thus, we will remove "Brunei" data points to see if our model accuracy will improve.

developingcountries.linreg_2 <- developingcountries.linreg[-c(52:63),]

m2 <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP` 
       , data = developingcountries.linreg_2)
summary(m2)
#Model accuracy remains relatively the same.

#Lets verify the diagnostic plots
par(mfrow = c(2,2))
plot(m2)
par(mfrow = c(1,1))

#From residuals vs fitted, there is a slight curvilinear trend being spotted.
#We cannot assume a linear relationship between GDP Per Capita(Year+1) and the predictors

#Furthermore, from scale-location, the residuals are not spread equally along the range of predictors
#It actually increases and there is a general upward trend
#Hence, we cannot verify that the variances in errors are constant.

#From normal Q-Q, we can see the residuals, all fall approximately on the reference line

#From residuals vs leverage, all points lie within the Cook's distance boundary.

#Since, the linear regression assumptions are not fulfilled,
#Linear regression is not the most suited to predict GDP Per Capita (Year+1) for developing countries.
#However, lets proceed to try..

#Lets verify the diagnostic plots to see if the assumptions of linear regression are met.


#Step 2: Train-test split
#We will now do train-test split on the dataset, after checking for outliers
#Data to be used here: #developingcountries.linreg_2

set.seed(2004)
train <- sample.split(Y = developingcountries.linreg_2$`GDP Per Capita PPP (Year+1)`, SplitRatio = 0.7)
trainset <- subset(developingcountries.linreg_2, train == T)
testset <- subset(developingcountries.linreg_2, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$`GDP Per Capita PPP (Year+1)`)
summary(testset$`GDP Per Capita PPP (Year+1)`)
#distribution are relatively similar, we can proceed. Mean and Median are approximately similar.

#Step 3: Feature selection using backward elimination, VIF & Domain Knowledge

#Training our model & feature selection
m3 <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP` , data = trainset)
summary(m3) #initial train model

#Perform backward elimination remove unimportant variables
m.full <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP`, data = trainset)
m4 <- step(m.full)
summary(m4)


#Lets verify the diagnostic plots to see if the linear regression assumptions are satisfied.
par(mfrow = c(2,2))
plot(m4)
par(mfrow = c(1,1))

#The Scale_location graph has a downward and upward trend, the variances are not evenly distributed.

#Check VIF
vif(m4)
#We will remove Female Adult Mortality Rate as it has a high VIF

m5 <- lm(formula = `GDP Per Capita PPP (Year+1)` ~ `Private Expenditure Consumption (% growth)` + 
           `Govt Expenditure Consumption (% GDP)` + `Exports of G&S (% GDP)` + 
           `Imports of G&S (% GDP)` + `Human Development Index` + `Gross Capital Formation` + 
           `Total Reserves` + `Inflation Rate` + 
           `Male Adult Mortality Rate`, data = trainset)
vif(m5)
#multicollinearity issue is resolved

#The final model chosen will be:
trainmodel <- lm(formula = `GDP Per Capita PPP (Year+1)` ~ `Private Expenditure Consumption (% growth)` + 
                   `Govt Expenditure Consumption (% GDP)` + `Exports of G&S (% GDP)` + 
                   `Imports of G&S (% GDP)` + `Human Development Index` + `Gross Capital Formation` + 
                   `Total Reserves` + `Inflation Rate` + 
                   `Male Adult Mortality Rate`, data = trainset)

summary(trainmodel)
#Regression Equation:
# `GDP Per Capita PPP (Year+1)` ~ `Private Expenditure Consumption (% growth)` + 
#`Govt Expenditure Consumption (% GDP)` + `Exports of G&S (% GDP)` + 
#  `Imports of G&S (% GDP)` + `Human Development Index` + `Gross Capital Formation` + 
#  `Total Reserves` + `Inflation Rate` + 
#  `Male Adult Mortality Rate`

#Our model has 69.27% R-Squared and 68.2% Adjusted R-squared.
#What else can we conclude based on the model?
#Lets rank the top factors in terms of p-values: (significance of 0.1% or below)
#1: Exports of G&S(%GDP)/Imports of G&S(%GDP)/Human Development Index
#2: Female Adult Mortality Rate
#3: Govt Expenditure Consumption (% GDP)
#4: `Total Reserves`

# Residuals = Error = `GDP Per Capita PPP (Year+1)` - `GDP Per Capita PPP (Year+1)`
RMSE.trainmodel <- sqrt(mean(residuals(trainmodel)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(trainmodel)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(trainmodel, newdata = testset)
testset.error <- testset$`GDP Per Capita PPP (Year+1)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 
MAPE(predict.test,testset$`GDP Per Capita PPP (Year+1)` )
#Visualise our predictions

developing_prediction <- data.frame(Predicted = predict.test,  # Create data for ggplot2
                                   Actual = testset$`GDP Per Capita PPP (Year+1)`)
# Draw plot using ggplot2 package
developing_plot <- ggplot(developing_prediction,                                     
                         aes(x = Predicted,
                             y = Actual)) +
  ggtitle("GDP Per Capita PPP (Year+1), developing countries") +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
developing_plot
#For developing countries:
#RMSE for train is 5898.52, while RMSE for test is 6782.894
#In general, given any random set of data, actual GDP Per Capita(Y+1) will differ from forecasted GDP Per Capita(Y+1) by 6782.894
#The MAPE is 34% which falls under the reasonable forecasting category,
#like expected the model does not perform very well since the linear regression assumptions are not met...

#Let's try to predict Argentina's GDP Per Capita in 2015 based on 2014 data. 
#We can do this because our model did not "see" the actual 2015 data.
#Argentina's actual GDP per capita PPP in 2015 is 20105.20
#set NAs in that particular row to be zero, to enable us to predict using linear regression.
forecast_gdp <- fread("forecast_gdp.csv")
forecast_gdp[Year == '2014' & `Country Name` == "Argentina"][is.na(forecast_gdp[Year == '2014' & `Country Name` == "Argentina"])] = 0
argentina_GDPpercapPPP_2015_linreg <- predict(trainmodel,forecast_gdp[Year == '2014' & `Country Name` == "Argentina"])
argentina_GDPpercapPPP_2015_linreg
#Our model predicts that Argentina's GDP Per capita PPP in 2015 based on 2014 information is 19383.24!
#This model albeit not the best, still forecast better compared to the EIU report... 
#This is probably because we included key fundamental data in our forecasting, which supports our hypothesis of...
#The interconnection of a country's political, social and economic indicators...

#===============================================================================

#Building a model for developing countries using CART

#Dataset: developingcountries.cart
#===============================================================================

#Removing columns, that won't be used in CART
developing_cart <- developingcountries.cart[,c(4:21)]

#CART automatically handles missing values, and outliers will be pushed to the leaf node.
#Therefore, we can proceed straight to step 2.
#Train test split:

set.seed(2004)
train <- sample.split(Y = developing_cart$`GDP Per Capita PPP (Year+1)`, SplitRatio = 0.7)
trainset <- subset(developing_cart, train == T)
testset <- subset(developing_cart, train == F)


set.seed(2004)
cart1 <- rpart(`GDP Per Capita PPP (Year+1)` ~ . ,data = trainset,
               method = 'anova', control = rpart.control(minsplit = 11, cp = 0))
par(mfrow = c(1,1))
rpart.plot(cart1, nn=T, tweak = 0.8, main = "Maximal Tree for GDP Per Capita PPP(Y+1) for developing countries")

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

rpart.plot(cart2, nn=T, tweak = 1, main = "Optimal Tree for GDP Per Capita PPP (Year+1) for developing countries")

print(cart2)

printcp(cart2)

summary(cart2)

scaledVarImpt <- round(100*cart2$variable.importance/sum(cart2$variable.importance))
scaledVarImpt[scaledVarImpt > 0]
#CART identifies the most important variables to predict GDP per capita for developing countries as:
# 1: Male Adult Mortality Rate 27%
#2: Female Adult Mortality Rate 15%
#3: Exchange Rate 13%
#4: Human Development Index 13%
#5: Exports of G&S (% GDP): 10%


# Residuals = Error = `GDP Per Capita PPP (Year+1)` - `GDP Per Capita PPP (Year+1)`
RMSE.trainmodel <- sqrt(mean(residuals(cart2)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(cart2)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(cart2, newdata = testset)
testset.error <- testset$`GDP Per Capita PPP (Year+1)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 
MAPE(predict.test,testset$`GDP Per Capita PPP (Year+1)`)
#In this case, the RMSE for train model is 6238.831 while RMSE for test set is 8634.534
#once again this is not the best, for developing countries better models can be used

#Lets try to predict Argentina's GDP Per Capita PPP in 2015 using data in 2014.
#We can do this because our model did not "see" the actual 2015 data.
#Argentina's actual GDP Per Capita PPP in 2015 is 20105.20
forecast_gdp <- fread("forecast_gdp.csv")
forecast_gdp$major.event <- factor(forecast_gdp$major.event)
argentina_GDPpercapPPP_2015_cart <- predict(cart2,forecast_gdp[Year == '2014' & `Country Name` == "Argentina"])
argentina_GDPpercapPPP_2015_cart
#The forecast using CART is 17924.47, which isn't the most accurate of predictions.

#===============================================================================

#Developing a linear regression model for least developed countries

#Data: leastdeveloped.linreg
#===============================================================================

#Step 1: Identify potential outliers and remove them


#In this step, we use diagnostic plots to identify potential outliers

m1 <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP` 
          , data = leastdeveloped.linreg)
summary(m1)
#From the initial model, R-squared is 73.3%, and the Adjusted R-squared is 72.1%
#Can we do better?
#Lets explore whether there are any outliers
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))

#We can see that in general, data points 316,317,318 appear out of trend with the most of the dataset
#Lets see if our model will improve by removing them.


leastdeveloped.linreg_2 <- leastdeveloped.linreg[-c(316,317,318),]

m2 <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP` 
          , data = leastdeveloped.linreg_2)
summary(m2)
#R-squared value increases to 76.3% and Adjusted R-squared increases to 75.2%. 
#Removing these points do actually improve our model!

#Lets verify the diagnostic plots
par(mfrow = c(2,2))
plot(m2)
par(mfrow = c(1,1))
#The residual plots for Normal Q-Q, Scale-Location and Residuals v Leverage, do seem to satisfy the linear regression assumptions.
#The residuals vs fitted plot, show some form of curve and is not entirely horizontal.
#Linear regression may not be the best in this case, but we can still proceed to try.
#After removing outliers, we can proceed to to the next step.

#Step 2: Train-test split
#We will now do train-test split on the dataset, after checking for outliers
#Data to be used here: #leastdeveloped.linreg_2

set.seed(2004)
train <- sample.split(Y = leastdeveloped.linreg_2$`GDP Per Capita PPP (Year+1)`, SplitRatio = 0.7)
trainset <- subset(leastdeveloped.linreg_2, train == T)
testset <- subset(leastdeveloped.linreg_2, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$`GDP Per Capita PPP (Year+1)`)
summary(testset$`GDP Per Capita PPP (Year+1)`)
#distribution are relatively similar, we can proceed. Mean and Median are approximately similar.

#Step 3: Feature selection using backward elimination, VIF & Domain Knowledge

#Training our model & feature selection
m3 <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP` 
         , data = trainset)
summary(m3) #initial train model

plot(x = leastdeveloped.linreg$`Human Development Index`, y = leastdeveloped.linreg$`GDP Per Capita PPP (Year+1)`, main = "Regression Line with HDI as sole factor")
#Here we introduce a new variable HDI^2, this is because HDI seems to have the shape of an upward sloping quadratic curve
#relationship with GDP Per Capita

#Perform backward elimination remove unimportant variables
m.full <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP` + I(`Human Development Index`^2)
             , data = trainset)
m4 <- step(m.full)
summary(m4)

#Lets check the diagnostic plots:
par(mfrow = c(2,2))
plot(m4)
par(mfrow = c(1,1))

#Check VIF
vif(m4)
#Expectantly, VIF for Human Development Index and its squared value is very high. 
#However, this is normal when we take the square of HDI, we can easily find HDI^2, this collinearity is acceptable.
#But, female adult mortality rate and male adult mortality rate have rather high VIF values
#We will remove Female Adult Mortality Rate since its higher in VIF.

m5 <- lm(formula = `GDP Per Capita PPP (Year+1)` ~ `Private Expenditure Consumption (% growth)` + 
           `Gross Fixed Investment (% growth)` + `Govt Expenditure Consumption (% GDP)` + 
           `Exports of G&S (% GDP)` + `Imports of G&S (% GDP)` + `Human Development Index` + 
           `Gross Capital Formation` + `Net FDI (BOP)` + `Exchange Rate` + 
            `Male Adult Mortality Rate` + 
           I(`Human Development Index`^2), data = trainset)

summary(m5)
vif(m5)
#No more variables show high VIF other than HDI and HDI^2, issue of multicollinearity is reduced

#Lets check the diagnostic plots:
par(mfrow = c(2,2))
plot(m5)
par(mfrow = c(1,1))
#The residuals vs fitted graph shows a flat horizontal line, we can assume a linear relation is true for our model.
#The Normal Q-Q plots have points that are closed to a straight line. Normality assumption is checked.
#The scale-location graph is close to horizontal with a mild increment. The variances in this case are relatively constant.
#The residuals vs leverage graph tells us that, there are no outliers as all points lie within the Cook's distance lines.

#The final model chosen will be:
trainmodel <- lm(formula = `GDP Per Capita PPP (Year+1)` ~ `Private Expenditure Consumption (% growth)` + 
                   `Gross Fixed Investment (% growth)` + `Govt Expenditure Consumption (% GDP)` + 
                   `Exports of G&S (% GDP)` + `Imports of G&S (% GDP)` + `Human Development Index` + 
                   `Gross Capital Formation` + `Net FDI (BOP)` + `Exchange Rate` + 
                   `Male Adult Mortality Rate` + 
                   I(`Human Development Index`^2), data = trainset)

summary(trainmodel)

#Regression Equation:
#`GDP Per Capita PPP (Year+1)` ~ `Private Expenditure Consumption (% growth)` + 
#     `Gross Fixed Investment (% growth)` + `Govt Expenditure Consumption (% GDP)` + 
#     `Exports of G&S (% GDP)` + `Imports of G&S (% GDP)` + `Human Development Index` + 
#     `Gross Capital Formation` + `Net FDI (BOP)` + `Exchange Rate` + 
#     `Male Adult Mortality Rate` + I(`Human Development Index`^2)

#Adjusted R-squared is 81.7% with R-squared at 80.9% as well, this is reasonably strong.

#The model tells us the importance of variables in forecasting GDP Per Capita PPP(Year+1)
#for least developed countries: (significance level <0.1%)
# 1: Human Development Index
# 2: Gross Capital Formation
# 3: Net FDI(BOP)
# 4: Imports of G&S (% GDP)

# Residuals = Error = `GDP Per Capita PPP (Year+1)` - `GDP Per Capita PPP (Year+1)`
RMSE.trainmodel <- sqrt(mean(residuals(trainmodel)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(trainmodel)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(trainmodel, newdata = testset)
testset.error <- testset$`GDP Per Capita PPP (Year+1)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 
MAPE(predict.test,testset$`GDP Per Capita PPP (Year+1)`)

#Visualise our predictions

least_developed_prediction <- data.frame(Predicted = predict.test,  # Create data for ggplot2
                                   Actual = testset$`GDP Per Capita PPP (Year+1)`)
# Draw plot using ggplot2 package
least_developed_plot <- ggplot(least_developed_prediction,                                     
                         aes(x = Predicted,
                             y = Actual)) +
  ggtitle("GDP Per Capita (Year+1), least developed countries") +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
least_developed_plot
#RMSE for train is 1054.967, while RMSE for test is 1196.597
#In general, given any random set of data, actual GDP Per Capita(Y+1) will differ 
#from forecasted GDP Per Capita(Y+1) by 1196.597
#There is a 33.5% MAPE indicating that the forecast is reasonable. 
#Maybe cart will perform better?

#===============================================================================

#Building a model for least developed countries using CART

#Dataset: leastdeveloped.cart
#===============================================================================

#Removing columns, that won't be used in CART
leastdeveloped_cart <- leastdeveloped.cart[,c(4:21)]

#CART automatically handles missing values, and outliers will be pushed to the leaf node.
#Therefore, we can proceed straight to step 2.
#Train test split:

set.seed(2004)
train <- sample.split(Y = leastdeveloped_cart$`GDP Per Capita PPP (Year+1)`, SplitRatio = 0.7)
trainset <- subset(leastdeveloped_cart, train == T)
testset <- subset(leastdeveloped_cart, train == F)

set.seed(2004)
cart1 <- rpart(`GDP Per Capita PPP (Year+1)` ~ . ,data = trainset,
               method = 'anova', control = rpart.control(minsplit = 14, cp = 0))
par(mfrow = c(1,1))
rpart.plot(cart1, nn=T, tweak = 0.8, main = "Maximal Tree for GDP Per Capita PPP(Y+1) for least developed countries")

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

rpart.plot(cart2, nn=T, tweak = 1, main = "Optimal Tree for GDP Per Capita PPP (Year+1) for least developed countries")

print(cart2)

printcp(cart2)

summary(cart2)

scaledVarImpt <- round(100*cart2$variable.importance/sum(cart2$variable.importance))
scaledVarImpt[scaledVarImpt > 0]
#CART identifies the most important variables to predict GDP per capita for least developed countries as:
# 1: Exports of G&S (% GDP) : 27%
# 2: Govt Effectiveness : 15%
# 3: Female Adult Mortality Rate: 12%
# 4: Imports of G&S (% GDP): 11%


# Residuals = Error = `GDP Per Capita PPP (Year+1)` - `GDP Per Capita PPP (Year+1)`
RMSE.trainmodel <- sqrt(mean(residuals(cart2)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(cart2)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(cart2, newdata = testset)
testset.error <- testset$`GDP Per Capita PPP (Year+1)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 
MAPE(predict.test,testset$`GDP Per Capita PPP (Year+1)`)
#In this case, the RMSE for train model is 2162.553 while RMSE for test set is 3260.256
#This means when CART is used to predict, on average the actual GDP Per Capita PPP (Year+1)
#will differ from the forecasted GDP Per Capita PPP (year+1) by 3260.256.
#CART has a 70% MAPE, this is indicative of inaccurate forecasting, CART does not perform well
#In general it seems GDP is better predicted using linear regression.
#For least developed countries, linear regression is reccomended.


#===========================================================================================

#Developing a linear regression model to forecast GDP Per Capita for transitioning economies

#Data: transitioningcountries.linreg
#===========================================================================================

#Step 1: Identify potential outliers and remove them


#In this step, we use diagnostic plots to identify potential outliers

m1 <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP` 
         , data = transitioningcountries.linreg)
summary(m1)
#From the initial model, R-squared is 93.4%, and the Adjusted R-squared is 92.4%
#Can we do better?
#Lets explore whether there are any outliers
par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))

#From residuals vs fitted, there is no pattern in the residual plot, with the red line almost horizontal
#We can assume a linear relationship between GDP Per Capita(Year+1) and the predictors

#From scale-location, the residuals are spread equally along the range of predictors
#Hence, we can verify that the variances in errors are constant.

#From normal Q-Q, we can see the residuals, all fall approximately on the reference line

#From residuals vs leverage, all data points lie within the Cook's distance line

#There is no outliers to remove, and the linear regression assumption is satisfied.


#Step 2: Train-test split
#We will now do train-test split on the dataset, after checking for outliers
#Data to be used here: #transitioningcountries.linreg

set.seed(2004)
train <- sample.split(Y = transitioningcountries.linreg$`GDP Per Capita PPP (Year+1)`, SplitRatio = 0.7)
trainset <- subset(transitioningcountries.linreg, train == T)
testset <- subset(transitioningcountries.linreg, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$`GDP Per Capita PPP (Year+1)`)
summary(testset$`GDP Per Capita PPP (Year+1)`)
#distribution are relatively similar, we can proceed. Mean and Median are approximately similar.

#Step 3: Feature selection using backward elimination, VIF & Domain Knowledge

#Training our model & feature selection
m2 <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP` 
          , data = trainset)
summary(m2) #initial train model

#Perform backward elimination remove unimportant variables
m.full <- lm(`GDP Per Capita PPP (Year+1)` ~ . - `Country Name` - `Year` - `GDP Per Capita PPP` 
              , data = trainset)
m3 <- step(m.full)
summary(m3)
#From this, we can summarise the variables that are important in forecasting GDP Per Capita for transitioning economies

#Check VIF
vif(m3)
#VIF for Male Adult Mortality rate, Female adult mortality rate, total reserves &
#net foreign assets are extremely high, this means they can be predicted
#using the other independent variables, let's try removing 1 of them to see how our model performs.
#We will eliminate Net Foreign Assets due to multicollinearity.

m4 <- lm(formula = `GDP Per Capita PPP (Year+1)` ~ `Exports of G&S (% GDP)` + 
           `Imports of G&S (% GDP)` + `Human Development Index` +  
           `Net FDI (BOP)` + `Total Reserves` + `Govt Effectiveness` + 
           `Female Adult Mortality Rate` + `Male Adult Mortality Rate` + 
           `Real GDP (% growth)`, data = trainset)

summary(m4) 
vif(m4)
#After verifying the VIF, the issue of multicollinearity is reduced.
#However, male adult mortality rate and female adult mortality rate still has high VIF. 
#Lets eliminate female adult mortality rate(the highest VIF).

m5 <- lm(formula = `GDP Per Capita PPP (Year+1)` ~ `Exports of G&S (% GDP)` + 
           `Imports of G&S (% GDP)` + `Human Development Index` +  
           `Net FDI (BOP)` + `Total Reserves` + `Govt Effectiveness` + 
           `Male Adult Mortality Rate` + 
           `Real GDP (% growth)`, data = trainset)

summary(m5) 

vif(m5)
#Now all the VIF values are relatively small, we have reduced the issue of multicollinearity significantly.
#Our final train model should exclude these variables that exhibit multicollinearity.

#Lets check our diagnostic plots:
par(mfrow = c(2,2))
plot(m5)
par(mfrow = c(1,1))
#Some minor issues, but nothing major, overall the linear regression assumptions are satisfied.

#The final model chosen will be:
trainmodel <- lm(formula = `GDP Per Capita PPP (Year+1)` ~ `Exports of G&S (% GDP)` + 
                   `Imports of G&S (% GDP)` + `Human Development Index` +  
                   `Net FDI (BOP)` + `Total Reserves` + `Govt Effectiveness` + 
                   `Male Adult Mortality Rate` + 
                   `Real GDP (% growth)`, data = trainset)

summary(trainmodel)
#Regression Equation:
#`GDP Per Capita PPP (Year+1)` ~ `Exports of G&S (% GDP)` + 
# `                               Imports of G&S (% GDP)` + `Human Development Index` + `Net FDI (BOP)` + 
#                                 `Total Reserves` + `Govt Effectiveness` + `Male Adult Mortality Rate` + 
#                                 `Real GDP (% growth)`

#Our model has 87.6% R-Squared and 86.5% Adjusted R-squared which shows that the accuracy based on trainset data is relatively high.

#What else does the model tell us? In terms of which are significant in predicting GDP Per Capita PPP(Year+1):
# 1: Human Development Index
# 2: Total Reserves
# 3: Imports of G&S (% GDP)
# 4: Exports of G&S (% GDP)
#Only those significant at below 0.1% level are included.

# Residuals = Error = `GDP Per Capita PPP (Year+1)` - `GDP Per Capita PPP (Year+1)`
RMSE.trainmodel <- sqrt(mean(residuals(trainmodel)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(trainmodel)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(trainmodel, newdata = testset)
testset.error <- testset$`GDP Per Capita PPP (Year+1)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 
MAPE(predict.test,testset$`GDP Per Capita PPP (Year+1)`)
#Visualise our predictions

transition_prediction <- data.frame(Predicted = predict.test,  # Create data for ggplot2
                                   Actual = testset$`GDP Per Capita PPP (Year+1)`)
# Draw plot using ggplot2 package
transition_plot <- ggplot(transition_prediction,                                     
                         aes(x = Predicted,
                             y = Actual)) +
  ggtitle("GDP Per Capita (Year+1), transitioning countries") +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
transition_plot
#RMSE for train is 2124.403, while RMSE for test is 2388.917
#In general, given any random set of data, actual GDP Per Capita(Y+1) will differ from forecasted GDP Per Capita(Y+1) by 2388.917
#The MAPE is 24.4% which is a reasonable forecasting
#This is supported by the visualisation plots, which show that the actual values lie reasonably close
#to the forecasted GDP Per Capita(Y+1) represented by the red line.

#===============================================================================

#Building a model for transitioning economies using CART

#Dataset: transitioningcountries.cart
#===============================================================================

#Removing columns that won't be used in CART
transition_cart <- transitioningcountries.cart[,c(4:21)]

#CART automatically handles missing values, and outliers will be pushed to the leaf node.
#Therefore, we can proceed straight to step 2.
#Train test split:

set.seed(2004)
train <- sample.split(Y = transition_cart$`GDP Per Capita PPP (Year+1)`, SplitRatio = 0.7)
trainset <- subset(transition_cart, train == T)
testset <- subset(transition_cart, train == F)

set.seed(2004)
cart1 <- rpart(`GDP Per Capita PPP (Year+1)` ~ . ,data = trainset,
               method = 'anova', control = rpart.control(minsplit = 3, cp = 0))
par(mfrow = c(1,1))
rpart.plot(cart1, nn=T, tweak = 0.8, main = "Maximal Tree for GDP Per Capita PPP(Y+1) for transitioning economies")

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

rpart.plot(cart2, nn=T, tweak = 1.5, main = "Optimal Tree for GDP Per Capita PPP (Year+1) for transitioning economies")

print(cart2)

printcp(cart2)

summary(cart2)

scaledVarImpt <- round(100*cart2$variable.importance/sum(cart2$variable.importance))
scaledVarImpt[scaledVarImpt > 0]
#CART identifies the most important variables to predict GDP per capita as:
# 1: Human Development Index 25%
# 2: Total Reserves & Govt Effectiveness 13%
# 3: Net foreign asset 10%


# Residuals = Error = `GDP Per Capita PPP (Year+1)` - `GDP Per Capita PPP (Year+1)`
RMSE.trainmodel <- sqrt(mean(residuals(cart2)^2))  # RMSE on trainset based on train model.
summary(abs(residuals(cart2)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.test <- predict(cart2, newdata = testset)
testset.error <- testset$`GDP Per Capita PPP (Year+1)` - predict.test

# Testset Errors
RMSE.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.trainmodel 
RMSE.test 
MAPE(predict.test,testset$`GDP Per Capita PPP (Year+1)`)
#In this case, the RMSE for train model is 1355.085 while RMSE for test set is 2358.611
#This means when CART is used to predict, on average the actual GDP Per Capita PPP (Year+1)
#will differ from the forecasted GDP Per Capita PPP (year+1) by 2358.611.
#The MAPE is 19% in this case, this is considered to be a good forecast. 
#Overall, it seems that CART performs better for transitioning countries
#however both models can be used to reinforce for each other, to validate the results of one another


#========= end of GDP analysis==================================================

