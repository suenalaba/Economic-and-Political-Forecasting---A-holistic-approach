#importing relevant libraries to be updated as we go...
library(data.table) 
library(tidyr)
library(dplyr)

#set your working directory here
setwd("C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Raw")

#===============================================================================

#Cleaning Data for GDP Per Capita PPP

#===============================================================================

#Import GDP per Capita PPP data
GDP_percap_PPP.dt <- fread("GDP per capita PPP.csv")

#convert 1st row of GDP_percap_PPP.dt to column header
names(GDP_percap_PPP.dt) <- as.matrix(GDP_percap_PPP.dt[1, ])
GDP_percap_PPP.dt <- GDP_percap_PPP.dt[-1, ]
GDP_percap_PPP.dt[] <- lapply(GDP_percap_PPP.dt, function(x) type.convert(as.character(x)))
GDP_percap_PPP.dt #check if done correctly

#Only want data from 1990 to 2019
GDP_percap_PPP.dt <- GDP_percap_PPP.dt[,c(1,35:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "GDP Per Capita PPP" 
GDP_percap_PPP.dt <- gather(GDP_percap_PPP.dt, Year, 'GDP Per Capita PPP', 2:31, na.rm = FALSE, convert = FALSE) #tranposing
setDT(GDP_percap_PPP.dt)

#View(GDP_percap_PPP.dt) #verify


#===============================================================================

#Cleaning Data for Real GDP Growth

#===============================================================================


#import Real GDP data ("your file name.csv") as data table
realGDP.dt <- fread("GDP Growth Data.csv")

#convert 1st row of realGDP.dt to column header
names(realGDP.dt) <- as.matrix(realGDP.dt[1, ])
realGDP.dt <- realGDP.dt[-1, ]
realGDP.dt[] <- lapply(realGDP.dt, function(x) type.convert(as.character(x)))
realGDP.dt #check if done correctly

#Only want data from 1990 onwards
realGDP.dt <- realGDP.dt[,c(1,35:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "Real GDP Growth (%growth)" 
realGDP.dt <- gather(realGDP.dt, Year, 'Real GDP (% growth)', 2:31, na.rm = FALSE, convert = FALSE) #tranpose
setDT(realGDP.dt)

#View(realGDP.dt)

#===============================================================================

#Clean data for Gross Fixed Investment

#===============================================================================

#Import Gross Fixed Investment Data
GFI.dt <- fread("Gross Fixed Investment Data.csv")

#convert 1st row of GFI.dt to column header
names(GFI.dt) <- as.matrix(GFI.dt[1, ])
GFI.dt <- GFI.dt[-1, ]
GFI.dt[] <- lapply(GFI.dt, function(x) type.convert(as.character(x)))
GFI.dt #check if done correctly

#Only want data from 1990 onwards
GFI.dt <- GFI.dt[,c(1,35:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "Gross Fixed Investment (% growth)" 
GFI.dt <- gather(GFI.dt, Year, 'Gross Fixed Investment (% growth)', 2:31, na.rm = FALSE, convert = FALSE)
setDT(GFI.dt)

#View(GFI.dt) #verify that GFI data is in the appropriate format


#===============================================================================

#Clean data for Export good & services (% GDP)

#===============================================================================

#Import Export g&s data
exportsgs <- fread("Exports G&S.csv")

#convert 1st row of exportsgs.dt to column header
names(exportsgs) <- as.matrix(exportsgs[1, ])
exportsgs <- exportsgs[-1, ]
exportsgs[] <- lapply(exportsgs, function(x) type.convert(as.character(x)))
exportsgs #check if done correctly

#Only want data from 1990 onwards
exportsgs <- exportsgs[,c(1,35:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "Exports of G&S (% GDP)" 
exportsgs <- gather(exportsgs, Year, 'Exports of G&S (% GDP)', 2:31, na.rm = FALSE, convert = FALSE)
setDT(exportsgs)

#View(exportsgs) #verify that Exportgs data is in the appropriate format


#===============================================================================

#Clean data for Import good & services (% GDP)

#===============================================================================

#import Import g&s data
importsgs <- fread("Imports G&S.csv")

#convert 1st row of importsgs to column header
names(importsgs) <- as.matrix(importsgs[1, ])
importsgs <- importsgs[-1, ]
importsgs[] <- lapply(importsgs, function(x) type.convert(as.character(x)))
importsgs #check if done correctly

#Only want data from 1990 onwards
importsgs <- importsgs[,c(1,35:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "Imports of G&S (% GDP)" 
importsgs <- gather(importsgs, Year, 'Imports of G&S (% GDP)', 2:31, na.rm = FALSE, convert = FALSE)
setDT(importsgs)

#View(importsgs) #verify


#===============================================================================

#Clean data for Government Consumption Expenditure

#===============================================================================

#import govt consumption expenditure dataset
GovtConExp <- fread("Govt Consumption Exp.csv")

#convert 1st row of GovtConExp to column header
names(GovtConExp) <- as.matrix(GovtConExp[1, ])
GovtConExp <- GovtConExp[-1, ]
GovtConExp[] <- lapply(GovtConExp, function(x) type.convert(as.character(x)))
GovtConExp #check if done correctly

#Only want data from 1990 onwards
GovtConExp <- GovtConExp[,c(1,35:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "Govt Expenditure Consumption (% GDP)" 
GovtConExp <- gather(GovtConExp, Year, 'Govt Expenditure Consumption (% GDP)', 2:31, na.rm = FALSE, convert = FALSE)
setDT(GovtConExp)

#View(GovtConExp)


#===============================================================================


#Clean data for Private Consumption Expenditure

#===============================================================================

#import private consumption expenditure dataset
PrivateExp.dt <- fread("Private Consumption Data.csv")

#convert 1st row of PrivateExp.dt to column header
names(PrivateExp.dt) <- as.matrix(PrivateExp.dt[1, ])
PrivateExp.dt <- PrivateExp.dt[-1, ]
PrivateExp.dt[] <- lapply(PrivateExp.dt, function(x) type.convert(as.character(x)))
PrivateExp.dt #check if done correctly

#Only want data from 1990 onwards
PrivateExp.dt <- PrivateExp.dt[,c(1,35:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "Private Expenditure Consumption (% growth)" 
PrivateExp.dt <- gather(PrivateExp.dt, Year, 'Private Expenditure Consumption (% growth)', 2:31, na.rm = FALSE, convert = FALSE)
setDT(PrivateExp.dt)

#View(PrivateExp.dt)

#===============================================================================


#Clean data Inflation Rate

#===============================================================================

#import Inflation Rate dataset
inflationrate.dt <- fread("Inflation Rate Data.csv")

#convert 1st row of inflationrate.dt to column header
names(inflationrate.dt) <- as.matrix(inflationrate.dt[1, ])
inflationrate.dt <- inflationrate.dt[-1, ]
inflationrate.dt[] <- lapply(inflationrate.dt, function(x) type.convert(as.character(x)))
inflationrate.dt #check if done correctly

#Only want data from 1990 onwards
inflationrate.dt <- inflationrate.dt[,c(1,35:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "Inflation Rate" 
inflationrate.dt <- gather(inflationrate.dt, Year, 'Inflation Rate', 2:31, na.rm = FALSE, convert = FALSE)
setDT(inflationrate.dt)

#View(inflationrate.dt)

#===============================================================================


#Clean data Total Reserves

#===============================================================================

#import Total Reserves dataset
totalreserves.dt <- fread("Total Reserves Data.csv")

#convert 1st row of totalreserves.dt to column header
names(totalreserves.dt) <- as.matrix(totalreserves.dt[1, ])
totalreserves.dt <- totalreserves.dt[-1, ]
totalreserves.dt[] <- lapply(totalreserves.dt, function(x) type.convert(as.character(x)))
totalreserves.dt #check if done correctly

#Only want data from 1990 onwards
totalreserves.dt <- totalreserves.dt[,c(1,35:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "Inflation Rate" 
totalreserves.dt <- gather(totalreserves.dt, Year, 'Total Reserves', 2:31, na.rm = FALSE, convert = FALSE)
setDT(totalreserves.dt)

#View(totalreserves.dt)

#===============================================================================


#Clean data Exchange Rate

#===============================================================================

#import exchange rate dataset
exchangerate.dt <- fread("Exchange Rate Data.csv")

#convert 1st row of exchangerate.dt to column header
names(exchangerate.dt) <- as.matrix(exchangerate.dt[1, ])
exchangerate.dt <- exchangerate.dt[-1, ]
exchangerate.dt[] <- lapply(exchangerate.dt, function(x) type.convert(as.character(x)))
exchangerate.dt #check if done correctly

#Only want data from 1990 onwards
exchangerate.dt <- exchangerate.dt[,c(1,35:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "Exchange Rate" 
exchangerate.dt <- gather(exchangerate.dt, Year, 'Exchange Rate', 2:31, na.rm = FALSE, convert = FALSE)
setDT(exchangerate.dt)

#View(exchangerate.dt)

#===============================================================================


#Clean data for net Foreign Direct Investment (BOP)

#===============================================================================

#import net FDI dataset
fdibop.dt <- fread("FDI BOP Data.csv")

#convert 1st row of fdibop.dt to column header
names(fdibop.dt) <- as.matrix(fdibop.dt[1, ])
fdibop.dt <- fdibop.dt[-1, ]
fdibop.dt[] <- lapply(fdibop.dt, function(x) type.convert(as.character(x)))
fdibop.dt #check if done correctly

#Only want data from 1990 onwards
fdibop.dt <- fdibop.dt[,c(1,35:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "net FDI BOP" 
fdibop.dt <- gather(fdibop.dt, Year, 'Net FDI (BOP)', 2:31, na.rm = FALSE, convert = FALSE)
setDT(fdibop.dt)

#View(fdibop.dt)

#===============================================================================


#Clean data for net Foreign Asset

#===============================================================================

#import net Foreign Asset dataset
netforeignasset.dt <- fread("Net Foreign Asset Data.csv")

#convert 1st row of netforeignasset.dt to column header
names(netforeignasset.dt) <- as.matrix(netforeignasset.dt[1, ])
netforeignasset.dt <- netforeignasset.dt[-1, ]
netforeignasset.dt[] <- lapply(netforeignasset.dt, function(x) type.convert(as.character(x)))
netforeignasset.dt #check if done correctly

#Only want data from 1990 onwards
netforeignasset.dt <- netforeignasset.dt[,c(1,35:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "Net foreign asset" 
netforeignasset.dt <- gather(netforeignasset.dt, Year, 'Net foreign asset', 2:31, na.rm = FALSE, convert = FALSE)
setDT(netforeignasset.dt)

#View(netforeignasset.dt)

#===============================================================================


#Clean data for Gross Capital Formation 

#===============================================================================

#import Gross Capital Formation dataset
grosscapformation.dt <- fread("Gross Capital Formation Data.csv")

#convert 1st row of grosscapformation.dt to column header
names(grosscapformation.dt) <- as.matrix(grosscapformation.dt[1, ])
grosscapformation.dt <- grosscapformation.dt[-1, ]
grosscapformation.dt[] <- lapply(grosscapformation.dt, function(x) type.convert(as.character(x)))
grosscapformation.dt #check if done correctly

#Only want data from 1990 onwards
grosscapformation.dt <- grosscapformation.dt[,c(1,35:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "Gross Capital Formation" 
grosscapformation.dt <- gather(grosscapformation.dt, Year, 'Gross Capital Formation', 2:31, na.rm = FALSE, convert = FALSE)
setDT(grosscapformation.dt)

#View(grosscapformation.dt)



#===============================================================================


#Clean data for Male Adult Mortality Rate

#===============================================================================

#import Male Adult Mortality Rate dataset
malemortality <- fread("Male Adult Mortality Rate.csv")

#convert 1st row of malemortality to column header
names(malemortality) <- as.matrix(malemortality[1, ])
malemortality <- malemortality[-1, ]
malemortality[] <- lapply(malemortality, function(x) type.convert(as.character(x)))
malemortality #check if done correctly

#Only want data from 1990 onwards
malemortality <- malemortality[,c(1,35:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "Male Adult Mortality Rate" 
malemortality <- gather(malemortality, Year, 'Male Adult Mortality Rate', 2:31, na.rm = FALSE, convert = FALSE) #transpose
setDT(malemortality)

#View(malemortality)

#===============================================================================


#Clean data for Female Adult Mortality Rate

#===============================================================================

#import Female Adult Mortality Rate dataset
femalemortality <- fread("Female Adult Mortality Rate.csv")

#convert 1st row of femalemortality to column header
names(femalemortality) <- as.matrix(femalemortality[1, ])
femalemortality <- femalemortality[-1, ]
femalemortality[] <- lapply(femalemortality, function(x) type.convert(as.character(x)))
femalemortality #check if done correctly

#Only want data from 1990 onwards
femalemortality <- femalemortality[,c(1,35:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "Female Adult Mortality Rate" 
femalemortality <- gather(femalemortality, Year, 'Female Adult Mortality Rate', 2:31, na.rm = FALSE, convert = FALSE) #transpose
setDT(femalemortality)

#View(femalemortality)

#===============================================================================

#Clean data for Government Effectiveness Index

#===============================================================================
#import Government Effectiveness dataset
govteff <- fread("Government Effectiveness.csv")

#Extract columns that are needed, Country Name, Year, Govt Effectiveness
govteff <- govteff[(govteff$Indicator == "Government Effectiveness" & govteff$`Subindicator Type` == "Estimate")]

#Only want data from 1990-2019 onwards
govteff <- govteff[,c(2,6:26)]

#convert columns that are Years to become Rows, New columns created will be Years & "Govt Effectiveness Index" 
govteff <- gather(govteff, Year, 'Govt Effectiveness', 2:22, na.rm = FALSE, convert = FALSE) #tranpose
setDT(govteff)

#View(govteff)

#===============================================================================


#Clean data for Human Development Index (HDI)

#===============================================================================

#import HDI data 
HDI.dt <- fread("HDI Values 1.csv",na.strings = c("..")) #there are many data with ".." due to type of dataset.


#remove rows that consist entirely of words that are unnecessary
HDI.dt <- HDI.dt[6:212]

#convert 1st row of HDI.dt to column header
names(HDI.dt) <- as.matrix(HDI.dt[1, ])
HDI.dt <- HDI.dt[-1, ]
HDI.dt[] <- lapply(HDI.dt, function(x) type.convert(as.character(x)))
HDI.dt #check if done correctly

#renaming column name to match with other dataset
setnames(HDI.dt, "Country", "Country Name")

#Because of the way CSV was structured, some columns are entirely NA, hence, we want to remove them:
HDI.dt <- HDI.dt[,which(unlist(lapply(HDI.dt, function(x)!all(is.na(x))))),with=F]

#For now, we are interested in the Human Development Index only, not the ranking 
HDI.dt <- HDI.dt[,2:32]


#convert columns that are Years to become Rows, New columns created will be Years & "Human Development Index" 
HDI.dt <- gather(HDI.dt, Year, 'Human Development Index', 2:31, na.rm = FALSE, convert = FALSE)
setDT(HDI.dt)

#Check that all data are cleaned and table is correct
#View(HDI.dt)


#===============================================================================


#Joining the datasets 


#===============================================================================

#Inner join of all the datasets when country name and year matches
#The final dt will be GDP_current
#GDP_percap_PPP.dt will be the main dataset, since its what we are going to predict

GDP_current.dt <- merge(GDP_percap_PPP.dt, PrivateExp.dt, by = c("Country Name","Year"))
GDP_current.dt <- merge(GDP_current.dt, GFI.dt, by = c("Country Name","Year"))
GDP_current.dt <- merge(GDP_current.dt, GovtConExp, by = c("Country Name","Year"))
GDP_current.dt <- merge(GDP_current.dt, exportsgs, by = c("Country Name","Year"))
GDP_current.dt <- merge(GDP_current.dt, importsgs, by = c("Country Name","Year"))
GDP_current.dt <- merge(GDP_current.dt, HDI.dt, by = c("Country Name","Year"))
GDP_current.dt <- merge(GDP_current.dt, grosscapformation.dt, by = c("Country Name","Year"))
GDP_current.dt <- merge(GDP_current.dt, netforeignasset.dt, by = c("Country Name","Year"))
GDP_current.dt <- merge(GDP_current.dt, fdibop.dt, by = c("Country Name","Year"))
GDP_current.dt <- merge(GDP_current.dt, exchangerate.dt, by = c("Country Name","Year"))
GDP_current.dt <- merge(GDP_current.dt, totalreserves.dt, by = c("Country Name","Year"))
GDP_current.dt <- merge(GDP_current.dt, inflationrate.dt, by = c("Country Name","Year"))
GDP_current.dt <- merge(GDP_current.dt, govteff, by = c("Country Name","Year"))
GDP_current.dt <- merge(GDP_current.dt, femalemortality, by = c("Country Name","Year"))
GDP_current.dt <- merge(GDP_current.dt, malemortality, by = c("Country Name","Year"))
GDP_current.dt <- merge(GDP_current.dt, realGDP.dt, by = c("Country Name","Year"))



#check and ensure, that all data are correctly merged, take a small sample size and crosscheck
#test case used here: Argentina
#View(GDP_current.dt)


#===============================================================================

#Cleaning the data of the merged dataset

#===============================================================================


#Add a new column called major.event, this is a static variable, 
#should there be any sharp spike/fall, it is probably due to a major event
GDP_current.dt$major.event <- 0
GDP_current.dt[Year == "1990", major.event := 1]
GDP_current.dt[Year == "1991", major.event := 1]
GDP_current.dt[Year == "1997", major.event := 1]
GDP_current.dt[Year == "2001", major.event := 1]
GDP_current.dt[Year == "2008", major.event := 1]
GDP_current.dt[Year == "2009", major.event := 1]
#convert to categorical variable. 
#1 represents a major event occur(eg: Global Financial Crisis/9-11 recession)
GDP_current.dt$major.event <- factor(GDP_current.dt$major.event)
# View(GDP_current.dt)


#===============================================================================

#Forecast 1 year into the future, we want to know, given current data, 
#how well can we predict next's year GDP per capita?

#===============================================================================

#Forecasting 1 year into the future
forecast_gdp <- GDP_current.dt %>%
  group_by(`Country Name`) %>%
  mutate(`GDP Per Capita PPP (Year+1)` = lead(`GDP Per Capita PPP`))
#Each row will be something like: GDP Per Capita (Y+1) ~ IV1 (Y) + IV2(Y) etc..

setDT(forecast_gdp)
# View(forecast_gdp) 

#Basis of comparison to country report: With only 2014 data, we want to forecast 2015 GDP Per Capita PPP
#Therefore, the model needs to exclude these points where actual 2015 GDP Per Capita can be "seen"
#This includes years 2014 onwards.

forecast_GDPpercap<-forecast_gdp[!(forecast_gdp$Year=="2015" | forecast_gdp$Year=="2016" |
            forecast_gdp$Year=="2017" | forecast_gdp$Year=="2018"|
            forecast_gdp$Year=="2019"|forecast_gdp$Year=="2014")]

#View(forecast_GDPpercap)

#====================================================================================================================

#Separating the dataset into different economies: 
#developed, developing,least developed and transitioning economies
#Sorting done based on reports on classification of countries.

#====================================================================================================================

developed_economies <- c("Austria", "Belgium", "Denmark", "Finland", "France","Germany", "Greece", 
                         "Ireland", "Italy","Luxembourg", "Netherlands", "Portugal", "Spain", "Sweden", 
                         "United Kingdom", "Bulgaria","Croatia", "Cyprus","Czech Republic", "Estonia", 
                         "Hungary","Latvia", "Lithuania", "Malta", "Poland", "Romania", "Slovakia", "Slovenia", 
                         "Iceland", "Norway","Switzerland", "Australia", "Canada", "Japan", "New Zealand", 
                         "United States", "Republic of Korea", "Singapore")
developing_economies <- c("Algeria", "Libya", "Belize", "Tunisia", "Botswana", "Mauritius", "Namibia", "South Africa", 
                          "Brunei Darussalam", "China", "Hong Kong SAR", "Malaysia", "Taiwan, Province of China", 
                          "Thailand", "Barbados", "Cuba", "Dominican Republic", "Jamaica", "Trinidad and Tobago", 
                          "Mexico", "Gabon", "Costa Rica", "Mexico", "Panama", "Cabo Verde", "Nigeria", "India", 
                          "Iran (Islamic Republic of)", "Argentina", "Brazil", "Chile", "Colombia", "Ecuador", 
                          "Peru", "Uruguay","Venezuela", "Bahrain", "Iraq", "Israel", "Jordan", "Kuwait", 
                          "Lebanon", "Oman", "Qatar", "Saudi Arabia", "Turkey", "United Arab Emirates","Suriname", "Maldives",
                          "Marshall Islands","Dominica","Grenada","Tonga","Palau","Antigua and Barbuda","Fiji")

least_developed_economies <- c("Angola", "Benin", "Bolivia","Burkina Faso", "Burundi", "Cameroon",
                               "Central African Republic", "Congo",  "Côte d'Ivoire", "Chad", "Comoros", 
                               "Democratic Republic of the Congo","Djibouti", "Equatorial Guinea", "Eritrea", "Egypt", 
                               "Morocco", "Indonesia", "Papua New Guinea", "Philippines", "Ethiopia", "Gambia",  
                               "Ghana", "Guinea", "Guinea-Bissau", "Guyana","Honduras",  "Vietnam",  "Bangladesh", 
                               "Pakistan", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", 
                               "Mozambique", "El Salvador", "Guatemala", "Sri Lanka", "Niger",  "Nicaragua", 
                               "Rwanda", "Sao Tome and Principe", "Senegal", "Sierra Leone", "Syrian Arab Republic", 
                               "Paraguay", "Somalia", "South Sudan", "Sudan", "Togo", "Uganda", 
                               "United Republic of Tanzania", "Zambia", "Cambodia", "Kiribati", 
                               "Lao People's Democratic Republic", "Myanmar", "Samoa", "Solomon Islands", 
                               "Timor-Leste", "Tuvalu", "Vanuatu", "Afghanistan", "Bangladesh", "Bhutan", "Nepal", 
                               "Yemen", "Haiti","Seychelles","Mongolia", "Kenya","Zimbabwe")


transitioning_economies <- c("Albania", "Bosnia and Herzegovina", "Montenegro", "Serbia", "North Macedonia",
                             "Armenia", "Azerbaijan", "Belarus", "Georgia", "Kazakhstan", "Kyrgyzstan", 
                             "Republic of Moldova", "Russian Federation", "Tajikistan", "Turkmenistan", "Ukraine", 
                             "Uzbekistan")

#Grouping the countries into different economy types
developed_countries.dt <- forecast_GDPpercap[forecast_GDPpercap$`Country Name` %in% developed_economies,]
developing_countries.dt <- forecast_GDPpercap[forecast_GDPpercap$`Country Name` %in% developing_economies,]
leastdeveloped_countries.dt <- forecast_GDPpercap[forecast_GDPpercap$`Country Name` %in% least_developed_economies,]
transitioning_countries.dt <- forecast_GDPpercap[forecast_GDPpercap$`Country Name` %in% transitioning_economies,]

# class(developing_countries.dt)
'%!in%' <- function(x,y)!('%in%'(x,y)) #use this function to check, if any of the countries were left out
crosscheck <- forecast_gdp[forecast_gdp$`Country Name` %!in% c(transitioning_economies,developing_economies,developed_economies,least_developed_economies),]
crosscheck
#We intentionally left out countries that are not classified. 
#In crosscheck, we verify that all countries classified are included.
#Andorra and Liechtenstein were not classified in the data source we used, thus they are excluded

#================================================================================================

#Next, for Linear Regression, we are going to remove all NAs.

#For CART, we will keep NAs as long as its not too many.

#================================================================================================

#For linear regression models: 

#Removing NAs here
allcountries.linreg <- forecast_GDPpercap[complete.cases(forecast_GDPpercap),]
developedcountries.linreg <- developed_countries.dt[complete.cases(developed_countries.dt),]
developingcountries.linreg <- developing_countries.dt[complete.cases(developing_countries.dt),]
leastdeveloped.linreg <- leastdeveloped_countries.dt[complete.cases(leastdeveloped_countries.dt),]
transitioningcountries.linreg <- transitioning_countries.dt[complete.cases(transitioning_countries.dt),]

summary(leastdeveloped.linreg) #verify
#View(leastdeveloped.linreg)


#function to allow deleting NAs up to a specified number
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}


#For CART models:

#Removing NAs, maximum allowable NAs will be four, we don't want to many NAs to skew our analysis
developedcountries.cart <- delete.na(developed_countries.dt, 4)
developingcountries.cart <- delete.na(developing_countries.dt, 4)
leastdeveloped.cart <- delete.na(leastdeveloped_countries.dt, 4)
transitioningcountries.cart <- delete.na(transitioning_countries.dt, 4)

#For CART models, the outcome variable shouldn't have NAs
#Here, we want to remove NAs in the outcome variable in leastdeveloped.cart
leastdeveloped.cart <- leastdeveloped.cart[!is.na(leastdeveloped.cart$`GDP Per Capita PPP (Year+1)`),]

summary(leastdeveloped.cart) #verify

#===============================================================================

#fwrite files out for convenience to 
#perform regression analysis for GDP Per Capita PPP(Year+1)

#===============================================================================


#set working directory to export files to
setwd("C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Cleaned")

#export the cleaned datasets using write.csv
#write.csv("datasetname,"filename.....")

write.csv(allcountries.linreg, "allcountries.linreg.csv")

write.csv(developedcountries.linreg, "developedcountries.linreg.csv")
write.csv(developingcountries.linreg, "developingcountries.linreg.csv")
write.csv(leastdeveloped.linreg, "leastdeveloped.linreg.csv")
write.csv(transitioningcountries.linreg,"transitioningcountries.linreg.csv")

write.csv(developedcountries.cart,"developedcountries.cart.csv")
write.csv(developingcountries.cart,"developingcountries.cart.csv")
write.csv(leastdeveloped.cart,"leastdeveloped.cart.csv")
write.csv(transitioningcountries.cart,"transitioningcountries.cart.csv")

write.csv(forecast_gdp,"forecast_gdp.csv")
