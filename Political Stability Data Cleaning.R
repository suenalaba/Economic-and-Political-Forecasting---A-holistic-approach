#importing relevant libraries to be updated as we go...
library(data.table) 
library(tidyr)
library(dplyr)

#set your working directory here
setwd("C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Raw")


#===============================================================================

#Clean Data for Fragile State Index, a quantifiable measure of political stability

#===============================================================================

#import data with fread
fsi2006 <- fread("fsi-2006.csv") 
fsi2007 <- fread("fsi-2007.csv") 
fsi2008 <- fread("fsi-2008.csv") 
fsi2009 <- fread("fsi-2009.csv") 
fsi2010 <- fread("fsi-2010.csv") 
fsi2011 <- fread("fsi-2011.csv")
fsi2012 <- fread("fsi-2012.csv") 
fsi2013 <- fread("fsi-2013.csv") 
fsi2014 <- fread("fsi-2014.csv") 
fsi2015 <- fread("fsi-2015.csv") 
fsi2016 <- fread("fsi-2016.csv")
fsi2017 <- fread("fsi-2017.csv") 
fsi2018 <- fread("fsi-2018.csv") 
fsi2019 <- fread("fsi-2019.csv") 

#removing unwanted columns, we only want Country Name, Year & FSI
fsi2006 <- fsi2006[,c(1,2,4)]
fsi2007 <- fsi2007[,c(1,2,4)]
fsi2008 <- fsi2008[,c(1,2,4)]
fsi2009 <- fsi2009[,c(1,2,4)]
fsi2010 <- fsi2010[,c(1,2,4)]
fsi2011 <- fsi2011[,c(1,2,4)]
fsi2012 <- fsi2012[,c(1,2,4)]
fsi2013 <- fsi2013[,c(1,2,4)]
fsi2014 <- fsi2014[,c(1,2,4)]
fsi2015 <- fsi2015[,c(1,2,4)]
fsi2016 <- fsi2016[,c(1,2,4)]
fsi2017 <- fsi2017[,c(1,2,4)]
fsi2018 <- fsi2018[,c(1,2,4)]
fsi2019 <- fsi2019[,c(1,2,4)]

#combine the dataset as new rows
FSI.dt <- rbind(fsi2006, fsi2007,fsi2008,fsi2009,fsi2010,fsi2011,fsi2012,fsi2013,
              fsi2014,fsi2015,fsi2016,fsi2017,fsi2018,fsi2019)

#renaming column for better user readability and to match with other data sets
setnames(FSI.dt, "Total", "FSI")
setnames(FSI.dt, "Country", "Country Name")

#change Year to chr data type to match
FSI.dt <- FSI.dt[, Year:=as.character(Year)]

#===============================================================================

#Cleaning Data for Corruption Perception Index

#===============================================================================

#import data
CorrPI <- fread("Corruption Perception Index.csv")
  
#convert 1st row of CorrPI.dt to column header
names(CorrPI) <- as.matrix(CorrPI[1, ])
CorrPI <- CorrPI[-1, ]
CorrPI[] <- lapply(CorrPI, function(x) type.convert(as.character(x)))
CorrPI #check if done correctly

#we only want data from 2006 - 2019
CorrPI <- CorrPI[,-c(2:9)]

#renaming column header for easier joining
setnames(CorrPI, "Jurisdiction", "Country Name")

#convert columns that are Years to become Rows, New columns created will be Years & "Corruption Perception Index" 
CorrPI <- gather(CorrPI, Year, 'CorruptionPI', 2:11, na.rm = FALSE, convert = FALSE) #transpose
setDT(CorrPI)

#import 2nd part of Corruption Perception Index data
CorrPI2 <- fread("Corruption Perception Index 2.csv")

#remove unnecessary rows, we only want the index, not the rank
CorrPI2<-CorrPI2[!(CorrPI2$Indicator=="Rank"),]

#we only want data from 2006-2019
CorrPI2 <- CorrPI2[,c(2,10:13)]

#convert columns that are Years to become Rows, New columns created will be Years & "Corruption Perception Index" 
CorrPI2 <- gather(CorrPI2, Year, 'CorruptionPI', 2:5, na.rm = FALSE, convert = FALSE) #transpose
setDT(CorrPI2)

#Combine both corruption perception index data
CPI.dt <- rbind(CorrPI,CorrPI2)

#Convert Corruption Perception Index to correct data type: char to numeric
CPI.dt <- CPI.dt[, CorruptionPI := as.numeric(CorruptionPI)]
#- will be converted to NA
CPI.dt

#===============================================================================

#Clean Data for Unemployment Rate (% of labour force)

#===============================================================================

#import unemployment rate dataset
unemployment.dt <- fread("Unemployment Rate.csv")

#convert 1st row of unemployment.dt to column header
names(unemployment.dt) <- as.matrix(unemployment.dt[1, ])
unemployment.dt <- unemployment.dt[-1, ]
unemployment.dt[] <- lapply(unemployment.dt, function(x) type.convert(as.character(x)))
unemployment.dt #check if done correctly

#Only want data from 2006-2019
unemployment.dt <- unemployment.dt[,c(1,51:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "Unemployment Rate" 
unemployment.dt <- gather(unemployment.dt, Year, 'Unemployment Rate', 2:15, na.rm = FALSE, convert = FALSE)
setDT(unemployment.dt)

#===============================================================================

#Clean data for Govt Healthcare Expenditure

#===============================================================================

#import govt healthcare expenditure dataset
healthcare_exp.dt <- fread("Healthcare Expenditure.csv")

#convert 1st row of healthcare_exp.dt to column header
names(healthcare_exp.dt) <- as.matrix(healthcare_exp.dt[1, ])
healthcare_exp.dt <- healthcare_exp.dt[-1, ]
healthcare_exp.dt[] <- lapply(healthcare_exp.dt, function(x) type.convert(as.character(x)))
healthcare_exp.dt #check if done correctly

#Only want data from 2006-2019
healthcare_exp.dt <- healthcare_exp.dt[,c(1,51:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "HealthCare_Exp" 
healthcare_exp.dt <- gather(healthcare_exp.dt, Year, 'Healthcare_Exp', 2:15, na.rm = FALSE, convert = FALSE)
setDT(healthcare_exp.dt)

#View(healthcare_exp.dt)

#===============================================================================

#Clean data for GDP Per Capita (Current USD)

#===============================================================================

#import GDP Per Capita dataset
GDPpercap <- fread("GDP Per Capita (USD).csv")

#convert 1st row of GDPpercap to column header
names(GDPpercap) <- as.matrix(GDPpercap[1, ])
GDPpercap <- GDPpercap[-1, ]
GDPpercap[] <- lapply(GDPpercap, function(x) type.convert(as.character(x)))
GDPpercap #check if done correctly

#Only want data from 2006-2019 onwards
GDPpercap <- GDPpercap[,c(1,51:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "GDP Per Capita" 
GDPpercap <- gather(GDPpercap, Year, 'GDP Per Capita', 2:15, na.rm = FALSE, convert = FALSE)
setDT(GDPpercap)

#View(GDPpercap)

#===============================================================================

#Clean data for Development Assistance per capita

#===============================================================================

#import Development Assistance per capita dataset
assistancepercap <- fread("Development Assistance per Capita.csv")

#convert 1st row of assistancepercap to column header
names(assistancepercap) <- as.matrix(assistancepercap[1, ])
assistancepercap <- assistancepercap[-1, ]
assistancepercap[] <- lapply(assistancepercap, function(x) type.convert(as.character(x)))
assistancepercap #check if done correctly

#Only want data from 2006 onwards
assistancepercap <- assistancepercap[,c(1,51:64)]

#convert columns that are Years to become Rows, New columns created will be Years & "Development Assistance Per Capita" 
assistancepercap <- gather(assistancepercap, Year, 'Aid per capita', 2:15, na.rm = FALSE, convert = FALSE)
setDT(assistancepercap)

#View(assistancepercap)

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

#We only want years 2006 - 2019
HDI.dt <- HDI.dt[,-c(2:17)]

#convert columns that are Years to become Rows, New columns created will be Years & "Human Development Index" 
HDI.dt <- gather(HDI.dt, Year, 'Human Development Index', 2:15, na.rm = FALSE, convert = FALSE) #tranpose
setDT(HDI.dt)
#View(HDI.dt)



#===============================================================================

#Clean data for Rule of Law Index

#===============================================================================

#import Rule of Law dataset
ruleoflaw <- fread("Rule of Law.csv")

#Extract columns that are needed, Country Name, Year, Rule of law index
ruleoflaw <- ruleoflaw[(ruleoflaw$Indicator == "Rule of Law" & ruleoflaw$`Subindicator Type` == "Estimate")]

#Only want data from 2006-2019 onwards
ruleoflaw <- ruleoflaw[,c(2,13:26)]

#convert columns that are Years to become Rows, New columns created will be Years & "Rule Of Law Index" 
ruleoflaw <- gather(ruleoflaw, Year, 'Rule of Law', 2:15, na.rm = FALSE, convert = FALSE) #tranpose
setDT(ruleoflaw)

#View(ruleoflaw)

#===============================================================================

#Clean data for Voice & Accountability Index

#===============================================================================

#import Rule of Law dataset
Voice_accountability <- fread("Rule of Law.csv")

#Extract columns that are needed, Country Name, Year, Voice & Accountability index
Voice_accountability <- Voice_accountability[(Voice_accountability$Indicator == "Voice and Accountability" & Voice_accountability$`Subindicator Type` == "Estimate")]

#Only want data from 2006-2019 onwards
Voice_accountability <- Voice_accountability[,c(2,13:26)]

#convert columns that are Years to become Rows, New columns created will be Years & "Voice & Accountability" 
Voice_accountability <- gather(Voice_accountability, Year, 'Voice.Accountability', 2:15, na.rm = FALSE, convert = FALSE) #tranpose
setDT(Voice_accountability)

#View(Voice_accountability)

#===============================================================================

#Clean data for Political Stability No Violence Index

#===============================================================================

#import Rule of Law dataset
noviolence <- fread("Rule of Law.csv")

#Extract columns that are needed, Country Name, Year, Political Stability No Violence index
noviolence <- noviolence[(noviolence$Indicator == "Political Stability No Violence" & noviolence$`Subindicator Type` == "Estimate")]

#Only want data from 2006-2019 onwards
noviolence <- noviolence[,c(2,13:26)]

#convert columns that are Years to become Rows, New columns created will be Years & "No violence index" 
noviolence <- gather(noviolence, Year, 'No violence Index', 2:15, na.rm = FALSE, convert = FALSE) #tranpose
setDT(noviolence)

#View(noviolence)

#===============================================================================

#Clean Data For Life Satisfaction Indicator

#===============================================================================
#import life satisfaction indicator
life_satisfaction <- fread("Life Satisfaction Data.csv")

#renaming column name to match with other dataset and easier reference
setnames(life_satisfaction, "Entity", "Country Name")
setnames(life_satisfaction, "Life satisfaction in Cantril Ladder (World Happiness Report 2021)", "Life_Satisfaction")

#We only want country, Year and Life Satisfaction indicator columns
life_satisfaction <- life_satisfaction[,c(1,3,4)]

#change Year to chr data type to match
life_satisfaction <- life_satisfaction[, Year:=as.character(Year)]

#===============================================================================

#joining of datasets

#===============================================================================
#Inner join of all the datasets when country name and year matches
#The final dt will be FSI_data
#FSI.dt will be the main dataset, since its what we are going to predict

FSI_data <- merge(FSI.dt, ruleoflaw, by = c("Country Name","Year"))
FSI_data <- merge(FSI_data, Voice_accountability, by = c("Country Name","Year"))
FSI_data <- merge(FSI_data, noviolence, by = c("Country Name","Year"))
FSI_data <- merge(FSI_data, CPI.dt, by = c("Country Name","Year"))
FSI_data <- merge(FSI_data, HDI.dt, by = c("Country Name","Year"))
FSI_data <- merge(FSI_data, life_satisfaction, by = c("Country Name","Year"))
FSI_data <- merge(FSI_data, healthcare_exp.dt, by = c("Country Name","Year"))
FSI_data <- merge(FSI_data, GDPpercap, by = c("Country Name","Year"))
FSI_data <- merge(FSI_data, unemployment.dt, by = c("Country Name","Year"))
FSI_data <- merge(FSI_data, assistancepercap, by = c("Country Name","Year"))


#check and ensure, that all data are correctly merged, take a small sample size and crosscheck
#test case used here: Argentina
#View(FSI_data)


#======================================================================================================

#Forecast 1 year into the future, and forecast 4 years into the future

#======================================================================================================
#Forecasting 1 year into the future

FSI_data <- FSI_data %>%
  group_by(`Country Name`) %>%
  mutate(`FSI (Year+1)` = lead(`FSI`))

#Forecasting 4 year into the future
FSI_data <- FSI_data %>%
  group_by(`Country Name`) %>%
  mutate(`FSI (Year+4)` = lead(`FSI`,4))


setDT(FSI_data)



#removing NAs for FSI Forecasting 1 year into the future (for linear regression)
FSI_data1 <- na.omit(FSI_data[,c(1:14)])

#removing NAs for FSI forecasting 4 years into the future (for linear regression)
FSI_data4 <- na.omit(FSI_data[,c(1:13,15)])

#FSI data for CART, we don't need to remove NAs cause CART can handle missing values through surrogates

#1 year ahead forecasting
FSI_cart1 <- FSI_data[,c(1:14)]


#Here, we want to remove NAs in the outcome variable in FSI_cart1 
FSI_cart1 <- FSI_cart1[!is.na(FSI_cart1$`FSI (Year+1)`),]

#Group the FSI into categorical, we will be using Categorical for CART
summary(FSI_cart1$`FSI (Year+1)`)
cut.off <- c(0, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110,120)
FSI_cart1[, `Fragility_Category (Year+1)` := cut(`FSI (Year+1)`, breaks=cut.off, include.lowest=T)]
summary(FSI_cart1$`Fragility_Category (Year+1)`)
levels(FSI_cart1$`Fragility_Category (Year+1)`) = c("Very sustainable","Sustainable","More Stable","Stable",
                       "Less Stable","Warning","Elevated Warning","High Warning",
                       "Alert","High Alert","Very High Alert")
#verify that the categories are changed correctly
summary(FSI_cart1$`Fragility_Category (Year+1)`)

#Forecast the political stability 4 years into the future
FSI_cart4 <- FSI_data[,c(1:13,15)]
#Here, we want to remove NAs in the outcome variable in FSI_cart4
FSI_cart4 <- FSI_cart4[!is.na(FSI_cart4$`FSI (Year+4)`),]
summary(FSI_cart4$`FSI (Year+4)`)
cut.off <- c(0, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110,120)
FSI_cart4[, `Fragility_Category (Year+4)` := cut(`FSI (Year+4)`, breaks=cut.off, include.lowest=T)]
summary(FSI_cart4$`Fragility_Category (Year+4)`)
levels(FSI_cart4$`Fragility_Category (Year+4)`) = c("Very sustainable","Sustainable","More Stable","Stable",
                                                    "Less Stable","Warning","Elevated Warning","High Warning",
                                                    "Alert","High Alert","Very High Alert")
#verify that the categories are changed correctly
summary(FSI_cart4$`Fragility_Category (Year+4)`)


#====================================================================================================================

#Separating the dataset into different economies: developed, developing,least developed and transitioning economies

#====================================================================================================================

#Grouping the different countries according to how developed they are
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
                          "Lebanon", "Oman", "Qatar", "Saudi Arabia", "Turkey", "United Arab Emirates","Suriname", "Maldives")

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

#Adding a control for the generalised CART model for forecasting 4 years ahead
#The control will be economy type, which is grouped into:
#developing, developed, least_developed and transitioning economies.
#A control is needed because the independent variables are time dependent.
FSI_cart4$economy_type <- 0
FSI_cart4[FSI_cart4$`Country Name` %in% developed_economies, economy_type := 1]
FSI_cart4[FSI_cart4$`Country Name` %in% developing_economies, economy_type := 2]
FSI_cart4[FSI_cart4$`Country Name` %in% least_developed_economies, economy_type := 3]
FSI_cart4[FSI_cart4$`Country Name` %in% transitioning_economies, economy_type := 4]
#convert to categorical variable. 
FSI_cart4$economy_type <- factor(FSI_cart4$economy_type)
levels(FSI_cart4$economy_type) = c("Developed","Developing","Least_Developed","Transitioning")
#verify that the categories are changed correctly
summary(FSI_cart4$economy_type)


#Forecasting 1 year ahead using linear regression for different economy types
#For developed_countries, the external assistance column is mostly empty, thus we will remove it.
#FSI_data is the data which NAs has not been removed, we will first remove the column of external assistance
#then afterwards, we will remove the remaining NAs.
developed_countries_linreg1 <- FSI_data[FSI_data$`Country Name` %in% developed_economies,]
developed_countries_linreg1 <- developed_countries_linreg1[,-c(13,15)]
developed_countries_linreg1 <- na.omit(developed_countries_linreg1[,c(1:13)])

#For the remaining types of economies, there is no problem with the external assistance column
developing_countries_linreg1 <- FSI_data1[FSI_data1$`Country Name` %in% developing_economies,]
leastdeveloped_linreg1 <- FSI_data1[FSI_data1$`Country Name` %in% least_developed_economies,]
transitioning_linreg1 <- FSI_data1[FSI_data1$`Country Name` %in% transitioning_economies,]

#Forecasting 4 years ahead using linear regression for different economy types
#For developed_countries, the external assistance column is mostly empty, thus we will remove it.
#FSI_data is the data which NAs has not been removed, we will first remove the column of external assistance
developed_countries_linreg4 <- FSI_data[FSI_data$`Country Name` %in% developed_economies,]
developed_countries_linreg4 <- developed_countries_linreg4[,-c(13,14)]
developed_countries_linreg4 <- na.omit(developed_countries_linreg4[,c(1:13)])


#For the remaining types of economies, there is no problem with the external assistance column
developing_countries_linreg4 <- FSI_data4[FSI_data4$`Country Name` %in% developing_economies,]
leastdeveloped_linreg4 <- FSI_data4[FSI_data4$`Country Name` %in% least_developed_economies,]
transitioning_linreg4 <- FSI_data4[FSI_data4$`Country Name` %in% transitioning_economies,]

#Forecasting 1 year ahead using CART for different economy types
developed_countries_cart1 <- FSI_cart1[FSI_cart1$`Country Name` %in% developed_economies,]
developing_countries_cart1 <- FSI_cart1[FSI_cart1$`Country Name` %in% developing_economies,]
leastdeveloped_cart1 <- FSI_cart1[FSI_cart1$`Country Name` %in% least_developed_economies,]
transitioning_cart1 <- FSI_cart1[FSI_cart1$`Country Name` %in% transitioning_economies,]


'%!in%' <- function(x,y)!('%in%'(x,y)) #use this function to check, if any of the countries were left out
crosscheck <- FSI_cart1[FSI_cart1$`Country Name` %!in% 
                        c(transitioning_economies,developing_economies,
                          developed_economies,least_developed_economies),]
crosscheck #replace FSI_cart1 accordingly to check, all should be empty data.table

#===============================================================================

#fwrite files out for convenience to perform regression analysis for political stability(measured via FSI)

#===============================================================================


#set working directory to export files to
setwd("C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Cleaned")

#export the cleaned datasets using write.csv
#write.csv("datasetname,"filename.....")

write.csv(developed_countries_linreg1, "developed_countries_linreg1.csv")
write.csv(developing_countries_linreg1, "developing_countries_linreg1.csv")
write.csv(leastdeveloped_linreg1, "leastdeveloped_linreg1.csv")
write.csv(transitioning_linreg1, "transitioning_linreg1.csv")

write.csv(developed_countries_linreg4,"developed_countries_linreg4.csv")
write.csv(leastdeveloped_linreg4,"leastdeveloped_linreg4.csv")
write.csv(developing_countries_linreg4,"developing_countries_linreg4.csv")
write.csv(transitioning_linreg4,"transitioning_linreg4.csv")

write.csv(developed_countries_cart1,"developed_countries_cart1.csv")
write.csv(developing_countries_cart1,"developing_countries_cart1.csv")
write.csv(leastdeveloped_cart1,"leastdeveloped_cart1.csv")
write.csv(transitioning_cart1,"transitioning_cart1.csv")

write.csv(FSI_cart4,"FSI_cart4.csv")
write.csv(FSI_data,"FSI_data.csv")
