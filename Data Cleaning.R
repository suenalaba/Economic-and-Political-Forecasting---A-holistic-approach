#importing relevant libraries to be updated as we go
library(data.table) 

#set your working directory here
setwd("C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Raw")

#===============================================================================

#Cleaning Data for HDI

#===============================================================================


#import HDI data ("your file name.csv") as data table
HDI.dt <- fread("HDI Values 1.csv",na.strings = c("..")) #note data.table library has to be run

#check if HDI data imported correctly
HDI.dt

#remove rows that consist entirely of words that are unnecessary
HDI.dt <- HDI.dt[6:212]

#convert 1st row of HDI.dt to column header
names(HDI.dt) <- as.matrix(HDI.dt[1, ])
HDI.dt <- HDI.dt[-1, ]
HDI.dt[] <- lapply(HDI.dt, function(x) type.convert(as.character(x)))
HDI.dt #check if done correctly

#Because of the way CSV was structured, some columns are entirely NA, hence, we want to remove them:
HDI.dt <- HDI.dt[,which(unlist(lapply(HDI.dt, function(x)!all(is.na(x))))),with=F]

#For now, we are interested in the index only, not the ranking 
HDI.dt <- HDI.dt[,2:32]

#Remove rows that are generic data eg world, region as we are specifically interested in predicting HDI For countries
HDI.dt <- HDI.dt[1:189]

#Only want data from 2005 onwards
HDI.dt <- HDI.dt[,c(1,17:29)]

#Check that all data are cleaned and table is correct
View(HDI.dt)


#Export cleaned data as new CSV for easier usage
#specify path and name of csv in 2nd paramater of function write.csv()
write.csv(HDI.dt, "C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Cleaned/HDI_cleaned.csv")

#===============================================================================

#Clean data for life expectancy at birth

#===============================================================================

#Import life expectancy at birth
life_expectancy.dt <- fread("Life Expectancy at Birth 3.csv")

#drop the code column, only country name needed
life_expectancy.dt <- life_expectancy.dt[,c(1,3:4)]



#only interested in data from 2005-2017
life_expectancy.dt <- setDT(life_expectancy.dt)[!(Year %between% c(1600, 2004))]
life_expectancy.dt <- setDT(life_expectancy.dt)[!(Year %between% c(2018, 2019))]

setnames(life_expectancy.dt, "Entity", "Country")

View(life_expectancy.dt)

write.csv(life_expectancy.dt, "C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Cleaned/Life Expectancy_cleaned.csv")
#===============================================================================

#Clean data for Mean Years of Schooling

#===============================================================================

#Import Mean Years of Schooling
mean_schooling.dt <- fread("Mean Years Of Schooling 1.csv")
setnames(mean_schooling.dt,"Average Total Years of Schooling for Adult Population (Lee-Lee (2016), Barro-Lee (2018) and UNDP (2018))","Average Total Years of Schooling",)
mean_schooling.dt <- mean_schooling.dt[,c(1,3:4)]
mean_schooling.dt <- setDT(mean_schooling.dt)[!(Year %between% c(1600, 2004))]

setnames(mean_schooling.dt, "Entity", "Country")

View(mean_schooling.dt)

write.csv(mean_schooling.dt, "C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Cleaned/Mean Schooling_cleaned.csv")
#===============================================================================

#Clean data for Expected Years of Schooling

#===============================================================================
expected_schooling.dt <- fread("Expected Years of Schooling 1.csv")
expected_schooling.dt <- expected_schooling.dt[,c(1,3:4)]
expected_schooling.dt <- setDT(expected_schooling.dt)[!(Year %between% c(1600, 2004))]

setnames(expected_schooling.dt, "Entity", "Country")

View(expected_schooling.dt)

write.csv(expected_schooling.dt, "C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Cleaned/Expected Schooling_cleaned.csv")
#===============================================================================

#Clean data for GNI Per Capita (Constant PPP 2011 $)

#===============================================================================
GNI_ppp_2011.dt <- fread("GNI Per Capita Constant 2011 1.csv")

#convert 1st row of GNI_ppp_2011.dt to column header
names(GNI_ppp_2011.dt) <- as.matrix(GNI_ppp_2011.dt[1, ])
GNI_ppp_2011.dt <- GNI_ppp_2011.dt[-1, ]
GNI_ppp_2011.dt[] <- lapply(GNI_ppp_2011.dt, function(x) type.convert(as.character(x)))
GNI_ppp_2011.dt #check if done correctly


#Extract only columns with country names, and years 2005-2017
GNI_ppp_2011.dt <- GNI_ppp_2011.dt[,c(1,21:33)]


View(GNI_ppp_2011.dt)

write.csv(GNI_ppp_2011.dt, "C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Cleaned/GNI_cleaned_2011ppp.csv")

#===============================================================================


#Clean data for GNI Per Capita (Constant PPP 2017 $)

#===============================================================================

GNI_ppp_2017.dt <- fread("GNI per Capita constant 2017 1.csv")

#remove rows that consist entirely of words that are unnecessary
GNI_ppp_2017.dt <- GNI_ppp_2017.dt[5:271]

#convert 1st row of GNI_ppp_2011.dt to column header
names(GNI_ppp_2017.dt) <- as.matrix(GNI_ppp_2017.dt[1, ])
GNI_ppp_2017.dt <- GNI_ppp_2017.dt[-1, ]
GNI_ppp_2017.dt[] <- lapply(GNI_ppp_2017.dt, function(x) type.convert(as.character(x)))
GNI_ppp_2017.dt #check if done correctly


#Extract only columns with country names, and years 2005-2017
GNI_ppp_2017.dt <- GNI_ppp_2017.dt[,c(1,50:62)]

#rename column
setnames(GNI_ppp_2017.dt, "Country Name", "Country")

View(GNI_ppp_2017.dt)

write.csv(GNI_ppp_2017.dt, "C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Cleaned/GNI_cleaned_2017ppp.csv")


#===============================================================================


#Clean data for GNI Per Capita (Constant PPP International $)

#===============================================================================
GNI_ppp_int.dt <- fread("GNI Per Capita PPP 1.csv")

#remove rows that consist entirely of words that are unnecessary
GNI_ppp_int.dt <- GNI_ppp_int.dt[5:271]

#convert 1st row of GNI_ppp_2011.dt to column header
names(GNI_ppp_int.dt) <- as.matrix(GNI_ppp_int.dt[1, ])
GNI_ppp_int.dt <- GNI_ppp_int.dt[-1, ]
GNI_ppp_int.dt[] <- lapply(GNI_ppp_int.dt, function(x) type.convert(as.character(x)))
GNI_ppp_int.dt #check if done correctly


#Extract only columns with country names, and years 2005-2017
GNI_ppp_int.dt <- GNI_ppp_int.dt[,c(1,50:62)]

#rename column
setnames(GNI_ppp_int.dt, "Country Name", "Country")

View(GNI_ppp_int.dt)

write.csv(GNI_ppp_int.dt, "C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Cleaned/GNI_cleaned_intppp.csv")



