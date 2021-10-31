library(data.table)
# Install psych package
library("psych")
library("tidyverse")
library("mtcars")

#sources
##http://hdr.undp.org/en/countries/profiles/ARG#
##https://ourworldindata.org/country/argentina
##https://data.worldbank.org/country/AR

####################################################################################################################Cleaning data for indexes

#HDI is the geometric mean (equally-weighted) of life expectancy, education, and GNI per capita

#obtain data for the 3 indexes
edu = fread("Education index.csv",fill = TRUE,header=TRUE)
inc = fread("Income index.csv",fill = TRUE,header=TRUE)
lif = fread("Life expectancy index.csv",fill = TRUE,header=TRUE)

#obtains Education index data
edu$Country
head(edu)
eduA= edu[Country==" Argentina",]
summary(eduA)
if (!require("tidyverse")) install.packages("tidyverse")
eduA=eduA %>% discard(~all(is.na(.) | . ==""))
eduA$`HDI Rank`="Education Index"
eduA


#obtaining income index data
incA = inc[Country=="Argentina",]
incA=incA %>% discard(~all(is.na(.) | . ==""))
summary(incA)
incA$`HDI Rank`="Income Index"
incA

#obtaining life expectency data
lifA = lif[Country == "Argentina",]
lifA=lifA %>% discard(~all(is.na(.) | . ==""))
summary(lifA)
lifA$`HDI Rank`="Life Expectency Index"
lifA

#combining data
temp=rbind(eduA,incA)
clean1 = rbind(temp,lifA)
head(clean1)

names(clean1)[1]="Indexes"
class(clean1)

clean1$Country=NULL

clean1
#transposing table to have years have columns
clean2 = t(clean1) 
clean2

head(clean2)

labels(clean2)

data_new <- clean2                         
head(data_new)              

data_new <- data_new[- 1, ]  
data_new

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

data

sum(is.na(data)) 
## No Nulls

#ensuring data is numerical
data <- data[ , Education_Index := as.numeric(Education_Index)]
data <- data[ , Income_Index := as.numeric(Income_Index)]
data <- data[ , Life_Expectency_Index := as.numeric(Life_Expectency_Index)]


#Calculating current year HDI
for(x in 1:30){
  data$HDI[x]=geometric.mean(data[x,c(Education_Index,Income_Index,Life_Expectency_Index)])
}
colnames(data) <- c("Education_Index", "Income_Index", "Life_Expectency_Index","year","HDI")
data

write.csv(data,"C:\\NTU\\Business\\Y2S1\\Analytics\\AY21 Team Assignment and Project\\data.csv", row.names = FALSE)


#shifting values of HDI 5 years to allow indexes(t) to point to HDI(t+5)
data1=data

shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

data1$HDI <- shift(data1$HDI, 5)

data1=data1[order(-data1$year)]

data1

arr = c();
y=2024
for(x in 1:30) 
{
  arr[x]=y
  y=y-1
}
arr

data1$pred_Year=arr

data1

wdata=data1[c(6:30),]

wdata

write.csv(wdata,"C:\\NTU\\Business\\Y2S1\\Analytics\\AY21 Team Assignment and Project\\wdata.csv", row.names = FALSE)




####################################################################################################################Cleaning data for Life Expectancy predictors(Argentina Only)

##greenhouse gas Emmsions per capita
co2 = fread("ghg-emissions-per-capita.csv")
co2
co2a = subset(co2,Entity=="Argentina",)
co2a
co2a$Code=NULL
co2a$Entity=NULL
names(co2a)[1]="year"
summary(co2a)
co2a
## Ferility rate
fr = fread("fertility-rate-complete-gapminder.csv",fill = TRUE,header=TRUE)
fr
fra = subset(fr,Entity=="Argentina",)
fra
fra$Code=NULL
fra$Entity=NULL
names(fra)[1]="year"
summary(fra)
fra
## public health expenditure by gdp
phe = fread("public-health-expenditure-share-GDP-OWID.csv")
phe
phe1= subset(phe,Entity=="Argentina")
phe1$Code=NULL
phe1$Entity=NULL
names(phe1)[1]="year"
summary(phe1)
phe1
## public health expenditure per capita
phpc = fread("out-of-pocket-expenditure-per-capita-on-healthcare.csv")
phpc
phpca = subset(phpc,Entity=="Argentina",)
phpca
phpca$Code=NULL
phpca$Entity=NULL
names(phpca)[1]="year"
summary(phpca)
phpca
## burden of disease
bod = fread("dalys-rate-from-all-causes.csv")
bod
bod1= subset(bod,Entity=="Argentina")
bod1$Code=NULL
bod1$Entity=NULL
names(bod1)[2]="year"
summary(bod1)
bod1
## youth mortality rate
mr = fread("youth-mortality-rate.csv")
mr
mr1= subset(mr,Entity=="Argentina")
mr1$Code=NULL
mr1$Entity=NULL
names(mr1)[1]="year"
summary(mr1)
mr1
##hdi argentina
hdi = fread("human-development-index.csv")
hdi
hdi1= subset(hdi,Entity=="Argentina")
hdi1$Code=NULL
hdi1$Entity=NULL
names(hdi1)[1]="year"
summary(hdi1)
hdi1


## combining tables
co2a
fra
phe1
phpca
bod1
mr1
hdi1

t=merge(x = co2b, y = fra, by = "year", all = TRUE)
t=merge(x = t, y = phe1, by = "year", all = TRUE)
t=merge(x = t, y = phpcb, by = "year", all = TRUE)
t=merge(x = t, y = tbb, by = "year", all = TRUE)
t=merge(x = t, y = mr1, by = "year", all = TRUE)
t=merge(x = t, y = hdi1, by = "year", all = TRUE)
t
names(t)=c("year","greenhouse_gas_per_capita","public_expenditure_health_%gdp","fertiltiy_rate","oop_expenditure_healthcare_per_capita","youth_mortality_rate","burden_of_disease","hdi")



##shifting hdi by 5 years

t2=t
t2$hdi <- shift(t2$hdi, 5)

t2=t2[order(-t2$year)]

t2$year
t3 = t2[year<=2013]

t3

write.csv(t3,"C:\\NTU\\Business\\Y2S1\\Analytics\\AY21 Team Assignment and Project\\LE factors.csv", row.names = FALSE)


####################################################################################################################Cleaning data for Life Expectancy predictors(all countries)
##greenhouse gas Emmsions per capita
ghpc = fread("ghg-emissions-per-capita.csv")
ghpc
ghpc1= ghpc
ghpc1$Code=NULL
names(ghpc1)[2]="year"
summary(ghpc1)
ghpc1

##public health expenditure
pheAC = fread("public-health-expenditure-share-GDP-OWID.csv")
pheAC
pheAC1= pheAC
pheAC1$Code=NULL
names(pheAC1)[2]="year"
summary(pheAC1)
pheAC1

##fertility rate
frAC = fread("fertility-rate-complete-gapminder.csv",fill = TRUE,header=TRUE)
frAC
frACa = frAC
frACa
frACa$Code=NULL
names(frACa)[2]="year"
summary(frACa)
frACa

##out of pocket expenditure-per capita on healthcare
oophAC = fread("out-of-pocket-expenditure-per-capita-on-healthcare.csv")
oophAC
oophACa = oophAC
oophACa
oophACa$Code=NULL
names(oophACa)[2]="year"
summary(oophACa)
oophACa

##youth mortality rate
mrAC = fread("youth-mortality-rate.csv")
mrAC
mrAC1= mrAC
mrAC1$Code=NULL
names(mrAC1)[2]="year"
summary(mrAC1)
mrAC1

## burden of disease
bodAC = fread("dalys-rate-from-all-causes.csv")
bodAC
bodAC1= bodAC
bodAC1$Code=NULL
names(bodAC1)[2]="year"
summary(bodAC1)
bodAC1

##HDI
hdiAC = fread("human-development-index.csv")
hdiAC
hdiAC1= hdiAC
hdiAC1$Code=NULL
names(hdiAC1)[2]="year"
summary(hdiAC1)
hdiAC1



#####merging
ghpc1
pheAC1
frACa
oophACa
mrAC1
bodAC1
hdiAC1



table1=merge(x = ghpc1, y = pheAC1, by = c("Entity","year"), all = TRUE)
table1=merge(x = table1, y = frACa, by = c("Entity","year"), all = TRUE)
table1=merge(x = table1, y = oophACa, by = c("Entity","year"), all = TRUE)
table1=merge(x = table1, y = mrAC1, by = c("Entity","year"), all = TRUE)
table1=merge(x = table1, y = bodAC1, by = c("Entity","year"), all = TRUE)
table1=merge(x = table1, y =hdiAC1, by = c("Entity","year"), all = TRUE)
table1
names(table1)=c("country","year","greenhouse_gas_per_capita","public_expenditure_health_%gdp","fertiltiy_rate","oop_expenditure_healthcare_per_capita","youth_mortality_rate","burden_of_disease","hdi")
table1
table1$year

##shifting hdi by 5 years


table2=table1
table2$hdi <- shift(table2$hdi, 5)

table2=table2[order(-table2$year)]

table2$year
table3 = table2[year<2013]

table3
table4=table3[complete.cases(table3),]
table4


write.csv(table4,"C:\\NTU\\Business\\Y2S1\\Analytics\\AY21 Team Assignment and Project\\All countries LE factors.csv", row.names = FALSE)




table2[country=="Argentina"&year>2005]


