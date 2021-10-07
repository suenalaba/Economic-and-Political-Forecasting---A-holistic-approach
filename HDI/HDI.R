library(clean2a.table)
install.packages("psych")          # Install psych package
library("psych")

Htable = fread("2020_Statistical_Annex_Table_1.csv")
Htable

#HDI is the geometric mean (equally-weighted) of life expectancy, education, and GNI per capita\
?fread

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
lifA$`HDI Rank`="Life Expectnecy Index"

temp=rbind(eduA,incA)
clean1 = rbind(temp,lifA)
head(clean1)

names(clean1)[1]="Indexes"

geometric.mean(as.numeric(clean1$'1990'))


clean1$Country=NULL

clean2 = t(clean1)

head(clean2)

labels(clean2)



data_new <- clean2                         # Duplicate data frame
colnames(data_new) <- clean2[1, ]          # Convert first row to header
head(data_new)              

data_new <- data_new[- 1, ]  

colnames(data_new) <- c("Education Index", "Income Index", "Life Expectnecy Index")

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

data$Year = arr

summary(data)
head(data)

sum(is.na(data)) 




