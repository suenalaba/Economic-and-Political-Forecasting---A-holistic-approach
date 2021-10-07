library(data.table)

setwd("C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Cleaned")


HDI.dt <- fread("HDI_cleaned_final.csv") #note data.table library has to be run
View(HDI.dt)

Life_Exp.dt <- fread("Life Expectancy_cleaned.csv") #note data.table library has to be run
Life_Exp.dt <- Life_Exp.dt[,2:4]
View(Life_Exp.dt)

Mean_Sch.dt <- fread("Mean Schooling_cleaned.csv") #note data.table library has to be run
Mean_Sch.dt <- Mean_Sch.dt[,2:4]
View(Mean_Sch.dt)

Exp_Sch.dt <- fread("Expected Schooling_cleaned.csv") #note data.table library has to be run
Exp_Sch.dt <- Exp_Sch.dt[,2:4]
View(Exp_Sch.dt)

GNI_ppp2011.dt <- fread("GNI_cleaned_2011ppp_final.csv") #note data.table library has to be run
View(GNI_ppp2011.dt)

GNI_ppp2017.dt <- fread("GNI_cleaned_2017ppp_final.csv") #note data.table library has to be run
View(GNI_ppp2017.dt)

GNI_pppint.dt <- fread("GNI_cleaned_intppp_final.csv") #note data.table library has to be run
View(GNI_pppint.dt)

#===============================================================================

#Merge datasets into a single table for analysis

#===============================================================================
HDI_final_table.dt <- merge(HDI.dt,Life_Exp.dt, by = c("Country","Year"), all = TRUE)
HDI_final_table.dt <- merge(HDI_final_table.dt,Mean_Sch.dt, by = c("Country","Year"), all = TRUE)
HDI_final_table.dt <- merge(HDI_final_table.dt,Exp_Sch.dt, by = c("Country","Year"), all = TRUE)
HDI_final_table.dt <- merge(HDI_final_table.dt,GNI_ppp2011.dt, by = c("Country","Year"), all = TRUE)

HDI_final_table.dt <- na.omit(HDI_final_table.dt)

View(HDI_final_table.dt)

write.csv(HDI_final_table.dt, "C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Cleaned/HDI_indicators_cleaned_final.csv")
