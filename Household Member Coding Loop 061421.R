
# Household matrix Loop

library(survey)
library(tibble)
library(dplyr)
library(tidyr)
library(haven)
library(stringr)
library(questionr)
library(sjlabelled)
library(xlsx)

options(scipen=999)


surveys  <- read.xlsx2("C:/Users/KristinBietsch/files/Desktop/Master DHS Survey List.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                       colClasses = c("character", "character", "character", "numeric",
                                      "character", "character", "character", "character", "numeric",
                                      "character", "numeric", "character", "character", "character"));

surveys <- surveys %>% filter(PR!="")
surveys <- surveys %>% filter(API_ID!="JO1990DHS")

# exlcude because no breakdown between parents and parent in law
surveys <- surveys %>% filter(API_ID!="BO2003DHS") %>% filter(API_ID!="DR1996DHS") %>% filter(API_ID!="DR2002DHS") %>% filter(API_ID!="DR2007DHS") %>% filter(API_ID!="DR2013DHS") %>% filter(API_ID!="NC2001DHS") %>% filter(API_ID!="PE1992DHS")


surveys$PRfile <- paste( surveys$PR, ".DTA" , sep="")


surveys<- surveys %>% filter(API_ID=="MZ2015AIS" | API_ID=="GM2019DHS")

res_hv101 <- setNames(data.frame(matrix(ncol = 3,  nrow = 0)),  c("lab_hv101" , "val_hv101", "Survey")) %>% mutate( lab_hv101=as.character(lab_hv101), val_hv101=as.character(val_hv101), Survey=as.character(Survey), )

setwd("C:/Users/KristinBietsch/Files/DHSLoop")


for (row in 1:nrow(surveys)) {
  
  
  couple_data <- surveys[row, "PRfile"]
  countryname <- surveys[row, "API_ID"]

  PR <- read_dta(couple_data, col_select = any_of(c("hhid", "hvidx", "hv101", "hv102", "hv104", "hv105", "hv116" )))
  
  lab_hv101=get_labels(PR$hv101)
  val_hv101=get_values(PR$hv101)
  Z_hv101=as.data.frame(cbind(lab_hv101, val_hv101))
  Z_hv101$Survey <- countryname
  res_hv101 <- bind_rows(res_hv101 , Z_hv101) 
}




results_long <- res_hv101 %>% spread(Survey, lab_hv101)


write.csv(results_long, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/HV101Coding.csv", row.names = F, na="")


levels(as.factor(res_hv101$lab_hv101))
