
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

 

res_hv116 <- setNames(data.frame(matrix(ncol = 3,  nrow = 0)),  c("lab_hv116" , "val_hv116", "Survey")) %>% mutate( lab_hv116=as.character(lab_hv116), val_hv116=as.character(val_hv116), Survey=as.character(Survey) )

setwd("C:/Users/KristinBietsch/Files/DHSLoop")


for (row in 1:nrow(surveys)) {
  
  
  couple_data <- surveys[row, "PRfile"]
  countryname <- surveys[row, "API_ID"]
  
  PR <- read_dta(couple_data, col_select = any_of(c("hhid", "hvidx", "hv101", "hv102", "hv104", "hv105", "hv116" )))
  
  if (exists("hv116", PR)) {
    lab_hv116=get_labels(PR$hv116)
    val_hv116=get_values(PR$hv116)
    Z_hv116=as.data.frame(cbind(lab_hv116, val_hv116))
    Z_hv116$Survey <- countryname
    res_hv116 <- bind_rows(res_hv116 , Z_hv116)
  }
 
}


# AM2005DHS
# AM2000DHS
# BD1997DHS
# BJ2001DHS
# BJ1996DHS
# BO1994DHS
# BR1996DHS
# BR1991DHS
# BF2003DHS
# CM2004DHS
# CM1998DHS
# CM1991DHS
# CF1994DHS

#I think for consistency sake we should just use IR files for marital status