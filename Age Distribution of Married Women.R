# Age Distribution of Married Women
# for MIL analysis decompostion


library(survey)
library(tibble)
library(dplyr)
library(tidyr)
library(haven)
library(stringr)
library(questionr)
library(sjlabelled)
library(xlsx)
require(RJSONIO)
library(WDI)
library(zoo)
library(cowplot)
library(ggpubr)
library(pdftools)


library(jsonlite) 
library(data.table)


options(scipen=999)
memory.limit(size = 2e6) 


#####################################################################
# Read in data
#####################################################################

surveys <- read.xlsx2("C:/Users/KristinBietsch/files/Desktop/Master DHS Survey List.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                      colClasses = c("character", "character", "character", "numeric",
                                     "character", "character", "character", "character", "numeric",
                                     "character", "numeric", "character", "character", "character", "character"));


surveys$IRfile <- paste( surveys$Survey, ".DTA" , sep="")


results_total <- setNames(data.frame(matrix(ncol = 3,  nrow = 0)),  c("Var1" ,  "Freq", "Survey")) %>% mutate(Var1=as.character(Var1),  Survey=as.character(Survey))

for (row in 1:nrow(surveys)) {
  data <- surveys[row, "IRfile"]
  countryname <- surveys[row, "API_ID"]
  countryid <- surveys[row, "Country"]

  setwd("C:/Users/KristinBietsch/Files/DHSLoop")
  
  
  
  women <- read_dta(data, col_select = any_of(c("v005",		"v013",		"v502" )))
  women <- women %>% filter(v502==1) %>% filter(v013<=7 & v013>=1) 
  
  women$sampleweights <- women$v005/100000
  
  
  results.mar <- as.data.frame(prop.table(wtd.table(x= as.factor(women$v013), weights = women$sampleweights))) %>% mutate(Var1=as.character(Var1),  Survey=as.character(countryname), Country=as.character(countryid))
  results_total <- bind_rows(results_total , results.mar)
  
  
}

setwd("C:/Users/KristinBietsch/files/Track20/MotherInLaw")

write.csv(results_total, "Age Distribution of Married Women.csv", row.names = FALSE, na="")
