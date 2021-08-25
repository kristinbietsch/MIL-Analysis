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
memory.limit(9999999999)

# Age distribution
age <- read.csv("C:/Users/KristinBietsch/files/Track20/MotherInLaw/Age Distribution of Married Women.csv")

# MIL by age
mil <- read.csv("C:/Users/KristinBietsch/files/Track20/MotherInLaw/Data_MILAGE072721.csv")

age_data <- age %>% filter(Survey== "NP2016DHS" | Survey==  "NP2011DHS" | Survey== "EG2014DHS" 	| Survey== "EG1992DHS" | Survey== 	"SN2019DHS" | Survey== "SN1993DHS" | Survey== "TR2013DHS" | Survey== "TR1993DHS" ) %>%
  rename(v013=Var1) %>% select(-Country)
mil_data <- mil %>% select(-Survey) %>% rename(Survey=API_ID) %>%
  filter(Survey== "NP2016DHS" | Survey==  "NP2011DHS" | Survey== "EG2014DHS" 	| Survey== "EG1992DHS" | Survey== 	"SN2019DHS" | Survey== "SN1993DHS" | Survey== "TR2013DHS" | Survey== "TR1993DHS" )

data <- full_join(age_data, mil_data, by=c("v013", "Survey")) %>% 
  select(v013, Country, StartYear, Freq, Live_MIL) %>% 
  group_by(Country) %>% mutate(max=max(StartYear)) %>% 
  mutate(Recent=case_when(max==StartYear ~ 1, max!=StartYear ~ 0)) %>%
  select(-StartYear, -max) %>%
  gather(Variable, Value, Freq:Live_MIL) %>%
  mutate(Variable=paste(Variable, Recent, sep="_")) %>%
  select(-Recent) %>%
  spread(Variable, Value) %>% 
  mutate(age.contribution=  (Freq_1 - Freq_0) * ((Live_MIL_1 + Live_MIL_0)/2) ,
           rates.contribution= (Live_MIL_1 -Live_MIL_0 ) * ((Freq_1 + Freq_0)/2)) %>%
  

names(data)
data_sum <- data %>%   group_by(Country) %>%summarize(age.contribution=sum(age.contribution), 
                                               rates.contribution=sum(rates.contribution))

write.csv(data_sum, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/Decomposition 081921.csv", row.names = F)
write.csv(data, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/Decomposition Age 081921.csv", row.names = F)
