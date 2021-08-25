# BO1994DHS
# BR1991DHS
# CM1998DHS
# CM1991DHS
# TD1997DHS
# CO1990DHS
# DR1991DHS
# HT1994DHS
# HN2005DHS
# IA1999DHS
# ML2001DHS
# ML1996DHS
# NI1998DHS
# NI1992DHS
# PE2012DHS
# SN2005DHS
# TG1998DHS




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
# Bringing in Recodes 
recodes <- read.csv("C:/Users/KristinBietsch/files/Track20/MotherInLaw/HV101Coding_Cleaned.csv")

recode_long <- recodes %>% gather(Survey, Clean_Label, AF2015DHS:ZW2015DHS)

# Relationship Matrix
rel_mat <- read.csv("C:/Users/KristinBietsch/files/Track20/MotherInLaw/Relationship Matrix.csv") %>% mutate(hv101=as.character(hv101), famhv101=as.character(famhv101) )
rel_mat_small <- rel_mat %>% select(Index, OtherRelative, Relationship)

####
surveys  <- read.xlsx2("C:/Users/KristinBietsch/files/Desktop/Master DHS Survey List.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                       colClasses = c("character", "character", "character", "numeric",
                                      "character", "character", "character", "character", "numeric",
                                      "character", "numeric", "character", "character", "character", "character"));

surveys$PRfile <- paste( surveys$PR, ".DTA" , sep="")
surveys$IRfile <- paste( surveys$Survey, ".DTA" , sep="")


setwd("C:/Users/KristinBietsch/files/DHSLoop")
####################################################
# BO1994DHS
# there are a few hv001 and hv002 that are the same

country_survey <- surveys %>% filter(API_ID=="BO1994DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]


PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "hv003", 'hv024', "hv101", "hv102", "hv104", "hv105", "hv116", "shsec")))
IR <- read_dta(ir_data, col_select = any_of(c( "caseid", "v003", "v001", "v002", "v005", "v013", "ssect", "v502", "scty", "v025", "v024", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))



PR_mini <- PR %>% filter(hv102==1) %>% select(hhid, hv001, hv002, hvidx, hv101,  hv104, hv105, shsec) %>% mutate(hhid_new=paste(hv001, hv002, shsec, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_")) %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, shsec,  hvidx, hv001, hv002,  hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, hv002, shsec, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -shsec, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002, ssect=shsec)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, ssect) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "ssect" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)

if (strata=="v023 v025"  )  {
  design <- svydesign(ids=~v001, strata=~v023 + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}

MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/BO1994DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/BO1994DHS_MILAGE072621.csv", row.names=F)



####################################################
# BR1991DHS
# Clusters are repeated across regions, add hv024 into id
country_survey <- surveys %>% filter(API_ID=="BR1991DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]


PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "hv024", "hv101", "hv102", "hv104", "hv105", "hv116")))
IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v002", "v005", "v013", "v502", "scty", "v025", "v024", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))


PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, hv002, hv024,  hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, hv024, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, hv024,  hvidx, hv001, hv002,  hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, hv002, hv024, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -hv024, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002, v024=hv024)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, v024) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "v024" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)

if (strata=="v023 v025"  )  {
  design <- svydesign(ids=~v001, strata=~v023 + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}

MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/BR1991DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/BR1991DHS_MILAGE072621.csv", row.names=F)



####################################################
# CM1998DHS
# hv001 and hv002 not unique, need to add in the structure number
country_survey <- surveys %>% filter(API_ID=="CM1998DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]


PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "hv101", "hv102", "hv104", "hv105", "hv116", "shstruct")))
IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v002", "v005", "v013", "v502", "scty", "v025", "v024", "sstruct", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))

PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, hv002, shstruct,  hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, shstruct, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, shstruct,  hvidx, hv001, hv002,  hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, hv002, shstruct, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -shstruct, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002, sstruct=shstruct)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, sstruct) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "sstruct" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)

if (strata=="sstrate")  {
  design <- svydesign(ids=~v001, strata=~sstrate, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}

MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/CM1998DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/CM1998DHS_MILAGE072621.csv", row.names=F)


####################################################
# CM1991DHS
# had to add in shstruct and shmenage
country_survey <- surveys %>% filter(API_ID=="CM1991DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]


PR <- read_dta(pr_data, col_select = any_of(c("hhid", "hvidx", "hv001",  "hv101", "hv102", "hv104", "hv105", "hv116", "shmenage",  "shstruct")))
IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v005", "v013", "v502", "sstruct", "smenage", "scty", "v025", "v024", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))


PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, shstruct, shmenage,  hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, shstruct, shmenage, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, shstruct, shmenage,  hvidx, hv001, hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, shstruct, shmenage, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -shmenage, -hvidx,  -hv001, -shstruct, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, sstruct=shstruct, smenage=shmenage)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, sstruct, v003, smenage) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "sstruct",  "v003" ,  "smenage" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)

if (strata=="sstrate")  {
  design <- svydesign(ids=~v001, strata=~sstrate, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}

MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/CM1991DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/CM1991DHS_MILAGE072621.csv", row.names=F)

####################################################
# TD1997DHS
# hv001 and hv002 not unique, need to add in the structure number
country_survey <- surveys %>% filter(API_ID=="TD1997DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]


PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "hv101", "hv102", "hv104", "hv105", "hv116", "shstruct")))
IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v002", "v005", "v013", "v502", "scty", "v025", "v024", "sstruct", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))

PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, hv002, shstruct,  hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, shstruct, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, shstruct,  hvidx, hv001, hv002,  hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, hv002, shstruct, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -shstruct, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002, sstruct=shstruct)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, sstruct) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "sstruct" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)

if (strata=="v022")  {
  design <- svydesign(ids=~v001, strata=~v022, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}


MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/TD1997DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/TD1997DHS_MILAGE072621.csv", row.names=F)


####################################################
# CO1990DHS
# hv001 and hv002 not unique, need to add in the structure number
country_survey <- surveys %>% filter(API_ID=="CO1990DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]


PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "hv101", "hv102", "hv104", "hv105", "hv116", "shviv")))
IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v002", "v005", "v013", "v502", "scty", "v025", "v024", "svivi", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))

PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, hv002, shviv,  hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, shviv, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, shviv,  hvidx, hv001, hv002,  hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, hv002, shviv, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -shviv, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002, svivi=shviv)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, svivi) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "svivi" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)


if (strata=="v024 v025")  {
  design <- svydesign(ids=~v001, strata=~v024 + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}

MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/CO1990DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/CO1990DHS_MILAGE072621.csv", row.names=F)


####################################################
# DR1991DHS
# hv001 and hv002 not unique, need to add in the structure number
# there are two households that are inputed twice
country_survey <- surveys %>% filter(API_ID=="DR1991DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]



PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "hv101", "hv102", "hv104", "hv105", "hv116", "shvivi")))
IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v002", "v005", "v013", "v502", "scty", "v025", "v024", "svivi", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))

PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, hv002, shvivi,  hvidx, hv101,  hv104, hv105) %>%
  group_by(hv001, hv002, shvivi,  hvidx, hv101,  hv104, hv105) %>% summarise(n=n()) %>% ungroup() %>% select(-n) %>% # gets rid of duplicate household
 mutate(hhid_new=paste(hv001, hv002, shvivi, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))  %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, shvivi,  hvidx, hv101,  hv104, hv105) %>% 
  group_by(hv001, hv002, shvivi,  hvidx, hv101,  hv104, hv105) %>% summarise(n=n()) %>% ungroup() %>% select(-n) %>%
  mutate(hhid_new=paste(hv001, hv002, shvivi, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -shvivi, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002, svivi=shvivi)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, svivi) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "svivi" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)


if (strata=="v023 v025"  )  {
  design <- svydesign(ids=~v001, strata=~v023 + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}

MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/DR1991DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/DR1991DHS_MILAGE072621.csv", row.names=F)

####################################################
# HT1994DHS
# hv001 and hv002 not unique, need to add in the structure number
country_survey <- surveys %>% filter(API_ID=="HT1994DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]



PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "hv101", "hv102", "hv104", "hv105", "hv116", "shstruct")))
IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v002", "v005", "v013", "v502", "scty", "v025", "v024", "sstruct", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))

PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, hv002, shstruct,  hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, shstruct, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, shstruct,  hvidx, hv001, hv002,  hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, hv002, shstruct, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -shstruct, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002, sstruct=shstruct)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, sstruct) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "sstruct" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)


if (strata=="sdepart v025" )  {
  design <- svydesign(ids=~v001, strata=~sdepart + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}

MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/HT1994DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/HT1994DHS_MILAGE072621.csv", row.names=F)

####################################################
# HN2005DHS
# hv001 and hv002 not unique, need to add in the structure number
country_survey <- surveys %>% filter(API_ID=="HN2005DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]



PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "hv101", "hv102", "hv104", "hv105", "hv116", "shhogar")))
IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v002", "v005", "v013", "v502", "scty", "v025", "v024", "shogar", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))

PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, hv002, shhogar,  hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, shhogar, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, shhogar,  hvidx, hv001, hv002,  hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, hv002, shhogar, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -shhogar, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002, shogar=shhogar)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, shogar) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "shogar" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)



if (strata=="v023 v025"  )  {
  design <- svydesign(ids=~v001, strata=~v023 + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}

MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/HN2005DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/HN2005DHS_MILAGE072621.csv", row.names=F)


####################################################
# IA1999DHS
# Clusters are repeated across regions, add hv024 into id
country_survey <- surveys %>% filter(API_ID=="IA1999DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]



PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "hv024", "hv101", "hv102", "hv104", "hv105", "hv116")))
IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v002", "v005", "v013", "v502", "scty", "v025", "v024", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))


PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, hv002, hv024,  hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, hv024, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, hv024,  hvidx, hv001, hv002,  hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, hv002, hv024, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -hv024, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002, v024=hv024)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, v024) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "v024" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)

if (strata=="v024 v025")  {
  design <- svydesign(ids=~v001, strata=~v024 + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}

MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/IA1999DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/IA1999DHS_MILAGE072621.csv", row.names=F)



####################################################
# ML2001DHS
# Clusters are repeated across regions, add structure into id
country_survey <- surveys %>% filter(API_ID=="ML2001DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]



PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "shconces", "hv101", "hv102", "hv104", "hv105", "hv116")))
IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v002", "v005", "v013", "v502", "scty", "v025", "sconces", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))


PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, hv002, shconces,  hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, shconces, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, shconces,  hvidx, hv001, hv002,  hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, hv002, shconces, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -shconces, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002, sconces=shconces)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, sconces) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "sconces" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)

if (strata=="v023 v025"  )  {
  design <- svydesign(ids=~v001, strata=~v023 + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}

MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/ML2001DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/ML2001DHS_MILAGE072621.csv", row.names=F)

####################################################
# ML1996DHS
# Clusters are repeated across regions, add structrue into id
country_survey <- surveys %>% filter(API_ID=="ML1996DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]



PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "shnumber", "hv101", "hv102", "hv104", "hv105", "hv116")))
IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v002", "v005", "v013", "v502", "scty", "v025", "sqnumber", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))


PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, hv002, shnumber,  hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, shnumber, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, shnumber,  hvidx, hv001, hv002,  hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, hv002, shnumber, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -shnumber, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002, sqnumber=shnumber)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, sqnumber) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "sqnumber" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)

if (strata=="v023")  {
  design <- svydesign(ids=~v001, strata=~v023, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}


MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/ML1996DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/ML1996DHS_MILAGE072621.csv", row.names=F)

####################################################
# NI1998DHS
# Clusters are repeated across regions, add structrue into id
country_survey <- surveys %>% filter(API_ID=="NI1998DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]



PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "shnumber", "hv101", "hv102", "hv104", "hv105", "hv116")))
IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v002", "v005", "v013", "v502", "scty", "v024", "v025", "snumber", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))


PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, hv002, shnumber,  hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, shnumber, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, shnumber,  hvidx, hv001, hv002,  hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, hv002, shnumber, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -shnumber, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002, snumber=shnumber)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, snumber) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "snumber" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)

if (strata=="v024 v025")  {
  design <- svydesign(ids=~v001, strata=~v024 + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}

MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/NI1998DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/NI1998DHS_MILAGE072621.csv", row.names=F)

####################################################
# NI1992DHS
# hv001 and hv002 not unique, need to add in the structure number
country_survey <- surveys %>% filter(API_ID=="NI1992DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]


PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "hv101", "hv102", "hv104", "hv105", "hv116", "shstruct")))
IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v002", "v005", "v013", "v502", "scty", "v025", "v024", "sstruct", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))

PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, hv002, shstruct,  hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, shstruct, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, shstruct,  hvidx, hv001, hv002,  hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, hv002, shstruct, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -shstruct, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002, sstruct=shstruct)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, sstruct) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "sstruct" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)

if (strata=="v024 v025")  {
  design <- svydesign(ids=~v001, strata=~v024 + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}


MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/NI1992DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/NI1992DHS_MILAGE072621.csv", row.names=F)

####################################################
# PE2012DHS
# hv001 and hv002 not unique, there is a dwelling numbers as part of the hhid, but cannot find the variable on its own
country_survey <- surveys %>% filter(API_ID=="PE2012DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]

PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "hv101", "hv102", "hv104", "hv105", "hv116", "shstruct")))
IR <- read_dta(ir_data, col_select = any_of(c("caseid", "v003", "v001", "v002", "v005", "v013", "v502", "scty", "v025", "v024", "sstruct", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))

PR$Dwell <- str_sub(PR$hhid, 8, 9)
IR$Dwell <- str_sub(IR$caseid, 11, 12)

PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, hv002, Dwell,  hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, Dwell, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, Dwell,  hvidx, hv001, hv002,  hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, hv002, Dwell, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -Dwell, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, Dwell) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "Dwell" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)

if (strata=="v022")  {
  design <- svydesign(ids=~v001, strata=~v022, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}


MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/PE2012DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/PE2012DHS_MILAGE072621.csv", row.names=F)

####################################################
# SN2005DHS
# Clusters are repeated across regions, add structure into id
country_survey <- surveys %>% filter(API_ID=="SN2005DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]



PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "shconces", "hv101", "hv102", "hv104", "hv105", "hv116")))
IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v002", "v005", "v013", "v502", "v024", "v025", "sconces", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))


PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, hv002, shconces,  hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, shconces, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, shconces,  hvidx, hv001, hv002,  hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, hv002, shconces, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -shconces, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002, sconces=shconces)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, sconces) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "sconces" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)

if (strata=="v024 v025")  {
  design <- svydesign(ids=~v001, strata=~v024 + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}


MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/SN2005DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/SN2005DHS_MILAGE072621.csv", row.names=F)

####################################################
# TG1998DHS
# Clusters are repeated across regions, add structure into id
country_survey <- surveys %>% filter(API_ID=="TG1998DHS")

pr_data <- country_survey[1, "PRfile"]
ir_data <- country_survey[1, "IRfile"]
countryname <- country_survey[1, "API_ID"]
strata <-  country_survey[1, "Strata"]



PR <- read_dta(pr_data, col_select = any_of(c( "hhid", "hvidx", "hv001", "hv002", "shconces", "hv101", "hv102", "hv104", "hv105", "hv116")))
IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v002", "v005", "v013", "v502", "v024", "v025", "sconces", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))


PR_mini <- PR %>% filter(hv102==1) %>%  select(hv001, hv002, shconces,  hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, shconces, sep=".")) %>%
  mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
  select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)

Survey_name <- countryname

hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)

PR_mini_indiv <- PR %>% select(hv001, hv002, shconces,  hvidx, hv001, hv002,  hv101,  hv104, hv105) %>%  mutate(hhid_new=paste(hv001, hv002, shconces, sep=".")) %>%
  full_join(PR_mini, by="hhid_new") %>%
  gather(Familymember, FamilyValue, -hhid_new, -shconces, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
  filter(!is.na(FamilyValue)) %>%
  separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
  select(-famhhid, -Familymember) %>%
  left_join(hv101, by="hv101") %>% 
  left_join(famhv101, by="famhv101") %>%
  left_join(rel_mat_small, by=c("Index", "OtherRelative")) 


mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
  rename(v003=hvidx, v001=hv001, v002=hv002, sconces=shconces)  %>%
  mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
  ungroup() %>% group_by(v001, v002, v003, sconces) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
  mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
  ungroup()


IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ,  "sconces" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)

if (strata=="v024 v025")  {
  design <- svydesign(ids=~v001, strata=~v024 + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
  options(survey.lonely.psu="adjust")
}


MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 

MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
MIL_prop_ci <- as.data.frame(confint(MIL_prop))

results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))

write.csv(results.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/TG1998DHS_MIL072621.csv", row.names=F)
write.csv(results.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/TG1998DHS_MILAGE072621.csv", row.names=F)
