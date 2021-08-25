# Kristin Bietsch, PhD
# Avenir Health


# Household matrix Loop with standard errors

#have to run  IA2015DHS Independently because of how long it takes to run

# 7/26/21 Editting code to not use HHID because of discrepencies in some surveys

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
levels(as.factor(recode_long$Clean_Label))

# Relationship Matrix
rel_mat <- read.csv("C:/Users/KristinBietsch/files/Track20/MotherInLaw/Relationship Matrix.csv") %>% mutate(hv101=as.character(hv101), famhv101=as.character(famhv101) )
rel_mat_small <- rel_mat %>% select(Index, OtherRelative, Relationship)

####
surveys  <- read.xlsx2("C:/Users/KristinBietsch/files/Desktop/Master DHS Survey List.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                       colClasses = c("character", "character", "character", "numeric",
                                      "character", "character", "character", "character", "numeric",
                                      "character", "numeric", "character", "character", "character", "character"));

surveys <- surveys %>% filter(PR!="")
surveys <- surveys %>% filter(API_ID!="JO1990DHS") # no labels for relationship to household head

# exlcude because no breakdown between parents and parent in law
surveys <- surveys %>% filter(API_ID!="BO2003DHS") %>% filter(API_ID!="DR1996DHS") %>% filter(API_ID!="DR2002DHS") %>% filter(API_ID!="DR2007DHS") %>% filter(API_ID!="DR2013DHS") %>% filter(API_ID!="NC2001DHS") %>% filter(API_ID!="PE1992DHS")


# hv001 and hv002 do not fully capture hhid, special code for each survey in "MIL Code for Surveys that do not run through loop"
surveys <- surveys %>% filter(API_ID!="BO1994DHS") %>% filter(API_ID!="BR1991DHS")  %>% filter(API_ID!="CM1998DHS") %>% filter(API_ID!="CM1991DHS") %>% filter(API_ID!="TD1997DHS")  %>% 
  filter(API_ID!="CO1990DHS")  %>% filter(API_ID!="DR1991DHS")   %>% filter(API_ID!="HT1994DHS")  %>% filter(API_ID!="HN2005DHS")  %>% filter(API_ID!="IA1999DHS")  %>% 
  filter(API_ID!="ML2001DHS") %>% filter(API_ID!="ML1996DHS") %>% filter(API_ID!="NI1998DHS")  %>% filter(API_ID!="NI1992DHS") %>% filter(API_ID!="PE2012DHS") %>% filter(API_ID!="SN2005DHS") %>% 
  filter(API_ID!="TG1998DHS") 


# 2015 India takes several hours to run, I run it on its own overnight
surveys <- surveys %>% filter(API_ID!="IA2015DHS")




surveys$PRfile <- paste( surveys$PR, ".DTA" , sep="")
surveys$IRfile <- paste( surveys$Survey, ".DTA" , sep="")


results.t.mil <- setNames(data.frame(matrix(ncol = 5,  nrow = 0)),  c("mean", "se", "l_ci", "u_ci" , "Survey")) %>% mutate(mean=as.numeric(mean), se=as.numeric(se), l_ci=as.numeric(l_ci), u_ci=as.numeric(u_ci), Survey=as.character(Survey))
results.t.mil.age <- setNames(data.frame(matrix(ncol = 5,  nrow = 0)),  c("v013", "Live_MIL", "ci_l", "ci_u", "Survey")) %>% mutate(v013=as.numeric(v013), Live_MIL=as.numeric(Live_MIL), ci_l=as.numeric(ci_l), ci_u=as.numeric(ci_u), Survey=as.character(Survey))

setwd("C:/Users/KristinBietsch/Files/DHSLoop")


for (row in 1:nrow(surveys)) {
  
  
  pr_data <- surveys[row, "PRfile"]
  ir_data <- surveys[row, "IRfile"]
  countryname <- surveys[row, "API_ID"]
  strata <-  surveys[row, "Strata"]
  
  PR <- read_dta(pr_data, col_select = any_of(c( "hvidx", "hv001", "hv002", "hv101", "hv102", "hv104", "hv105", "hv116")))
  IR <- read_dta(ir_data, col_select = any_of(c( "v003", "v001", "v002", "v005", "v013", "v502", "scty", "v025", "v024", "sdepart",  "secozone", "sstrate", "sstratum",  "v022", "v023", "v101", "v102"  , "v021" )))
  
  
  PR_mini <- PR %>% filter(hv102==1) %>% select(hv001, hv002, hvidx, hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, sep=".")) %>%
    mutate(Var=paste(hhid_new,  hvidx, hv101, hv104, hv105, sep="_"))   %>%
    select(hhid_new, hvidx, Var) %>% mutate(hvidx=paste("fam", hvidx, sep="")) %>% spread(hvidx, Var)
  
  Survey_name <- countryname
  
  hv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="")  %>% dplyr::rename(hv101 = val_hv101, Index=Clean_Label)
  famhv101 <- recode_long  %>% filter(Survey== Survey_name) %>% select(-Survey) %>% filter(Clean_Label!="") %>% mutate(val_hv101=as.character(val_hv101)) %>% dplyr::rename(famhv101 = val_hv101, OtherRelative=Clean_Label)
  
  PR_mini_indiv <- PR %>% select(hv001, hv002, hvidx, hv001, hv002,  hv101,  hv104, hv105) %>% mutate(hhid_new=paste(hv001, hv002, sep=".")) %>%
    full_join(PR_mini, by="hhid_new") %>%
    gather(Familymember, FamilyValue, -hhid_new, -hvidx,  -hv001, -hv002, -hv101, -hv104, -hv105 ) %>% 
    filter(!is.na(FamilyValue)) %>%
    separate(FamilyValue, c("famhhid",  "famhvidx",  "famhv101",    "famhv104",  "famhv105"), sep="_")  %>%
    select(-famhhid, -Familymember) %>%
    left_join(hv101, by="hv101") %>% 
    left_join(famhv101, by="famhv101") %>%
    left_join(rel_mat_small, by=c("Index", "OtherRelative")) 
  
  
  mw1549 <- PR_mini_indiv %>% filter(hv104==2) %>% filter(hv105>=15 & hv105<=49) %>% 
    rename(v003=hvidx, v001=hv001, v002=hv002)  %>%
    mutate(Live_MIL= case_when(Relationship=="SIL or DIL" & famhv104==2 ~ 1, TRUE ~ 0)) %>%
    ungroup() %>% group_by(v001, v002, v003) %>% summarise(Live_MIL=sum(Live_MIL)) %>%
    mutate(Live_MIL=case_when(Live_MIL>=1 ~ 1 , Live_MIL==0 ~ 0)) %>%
    ungroup()
  
  
  IR_MIL <- IR %>%  left_join(mw1549, by=c("v001", "v002",  "v003" ))  %>% filter(v502==1) %>% filter(v013>=1 & v013<=7) %>% mutate(sampleweights=v005/1000000)
  
  #########################################################################################################
  
  if (strata=="v022")  {
    design <- svydesign(ids=~v001, strata=~v022, weights=~sampleweights, data=IR_MIL, nest=TRUE)
    options(survey.lonely.psu="adjust")
  }
  
  
  if (strata=="v023")  {
    design <- svydesign(ids=~v001, strata=~v023, weights=~sampleweights, data=IR_MIL, nest=TRUE)
    options(survey.lonely.psu="adjust")
  }
  
  if (strata=="sstrate")  {
    design <- svydesign(ids=~v001, strata=~sstrate, weights=~sampleweights, data=IR_MIL, nest=TRUE)
    options(survey.lonely.psu="adjust")
  }
  
  if (strata=="sstratum")  {
    design <- svydesign(ids=~v001, strata=~sstratum, weights=~sampleweights, data=IR_MIL, nest=TRUE)
    options(survey.lonely.psu="adjust")
  }
  
  if (strata=="v024 v025")  {
    design <- svydesign(ids=~v001, strata=~v024 + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
    options(survey.lonely.psu="adjust")
  }
  
  if (strata=="scty v025 v024" )  {
    design <- svydesign(ids=~v001, strata= ~scty + ~v024 + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
    options(survey.lonely.psu="adjust")
  }
  
  if (strata=="sdepart v025" )  {
    design <- svydesign(ids=~v001, strata=~sdepart + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
    options(survey.lonely.psu="adjust")
  }
  
  if (strata=="sdepto v102" )  {
    design <- svydesign(ids=~v001, strata=~sdepto + ~v102, weights=~sampleweights, data=IR_MIL, nest=TRUE)
    options(survey.lonely.psu="adjust")
  }
  
  if (strata=="secozone v024" )  {
    design <- svydesign(ids=~v001, strata=~secozone + ~v024, weights=~sampleweights, data=IR_MIL, nest=TRUE)
    options(survey.lonely.psu="adjust")
  }
  
  if (strata=="v023 v025"  )  {
    design <- svydesign(ids=~v001, strata=~v023 + ~v025, weights=~sampleweights, data=IR_MIL, nest=TRUE)
    options(survey.lonely.psu="adjust")
  }
  
  if (strata=="v101 v102")  {
    design <- svydesign(ids=~v001, strata=~v101 + ~v102, weights=~sampleweights, data=IR_MIL, nest=TRUE)
    options(survey.lonely.psu="adjust")
  }
  
  
  #########################################################################################################
  
  MIL_mean <- as.data.frame(round(svymean( ~Live_MIL ,   design,  na.rm=TRUE),3)) 
  
  MIL_prop <- svyciprop(~Live_MIL, design, na.rm=TRUE, method = c( "mean"), level = 0.95)
  MIL_prop_ci <- as.data.frame(confint(MIL_prop))
  
  results.mil <- bind_cols(MIL_mean, MIL_prop_ci) %>% mutate(Survey= countryname ) %>% rename(se=Live_MIL, l_ci="2.5%", u_ci="97.5%" )
  
  
  results.mil.age <- as.data.frame(svyby( ~Live_MIL , ~ v013 ,  design, svyciprop, vartype="ci")) %>% mutate(Survey= countryname ) %>% mutate(v013=as.numeric(v013))
  
  
  results.t.mil <- bind_rows(results.t.mil , results.mil)
  results.t.mil.age <- bind_rows(results.t.mil.age , results.mil.age)
  
}


#write.csv(results.t.mil, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/Data_MIL072621.csv", row.names=F)
#write.csv(results.t.mil.age, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/Data_MILAGE072621.csv", row.names=F)


# Bring in other surveys
India2015.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/India2015_MIL_072621.csv")
India2015.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/India2015_MILAGE_072621.csv")
BO1994DHS.all<-  read.csv("C:/Users/KristinBietsch/files/Track20/MotherInLaw/BO1994DHS_MIL072621.csv")
BO1994DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/BO1994DHS_MILAGE072621.csv")
BR1991DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/BR1991DHS_MIL072621.csv")
BR1991DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/BR1991DHS_MILAGE072621.csv")
CM1998DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/CM1998DHS_MIL072621.csv")
CM1998DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/CM1998DHS_MILAGE072621.csv")
CM1991DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/CM1991DHS_MIL072621.csv")
CM1991DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/CM1991DHS_MILAGE072621.csv")
TD1997DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/TD1997DHS_MIL072621.csv")
TD1997DHS.age<-   read.csv("C:/Users/KristinBietsch/files/Track20/MotherInLaw/TD1997DHS_MILAGE072621.csv")
CO1990DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/CO1990DHS_MIL072621.csv")
CO1990DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/CO1990DHS_MILAGE072621.csv")
DR1991DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/DR1991DHS_MIL072621.csv")
DR1991DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/DR1991DHS_MILAGE072621.csv")
HT1994DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/HT1994DHS_MIL072621.csv")
HT1994DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/HT1994DHS_MILAGE072621.csv")
HN2005DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/HN2005DHS_MIL072621.csv")
HN2005DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/HN2005DHS_MILAGE072621.csv")
IA1999DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/IA1999DHS_MIL072621.csv")
IA1999DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/IA1999DHS_MILAGE072621.csv")
ML2001DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/ML2001DHS_MIL072621.csv")
ML2001DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/ML2001DHS_MILAGE072621.csv")
ML1996DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/ML1996DHS_MIL072621.csv")
ML1996DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/ML1996DHS_MILAGE072621.csv")
NI1998DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/NI1998DHS_MIL072621.csv")
NI1998DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/NI1998DHS_MILAGE072621.csv")
NI1992DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/NI1992DHS_MIL072621.csv")
NI1992DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/NI1992DHS_MILAGE072621.csv")
PE2012DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/PE2012DHS_MIL072621.csv")
PE2012DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/PE2012DHS_MILAGE072621.csv")
SN2005DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/SN2005DHS_MIL072621.csv")
SN2005DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/SN2005DHS_MILAGE072621.csv")
TG1998DHS.all<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/TG1998DHS_MIL072621.csv")
TG1998DHS.age<-  read.csv( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/TG1998DHS_MILAGE072621.csv")












results.t.mil.TOTAL <- bind_rows(results.t.mil , BO1994DHS.all,	BR1991DHS.all,	CM1991DHS.all,	CM1998DHS.all,	CO1990DHS.all,	DR1991DHS.all,
                                 HN2005DHS.all,	HT1994DHS.all,	IA1999DHS.all,	India2015.all,	ML1996DHS.all,	ML2001DHS.all,	NI1992DHS.all,
                                 NI1998DHS.all,	PE2012DHS.all,	SN2005DHS.all,	TD1997DHS.all,	TG1998DHS.all) %>% 
  rename(API_ID=Survey)
results.t.mil.age.TOTAL  <- bind_rows(results.t.mil.age , BO1994DHS.age,	BR1991DHS.age,	CM1991DHS.age,	CM1998DHS.age,	CO1990DHS.age,	
                                      DR1991DHS.age,	HN2005DHS.age,	HT1994DHS.age,	IA1999DHS.age,	India2015.age,	ML1996DHS.age,	ML2001DHS.age,	
                                      NI1992DHS.age,	NI1998DHS.age,	PE2012DHS.age,	SN2005DHS.age,	TD1997DHS.age,	TG1998DHS.age) %>% 
  rename(API_ID=Survey)

surveys  <- read.xlsx2("C:/Users/KristinBietsch/files/Desktop/Master DHS Survey List.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                       colClasses = c("character", "character", "character", "numeric",
                                      "character", "character", "character", "character", "numeric",
                                      "character", "numeric", "character", "character", "character", "character"));

surveys <- surveys %>% filter(PR!="")
surveys <- surveys %>% filter(API_ID!="JO1990DHS") # no labels for relationship to household head

# exlcude because no breakdown between parents and parent in law
surveys <- surveys %>% filter(API_ID!="BO2003DHS") %>% filter(API_ID!="DR1996DHS") %>% filter(API_ID!="DR2002DHS") %>% filter(API_ID!="DR2007DHS") %>% filter(API_ID!="DR2013DHS") %>% filter(API_ID!="NC2001DHS") %>% filter(API_ID!="PE1992DHS")


results.t.mil.TOTAL <- full_join(surveys, results.t.mil.TOTAL)
results.t.mil.age.TOTAL <- full_join(surveys, results.t.mil.age.TOTAL)


write.csv(results.t.mil.TOTAL, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/Data_MIL081921.csv", row.names=F)
write.csv(results.t.mil.age.TOTAL, "C:/Users/KristinBietsch/files/Track20/MotherInLaw/Data_MILAGE081921.csv", row.names=F)