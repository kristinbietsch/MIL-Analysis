# Mother In Law Further Analysis


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
library(forcats)

library(jsonlite) 
library(data.table)


library(kimisc)
library(ggplot2)
library(magick)
require(foreign)
require(maptools)
library(sp)
library(RColorBrewer)
library(latticeExtra)
#library(raster)
library(rgdal)

#library(rasterVis)
library(gridExtra)

library(rgeos)
library(rworldmap)



options(scipen=999)
memory.limit(size = 2e6) 


#####################################################################
# Read in data
#####################################################################

surveys <- read.xlsx2("C:/Users/KristinBietsch/files/Desktop/Master DHS Survey List.xlsx",sheetName="Sheet1",startRow=1,header=TRUE,
                      colClasses = c("character", "character", "character", "numeric",
                                     "character", "character", "character", "character", "numeric",
                                     "character", "numeric", "character", "character", "character", "character"));

countryregions <- read.csv("C:/Users/KristinBietsch/files/Momentum/Country Regions.csv")

setwd("C:/Users/KristinBietsch/files/Track20/MotherInLaw")

data <- read.csv("Data_MIL081921.csv")
mar_age <- read.csv("Age Distribution of Married Women.csv")  %>% 
  mutate(age=case_when(Var1==1 ~ "15-19", Var1==2 ~ "20-24", Var1==3 ~ "25-29", Var1==4 ~ "30-34", Var1==5 ~ "35-39", Var1==6 ~ "40-44", Var1==7 ~ "45-49")) %>% 
  select(-Country, -Var1) %>%
  rename(API_ID=Survey, MarDist=Freq)

data_age <- read.csv("Data_MILAGE081921.csv") %>% mutate(age=case_when(v013==1 ~ "15-19", v013==2 ~ "20-24", v013==3 ~ "25-29", v013==4 ~ "30-34", v013==5 ~ "35-39", v013==6 ~ "40-44", v013==7 ~ "45-49")) %>%
  left_join(mar_age, by=c("API_ID", "age"))







#####################################################

#Regional/Country differeces in levels
#Change over time
#do we see changes over time
#for how many countries
#change over time by age
#decompostition- is this due to changes of living with mother in law or age at marriage

levels(as.factor(data$Country))
# Graphics
regions <- c( "Afghanistan" ,              "Albania"   ,                "Angola"   ,                 "Armenia" ,                  "Azerbaijan"  ,              "Bangladesh"    ,            "Benin"  ,                  
              "Bolivia"     ,              "Brazil"   ,                 "Burkina Faso",              "Burundi",                   "Cambodia"    ,              "Cameroon"   ,               "Central African Republic" ,
              "Chad"        ,              "Colombia" ,                 "Comoros"     ,              "Congo"  ,                   "Congo Democratic Republic", "Cote d'Ivoire",             "Dominican Republic"  ,     
              "Egypt"       ,              "Ethiopia" ,                 "Gabon"       ,              "Gambia" ,                   "Ghana"   ,                  "Guatemala"  ,               "Guinea"      ,             
              "Guyana"      ,              "Haiti"    ,                 "Honduras"    ,              "India"  ,                   "Indonesia",                 "Jordan"     ,               "Kazakhstan"  ,             
              "Kenya"       ,              "Kyrgyz Republic",           "Lesotho"     ,              "Liberia"  ,                 "Madagascar",                "Malawi"   ,                 "Maldives" ,                
              "Mali"        ,              "Moldova"    ,               "Morocco"     ,              "Mozambique" ,               "Myanmar" ,                  "Namibia"  ,                 "Nepal"  ,                  
              "Nicaragua"   ,              "Niger"      ,               "Nigeria"     ,              "Pakistan"   ,               "Papua New Guinea"  ,        "Paraguay" ,                 "Peru"  ,                   
              "Philippines" ,              "Rwanda"     ,               "Sao Tome and Principe",     "Senegal"    ,               "Sierra Leone"  ,            "South Africa",              "Swaziland" ,               
              "Tajikistan"  ,              "Tanzania"   ,               "Timor-Leste"   ,            "Togo"       ,               "Turkey"    ,                "Uganda"   ,                 "Ukraine" ,                 
              "Uzbekistan"  ,              "Vietnam"    ,               "Yemen"   ,                  "Zambia"     ,               "Zimbabwe"   )
regions.df <- data.frame(regions)


for (row in 1:nrow(regions.df)) {
  
  region_name <- as.character(regions.df[row, 1])
  
  country_full <- data %>% filter(Country==region_name)
  
  country_age <- data_age %>% filter(Country==region_name)
  

  #################################################################
  
  trend  <- ggplot(country_full, aes(x=StartYear, y=mean*100)) +
    geom_point(color="blue") +
    geom_line(color="blue") +
    geom_ribbon(aes(ymin = l_ci*100, ymax = u_ci*100), fill = "blue", alpha=.1) +
    labs(title=region_name,
         x ="", y = "") +
    theme_bw() 
  
  
age_trend <-   ggplot(country_age, aes(x=StartYear, y=Live_MIL*100, color=age)) +
    geom_point() +
    geom_line() +
    geom_ribbon(aes(ymin = ci_l*100, ymax = ci_u*100), alpha=.1) +
    facet_wrap( ~ age) +
    labs(title="% of Women Living with Mother in Law, by Age",
         x ="", y = "") +
    theme_bw() +
    theme(legend.position = "none")
  

marage_trend <-    ggplot(country_age, aes(x=StartYear, y=MarDist, color=age)) +
  geom_point(size=2) +
  geom_line() +
  labs(title="Age Distribution of Married Women",
       x ="", y = "") +
  theme_bw() 

output_1 <- ggdraw() +
  draw_plot(trend, x = 0, y = .5, width = .5, height = .5) +
  draw_plot(marage_trend, x = .5, y = .5, width = .5, height = .5) +
  draw_plot(age_trend, x = 0, y = 0,  width = 1, height = .5) 

ggexport(output_1, filename=paste("Results/MIL_" , region_name,  "_Graphics072721.pdf", sep=""), height=12, width=8)

}

pdf_combine(c("Results/MIL_Afghanistan_Graphics072721.pdf",
              "Results/MIL_Albania_Graphics072721.pdf",
              "Results/MIL_Angola_Graphics072721.pdf",
              "Results/MIL_Armenia_Graphics072721.pdf",
              "Results/MIL_Azerbaijan_Graphics072721.pdf",
              "Results/MIL_Bangladesh_Graphics072721.pdf",
              "Results/MIL_Benin_Graphics072721.pdf",
              "Results/MIL_Bolivia_Graphics072721.pdf",
              "Results/MIL_Brazil_Graphics072721.pdf",
              "Results/MIL_Burkina Faso_Graphics072721.pdf",
              "Results/MIL_Burundi_Graphics072721.pdf",
              "Results/MIL_Cambodia_Graphics072721.pdf",
              "Results/MIL_Cameroon_Graphics072721.pdf",
              "Results/MIL_Central African Republic_Graphics072721.pdf",
              "Results/MIL_Chad_Graphics072721.pdf",
              "Results/MIL_Colombia_Graphics072721.pdf",
              "Results/MIL_Comoros_Graphics072721.pdf",
              "Results/MIL_Congo_Graphics072721.pdf",
              "Results/MIL_Congo Democratic Republic_Graphics072721.pdf",
              "Results/MIL_Cote d'Ivoire_Graphics072721.pdf",
              "Results/MIL_Dominican Republic_Graphics072721.pdf",
              "Results/MIL_Egypt_Graphics072721.pdf",
              "Results/MIL_Ethiopia_Graphics072721.pdf",
              "Results/MIL_Gabon_Graphics072721.pdf",
              "Results/MIL_Gambia_Graphics072721.pdf",
              "Results/MIL_Ghana_Graphics072721.pdf",
              "Results/MIL_Guatemala_Graphics072721.pdf",
              "Results/MIL_Guinea_Graphics072721.pdf",
              "Results/MIL_Guyana_Graphics072721.pdf",
              "Results/MIL_Haiti_Graphics072721.pdf",
              "Results/MIL_Honduras_Graphics072721.pdf",
              "Results/MIL_India_Graphics072721.pdf",
              "Results/MIL_Indonesia_Graphics072721.pdf",
              "Results/MIL_Jordan_Graphics072721.pdf",
              "Results/MIL_Kazakhstan_Graphics072721.pdf",
              "Results/MIL_Kenya_Graphics072721.pdf",
              "Results/MIL_Kyrgyz Republic_Graphics072721.pdf",
              "Results/MIL_Lesotho_Graphics072721.pdf",
              "Results/MIL_Liberia_Graphics072721.pdf",
              "Results/MIL_Madagascar_Graphics072721.pdf",
              "Results/MIL_Malawi_Graphics072721.pdf",
              "Results/MIL_Maldives_Graphics072721.pdf",
              "Results/MIL_Mali_Graphics072721.pdf",
              "Results/MIL_Moldova_Graphics072721.pdf",
              "Results/MIL_Morocco_Graphics072721.pdf",
              "Results/MIL_Mozambique_Graphics072721.pdf",
              "Results/MIL_Myanmar_Graphics072721.pdf",
              "Results/MIL_Namibia_Graphics072721.pdf",
              "Results/MIL_Nepal_Graphics072721.pdf",
              "Results/MIL_Nicaragua_Graphics072721.pdf",
              "Results/MIL_Niger_Graphics072721.pdf",
              "Results/MIL_Nigeria_Graphics072721.pdf",
              "Results/MIL_Pakistan_Graphics072721.pdf",
              "Results/MIL_Papua New Guinea_Graphics072721.pdf",
              "Results/MIL_Paraguay_Graphics072721.pdf",
              "Results/MIL_Peru_Graphics072721.pdf",
              "Results/MIL_Philippines_Graphics072721.pdf",
              "Results/MIL_Rwanda_Graphics072721.pdf",
              "Results/MIL_Sao Tome and Principe_Graphics072721.pdf",
              "Results/MIL_Senegal_Graphics072721.pdf",
              "Results/MIL_Sierra Leone_Graphics072721.pdf",
              "Results/MIL_South Africa_Graphics072721.pdf",
              "Results/MIL_Swaziland_Graphics072721.pdf",
              "Results/MIL_Tajikistan_Graphics072721.pdf",
              "Results/MIL_Tanzania_Graphics072721.pdf",
              "Results/MIL_Timor-Leste_Graphics072721.pdf",
              "Results/MIL_Togo_Graphics072721.pdf",
              "Results/MIL_Turkey_Graphics072721.pdf",
              "Results/MIL_Uganda_Graphics072721.pdf",
              "Results/MIL_Ukraine_Graphics072721.pdf",
              "Results/MIL_Uzbekistan_Graphics072721.pdf",
              "Results/MIL_Vietnam_Graphics072721.pdf",
              "Results/MIL_Yemen_Graphics072721.pdf",
              "Results/MIL_Zambia_Graphics072721.pdf",
              "Results/MIL_Zimbabwe_Graphics072721.pdf"), output = "Results/AllCountryResults072721.pdf")

#########################################################
# if survey past 2010
# MIL distribution

recent_data <- data %>% group_by(Country) %>% 
  mutate(max=max(StartYear)) %>% 
  ungroup() %>% 
  filter(StartYear==max) %>% 
  filter(StartYear>=2010) %>% 
  mutate(MIL_Group=case_when(mean<0.05 ~ "Under 5%",
                             mean>=0.05 & mean<.10 ~ "5%-9%",
                             mean>=0.1 & mean<.20 ~ "10%-19%",
                             mean>=0.2 & mean<.30 ~ "20%-29%",
                             mean>=0.3 & mean<.40 ~ "30%-39%",
                             mean>=0.4  ~ "Greater\nthan 40%")) %>%
  mutate(MIL_Group = fct_relevel(MIL_Group, 
                                 "Under 5%",  "5%-9%", "10%-19%", "20%-29%",  "30%-39%",  "Greater\nthan 40%")) %>%
  left_join(countryregions, by="ISONum")

recent_group <- recent_data %>% group_by(MIL_Group) %>% count()
recent_group_region <- recent_data %>% group_by(MIL_Group, SDG) %>% count() %>% spread(SDG, n) %>% gather(Variable, Value,   "Central and Southern Asia":"Sub-Saharan Africa" ) %>% replace(is.na(.), 0)

ggplot(recent_group, aes(x=MIL_Group, y=n, fill=MIL_Group)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = n), vjust = -.5, colour = "black") +
  labs(title="Distribution of Countries by Proportion of Women who Live with their Mothers-in-Law",
       x ="", y = "",
       caption= "Most recent post 2010 DHS for each country") +
  theme_bw() +
  theme(legend.position = "none")
ggsave("Results/MILDistribution.png", width = 20, height = 15, units = "cm")


ggplot(recent_group_region, aes(x=MIL_Group, y=Value, fill=MIL_Group)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = Value), vjust = -.5, colour = "black") +
  facet_wrap( ~ Variable, ncol=2) +
  labs(title="Distribution of Countries by Proportion of Women who Live with their Mothers-in-Law",
       x ="", y = "",
       caption= "Most recent post 2010 DHS for each country") +
  theme_bw() +
  theme(legend.position = "none")
ggsave("Results/MILRegionDistribution.png", width = 20, height = 30, units = "cm")

names(recent_group_region)


recent_data <- data %>% group_by(Country) %>% 
  mutate(max=max(StartYear)) %>% 
  ungroup() %>% 
  filter(StartYear==max) %>% 
  mutate(MIL_Group=case_when(mean<0.05 ~ "Under 5%",
                             mean>=0.05 & mean<.10 ~ "5%-9%",
                             mean>=0.1 & mean<.15 ~ "10%-14%",
                             mean>=0.15 & mean<.20 ~ "15%-19%",
                             mean>=0.2 & mean<.25 ~ "20%-24%",
                             mean>=0.25 & mean<.30 ~ "25%-29%",
                             mean>=0.3 & mean<.35 ~ "30%-34%",
                             mean>=0.35 & mean<.40 ~ "35%-39%",
                             mean>=0.4  ~ "Greater\nthan 40%")) %>%
  mutate(MIL_Group = fct_relevel(MIL_Group, 
                                 "Under 5%",  "5%-9%", "10%-14%", "15%-19%",  "20%-24%", "25%-29%",  "30%-34%", "35%-39%", "Greater\nthan 40%")) %>%
  left_join(countryregions, by="ISONum")
levels(as.factor(recent_data$SDG))

recent_group_region <- recent_data %>% mutate(SDG_edit=case_when(SDG=="Europe and Northern America"  | SDG=="Northern Africa and Western Asia"  ~ "Northern Africa, Western Asia, and Europe",
                                                                 SDG==  "Oceania (excluding Australia and New Zealand)" ~ "Oceania",
                                                                  !is.na(SDG) ~ SDG)) %>%
  group_by(MIL_Group, SDG_edit) %>% count() %>% spread(SDG_edit, n) %>% gather(Variable, Value,   "Central and Southern Asia":"Sub-Saharan Africa" ) %>% replace(is.na(.), 0)

ggplot(recent_group_region, aes(x=MIL_Group, y=Value, fill=MIL_Group)) +
  geom_bar(stat="identity") +
  facet_wrap( ~ Variable, ncol=2, scales="free_y") +
  labs(title="Distribution of Countries by Proportion of Women who Live with their Mothers-in-Law",
       x ="", y = "",
       caption= "Most recent DHS for each country") +
  scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) +
  theme_bw() +
  theme(legend.position = "none") 

ggsave( "C:/Users/KristinBietsch/files/Track20/MotherInLaw/Results/Regional Distribution.png", width = 30, height = 30, units = "cm")
############################################################
# Trends
data_n <- data %>% group_by(Country) %>% tally() %>% filter(n>1)

############################################################

# maps
projectionbounds <- CRS("+proj=longlat +datum=WGS84")

bounds <- readShapePoly("C:/Users/KristinBietsch/files/Track20/Center Update/WorldShape/ne_50m_admin_0_countries.shp", verbose=TRUE, proj4string=projectionbounds)
bounds.df <- as.data.frame(bounds)

recent_data_clean <- recent_data %>% select(ISONum, mean, MIL_Group) %>% rename(UN_A3=ISONum)

full_iso <- read.csv("ISOListFull.csv") %>% full_join(recent_data_clean, by="UN_A3")


bounds_fortify <- fortify(bounds, region = "UN_A3")

world<- merge(bounds_fortify, full_iso, by="id", by.y="UN_A3")

levels(as.factor(world$MIL_Group))

ggplot() +
  geom_polygon(data =world , aes(long, lat, group = group, fill=MIL_Group), color=NA) + 
  geom_polygon(data =bounds_fortify , aes(long, lat, group = group), fill=NA, color="azure4", size=.5) +
  ggtitle("")+
  coord_equal(xlim = c(-100, 150), ylim = c(-50, 60)) +
  scale_fill_manual("",
                    values=c("Under 5%"   = "#3288BD",        
                             "5%-9%"   = "#66C2A5",           
                             "10%-14%"    = "#ABDDA4",        
                             "15%-19%"   = "#E6F598",         
                             "20%-24%"   = "#FFFFBF" ,         
                             "25%-29%"  = "#FEE08B",          
                             "30%-34%"   =   "#FDAE61",         
                             "35%-39%"    =  "#F46D43",        
                             "Greater\nthan 40%" =  "#D53E4F"),
                    na.translate=FALSE)+
  theme_bw()+ 
  theme(axis.line=element_blank(),
        axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        title =element_text(size=16, face='bold'),
        legend.position = "bottom")

ggsave("C:/Users/KristinBietsch/files/Track20/MotherInLaw/Results/MIL Map 082021.png", width = 20, height = 15, units = "cm")

