#------------------------------------------------------------------------------
# Pull in IDFG carcass data and format for processing
# Raw data is a complete query/download from IFWIS Carcass Detail Viewer,
# then exported to a .csv file and saved in the projects data folder.  
# The queries did not limit the dataset by any field and should contain all 
# records.
# Carcass Detail Data Exported on 3.7.16 at 9:04 AM by RK
#
# Ryan N. Kinzer
# Created 3.11.16 and Modified 3.23.16
#------------------------------------------------------------------------------
# Load packages to organize and explore
library(dplyr)
library(tidyr)
library(ggplot2)
#------------------------------------------------------------------------------
# Load detailed carcass data
carcass_df <- read.csv("./Data/biological_data/CSV_9.04.3_7_16_carcass_detail_AM.csv")

# Load index transect table (Will probably need if we get a table for
# above and below weir)
#transect_df <- read.csv("./Data/index_attribute_table.csv")
#index_transects <- unique(transect_df$Transect)

species <- unique(carcass_df$Species)

#------------------------------------------------------------------------------
# Sums up observations for pHOS, female prop. and prespawn mortality by 
# waterbody first.  Sum by water body first because this field is complete
# for all records.  We later organize and sum by TRT populations.
#------------------------------------------------------------------------------
# Methods used to summarize the dataset:
#   1. Only used Sp/Sm Chinook records and those labeled as carcasses = TRUE
#   2. Summed across the NFish field
#   3. For prespawned calculations
#     a. Only used female carcasses and removed all entries = -99
#     b. Changed entries entered as 1 to 100 percent spawned.
#         (It appears some records were entered as binary 0 or 1)
#     c. Prespawn considered if PercentSpawned <= 25.
#------------------------------------------------------------------------------
pHOS <- carcass_df %>%
  filter(Species == species[1] & CarcassYN == "TRUE") %>%
  group_by(Waterbody,SurveyYear,ProductionType) %>%
  summarise(n = sum(NFish,na.rm="TRUE")) %>%
  spread(ProductionType,n)

female <- carcass_df %>%
  filter(Species == species[1] & CarcassYN == "TRUE") %>% 
  group_by(Waterbody,SurveyYear,Sex) %>%
  summarise(n = sum(NFish,na.rm="TRUE")) %>%
  spread(Sex,n)

prespawn <- carcass_df %>%
  filter(Species == species[1] & PercentSpawned != -99 & Sex=="F" & CarcassYN == "TRUE") %>% 
  mutate(PercentSpawned = ifelse(PercentSpawned == 1, 100, PercentSpawned)) %>%
  group_by(Waterbody,SurveyYear) %>%
  summarise(Prespawn = sum(PercentSpawned <= 25 ,na.rm="TRUE"), Spawned = sum(PercentSpawned > 25))

#------------------------------------------------------------------------------
# Join the three seperate queries, keeping all records from each
#------------------------------------------------------------------------------
carcass_dat <- full_join(pHOS,female)
carcass_dat <- full_join(carcass_dat,prespawn)

#------------------------------------------------------------------------------
# Join Waterbody data with Population metadata
#------------------------------------------------------------------------------
metadata_df <- carcass_df %>%
  select(Waterbody:TRT_ESU) %>%
  distinct(Waterbody)

carcass_dat <- left_join(carcass_dat,metadata_df)

#------------------------------------------------------------------------------
# Estimate pHOS, Female proportion and Prespawn Mortality
#------------------------------------------------------------------------------

carcass_dat[is.na(carcass_dat)] <- 0  # Change NA's to 0

carcass_dat <- carcass_dat %>%     # NA's now exists where both obs = 0
        mutate(pNAT = Natural / (Hatchery + Natural)) %>%
        mutate(pHOS = 1-pNAT) %>%
        mutate(pFemale= F/ (F + M)) %>%
        mutate(a = Prespawn/ (Prespawn + Spawned))

# ggplot(data=carcass_dat)+
#   geom_line(aes(x=SurveyYear,y=pHOS,group=Waterbody,colour=TRT_POP))+
#   ggtitle("pHOS")+
#   facet_wrap(~Waterbody)+  
#   theme(legend.position = "bottom")
# 
# ggplot(data=carcass_dat)+
#   geom_line(aes(x=SurveyYear,y=pFemale,group=Waterbody,colour=TRT_POP))+
#   ggtitle("pFemale")+
#   facet_wrap(~Waterbody)+  
#   theme(legend.position = "bottom")
# 
# ggplot(data=carcass_dat)+  #[!is.na(carcass_dat$a),]
#     geom_line(aes(x=SurveyYear,y=a,group=Waterbody,colour=TRT_POP))+
#     ggtitle("Prespawn Mortality")+
#     facet_wrap(~Waterbody)+  
#     theme(legend.position = "bottom")

save(carcass_dat,file="./Data/carcass_data.Rdata")
#rm(list=ls())
