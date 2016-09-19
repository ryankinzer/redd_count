#------------------------------------------------------------------------------
# Pull in IDFG summarized redd and carcass data that was outputed from
# idfg_redd_data_prep.R and idfg_carcass_data_prep.R. Combine data and estimate
# abundance using N = R/(f*(1-a)) formula for SPI and MPE.
#
# Ryan N. Kinzer
# Created 3.23.16 and Modified 3.23.16
#------------------------------------------------------------------------------
# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)
#------------------------------------------------------------------------------
# Load Spawning Ground Survey Data
#------------------------------------------------------------------------------
load(file="./Data/redd_data.Rdata")
load(file="./Data/carcass_data.Rdata")
#------------------------------------------------------------------------------
# Load Escapement Data - from RK's "ISS Final Data Draft 1_8_15"
# the escapement data needs updating for 2013,2014 and 2015
#------------------------------------------------------------------------------
esc_dat <- read.csv("./Data/biological_data/MR_escapement_ISS.csv")
#------------------------------------------------------------------------------

SGS_dat <- full_join(redd_dat,carcass_dat) %>%
            #select(TRT_ESU,TRT_MPG,TRT_POP,Waterbody,SurveyYear,SPI,MPE,pHOS,pFemale,a) %>%
            mutate(N_SPI = SPI/(pFemale*(1-a)),
                   N_MPE = MPE/(pFemale*(1-a)))

mod_dat <- full_join(SGS_dat,esc_dat[,-1])

long_dat <- gather(mod_dat,key=SurveyType,value=Abundance,SPI,MPE,N_SPI,N_MPE,N_MR)


# ggplot(data=long_dat[!is.na(long_dat$Abundance),])+   
#   geom_line(aes(x=SurveyYear,y=Abundance,group=SurveyType,colour=SurveyType))+
#   ggtitle("Abundance")+
#   facet_wrap(~Waterbody,scales="free_y")+  
#   theme(legend.position = "bottom")

save(mod_dat,file="./Data/mod_data.Rdata")
#rm(list=ls())
