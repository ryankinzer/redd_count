#------------------------------------------------------------------------------
# Pull in IDFG redd data and format for processing
# Raw data is a complete query/download from IFWIS Redd Detail Viewer and
# IFWIS Redd Count Summary viewer, then exported to a .csv file and saved in 
# the projects data folder.  The queries did not limit the dataset by any 
# field and should contain all records.
# Detail Data Exported on 3.6.16 at 8:34 AM by RK
# Summary Data Exported on 3.22.16 at 8:19 AM by RK
# Index area attribute table from EZ email on 10/13/15
# Ryan N. Kinzer
# Created 3.11.16 and Modified 3.22.16
#------------------------------------------------------------------------------
# Load packages to organize and explore
library(dplyr)
library(ggplot2)
#------------------------------------------------------------------------------
# Load detailed redd data
detail_df <- read.csv("./Data/biological_data/CSV_8.34.3_6_16_AM_redd_detail.csv")

# Load summary redd data
summary_df <- read.csv("./Data/biological_data/CSV_8.19.3_22_16_redd_summary.csv")

# Load index transect table (Will probably need if we get a table for
# above and below weir)
#transect_df <- read.csv("./Data/spatial_data/index_attribute_table.csv")
#index_transects <- unique(transect_df$Transect)

#------------------------------------------------------------------------------
# Count detailed redd data for index area only redds
#------------------------------------------------------------------------------
# species <- unique(detail_df$Species)
# 
# detail_index <- detail_df %>%
#                 filter(Species == species[1] & SGR_TrendCountYN == "TRUE") %>%
#                 arrange(SurveyYear) %>%
#                 group_by(Waterbody,SurveyYear) %>%
#                 summarise(SPI = sum(CountNew,na.rm="TRUE"))
# 
# detail_total <- detail_df %>%
#                 filter(Species == species[1]) %>%
#                 arrange(SurveyYear) %>%
#                 group_by(Waterbody,SurveyYear) %>%
#                 summarise(MPE = sum(CountNew,na.rm="TRUE"))
# 
# detail_dat <- inner_join(detail_total,detail_index)

#------------------------------------------------------------------------------
# Count summary redd data for index area only redds
#------------------------------------------------------------------------------
species <- unique(summary_df$Species)

summary_index <- summary_df %>%
              filter(Species == species[1] & SGR_TrendCountYN == "TRUE") %>%
              arrange(SurveyYear) %>%
              group_by(Waterbody,SurveyYear) %>%
              summarise(SPI = sum(TotalNew,na.rm="TRUE"),
                        SPI_n = length(TotalNew))

summary_total <- summary_df %>%
              filter(Species == species[1]) %>%
              arrange(SurveyYear) %>%
              group_by(Waterbody,SurveyYear) %>%
              summarise(MPE = sum(TotalNew,na.rm="TRUE"),                        
                        MPE_n = length(TotalNew))

summary_dat <- inner_join(summary_total,summary_index)

#------------------------------------------------------------------------------
# Combine data from detail and summary tables; check for errors
# Should use only the summary_dat data frame, detail_dat is less extensive b/c
# not all counted redds were assigned to a waypoint.
#------------------------------------------------------------------------------
#redd_dat <- left_join(summary_dat,detail_dat)

#-----------------------------------------------------------------------------
# Gather metadata from summary_df (e.g. TRT info) and attach to redd_dat
#-----------------------------------------------------------------------------
metadata_df <- summary_df %>%
                select(Waterbody:TRT_ESU) %>%
                distinct(Waterbody)

redd_dat <- inner_join(summary_dat,metadata_df) %>%
            mutate(MPE = ifelse((MPE_n-SPI_n)==0,as.integer("NA"),MPE))
            
# ggplot(data=pop_dat)+
#     geom_line(aes(x=SurveyYear,y=SPI,group=TRT_POP),linetype=2)+
#     geom_line(aes(x=SurveyYear,y=MPE,group=TRT_POP),linetype=1)+
#     ggtitle("IDFG SGS Redd Counts")+
#     facet_wrap(~TRT_POP,scale="free")+  
#     theme(legend.position = "bottom")

save(redd_dat,file="./Data/redd_data.Rdata")
#rm(list=ls())

#load("./Data/redd_data.Rdata")
