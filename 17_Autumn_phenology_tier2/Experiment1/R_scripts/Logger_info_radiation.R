##############################################################################################################################################
############################################################# Rscript for: ###################################################################
##############################################################################################################################################
#### Insert publication name #################################################################################################################
##############################################################################################################################################
# This script extracts the climate information for photosynthesis settings ###################################################################
##############################################################################################################################################



#required packages
require(tidyverse)
require(data.table)
require(lubridate)



##############################################################################################################################################
##############################################################################################################################################



#####################
## Set directories ##
#####################



# set the working directory
setwd("/Users/consti/Desktop/PhD/Publication_material/17_Autumn_phenology_tier2/Experiment1")

# paths
climate_path   = "Data/Climate/Merged"
output_path    = "Data/Photosynthesis_settings"



##############################################################################################################################################
##############################################################################################################################################



#################
## Import data ##
#################



climate.df <- fread(paste(climate_path, "Hfloor.csv", sep="/"))



##############################################################################################################################################
##############################################################################################################################################



####################
## Data wrangling ##
####################



climate.df = climate.df %>%
  #separate date/time column
  separate(date, into = c("date","time"), sep=" ") %>%
  #set date as date and time as numeric
  mutate(lux_sun   = as.numeric(lux_sun),
         lux_shade = as.numeric(lux_shade),
         date      = lubridate::dmy(date),
         time      = as.numeric(gsub("\\:.*","", time)),
         luxRel    = lux_shade / lux_sun,
         tempDiff  = temp_sun - temp_shade) %>%
  #keep only measurements 10 days before measurement and from 9-14 o'clock
  filter(luxRel <= 1,
         lux_sun > 0,
         lux_shade > 0)

hist(climate.df$luxRel, breaks = 100)
  
#mean light reduction (%)
round(100 - mean(climate.df$luxRel)  * 100, 0)
#Standard deviation of light reduction (%)
round(sd(climate.df$luxRel) * 100, 0)

#mean temperature difference
round(mean(climate.df$tempDiff), 1)
#Standard deviation of temperature difference
round(sd(climate.df$tempDiff), 1)



##############################################################################################################################################
##############################################################################################################################################



###################
## Summary table ##
###################



#vector of desired variables
VariablesVector = c("temp_sun","lux_sun","temp_shade","lux_shade")
#summarize
summary.df = climate.df %>% 
  #keep only measurements from 8-16 o'clock
  #filter(time > 7 & time < 17) %>% 
  #get treatment-level means
  group_by(measurement) %>% 
  summarize_at(VariablesVector, median, na.rm = TRUE) %>%
  #get PAR and round
  #http://www.egc.com/useful_info_lighting.php
  mutate(PAR_sun    = round(lux_sun * 0.019),
         PAR_shade  = round(lux_shade * 0.019),
         temp_sun   = round(temp_sun,1),
         temp_shade = round(temp_shade,1),
         lux_sun    = round(lux_sun),
         lux_shade  = round(lux_shade) )

#print
as.data.frame(summary.df)



##############################################################################################################################################
##############################################################################################################################################


