


#############################################################################################################
############################################## R script for: ################################################
#############################################################################################################
##### Effect of climate warming on the timing of autumn leaf senescence reverses at the summer solstice #####
#############################################################################################################


#############################################################################################################
# Extended Data Fig. 4 ######################################################################################
#############################################################################################################



#required packages
require(tidyverse)
require(data.table)
require(ggplot2)
require(broom)



##############################################################################################################################################
##############################################################################################################################################



#####################
## Set directories ##
#####################



# set the working directory
setwd("/Users/consti/Desktop/PhD/Publication_material/17_Autumn_phenology_tier2/Remote_sensing/Analysis")


# Paths

#input
Drivers_path      = "Analysis_input/Drivers_final_startSen/Merged_file"
Analysis_path     = "Analysis_output_startSen/Data"

#output
output_path       = "Analysis_output_startSen/LatitudePlot"



##############################################################################################################################################
##############################################################################################################################################



plotTheme1 = theme(
  legend.position   = "none",
  legend.background = element_blank(),
  legend.text       = element_text(color="black"),
  legend.title      = element_blank(),
  legend.key        = element_blank(),
  panel.grid.major  = element_blank(),
  panel.grid.minor  = element_blank(),
  panel.background  = element_blank(),
  panel.border      = element_rect(colour = "black", fill=NA),
  axis.line         = element_line(color = "black"),
  axis.text         = element_text(colour = "black"),
  strip.background  = element_rect(fill=NA),
  strip.text        = element_text(colour = 'black',face = "italic"),
  plot.title        = element_text(face="bold"))



##############################################################################################################################################
##############################################################################################################################################



#################
## Import data ##
#################



#Phenology data
###############

Pheno.df = fread(paste(Drivers_path, "Remote_sensing_drivers_data_startSen_preseason.csv", sep="/"))


#Spatial (pixel-level) models
#############################

Analysis.df = fread(paste(Analysis_path, "Spatial_effect_data.csv", sep="/"))

#geometry: unique pixel identifier
#Lat: Latitude
#Lon: Longitude
#LC_type: All, DecB, DecN, EvgN, Mixed (Landcover type)
#term: monthly coefficients (1-10) and seasonal coefficients
#estimate: slopes or standardized coefficients of mixed effects models
#std.error: std.error of coefficients
#equation: full model 1/2, monthly/seasonal/solstice
#variable: climate variable (LAI, GPP, Apm, Azani, Tday, Tnight, SWrad)


# get models of climate/growth effects from spring/leaf-out to May 22, June 21, July 21, or August 20
#####################################################################################################

Model.df = Analysis.df %>%
  filter(equation == "Seasonal.scaled",
         variable %in% c("GPPstart"),
         Lat>29.5, Lat<65.5) %>%
  mutate(latRound = round(Lat,-.5),
         period   = gsub("^.*?\\.","", term)) %>%
  filter(!period %in% c("LO.SE","SO.SE","SOm30.SE","SOp30.SE")) %>%
  #create preseason length and temperature class columns
  mutate(period_end = ifelse(period == "LO.SOm30", 172-30, 
                             ifelse(period == "LO.SO", 172, 
                                    ifelse(period == "LO.SOp30", 172+30, 172+60))))



#########################################################################################################################
#########################################################################################################################



#keep only models with most negative estimates and summarize by latitude
########################################################################

Model.df = Model.df %>%   
  
  #keep only model with most negative estimate for each pixel
  group_by(geometry, latRound) %>% 
  top_n(-1, estimate) %>%
  ungroup() %>%
  
  #Summarize by latitude
  group_by(latRound) %>% 
  summarise(end            = mean(period_end),
            end.lowCI      = t.test(period_end)$conf.int[1],
            end.hiCI       = t.test(period_end)$conf.int[2])%>%
  ungroup() %>%
  
  #transform to date
  mutate(end.date = as.Date(end, origin="1970-01-01"),
         end.date.lowCI = as.Date(end.lowCI, origin="1970-01-01"),
         end.date.hiCI = as.Date(end.hiCI, origin="1970-01-01"))



#get leaf-out dates per latitude
################################

Leafout.df = Pheno.df %>%   
  filter(Lat>29.5, Lat<65.5) %>%
  mutate(latRound = round(Lat)) %>%
  group_by(latRound) %>% 
  summarise(start            = mean(Greenup_DOY),
            start.lowCI      = t.test(Greenup_DOY)$conf.int[1],
            start.hiCI       = t.test(Greenup_DOY)$conf.int[2])%>%
  ungroup() %>%
  
  #transform to date
  mutate(start.date = as.Date(start, origin="1970-01-01"),
         start.date.lowCI = as.Date(start.lowCI, origin="1970-01-01"),
         start.date.hiCI = as.Date(start.hiCI, origin="1970-01-01"))


#Run linear models to test effect of latitude
################################

EndDateLM = summary(lm(end ~ latRound, data=Model.df))
leafoutLM = summary(lm(start ~ latRound, data=Leafout.df))



#########################################################################################################################
#########################################################################################################################



########
# Plot #
########



LatPlot = ggplot(Model.df, aes(x = latRound, y = end.date)) + 
  
  #Solstice line
  geom_hline(yintercept = as.Date('1970-06-21'), color="grey",size=3)+
  
  
  #End of early-season effect data
  geom_smooth(method='lm', formula = y~x, se = FALSE, linetype="dashed", color="black")+
  
  geom_ribbon(aes(ymin = end.date.lowCI, ymax = end.date.hiCI), 
              fill = '#F21A00', color=NA, alpha = 0.3) +
  
  geom_line(size=1, color="#F21A00") +
  
  
  
  annotate(geom="text", x=-Inf, y = as.Date(Inf, origin="1970-01-01"), vjust=1.5, hjust=-.03, color= '#F21A00',
           label=paste0("Mean = ", format(as.Date(floor(mean(Model.df$end)), origin="1970-01-01"), "%b %d"),
                        "\n",
                        round(EndDateLM$coefficients[2,1],2),
                        ' days per ??Lat, R2 = ', round(EndDateLM$r.squared,2),ifelse(EndDateLM$coefficients[2,4]>0.05," (n.s.)","check again")))+
  
  
  #Add leaf-out data
  geom_smooth(data = Leafout.df, aes(x=latRound, y=start.date),
              method='lm', formula = y~x, se = FALSE, linetype="dashed", color="black")+
  
  geom_ribbon(data = Leafout.df, aes(x=latRound, y=start.date, ymin = start.date.lowCI, ymax = start.date.hiCI), 
              fill = "green4", color=NA, alpha = 0.3) +
  
  geom_line(data = Leafout.df, aes(x=latRound, y=start.date), color="green4", size=1)+
  
  annotate(geom="text", x=Inf, y = as.Date(-Inf, origin="1970-01-01"), vjust=-.5, hjust=1.05, color= "green4",
           label=paste0("Mean = ", format(as.Date(floor(mean(Leafout.df$start)), origin="1970-01-01"),"%b %d"),
                        "\n",
                        round(leafoutLM$coefficients[2,1],2),
                        ' days per ??Lat, R2 = ', 
                        round(leafoutLM$r.squared,2),
                        ifelse(leafoutLM$coefficients[2,4]<0.001,"***","check again")))+
  
  
  #Plot settings
  coord_cartesian(xlim=c(31.47,61.5),ylim=c(as.Date('1970-04-01'),as.Date('1970-08-10')))+
  xlab("Latitude") + ylab('End date of negative early-season effect')+
  scale_y_date(date_labels = "%b %d")+
  plotTheme1



#save plots as .pdf
ggsave(LatPlot, file="LatitudePlot.pdf", 
       path=output_path,
       width=4, height=4)



##############################################################################################################################################
#############################################################THE END##########################################################################
##############################################################################################################################################


