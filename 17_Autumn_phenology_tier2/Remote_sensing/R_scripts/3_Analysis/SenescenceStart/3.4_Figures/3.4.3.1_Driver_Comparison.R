


#############################################################################################################
############################################## R script for: ################################################
#############################################################################################################
##### Effect of climate warming on the timing of autumn leaf senescence reverses at the summer solstice #####
#############################################################################################################


#############################################################################################################
# Fig. 5 and Extended Data Fig. 9 ###########################################################################
#############################################################################################################



#required packages
require(tidyverse)
require(data.table)
require(broom.mixed)
require(gmodels)
require(lme4)
require(car)
require(sjmisc)
require(wesanderson)
require(patchwork)



##############################################################################################################################################
##############################################################################################################################################



#####################
## Set directories ##
#####################



# set the working directory
setwd("/Users/consti/Desktop/PhD/Publication_material/17_Autumn_phenology_tier2")

# paths
startSen_path     = "Remote_sensing/Analysis/Analysis_input/Drivers_final_startSen/Merged_file"
MidGreendown_path = "Remote_sensing/Analysis/Analysis_input/Drivers_final/Merged_file"
PEP_path          = "PEP_analysis/Analysis/Analysis_input/PEP_drivers_final/Merged_file"
output_path       = "Remote_sensing/Analysis/Analysis_output_startSen/DriverComparison"



##############################################################################################################################################
##############################################################################################################################################



#################
## Import data ##
#################



#PEP dataframe
##############

PEP.df <- fread(paste(PEP_path, "pep_drivers_data_preseason.csv", sep="/")) %>%
  mutate(SWrad.LO.SO = rowSums(.[,363:365]))



#MODIS Senescence dataframe
###########################

startSen.df <- fread(paste(startSen_path, "Remote_sensing_drivers_data_startSen_preseason.csv", sep="/")) %>%
  #data transformation
  mutate(Prcp.SO.SE  = log(Prcp.SO.SE+1),
         Prcp.LO.SO  = log(Prcp.LO.SO+1))%>%
  #delete pixels with no photosynthesis before solstice
  group_by(geometry) %>%
  filter(!(mean(GPPstart.LO.SO)<.1)) %>%
  ungroup()



#MODIS MidGreendown dataframe
#############################

MidGreendown.df <- fread(paste(MidGreendown_path, "Remote_sensing_drivers_data_preseason.csv", sep="/")) %>%
  #data transformation
  mutate(Prcp.SO.SE  = log(Prcp.SO.SE+1),
         Prcp.LO.SO  = log(Prcp.LO.SO+1))%>%
  #delete pixels with no photosynthesis before solstice
  group_by(geometry) %>%
  filter(!(mean(GPPstart.LO.SO)<.1)) %>%
  ungroup()



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
  axis.text.y       = element_text(colour = 'black'),
  axis.text.x       = element_text(angle = 45, hjust=1, colour = 'black'),
  strip.background  = element_rect(fill=NA),
  strip.text        = element_text(colour = 'black'),
  plot.title        = element_text(face="bold", size=11, hjust = 0.5))



##############################################################################################################################################
##############################################################################################################################################



#################################################
## MODIS: Pixel-level linear senescence models ##
#################################################



RSModel.df = startSen.df %>%
  group_by(LC_Type, geometry) %>%
  do({
    
    #run model
    ##########
    
    model1 = lm(scale(Senesc_DOY)~
                  scale(SWrad.LO.SO)+scale(Prcp.LO.SO)+scale(Greenup_DOY)+
                  scale(Tday.SO.SE)+scale(SWrad.SO.SE)+scale(Prcp.SO.SE)+scale(Tday.LO.SO),  
                  data=.)
    
    #create combined dataframe
    ##########################
    
    data.frame(tidy(model1) )
    
  }) %>%
  mutate(term     = gsub("scale","",term)) %>%
  mutate(term     = str_replace_all(term,"\\(|\\)", "") ) %>%
  #delete intercept
  filter(!term %in% c("Intercept")) %>%
  ungroup()


#Summarize table by land cover type
RSModel.df2 = RSModel.df %>%
  group_by(LC_Type, term) %>%
  summarise(mean = ci(estimate)[1], 
            lowCI = ci(estimate)[2],
            hiCI = ci(estimate)[3]) %>%
  mutate(LC_Type  = factor(LC_Type, levels=c("Mixed","DecB","EvgN","DecN")),
         LC_Type = plyr::revalue(LC_Type, c("Mixed" = "Mixed", 
                                            "DecB" = "Deciduous broadleaf",
                                            "EvgN" = "Evergreen needleleaf",
                                            "DecN" = "Deciduous needleleaf")),
         term = factor(term, 
                       levels=c('Tday.LO.SO','Tday.SO.SE','SWrad.LO.SO', 'SWrad.SO.SE',"Greenup_DOY", "Prcp.LO.SO", 'Prcp.SO.SE'), ordered=T) )


#Summarize table for all pixels
RSModel.df3 = RSModel.df %>%
  group_by(term) %>%
  summarise(mean = ci(estimate)[1], 
            lowCI = ci(estimate)[2],
            hiCI = ci(estimate)[3]) %>%
  mutate(term = factor(term, 
                       levels=c('Tday.LO.SO','Tday.SO.SE','SWrad.LO.SO', 'SWrad.SO.SE',"Greenup_DOY", "Prcp.LO.SO", 'Prcp.SO.SE'), ordered=T) )



##############################################################################################################################################
##############################################################################################################################################



###################################################
## MODIS: Pixel-level linear MidGreendown models ##
###################################################



MidGreendownModel.df = MidGreendown.df %>%
  group_by(LC_Type, geometry) %>%
  do({
    
    #run model
    ##########
    
    model1 = lm(scale(MidGreendown_DOY)~
                  scale(Tday.LO.SO)+scale(SWrad.LO.SO)+scale(Prcp.LO.SO)+scale(Greenup_DOY)+
                  scale(Tday.SO.SE)+scale(SWrad.SO.SE)+scale(Prcp.SO.SE),  
                data=.)
    
    #create combined dataframe
    ##########################
    
    data.frame(tidy(model1) )
    
  }) %>%
  mutate(term     = gsub("scale","",term)) %>%
  mutate(term     = str_replace_all(term,"\\(|\\)", "") ) %>%
  #delete intercept
  filter(!term %in% c("Intercept")) %>%
  ungroup()


#Summarize table by land cover type
MidGreendownModel.df2 = MidGreendownModel.df %>%
  group_by(LC_Type, term) %>%
  summarise(mean = ci(estimate)[1], 
            lowCI = ci(estimate)[2],
            hiCI = ci(estimate)[3]) %>%
  mutate(LC_Type  = factor(LC_Type, levels=c("Mixed","DecB","EvgN","DecN")),
         LC_Type = plyr::revalue(LC_Type, c("Mixed" = "Mixed", 
                                            "DecB" = "Deciduous broadleaf",
                                            "EvgN" = "Evergreen needleleaf",
                                            "DecN" = "Deciduous needleleaf")),
         term = factor(term, 
                       levels=c('Tday.LO.SO','Tday.SO.SE','SWrad.LO.SO', 'SWrad.SO.SE',"Greenup_DOY", "Prcp.LO.SO", 'Prcp.SO.SE'), ordered=T) )


#Summarize table for all pixels
MidGreendownModel.df3 = MidGreendownModel.df %>%
  group_by(term) %>%
  summarise(mean = ci(estimate)[1], 
            lowCI = ci(estimate)[2],
            hiCI = ci(estimate)[3]) %>%
  mutate(term = factor(term, 
                       levels=c('Tday.LO.SO','Tday.SO.SE','SWrad.LO.SO', 'SWrad.SO.SE',"Greenup_DOY", "Prcp.LO.SO", 'Prcp.SO.SE'), ordered=T) )



##############################################################################################################################################
##############################################################################################################################################



############################################
## PEP725: Individual-level linear models ##
############################################



PEPmodel.df = PEP.df %>%
  group_by(species, pep_id) %>%
  do({
    
    #run model
    ##########
    
    model1 = lm(scale(leaf_off)~
                  scale(Tday.LO.SO)  +scale(SWrad.LO.SO)+scale(Prcp.LO.SO)+scale(leaf_out)+
                  scale(Tnight.SO.SE)+scale(SWrad.SO.SE)+scale(Prcp.SO.SE),  
                data=.)
    
    #create combined dataframe
    ##########################
    
    data.frame(rbind(tidy(model1)))
    
  }) %>%
  mutate(term     = gsub("scale","",term)) %>%
  mutate(term     = str_replace_all(term,"\\(|\\)", "") ) %>%
  #delete intercept
  filter(!term %in% c("Intercept")) %>%
  ungroup()


#Summarize table by species
PEPmodel.df2 = PEPmodel.df %>%
  group_by(species, term) %>%
  summarise(mean = ci(estimate)[1], 
            lowCI = ci(estimate)[2],
            hiCI = ci(estimate)[3]) %>%
  mutate(term = factor(term, 
                       levels=c('Tday.LO.SO','Tnight.SO.SE','SWrad.LO.SO', 'SWrad.SO.SE',"leaf_out", "Prcp.LO.SO", 'Prcp.SO.SE'), ordered=T) )


#Summarize table for all time series
PEPmodel.df3 = PEPmodel.df %>%
  group_by(term) %>%
  summarise(mean = ci(estimate)[1], 
            lowCI = ci(estimate)[2],
            hiCI = ci(estimate)[3]) %>%
  mutate(term = factor(term, 
                       levels=c('Tday.LO.SO','Tnight.SO.SE','SWrad.LO.SO', 'SWrad.SO.SE',"leaf_out", "Prcp.LO.SO", 'Prcp.SO.SE'), ordered=T) )



##############################################################################################################################################
##############################################################################################################################################



##################################
# Land-cover type-specific plots #
##################################



A = ggplot(data = RSModel.df2, aes(x = term, y = mean, fill=term)) + 
  geom_bar(position=position_dodge(),stat = "identity")+
  geom_errorbar(aes(ymin=lowCI, ymax=hiCI),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
geom_hline(yintercept = 0)+
  xlab("") + ylab("Standardized effect") +
  facet_grid(LC_Type~.)+
  coord_cartesian(ylim=c(-.29,.29))+
  scale_fill_manual(values = rev(wes_palette(7, name = "Zissou1", type = "continuous")))+
  ggtitle(expression(EOS[10]~(Satellite))) +
  plotTheme1

B = ggplot(data = MidGreendownModel.df2, aes(x = term, y = mean, fill=term)) + 
  geom_bar(position=position_dodge(),stat = "identity")+
  geom_errorbar(aes(ymin=lowCI, ymax=hiCI),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  geom_hline(yintercept = 0)+
  xlab("") + ylab("") +
  facet_grid(LC_Type~.)+
  coord_cartesian(ylim=c(-.29,.29))+
  scale_fill_manual(values = rev(wes_palette(7, name = "Zissou1", type = "continuous")))+
  ggtitle(expression(EOS[50]~(Satellite))) +
  plotTheme1+
  theme(axis.text.y=element_blank())

C = ggplot(data = PEPmodel.df2, aes(x = term, y = mean, fill=term)) + 
  geom_bar(position=position_dodge(),stat = "identity")+
  geom_errorbar(aes(ymin=lowCI, ymax=hiCI),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  geom_hline(yintercept = 0)+
  xlab("") + ylab("") +
  facet_grid(species~.)+
  coord_cartesian(ylim=c(-.29,.29))+
  scale_fill_manual(values = rev(wes_palette(7, name = "Zissou1", type = "continuous")))+
  ggtitle(expression(EOS[50]~(PEP725~data))) +
  plotTheme1 +
  theme(axis.text.y=element_blank(),
        strip.text        = element_text(colour = 'black', face="italic"))

#define plot layout
layout <- "
ABC"

#Merge plots
DriverPlot = A + B + C +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'a')&
  theme(plot.tag = element_text(face = 'bold'))

#save plots as .pdf
ggsave(DriverPlot, file="DriverPlot_LCtype.pdf", path=output_path,
       width=7, height=7)



##############################################################################################################################################
##############################################################################################################################################



##################
# Combined plots #
##################



A = ggplot(data = RSModel.df3, aes(x = term, y = mean, fill=term)) + 
  geom_bar(position=position_dodge(),stat = "identity")+
  geom_errorbar(aes(ymin=lowCI, ymax=hiCI),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  geom_hline(yintercept = 0)+
  xlab("") + ylab("Standardized effect") +
  coord_cartesian(ylim=c(-.29,.29))+
  scale_fill_manual(values = rev(wes_palette(7, name = "Zissou1", type = "continuous")))+
  ggtitle(expression(EOS[10]~(Satellite))) +
  plotTheme1 +
  theme(axis.text.x=element_blank())

B = ggplot(data = MidGreendownModel.df3, aes(x = term, y = mean, fill=term)) + 
  geom_bar(position=position_dodge(),stat = "identity")+
  geom_errorbar(aes(ymin=lowCI, ymax=hiCI),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  geom_hline(yintercept = 0)+
  xlab("") + ylab("Standardized effect") +
  coord_cartesian(ylim=c(-.29,.29))+
  scale_fill_manual(values = rev(wes_palette(7, name = "Zissou1", type = "continuous")))+
  ggtitle(expression(EOS[50]~(Satellite))) +
  plotTheme1+
  theme(axis.text.x=element_blank())

C = ggplot(data = PEPmodel.df3, aes(x = term, y = mean, fill=term)) + 
  geom_bar(position=position_dodge(),stat = "identity")+
  geom_errorbar(aes(ymin=lowCI, ymax=hiCI),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  geom_hline(yintercept = 0)+
  xlab("") + ylab("Standardized effect") +
  coord_cartesian(ylim=c(-.29,.29))+
  scale_fill_manual(values = rev(wes_palette(7, name = "Zissou1", type = "continuous")))+
  ggtitle(expression(EOS[50]~(PEP725~data))) +
  plotTheme1 

#define plot layout
layout <- "
A
B
C"

#Merge plots
DriverPlot = A + B + C +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'a')&
  theme(plot.tag = element_text(face = 'bold'))

#save plots as .pdf
ggsave(DriverPlot, file="DriverPlot_All.pdf", path=output_path,
       width=2.5, height=7)



##############################################################################################################################################
#############################################################THE END##########################################################################
##############################################################################################################################################


