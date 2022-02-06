


#############################################################################################################
############################################## R script for: ################################################
#############################################################################################################
##### Effect of climate warming on the timing of autumn leaf senescence reverses at the summer solstice #####
#############################################################################################################


#############################################################################################################
# Extended Data Fig. 8 ######################################################################################
#############################################################################################################



#required packages
require(tidyverse)
require(data.table)
require(ggplot2)
require(wesanderson)
require(patchwork)
require(broom)
require(gmodels)



##############################################################################################################################################
##############################################################################################################################################



#####################
## Set directories ##
#####################



# set the working directory
setwd("/Users/consti/Desktop/PhD/Publication_material/17_Autumn_phenology_tier2")


# Paths

#input
MODIS.EOS10_path = "Remote_sensing/Analysis/Analysis_output_startSen/Data"
MODIS.EOS50_path = "Remote_sensing/Analysis/Analysis_output/Data"
PEP_path         = "PEP_analysis/Analysis/Analysis_output/Autumn/Data"

#output
output_path      = "Remote_sensing/Analysis/Analysis_output_startSen/ModelComparison"



##############################################################################################################################################
##############################################################################################################################################



#################
## Import data ##
#################



#Timeseries-level linear models
###############################

#MODIS EOS10 data
MODIS.EOS10.df = fread(paste(MODIS.EOS10_path, "Model_R2_data.csv", sep="/")) %>%
  filter(model == "pre",
         !variable %in% c("GDDday","GDDnight")) %>%
  mutate(variable = plyr::revalue(variable, c(
                                      "GPPstart" = "GPP",
                                      "SWrad" = "SW radiation",
                                      "Moist" = "Moisture",
                                      "Greenup_DOY" = "Leafout")),
         variable = factor(variable, levels=c("GPP","Tday","Leafout","SW radiation","Moisture"), ordered=T)) 
  
#MODIS EOS50 data
MODIS.EOS50.df = fread(paste(MODIS.EOS50_path, "Model_R2_data.csv", sep="/")) %>%
  filter(model == "pre",
         !variable %in% c("LAIstart","GDDday","GDDnight")) %>%
  mutate(variable = plyr::revalue(variable, c(
    "GPPstart" = "GPP",
    "SWrad" = "SW radiation",
    "Moist" = "Moisture",
    "Greenup_DOY" = "Leafout")),
    variable = factor(variable, levels=c("GPP", "Tday","Leafout","SW radiation","Moisture"), ordered=T)) 

#PEP725 data
PEP.df = fread(paste(PEP_path, "Model_R2_data.csv", sep="/")) %>%
  filter(model == "pre",
         !variable %in% c("GDDday","GDDnight")) %>%
  mutate(species = gsub("^.*?\\_","", timeseries),#delete before _
         variable = plyr::revalue(variable, c("Azani" = "Anetday (LPJ)",
                                              "SWrad" = "SW radiation",
                                              "Moist" = "Moisture",
                                              "leaf_out" = "Leafout")),
         variable = factor(variable, levels=c('Anetday (LPJ)',"Tday", "Leafout","SW radiation","Moisture"), ordered=T))  

#LC_Type: Land cover type
#geometry: pixel identifier
#r.squared: normal R2



##############################################################################################################################################
##############################################################################################################################################



################
## Plot theme ##
################

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
  axis.text.x       = element_text(angle = 45, hjust=1, colour = 'black'),
  strip.background  = element_rect(fill=NA),
  plot.title        = element_text(face="bold",hjust = 0.5))



##############################################################################################################################################
##############################################################################################################################################



#############
## Summary ##
#############



#summarize MODIS EOS10 table by groups
data.frame(MODIS.EOS10.df %>% 
             group_by(model,variable) %>% 
             summarise(mean  = mean(r.squared)*100, 
                       mean_round = round(mean),
                       lowCI = ceiling((ci(r.squared)[2]*100)),
                       hiCI  = ceiling((ci(r.squared)[3]*100)),
                       meanAIC = mean(AIC)))

#summarize MODIS EOS50 table by groups
data.frame(MODIS.EOS50.df %>% 
             group_by(model,variable) %>% 
             summarise(mean       = mean(r.squared)*100, 
                       mean_round = round(mean),
                       lowCI      = ceiling((ci(r.squared)[2]*100)),
                       hiCI       = ceiling((ci(r.squared)[3]*100))) )

#summarize PEP725 table by groups
data.frame(PEP.df %>% 
             group_by(model,variable) %>% 
             summarise(mean       = mean(r.squared)*100, 
                       mean_round = round(mean),
                       lowCI      = ceiling((ci(r.squared)[2]*100)),
                       hiCI       = ceiling((ci(r.squared)[3]*100))) )



##############################################################################################################################################
##############################################################################################################################################



##########
## Plot ##
##########



#MODIS EOS10
MODIS.EOS10.plot = MODIS.EOS10.df %>%
  ggplot(aes(x=variable, y=r.squared, fill=variable)) + 
  stat_summary(fun = "mean", geom="bar", position=position_dodge(), width = 0.8, color="black") +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size = .5, width=0.1, 
               position = position_dodge(0.8)) +
  labs(x = "", y = "R2") +
  scale_fill_manual(values = rev(wes_palette(6, name = "Zissou1", type = "continuous")))+
  coord_cartesian(ylim = c(0.007, 0.15))+
  ggtitle(expression(EOS[10]~(Satellite))) +
  plotTheme1

#MODIS EOS50
MODIS.EOS50.plot = MODIS.EOS50.df %>%
  ggplot(aes(x=variable, y=r.squared, fill=variable, group=model)) + 
  stat_summary(fun = "mean", geom="bar", position=position_dodge(), width = 0.8, color="black") +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size = .5, width=0.1, 
               position = position_dodge(0.8)) +
  labs(x = "", y = "") +
  scale_fill_manual(values = rev(wes_palette(6, name = "Zissou1", type = "continuous")))+
  coord_cartesian(ylim = c(0.007, 0.15))+
  ggtitle(expression(EOS[50]~(Satellite))) +
  plotTheme1+
  theme(axis.text.y=element_blank())

#PEP725
PEP.plot = PEP.df %>%
  ggplot(aes(x=variable, y=r.squared, fill=variable, group=model)) + 
  stat_summary(fun = "mean", geom="bar", position=position_dodge(),  width = 0.8, color="black") +
  stat_summary(fun.data = "mean_cl_boot", geom="errorbar", size = .5, width=0.1, 
               position = position_dodge(0.8)) +
  labs(x = "", y = "") +
  scale_fill_manual(values = rev(wes_palette(6, name = "Zissou1", type = "continuous")))+
  coord_cartesian(ylim = c(0.007, 0.15)) +
  ggtitle(expression(EOS[50]~(PEP725~data))) +
  plotTheme1+
  theme(axis.text.y=element_blank())


#define plot layout
layout <- "ABC"

#Merge plots
R2_plot =  MODIS.EOS10.plot + MODIS.EOS50.plot + PEP.plot +
  plot_layout(design = layout) + plot_annotation(tag_levels = 'a')&
  theme(plot.tag = element_text(face = 'bold'))

#save plot as pdf
ggsave(R2_plot, file="ModelComparison.pdf", path=output_path,
       width=7, height=4)



##############################################################################################################################################
#############################################################THE END##########################################################################
##############################################################################################################################################


