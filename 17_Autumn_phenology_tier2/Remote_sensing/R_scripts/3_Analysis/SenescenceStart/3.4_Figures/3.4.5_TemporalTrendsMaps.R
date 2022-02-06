


#############################################################################################################
############################################## R script for: ################################################
#############################################################################################################
##### Effect of climate warming on the timing of autumn leaf senescence reverses at the summer solstice #####
#############################################################################################################


#############################################################################################################
# Extended Data Fig. 5 ######################################################################################
#############################################################################################################



#required packages
require(tidyverse)
require(data.table)
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
Drivers_path     = "Remote_sensing/Analysis/Analysis_input/Drivers_final_startSen/Merged_file"
EOS50_path       = "Remote_sensing/Analysis/Analysis_input/Drivers_final/Merged_file"
output_path      = "Remote_sensing/Analysis/Analysis_output_startSen/TemporalTrendsMaps"



##############################################################################################################################################
##############################################################################################################################################



#################
## Import data ##
#################



#EOS10 data
Pheno.df <- fread(paste(Drivers_path, "Remote_sensing_drivers_data_startSen_preseason.csv", sep="/")) %>%
  mutate(GPPstart.LO.SO = GPPstart.LO.SO*0.1,
         AutumnTday     = rowMeans(dplyr::select(.,`Tday9`,`Tday10`))) 

#EOS50 data
EOS50.df <- fread(paste(EOS50_path, "Remote_sensing_drivers_data_preseason.csv", sep="/"))%>%
  dplyr::select(c(geometry, Year, MidGreendown_DOY, Tday))

#Merge
Pheno.df = merge(Pheno.df, EOS50.df, by=c("geometry", "Year")) %>%
  mutate(EOS50_EOS10 = MidGreendown_DOY-Senesc_DOY)



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
  axis.text         = element_text(color="black"),
  strip.background  = element_rect(fill=NA),
  strip.text        = element_text(colour = 'black'),
  plot.title        = element_text(hjust=0.5))



##############################################################################################################################################
##############################################################################################################################################



###################################
## Run pixel-level linear models ##
###################################



Model.df = Pheno.df %>%
  group_by(geometry, Lat, Lon) %>%
  do({
    
    #run model
    ##########
    
    model1 = lm(GPPstart.LO.SO   ~ Year, data=.)
    model2 = lm(Greenup_DOY      ~ Year, data=.)
    model3 = lm(Tday.y           ~ Year, data=.)
    model4 = lm(Senesc_DOY       ~ Year, data=.)
    model5 = lm(MidGreendown_DOY ~ Year, data=.)
    model6 = lm(EOS50_EOS10      ~ Year, data=.)
    
    #create combined dataframe
    ##########################
    
    data.frame(rbind(
      tidy(model1) %>% mutate(model = "GPP"),
      tidy(model2) %>% mutate(model = "Greenup"),
      tidy(model3) %>% mutate(model = "AutumnTday"),
      tidy(model4) %>% mutate(model = "EOS10"),
      tidy(model5) %>% mutate(model = "EOS50"),
      tidy(model6) %>% mutate(model = "EOSduration")
      ))
  }) %>%
  #delete intercept
  filter(!term %in% c("(Intercept)")) 



##############################################################################################################################################
##############################################################################################################################################



#################
## Create maps ##
#################



#Get world map
mp <- NULL
mapWorld <- borders("world", colour="gray40", fill="gray40") # create a layer of borders

#---------------------------------------------------------------------------------------

#Add GPP info
#############

mapGPP = 
  Model.df %>%
  filter(model == "GPP") %>%
  mutate(estimate = ifelse(estimate > 10, 10, 
                            ifelse(estimate < -10, -10, estimate))) %>%
  ggplot() + mapWorld + plotTheme1 +
  geom_tile(show.legend=T,
               aes(x = Lon, y = Lat, fill=estimate)) +
  scale_fill_gradient2(midpoint=0, 
                       low='#3B9AB2', 
                       mid="white",  
                       high='#F21A00', 
                       space ="Lab" ) +
  coord_cartesian(ylim = c(27, 75), xlim = c(-160, 175)) +
  xlab("") + ylab('Latitude') +
  ggtitle("Pre-solstice GPP")+
  theme(legend.position = c(0.08,0.33),
        panel.background = element_rect(fill = "grey1", colour = NA),
        legend.text=element_text(color="white"))

#---------------------------------------------------------------------------------------

#Add Greenup info
#################

mapGreenup = Model.df %>%
  filter(model == "Greenup") %>%
  mutate(estimate = ifelse(estimate > 1.5, 1.5, 
                            ifelse(estimate < -1.5, -1.5, estimate))) %>%
  ggplot() + mapWorld + plotTheme1 +
  geom_tile(show.legend=T, aes(x = Lon, y = Lat, fill=estimate)) +
  scale_fill_gradient2(midpoint=0, 
                       low='#3B9AB2', 
                       mid="white",  
                       high='#F21A00', 
                       space ="Lab" ) +
  coord_cartesian(ylim = c(27, 75), xlim = c(-160, 175)) +
  xlab("") + ylab('Latitude') +
  ggtitle("Spring leaf-out")+
  theme(legend.position = c(0.08,0.33),
        panel.background = element_rect(fill = "grey1", colour = NA),
        legend.text=element_text(color="white"))

#---------------------------------------------------------------------------------------

#Add autumn temperature info
############################

mapAutumn = Model.df %>%
  filter(model == "AutumnTday") %>%
  mutate(estimate = ifelse(estimate > .2, .2, 
                           ifelse(estimate < -.2, -.2, estimate))) %>%
  ggplot() + mapWorld + plotTheme1 +
  geom_tile(show.legend=T, aes(x = Lon, y = Lat, fill=estimate)) +
  scale_fill_gradient2(midpoint=0, 
                       low='#3B9AB2', 
                       mid="white",  
                       high='#F21A00', 
                       space ="Lab" ) +
  coord_cartesian(ylim = c(27, 75), xlim = c(-160, 175)) +
  xlab("") + ylab('Latitude') +
  ggtitle("Autumn temperature")+
  theme(legend.position = c(0.08,0.33),
        panel.background = element_rect(fill = "grey1", colour = NA),
        legend.text=element_text(color="white"))

#---------------------------------------------------------------------------------------

#Add EOS10 info
###############

mapEOS10 = Model.df %>%
  filter(model == "EOS10") %>%
  mutate(estimate = ifelse(estimate > .8, .8, 
                           ifelse(estimate < -.8, -.8, estimate))) %>%
  ggplot() + mapWorld + plotTheme1 +
  geom_tile(show.legend=T, aes(x = Lon, y = Lat, fill=estimate)) +
  scale_fill_gradient2(midpoint=0, 
                       low='#3B9AB2', 
                       mid="white",  
                       high='#F21A00', 
                       space ="Lab" ) +
  coord_cartesian(ylim = c(27, 75), xlim = c(-160, 175)) +
  xlab("") + ylab('Latitude') +
  ggtitle(expression(Senescence~onset~(EOS[10])))+
  theme(legend.position = c(0.08,0.33),
        panel.background = element_rect(fill = "grey1", colour = NA),
        legend.text=element_text(color="white"))

#---------------------------------------------------------------------------------------

#Add EOS50 info
###############

mapEOS50 = Model.df %>%
  filter(model == "EOS50") %>%
  mutate(estimate = ifelse(estimate > .8, .8, 
                           ifelse(estimate < -.8, -.8, estimate))) %>%
  ggplot() + mapWorld + plotTheme1 +
  geom_tile(show.legend=T, aes(x = Lon, y = Lat, fill=estimate)) +
  scale_fill_gradient2(midpoint=0, 
                       low='#3B9AB2', 
                       mid="white",  
                       high='#F21A00', 
                       space ="Lab" ) +
  coord_cartesian(ylim = c(27, 75), xlim = c(-160, 175)) +
  xlab("") + ylab('Latitude') +
  ggtitle(expression("Mid-senescence"~(EOS[50])))+
  theme(legend.position = c(0.08,0.33),
        panel.background = element_rect(fill = "grey1", colour = NA),
        legend.text=element_text(color="white"))

#---------------------------------------------------------------------------------------

#Add EOS duration info
######################

mapEOSduration = Model.df %>%
  filter(model == "EOSduration") %>%
  mutate(estimate = ifelse(estimate > 1.2, 1.2, 
                           ifelse(estimate < -1.2, -1.2, estimate))) %>%
  ggplot() + mapWorld + plotTheme1 +
  geom_tile(show.legend=T, aes(x = Lon, y = Lat, fill=estimate)) +
  scale_fill_gradient2(midpoint=0, 
                       low='#3B9AB2', 
                       mid="white",  
                       high='#F21A00', 
                       space ="Lab" ) +
  coord_cartesian(ylim = c(27, 75), xlim = c(-160, 175)) +
  xlab("") + ylab('Latitude') +
  ggtitle(expression(Senescence~duration~(EOS[50]-EOS[10])))+
  theme(legend.position = c(0.08,0.33),
        panel.background = element_rect(fill = "grey1", colour = NA),
        legend.text=element_text(color="white"))



#############################################################################



###################
# Latitudinal plots
###################



LatPlotGPP = Model.df %>%
  filter(model == "GPP") %>%
  mutate(LatRound = round(Lat)) %>%
  group_by(term, LatRound) %>%
  summarise(mean   = mean(estimate),
            lowCI  = ci(estimate)[2],
            highCI = ci(estimate)[3]) %>% 
  ggplot(aes(x = LatRound, y = mean, group=term, color=term, group=term)) + 
  geom_ribbon(aes(ymin=lowCI, ymax=highCI, fill=term), color=NA, alpha=0.4)+
  geom_line()+
  geom_hline(yintercept=0)+
  scale_color_manual(values = c('#F21A00'))+
  scale_fill_manual(values  = c('#F21A00'))+
  ylab(expression(gC~m^2~year^-1)) +
  coord_flip(ylim = c(-10, 10),xlim=c(27,75))+
  plotTheme1 +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

#---------------------------------------------------------------------------------------

LatPlotGreenup = Model.df %>%
  filter(model == "Greenup") %>%
  mutate(LatRound = round(Lat)) %>%
  group_by(term, LatRound) %>%
  summarise(mean   = mean(estimate),
            lowCI  = ci(estimate)[2],
            highCI = ci(estimate)[3]) %>% 
  ggplot(aes(x = LatRound, y = mean, group=term, color=term, group=term)) + 
  geom_ribbon(aes(ymin=lowCI, ymax=highCI, fill=term), color=NA, alpha=0.4)+
  geom_line()+
  geom_hline(yintercept=0)+
  scale_color_manual(values = c('#3B9AB2'))+
  scale_fill_manual(values = c('#3B9AB2'))+
  ylab("days per year") +
  coord_flip(ylim = c(-1, 1),xlim=c(27,75))+
  plotTheme1 +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

#---------------------------------------------------------------------------------------

LatPlotAutumn = Model.df %>%
  filter(model == "AutumnTday") %>%
  mutate(LatRound = round(Lat)) %>%
  group_by(term, LatRound) %>%
  summarise(mean   = mean(estimate),
            lowCI  = ci(estimate)[2],
            highCI = ci(estimate)[3]) %>% 
  ggplot(aes(x = LatRound, y = mean, group=term, color=term, group=term)) + 
  geom_ribbon(aes(ymin=lowCI, ymax=highCI, fill=term), color=NA, alpha=0.4)+
  geom_line()+
  geom_hline(yintercept=0)+
  scale_color_manual(values = c('#F21A00'))+
  scale_fill_manual(values = c('#F21A00'))+
  ylab("??C per year") +
  coord_flip(ylim = c(-.12, .12),xlim=c(27,75))+
  plotTheme1 +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

#---------------------------------------------------------------------------------------

LatPlotEOS10 = Model.df %>%
  filter(model == "EOS10") %>%
  mutate(LatRound = round(Lat)) %>%
  group_by(term, LatRound) %>%
  summarise(mean   = mean(estimate),
            lowCI  = ci(estimate)[2],
            highCI = ci(estimate)[3]) %>% 
  ggplot(aes(x = LatRound, y = mean, group=term, color=term, group=term)) + 
  geom_ribbon(aes(ymin=lowCI, ymax=highCI, fill=term), color=NA, alpha=0.4)+
  geom_line()+
  geom_hline(yintercept=0)+
  scale_color_manual(values = c('#3B9AB2'))+
  scale_fill_manual(values = c('#3B9AB2'))+
  ylab("days per year") +
  coord_flip(ylim = c(-.3, .3),xlim=c(27,75))+
  plotTheme1 +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

#---------------------------------------------------------------------------------------

LatPlotEOS50 = Model.df %>%
  filter(model == "EOS50") %>%
  mutate(LatRound = round(Lat)) %>%
  group_by(term, LatRound) %>%
  summarise(mean   = mean(estimate),
            lowCI  = ci(estimate)[2],
            highCI = ci(estimate)[3]) %>% 
  ggplot(aes(x = LatRound, y = mean, group=term, color=term, group=term)) + 
  geom_ribbon(aes(ymin=lowCI, ymax=highCI, fill=term), color=NA, alpha=0.4)+
  geom_line()+
  geom_hline(yintercept=0)+
  scale_color_manual(values = c('#F21A00'))+
  scale_fill_manual(values = c('#F21A00'))+
  ylab("days per year") +
  coord_flip(ylim = c(-.3, .3),xlim=c(27,75))+
  plotTheme1 +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

#---------------------------------------------------------------------------------------

LatPlotEOSduration = Model.df %>%
  filter(model == "EOSduration") %>%
  mutate(LatRound = round(Lat)) %>%
  group_by(term, LatRound) %>%
  summarise(mean   = mean(estimate),
            lowCI  = ci(estimate)[2],
            highCI = ci(estimate)[3]) %>% 
  ggplot(aes(x = LatRound, y = mean, group=term, color=term, group=term)) + 
  geom_ribbon(aes(ymin=lowCI, ymax=highCI, fill=term), color=NA, alpha=0.4)+
  geom_line()+
  geom_hline(yintercept=0)+
  scale_color_manual(values = c('#F21A00'))+
  scale_fill_manual(values = c('#F21A00'))+
  ylab("days per year") +
  coord_flip(ylim = c(-.5, .5),xlim=c(27,75))+
  plotTheme1 +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())



#############################################################################



################
# Correlations #
################



#filter and long format
Model.df.wide = Model.df%>%
  select(c(estimate, model)) %>%
  filter(!Lat>62) %>%
  pivot_wider(., names_from = model, names_sep = ".", values_from = estimate) 

#GPP trend versus EOS10 trend
GPP_EOS10 = ggplot(Model.df.wide, aes(x=GPP, y=EOS10)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_smooth(method=lm, color='#3B9AB2')+
  coord_cartesian(xlim = c(quantile(Model.df.wide$GPP,probs=0.01), quantile(Model.df.wide$GPP,probs=0.99)),
                  ylim = c(-0.4,0.4)) +
  xlab(expression("Pre-solstice GPP trend"~(gC~m^-2~yr^-1))) +
  ylab(expression(EOS[10]~trend~(days~yr^-1))) +
  plotTheme1 

#Autumn Tday trend versus EOS50 trend
AutumnTemp_EOS50 = ggplot(Model.df.wide, aes(x=AutumnTday, y=EOS50)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_smooth(method=lm, color='#F21A00') +
  coord_cartesian(xlim = c(quantile(Model.df.wide$AutumnTday,probs=0.01), quantile(Model.df.wide$AutumnTday,probs=0.99)),
                  ylim = c(-0.4,0.4)) +
  xlab(expression(Autumn~temperature~trend~(degree*C~yr^-1))) +
  ylab(expression(EOS[50]~trend~(days~yr^-1))) +
  plotTheme1 

#Autumn Tday trend versus EOS duration trend
AutumnTemp_EOSduration = ggplot(Model.df.wide, aes(x=AutumnTday, y=EOSduration)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_smooth(method=lm, color='#F21A00') +
  coord_cartesian(xlim = c(quantile(Model.df.wide$AutumnTday,probs=0.01), quantile(Model.df.wide$AutumnTday,probs=0.99)),
                  ylim = c(-0.4,0.4)) +
  xlab(expression(Autumn~temperature~trend~(degree*C~yr^-1))) +
  ylab(expression(Senescence~duration~trend~(days~yr^-1))) +
  plotTheme1 



#############################################################################



#############
# All plots #
#############


#define plot layout
layout <- "
AAAAAB
CCCCCD
EEEEEF
GGGGGH
IIIIIJ
KKKKKL
MMNNOO"

#Merge plots
All_Plot = 
  mapGreenup     + LatPlotGreenup + 
  mapGPP         + LatPlotGPP + 
  mapEOS10       + LatPlotEOS10 +
  mapEOS50       + LatPlotEOS50 +
  mapEOSduration + LatPlotEOSduration +
  mapAutumn      + LatPlotAutumn +
  
  GPP_EOS10 + AutumnTemp_EOS50 + AutumnTemp_EOSduration +
  
  plot_layout(design = layout) + plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold'))

#save plots as .pdf
ggsave(All_Plot, file="All_temporal_changes.pdf", 
       path=output_path,
       width=12, height=3*7.1)



##############################################################################################################################################
##############################################################################################################################################



#####################
## Reproducibility ##	
#####################



## datetime
Sys.time()
#"2021-12-05 20:16:23 CET"

## session info
sessionInfo()
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: macOS Big Sur 11.2.3

#Matrix products: default
#LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib

#locale:
#  [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#  [1] effects_4.2-0     carData_3.0-4     lme4_1.1-27.1     Matrix_1.3-3      pracma_2.3.3      wesanderson_0.3.6
#[7] gmodels_2.18.1    patchwork_1.1.1   data.table_1.14.0 forcats_0.5.1     stringr_1.4.0     dplyr_1.0.7      
#[13] purrr_0.3.4       readr_1.4.0       tidyr_1.1.3       tibble_3.1.2      ggplot2_3.3.4     tidyverse_1.3.1  

#loaded via a namespace (and not attached):
#  [1] httr_1.4.2         maps_3.3.0         jsonlite_1.7.2     splines_4.1.0      modelr_0.1.8       gtools_3.9.2      
#[7] assertthat_0.2.1   cellranger_1.1.0   pillar_1.6.1       backports_1.2.1    lattice_0.20-44    glue_1.4.2        
#[13] digest_0.6.27      RColorBrewer_1.1-2 rvest_1.0.0        minqa_1.2.4        colorspace_2.0-1   survey_4.1-1      
#[19] pkgconfig_2.0.3    broom_0.7.8        haven_2.4.1        scales_1.1.1       gdata_2.18.0       generics_0.1.0    
#[25] farver_2.1.0       ellipsis_0.3.2     withr_2.4.2        nnet_7.3-16        cli_2.5.0          survival_3.2-11   
#[31] magrittr_2.0.1     crayon_1.4.1       readxl_1.3.1       estimability_1.3   fs_1.5.0           fansi_0.5.0       
#[37] nlme_3.1-152       MASS_7.3-54        xml2_1.3.2         tools_4.1.0        hms_1.1.0          mitools_2.4       
#[43] lifecycle_1.0.0    munsell_0.5.0      reprex_2.0.0       isoband_0.2.4      compiler_4.1.0     rlang_0.4.11      
#[49] grid_4.1.0         nloptr_1.2.2.2     rstudioapi_0.13    labeling_0.4.2     boot_1.3-28        gtable_0.3.0      
#[55] DBI_1.1.1          R6_2.5.0           lubridate_1.7.10   utf8_1.2.1         insight_0.14.2     stringi_1.6.2     
#[61] Rcpp_1.0.6         vctrs_0.3.8        dbplyr_2.1.1       tidyselect_1.1.1  



##############################################################################################################################################
#############################################################THE END##########################################################################
##############################################################################################################################################


