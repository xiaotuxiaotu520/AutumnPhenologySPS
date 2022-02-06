


#############################################################################################################
############################################## R script for: ################################################
#############################################################################################################
##### Effect of climate warming on the timing of autumn leaf senescence reverses at the summer solstice #####
#############################################################################################################


#############################################################################################################
# Solstice moving-window figures (not used so far) ##########################################################
#############################################################################################################



#required packages
require(tidyverse)
require(data.table)
require(ggplot2)
require(pracma)
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
setwd("/Users/consti/Desktop/PhD/Publication_material/17_Autumn_phenology_tier2/PEP_analysis/Analysis")


# Paths

#input
PEP_analysis_path = "Analysis_output/Autumn/Data"

#output
output_path = "Analysis_output/Autumn/Moving_window_pre_solstice"



##############################################################################################################################################
##############################################################################################################################################



#################
## Import data ##
#################



MW.df = fread(paste(PEP_analysis_path, "Moving_window_data_pre_solstice.csv", sep="/")) %>%
  filter(variable == 'Tday') %>%
  mutate(end.date = as.Date(end, origin="1970-01-01"),
         end.date.lowCI = as.Date(end.lowCI, origin="1970-01-01"),
         end.date.hiCI = as.Date(end.hiCI, origin="1970-01-01"))

range(MW.df[MW.df$species=="Aall",]$end.date)

##############################################################################################################################################
##############################################################################################################################################



################
## Plot theme ##
################


#Color.palette:  col=c('#F21A00','#E1AF00','#EBCC2A','#78B7C5','#3B9AB2')

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


  
#####################
# Moving window plots
#####################
  

  
# 20-year moving window
#######################
  
#subset
MW.df.sub = MW.df  %>% 
  filter(dataset == 'Long')
  
#get linear model coefficients
resultsLM = MW.df.sub %>% 
  group_by(variable)%>%
  filter(species == 'Aall') %>% 
  do({model = lm(end ~ year, data=.)  # create your model
  data.frame(tidy(model),                # get coefficient info
             lowCI=ci(model)[2,2],
             hiCI=ci(model)[2,3],
             glance(model))})%>%            # get model info
  filter(!term %in% c("(Intercept)")) %>%
  ungroup()

#plot
Long.MW = ggplot(MW.df.sub, aes(x = year, y = end.date, group=species, color=species)) + 
    
  geom_hline(yintercept = as.Date('1970-06-21'))+
  
    geom_line(size = 0.75) +
    
    geom_ribbon(data=MW.df.sub[MW.df.sub$species=='Aall',], 
                aes(ymin = end.date.lowCI, ymax = end.date.hiCI), 
                fill = "darkgrey", color=NA, alpha = 0.7) + 
    
    #geom_smooth(data=MW.df.sub[MW.df.sub$species=='Aall',], 
    #            aes(x = year, y = end.date), 
    #            method='lm', formula = y~x, se = FALSE, linetype="dashed")+
  
    geom_line(data=MW.df.sub[MW.df.sub$species=='Aall',], 
              aes(x = year, y = end.date), size = 1.25) +
    
    scale_color_manual(values = rev(wes_palette("Darjeeling2", n = 5))) +
    
    coord_cartesian(xlim=c(1967.3,1994.7),ylim=c(as.Date('1970-06-03'),as.Date('1970-08-01')))+
    
    annotate(geom="text", x=Inf, y = as.Date(Inf, origin="1970-01-01"), vjust=1.5, hjust=1.05, 
             label=paste0(round(resultsLM$estimate*10,1),' days per decade, R2 = ', round(resultsLM$r.squared,2)))+
    
    xlab("") + ylab('End temperature-sensitive period')+
    scale_y_date(date_labels = "%b %d")+
    scale_x_continuous(breaks = seq(1966,1996,by=5),
                       labels = c('1966-1985','1971-1990','1976-1995','1981-2000','1986-2005','1991-2010','1996-2015'))+
    plotTheme1+
    guides(col = guide_legend(ncol = 2))+
    theme(axis.text.x          = element_text(angle = 45, hjust=1)) 
  
  #-----------------------------------------------------------------------------------------------------------------------
  
  # 15-year moving window
  #######################
  
  #subset
  MW.df.sub = MW.df  %>% 
    filter(dataset == 'Short')
  
  #get linear model coefficients
  resultsLM = MW.df.sub %>% 
    filter(species == 'Aall') %>% 
    do({model = lm(end ~ year, data=.)     # create your model
    data.frame(tidy(model),                # get coefficient info
               lowCI=ci(model)[2,2],
               hiCI=ci(model)[2,3],
               glance(model))})%>%            # get model info
    filter(!term %in% c("(Intercept)")) 
  
  #plot
  Short.MW = ggplot(MW.df.sub, aes(x = year, y = end.date, group=species, color=species)) + 
    
    geom_hline(yintercept = as.Date('1970-06-21'))+
    
    geom_line(size = 0.75) +
    
    geom_ribbon(data=MW.df.sub[MW.df.sub$species=='Aall',], 
                aes(ymin = end.date.lowCI, ymax = end.date.hiCI), 
                fill = "darkgrey", color=NA, alpha = 0.7) + 
    
    geom_smooth(data=MW.df.sub[MW.df.sub$species=='Aall',], 
                aes(x = year, y = end.date), 
                method='lm', formula = y~x, se = FALSE, linetype="dashed")+
    
    geom_line(data=MW.df.sub[MW.df.sub$species=='Aall',], 
              aes(x = year, y = end.date), size = 1.25) +
    
    scale_color_manual(values = rev(wes_palette("Darjeeling2", n = 5))) +
    
    coord_cartesian(xlim=c(1981.5,2000.08),ylim=c(as.Date('1970-06-20'),as.Date('1970-07-10')))+
    
    annotate(geom="text", x=Inf, y = as.Date(Inf, origin="1970-01-01"), vjust=1.5, hjust=1.05, 
             label=paste0(round(resultsLM$estimate*10,1),' days per decade, R2 = ', round(resultsLM$r.squared,2)))+
    
    xlab("") + ylab('')+
    scale_y_date(date_labels = "%b %d")+
    scale_x_continuous(breaks = seq(1981,2001,by=5),
                       labels = c('1981-1995','1986-2000','1991-2005','1996-2010','2001-2015'))+
    plotTheme1
    theme(axis.text.x     = element_text(angle = 45, hjust=1),
          axis.text.y     = element_blank(),
          legend.position = 'right')
  
  
  
  ##############################################################################################################################################
  ##############################################################################################################################################
  
  
  
  ##########################
  # Arrange and safe plots #
  ##########################
  

  
  #define plot layout
  layout <- "AB"
  
  #Merge plots
  MW_Plot = Long.MW + Short.MW +
    plot_layout(design = layout) + plot_annotation(tag_levels = 'a')&
    theme(plot.tag = element_text(face = 'bold'))
  
  #save plots as .pdf
  ggsave(MW_Plot, file="Moving_window_preseason_sensitivity.pdf", path=output_path,
         width=8, height=4)
  
  
  
##############################################################################################################################################
##############################################################################################################################################



#####################
## Reproducibility ##	
#####################



## date time
Sys.time()
#"2021-12-08 09:55:07 CET"


## session info
sessionInfo()
#R version 4.1.0 (2021-05-18)
#Platform: x86_64-apple-darwin17.0 (64-bit)
#Running under: macOS Big Sur 11.2.3

#Matrix products: default
#LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib

#locale:
#  [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#  [1] broom_0.7.8       patchwork_1.1.1   wesanderson_0.3.6 pracma_2.3.3      data.table_1.14.0 forcats_0.5.1    
#[7] stringr_1.4.0     dplyr_1.0.7       purrr_0.3.4       readr_1.4.0       tidyr_1.1.3       tibble_3.1.2     
#[13] ggplot2_3.3.4     tidyverse_1.3.1  

#loaded via a namespace (and not attached):
#  [1] tidyselect_1.1.1 splines_4.1.0    haven_2.4.1      lattice_0.20-44  colorspace_2.0-1 vctrs_0.3.8      generics_0.1.0  
#[8] mgcv_1.8-35      utf8_1.2.1       rlang_0.4.11     pillar_1.6.1     glue_1.4.2       withr_2.4.2      DBI_1.1.1       
#[15] dbplyr_2.1.1     modelr_0.1.8     readxl_1.3.1     lifecycle_1.0.0  plyr_1.8.6       munsell_0.5.0    gtable_0.3.0    
#[22] cellranger_1.1.0 rvest_1.0.0      fansi_0.5.0      Rcpp_1.0.6       scales_1.1.1     backports_1.2.1  jsonlite_1.7.2  
#[29] farver_2.1.0     fs_1.5.0         digest_0.6.27    hms_1.1.0        stringi_1.6.2    grid_4.1.0       cli_2.5.0       
#[36] tools_4.1.0      magrittr_2.0.1   crayon_1.4.1     pkgconfig_2.0.3  ellipsis_0.3.2   Matrix_1.3-3     xml2_1.3.2      
#[43] reprex_2.0.0     lubridate_1.7.10 assertthat_0.2.1 httr_1.4.2       rstudioapi_0.13  R6_2.5.0         nlme_3.1-152    
#[50] compiler_4.1.0  



##############################################################################################################################################
#############################################################THE END##########################################################################
##############################################################################################################################################


