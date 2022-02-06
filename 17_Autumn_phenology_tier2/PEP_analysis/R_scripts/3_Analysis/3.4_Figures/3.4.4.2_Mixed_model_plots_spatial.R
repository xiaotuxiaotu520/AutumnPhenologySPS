


#############################################################################################################
############################################## R script for: ################################################
#############################################################################################################
##### Effect of climate warming on the timing of autumn leaf senescence reverses at the summer solstice #####
#############################################################################################################


#############################################################################################################
# Supplementary Fig. S9: Spatial regression plots based on mixed effects models for the PEP725 data set #####
#############################################################################################################



#required packages
require(tidyverse)
require(data.table)
require(lme4)
require(effects) #plot effects



#Plot theme
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
  strip.background  = element_blank(),
  strip.text        = element_text(colour = 'black'),
  plot.title        = element_text(face="bold"))



##############################################################################################################################################
##############################################################################################################################################



#####################
## Set directories ##
#####################

# set the working directory
setwd("/Users/consti/Desktop/PhD/Publication_material/17_Autumn_phenology_tier2/PEP_analysis/Analysis")

# paths
PEP_drivers_path    = "Analysis_input/PEP_drivers_final/Merged_file"
output_path         = "Analysis_output/Autumn/Mixed_model_plots"



##############################################################################################################################################
##############################################################################################################################################



#################
## Import data ##
#################



#Phenology dataframe
####################

PEP.df <- fread(paste(PEP_drivers_path, "pep_drivers_data_preseason.csv", sep="/")) %>%
  mutate(SWrad.LO.SO = rowSums(.[,363:365]))



##############################################################################################################################################
##############################################################################################################################################



########################
# Mixed effects models #
########################



# Preseason-variable + Autumn temperature
#########################################

#Photosynthesis
fit_multi_Azani = lmer(leaf_off ~ Azani.LO.SO + Tnight9 + (1|year) + (1|species), data = PEP.df, 
                 na.action = "na.exclude", control = lmerControl(optimizer ="Nelder_Mead"))
summary(fit_multi_Azani)
plot(allEffects(fit_multi_Azani))
#Temperature
fit_multi_Tday = lmer(leaf_off ~ Tday.LO.SO + Tnight9 + (1|year) + (1|species), data = PEP.df, 
                       na.action = "na.exclude", control = lmerControl(optimizer ="Nelder_Mead"))
#Radiation
fit_multi_SWrad = lmer(leaf_off ~ SWrad.LO.SO + Tnight9 + (1|year) + (1|species), data = PEP.df, 
                      na.action = "na.exclude", control = lmerControl(optimizer ="Nelder_Mead"))
#Leaf-out
fit_multi_out = lmer(leaf_off ~ leaf_out + Tnight9 + (1|year) + (1|species), data = PEP.df, 
                       na.action = "na.exclude", control = lmerControl(optimizer ="Nelder_Mead"))


# Univariate
###########

#Autumn temperature
fit_year      = lmer(leaf_off ~ Tnight9 + (1|year) + (1|species), data = PEP.df, na.action = "na.exclude")
summary(fit_year)
#Photosynthesis
fit_year_out  = lmer(leaf_off ~ Azani.LO.SO + (1|year) + (1|species), data = PEP.df, na.action = "na.exclude")
summary(fit_year_out)


# Extract information for plotting
plotMultiAzani   = allEffects(fit_multi_Azani)
plotMultiTday    = allEffects(fit_multi_Tday)
plotMultiSWrad   = allEffects(fit_multi_SWrad)
plotMultiOut     = allEffects(fit_multi_out)
plotYear         = allEffects(fit_year)
plotYearOut      = allEffects(fit_year_out)


# Extract coefficients
df.coefficients = tibble(Coefficient = coef(summary(fit_multi_Azani))[ , "Estimate"][2:3],
                         variable = c("Pre-solstice","Tnight"),
                         class = "2.Azani") %>% 
  bind_rows(tibble(Coefficient = coef(summary(fit_multi_Tday))[ , "Estimate"][2:3],
                   variable = c("Pre-solstice","Tnight"),
                   class = "3.Tday")) %>% 
  bind_rows(tibble(Coefficient = coef(summary(fit_multi_SWrad))[ , "Estimate"][2:3],
                   variable = c("Pre-solstice","Tnight"),
                   class = "4.SWrad")) %>% 
  bind_rows(tibble(Coefficient = coef(summary(fit_multi_out))[ , "Estimate"][2:3],
                   variable = c("Pre-solstice","Tnight"),
                   class = "5.Out")) %>% 
  bind_rows(tibble(Coefficient = coef(summary(fit_year))[ , "Estimate"][2],
                   variable = c("Tnight"),
                   class = "1.Univariate")) %>% 
  bind_rows(tibble(Coefficient = coef(summary(fit_year_out))[ , "Estimate"][2],
                   variable = c("Pre-solstice"),
                   class = "1.Univariate")) %>%
  #Increase in expected delay over time after controlling for pre-solstice conditions
  mutate(SlopeIncrease = Coefficient / coef(summary(fit_year))[ , "Estimate"][2])


# Final table
df <- tibble(upper    = plotYear$Tnight9$upper[,1],
             lower    = plotYear$Tnight9$lower[,1],
             off      = plotYear$Tnight9$fit[,1],
             xval     = plotYear$Tnight9$x[,1],
             class    = "1.Univariate",
             variable = "Tnight") %>% 
  bind_rows(
    tibble(upper    = plotYearOut$Azani.LO.SO$upper[,1],
           lower    = plotYearOut$Azani.LO.SO$lower[,1],
           off      = plotYearOut$Azani.LO.SO$fit[,1],
           xval     = plotYearOut$Azani.LO.SO$x[,1],
           class    = "1.Univariate",
           variable = "Pre-solstice"))%>% 
  
  #Photosynthesis
  bind_rows(
    tibble(upper    = plotMultiAzani$Tnight9$upper[,1],
           lower    = plotMultiAzani$Tnight9$lower[,1],
           off      = plotMultiAzani$Tnight9$fit[,1],
           xval     = plotMultiAzani$Tnight9$x[,1],
           class    = "2.Azani",
           variable = "Tnight")
  )%>% 
  bind_rows(
    tibble(upper    = plotMultiAzani$Azani.LO.SO$upper[,1],
           lower    = plotMultiAzani$Azani.LO.SO$lower[,1],
           off      = plotMultiAzani$Azani.LO.SO$fit[,1],
           xval     = plotMultiAzani$Azani.LO.SO$x[,1],
           class    = "2.Azani",
           variable = "Pre-solstice")
  )%>% 
  
  #Temperature
  bind_rows(
    tibble(upper    = plotMultiTday$Tnight9$upper[,1],
           lower    = plotMultiTday$Tnight9$lower[,1],
           off      = plotMultiTday$Tnight9$fit[,1],
           xval     = plotMultiTday$Tnight9$x[,1],
           class    = "3.Tday",
           variable = "Tnight")
  )%>% 
  bind_rows(
    tibble(upper    = plotMultiTday$Tday.LO.SO$upper[,1],
           lower    = plotMultiTday$Tday.LO.SO$lower[,1],
           off      = plotMultiTday$Tday.LO.SO$fit[,1],
           xval     = plotMultiTday$Tday.LO.SO$x[,1],
           class    = "3.Tday",
           variable = "Pre-solstice")
  )%>% 
  
  #Radiation
  bind_rows(
    tibble(upper    = plotMultiSWrad$Tnight9$upper[,1],
           lower    = plotMultiSWrad$Tnight9$lower[,1],
           off      = plotMultiSWrad$Tnight9$fit[,1],
           xval     = plotMultiSWrad$Tnight9$x[,1],
           class    = "4.SWrad",
           variable = "Tnight")
  )%>% 
  bind_rows(
    tibble(upper    = plotMultiSWrad$SWrad.LO.SO$upper[,1],
           lower    = plotMultiSWrad$SWrad.LO.SO$lower[,1],
           off      = plotMultiSWrad$SWrad.LO.SO$fit[,1],
           xval     = plotMultiSWrad$SWrad.LO.SO$x[,1],
           class    = "4.SWrad",
           variable = "Pre-solstice")
  )%>% 
  
  #Leaf-out
  bind_rows(
    tibble(upper    = plotMultiOut$Tnight9$upper[,1],
           lower    = plotMultiOut$Tnight9$lower[,1],
           off      = plotMultiOut$Tnight9$fit[,1],
           xval     = plotMultiOut$Tnight9$x[,1],
           class    = "5.Out",
           variable = "Tnight")
  )%>% 
  bind_rows(
    tibble(upper    = plotMultiOut$leaf_out$upper[,1],
           lower    = plotMultiOut$leaf_out$lower[,1],
           off      = plotMultiOut$leaf_out$fit[,1],
           xval     = plotMultiOut$leaf_out$x[,1],
           class    = "5.Out",
           variable = "Pre-solstice")
  )


# get phenology anomalies
df = df %>%
  group_by(class, variable) %>%
  mutate(anomaly       = off - mean(off),
         anomaly.upper = upper - mean(off),
         anomaly.lower = lower - mean(off)) %>%
  ungroup()



##############################################################################################################################################
##############################################################################################################################################



########
# Plot #
########



MixedPlot = ggplot() + 
  geom_hline(yintercept = 0, linetype="dashed")+
  geom_ribbon(data = df, aes(x = xval, ymin = anomaly.lower, ymax = anomaly.upper, fill=class), 
              alpha = 0.3) +
  geom_line(data=df, aes(xval, anomaly, color=class)) +
  theme_classic() +
  geom_text(data=df.coefficients, aes(label=paste0(round(Coefficient,3)," days per unit"),
                                      x=Inf, y=Inf,hjust = "inward", vjust = "inward"))+
  coord_cartesian(ylim=c(-15,15))+
  labs(x = "", y = "Senescence (DOY)")+
  scale_color_manual(values = c("black","darkblue","darkblue","darkblue","darkblue"))+
  scale_fill_manual(values = c("black","darkblue","darkblue","darkblue","darkblue"))+
  facet_wrap(class~variable, scales="free_x", ncol=2) +
  plotTheme1


#save plots as .pdf
ggsave(MixedPlot, file="MixedPlotSpatial.pdf", path=output_path,
       width=5, height=12)



##############################################################################################################################################
##############################################################################################################################################



#####################
## Reproducibility ##	
#####################



## datetime
Sys.time()
#"2021-11-15 19:53:35 CET"

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
#  [1] effects_4.2-0     carData_3.0-4     lme4_1.1-27.1     Matrix_1.3-3      data.table_1.14.0 forcats_0.5.1    
#[7] stringr_1.4.0     dplyr_1.0.7       purrr_0.3.4       readr_1.4.0       tidyr_1.1.3       tibble_3.1.2     
#[13] ggplot2_3.3.4     tidyverse_1.3.1  

#loaded via a namespace (and not attached):
#  [1] httr_1.4.2        jsonlite_1.7.2    splines_4.1.0     modelr_0.1.8      assertthat_0.2.1  cellranger_1.1.0 
#[7] yaml_2.2.1        pillar_1.6.1      backports_1.2.1   lattice_0.20-44   glue_1.4.2        digest_0.6.27    
#[13] rvest_1.0.0       minqa_1.2.4       colorspace_2.0-1  htmltools_0.5.1.1 survey_4.1-1      plyr_1.8.6       
#[19] pkgconfig_2.0.3   broom_0.7.8       haven_2.4.1       scales_1.1.1      farver_2.1.0      generics_0.1.0   
#[25] ellipsis_0.3.2    withr_2.4.2       nnet_7.3-16       cli_2.5.0         survival_3.2-11   magrittr_2.0.1   
#[31] crayon_1.4.1      readxl_1.3.1      estimability_1.3  evaluate_0.14     fs_1.5.0          fansi_0.5.0      
#[37] nlme_3.1-152      MASS_7.3-54       xml2_1.3.2        tools_4.1.0       hms_1.1.0         mitools_2.4      
#[43] lifecycle_1.0.0   munsell_0.5.0     reprex_2.0.0      compiler_4.1.0    rlang_0.4.11      grid_4.1.0       
#[49] nloptr_1.2.2.2    rstudioapi_0.13   labeling_0.4.2    rmarkdown_2.9     boot_1.3-28       gtable_0.3.0     
#[55] DBI_1.1.1         R6_2.5.0          lubridate_1.7.10  knitr_1.33        utf8_1.2.1        insight_0.14.2   
#[61] stringi_1.6.2     Rcpp_1.0.6        vctrs_0.3.8       dbplyr_2.1.1      tidyselect_1.1.1  xfun_0.24      



##############################################################################################################################################
#############################################################THE END##########################################################################
##############################################################################################################################################













