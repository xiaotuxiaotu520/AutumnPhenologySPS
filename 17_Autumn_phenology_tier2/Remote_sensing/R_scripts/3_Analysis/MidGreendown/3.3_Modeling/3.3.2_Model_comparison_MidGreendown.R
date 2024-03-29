


#############################################################################################################
############################################## R script for: ################################################
#############################################################################################################
##### Effect of climate warming on the timing of autumn leaf senescence reverses at the summer solstice #####
#############################################################################################################


#############################################################################################################
# Univariate pre-/post-solstice models (EOS50) ##############################################################
#############################################################################################################



#required packages
require(tidyverse)
require(data.table)
require(broom)



##############################################################################################################################################
##############################################################################################################################################



#####################
## Set directories ##
#####################



# set the working directory
setwd("/Users/consti/Desktop/PhD/Publication_material/17_Autumn_phenology_tier2/Remote_sensing/Analysis")

# paths
drivers_path    = "Analysis_input/Drivers_final/Merged_file"
output_path     = "Analysis_output/Data"



##############################################################################################################################################
##############################################################################################################################################



#################
## Import data ##
#################



#Phenology dataframe
####################

Pheno.df <- fread(paste(drivers_path, "Remote_sensing_drivers_data_preseason.csv", sep="/")) 



##############################################################################################################################################
##############################################################################################################################################



#######################################
## Timeseries-level model assessment ##
#######################################



#variable vector
variables=c("LAIstart","GPPstart","Tday","SWrad","Moist","Greenup_DOY","GDDday","GDDnight")

#create List object to store results
DataList = replicate(length(variables), data.frame())
names(DataList) = variables


#Loop through variables
#######################

for (i in 1:length(variables)){
  
  #define variable names
  if (variables[i] == "Greenup_DOY") {
    covariates = c('Greenup_DOY','CO2')
  } else {covariates = paste0(variables[i], c('.LO.SO','.SO.SE')) }
  
  
  #set equations
  ##############
  
  equation.full = as.formula(paste("MidGreendown_DOY ~ ", paste0(covariates[1], '+', covariates[2])))
  equation.pre  = as.formula(paste("MidGreendown_DOY ~ ", paste0(covariates[1])))
  equation.post = as.formula(paste("MidGreendown_DOY ~ ", paste0(covariates[2])))
  
  #---------------------------------------------------------
  
  ###############
  #Get model info
  ###############
  
  ModelResults.df = Pheno.df %>%
    group_by(LC_Type, geometry)%>%
    do({
      
      #run models
      ###########
      
      modelFull = lm(equation.full, data=.)
      modelPre  = lm(equation.pre,  data=.)
      modelPost = lm(equation.post, data=.)
      
      
      #create combined dataframe
      ##########################
      
      data.frame(rbind(
        
        #Equation full
        glance(modelFull) %>% 
          mutate(model = 'full'),
        
        #Equation pre-solstice
        glance(modelPre) %>% 
          mutate(model='pre'),
        
        #Equation post-solstice
        glance(modelPost) %>% 
          mutate(model='post') ) )
    })%>%
    mutate(variable = variables[i]) %>%
    ungroup()
  
  #---------------------------------------------------------
  
  #store dataframe in variable list
  DataList[[i]] = ModelResults.df
  
  #count
  print(paste0('...',i,' out of ',length(variables), ' (',variables[i],') done'))
}

#bind rows
Analysis.df = bind_rows(DataList) 



##############################################################################################################################################
##############################################################################################################################################



##########
## Safe ##
##########



write.csv(Analysis.df, paste(output_path, "Model_R2_data.csv", sep="/"))



##############################################################################################################################################
##############################################################################################################################################



#####################
## Reproducibility ##	
#####################


## datetime
Sys.time()
#"2021-12-08 21:08:38 CET"

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
#  [1] broom_0.7.8       data.table_1.14.0 forcats_0.5.1     stringr_1.4.0     dplyr_1.0.7       purrr_0.3.4      
#[7] readr_1.4.0       tidyr_1.1.3       tibble_3.1.2      ggplot2_3.3.4     tidyverse_1.3.1  

#loaded via a namespace (and not attached):
#  [1] tidyselect_1.1.1  xfun_0.24         haven_2.4.1       colorspace_2.0-1  vctrs_0.3.8       generics_0.1.0   
#[7] htmltools_0.5.1.1 yaml_2.2.1        utf8_1.2.1        rlang_0.4.11      pillar_1.6.1      glue_1.4.2       
#[13] withr_2.4.2       DBI_1.1.1         dbplyr_2.1.1      modelr_0.1.8      readxl_1.3.1      plyr_1.8.6       
#[19] lifecycle_1.0.0   munsell_0.5.0     gtable_0.3.0      cellranger_1.1.0  rvest_1.0.0       evaluate_0.14    
#[25] knitr_1.33        fansi_0.5.0       Rcpp_1.0.6        scales_1.1.1      backports_1.2.1   jsonlite_1.7.2   
#[31] fs_1.5.0          hms_1.1.0         digest_0.6.27     stringi_1.6.2     grid_4.1.0        cli_2.5.0        
#[37] tools_4.1.0       magrittr_2.0.1    crayon_1.4.1      pkgconfig_2.0.3   ellipsis_0.3.2    xml2_1.3.2       
#[43] reprex_2.0.0      lubridate_1.7.10  assertthat_0.2.1  rmarkdown_2.9     httr_1.4.2        rstudioapi_0.13  
#[49] R6_2.5.0          compiler_4.1.0   



##############################################################################################################################################
#############################################################THE END##########################################################################
##############################################################################################################################################


