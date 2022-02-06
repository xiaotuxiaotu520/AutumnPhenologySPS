


#############################################################################################################
############################################## R script for: ################################################
#############################################################################################################
##### Effect of climate warming on the timing of autumn leaf senescence reverses at the summer solstice #####
#############################################################################################################


#############################################################################################################
# Monthly and seasonal mixed effects models for the PEP725 data set #########################################
#############################################################################################################



#required packages
require(tidyverse)
require(data.table)
require(broom.mixed)
require(gmodels)
require(sjmisc)
require(lme4)
require(MuMIn)
require(car)



##############################################################################################################################################
##############################################################################################################################################



#####################
## Set directories ##
#####################

# set the working directory
setwd("/Users/consti/Desktop/PhD/Publication_material/17_Autumn_phenology_tier2/PEP_analysis/Analysis")

# paths
PEP_drivers_path    = "Analysis_input/PEP_drivers_final/Merged_file"
output_path         = "Analysis_output/Autumn/Data"



##############################################################################################################################################
##############################################################################################################################################



#################
## Import data ##
#################



#Phenology dataframe
####################

PEP.df <- fread(paste(PEP_drivers_path, "pep_drivers_data_preseason.csv", sep="/")) 



##############################################################################################################################################
##############################################################################################################################################



########################
## VARIABLE EXPLANATION:
########################

#General
########

#pep_id...site identifuer
#species...speces name
#year...observation year
#timeseries...unique site x species identifier
#site_year...unique site x year identifier
#ts_year...unique site x species x year identifier
#lat...site latitude (decimal degrees)
#lon...site longitude (decomal degrees)
#alt...site altitude (m a.s.l.)

#---------------------------------------------------

#Phenology
##########

#leaf_out...leaf-out date (DOY)
#leaf_out_mean...mean timeseries leaf-out date (DOY)
#leaf_off...senescence date (DOY)
#leaf_off_mean...mean timeseries senescence date (DOY)

#---------------------------------------------------

#Day-of-year maximum temperature and radiation
##############################################

#HottestDOY...DOY with maximum annual temperature
#MaxRadDOY...DOY with maximum irradiance

#---------------------------------------------------

#Seasonal drivers
#################

#SUMS (flexible start = leaf-out):
#---------------------------------
#Apm...Daily net photosynthesis p-model
#Azani...Daily net photosynthesis Zani model
#GSI...photoperiod-influenced growing-season index
#GSIrad...radiation-influenced growing-season index
#GDDday...daytime degree days
#GDDnight...nighttime degree days
#SWrad...Radiation sum

#MEANS (fixed start = March equinox):
#------------------------------------
#Tday...mean daytime temperature
#Tnight...mean nighttime temperature
#Moist...mean soil moisture (10-40cm)
#Prcp...precipitation sum

#-----------------

#PERIODS

#seasonal
#--------
#LO.SOm30...leaf-out to 30 days before summer solstice
#LO.SO...leaf-out to summer solstice
#LO.SOp30...leaf-out to 30 days after solstice
#LO.SOp60...leaf-out to 60 days after solstice
#LO.SE...leaf-out to mean senescence
#SOm30.SE...30 days before solstice to mean senescence
#SO.SE...solstice to mean senescence
#SOp30.SE...30 days after solstice to mean senescence
#SOp60.SE...60 days after solstice to mean senescence

#around summer solstice
#----------------------
#solstice1...40 to 10 days before solstice
#solstice2...30 days before solstice
#solstice3...20 days before until 10 days after solstice
#solstice4...10 days before until 20 days after solstice
#solstice5...30 days after solstice
#solstice6...10 to 40 days after solstice

#---------------------------------------------------           

#Monthly drivers
################

#Tnight1-12...mean of nighttime temperatures in January (1) to December (12)
#Tday1-12...mean of daytime temperatures in January (1) to December (12)
#GDDnight1-12...sum of nighttime degree-days in January (1) to December (12)
#GDDday1-12...sum of daytime degree-days in January (1) to December (12)
#SWrad1-12...mean of daily short-wave radiation in January (1) to December (12)
#Moist1-12...mean of daily soil moisture (10-40cm) in January (1) to December (12)
#Prcp1-12...sum of daily precipitation in January (1) to December (12)
#Apm1-12...sum of daily gross photosynthesis in January (1) to December (12)
#Azani1-12...sum of daily net photosynthesis in January (1) to December (12)
#GSI...day length-influenced growing-season index
#GSIrad...radiation-influenced growing-season index

#---------------------------------------------------           

#Preseason temperatures
#######################

#Tmin.PS15-90...mean daily minimum temperature 15 to 90 days prior to mean timeseries senescence date
#tmean.PS15-90...mean daily mean temperature 15 to 90 days prior to mean timeseries senescence date
#tmax.PS15-90...mean daily maximum temperature 15 to 90 days prior to mean timeseries senescence date

#Tday...time series-specific best preseason (R2-based) for daytime temperatures
#tnight...time series-specific best preseason (R2-based) for nighttime temperatures



##############################################################################################################################################
##############################################################################################################################################



######################################
## Check variance inflation factors ##
######################################


# No multicollinearity among explanatory variables (all VIFs <2)

#Full model:
VIFfull = vif(lmer(scale(leaf_off) ~ 
           scale(Azani.LO.SO) + scale(Azani.SO.SE) + scale(Tnight) + scale(CO2)+ scale(Prcp.LO.SO)+ scale(Prcp.SO.SE)+
           (1|timeseries) + (1|species), data=PEP.df, control = lmerControl(optimizer ="Nelder_Mead")))

#Monthly models:

#Photosynthesis
VIFphot = vif(lmer(scale(leaf_off) ~ 
           scale(Azani3) + scale(Azani4) + scale(Azani5) + scale(Azani6) + 
           scale(Azani7) + scale(Azani8) + scale(Azani9) + scale(Azani10) +
           (1|timeseries) + (1|species), data=PEP.df, 
         control = lmerControl(optimizer ="Nelder_Mead")))

#Temperature
VIFtemp = vif(lmer(scale(leaf_off) ~ 
           scale(Tnight1) + scale(Tnight2) + scale(Tnight3) + scale(Tnight4) + scale(Tnight5)+
           scale(Tnight6) + scale(Tnight7) + scale(Tnight8) + scale(Tnight9) + scale(Tnight10)+
           (1|timeseries) + (1|species), data=PEP.df, 
         control = lmerControl(optimizer ="Nelder_Mead")))

#look at maximum VIFs per model
max(VIFfull) #1.6
max(VIFphot) #1.4
max(VIFtemp) #1.4



##############################################################################################################################################
##############################################################################################################################################



#################
## Full models ##
#################



#Define variables
variables = c('Apm', 'Azani', 'GSI', 'GSIrad', 'GDDday', 'GDDnight', 'Tnight', 'Tday', 'SWrad')


#create List object to store results
DataList = replicate(length(variables), data.frame())
names(DataList) = variables


##############################################################################################################################################


#Loop through variables
#######################

for (i in 1:length(variables)){

  #set equations
  ##############
  
  #define variable names
  covariates = paste0(variables[i], c('.LO.SO','.SO.SE')) 

  
  #full models
  equation.scaled = as.formula(paste("scale(leaf_off) ~ ", paste0('scale(',covariates[1], ') + scale(', covariates[2], ')', 
                                                     collapse="+"), 
                               '+ scale(Prcp.LO.SO) + scale(Prcp.SO.SE) + scale(CO2) + scale(Tnight) + (1|timeseries) + (1|species)', collapse=""))
  
  equation.species = as.formula(paste("scale(leaf_off) ~ ", paste0('scale(',covariates[1], ') + scale(', covariates[2], ')', 
                                                                   collapse="+"), 
                                      '+ scale(Prcp.LO.SO) + scale(Prcp.SO.SE) + scale(CO2) + scale(Tnight) + (1|timeseries)', collapse=""))
  
  #Prediction equations
  equation.full = as.formula(paste("leaf_off ~ ", paste0(covariates[1], '+', covariates[2], collapse="+"), 
                              '+ Prcp.LO.SO + Prcp.SO.SE + CO2 + Tnight + (1|timeseries) + (1|species)', collapse=""))
  
  equation.preSolstice = as.formula(paste("leaf_off ~ ", paste0(covariates[1], collapse="+"), 
                              '+ Prcp.LO.SO + CO2 + (1|timeseries) + (1|species)', collapse=""))
  
  equation.postSolstice = as.formula(paste("leaf_off ~ ", paste0(covariates[2], collapse="+"), 
                                          '+ Prcp.SO.SE + CO2 + Tnight + (1|timeseries) + (1|species)', collapse=""))
  
  
  ##############################################################################################################################################
  
  
  #########################
  #Predict senescence dates
  #########################

  
  PEP.df = PEP.df %>%
    #run predictions
    mutate(Prediction.Full          = predict(lmer(equation.full,         data=PEP.df, control = lmerControl(optimizer ="Nelder_Mead"))),
           Prediction.PreSolstice   = predict(lmer(equation.preSolstice,  data=PEP.df, control = lmerControl(optimizer ="Nelder_Mead"))),
           Prediction.PostSolstice  = predict(lmer(equation.postSolstice, data=PEP.df, control = lmerControl(optimizer ="Nelder_Mead"))) )%>%
    #rename columns
    plyr::rename(replace = c(Prediction.Full          = paste0('Prediction.Full.',         variables[i]),
                             Prediction.PreSolstice   = paste0('Prediction.PreSolstice.',  variables[i]),
                             Prediction.PostSolstice  = paste0('Prediction.PostSolstice.', variables[i]) ))

  
  ##############################################################################################################################################
  
  
  ######################
  # Mixed effects models
  ######################
  
  
  # All species
  #############
  
  ModelResults.df = PEP.df %>%
    do({
      
      #run model
      ##########
      
      model = lmer(equation.scaled,  data=., control = lmerControl(optimizer ="Nelder_Mead"))
      
      #create combined dataframe
      ##########################
      
      data.frame(tidy(model, effects="fixed") %>% 
                   mutate(species  = 'Aall') )  
      
      }) 

  #----------------------------------------------------
  
  # Species-specific
  ##################
  
  ModelResultsSpecies.df = PEP.df %>%
    group_by(species)%>%
    do({
      
      #run model
      ##########
      
      model = lmer(equation.species,  data=., control = lmerControl(optimizer ="Nelder_Mead"))
      
      #create combined dataframe
      ##########################
      
      data.frame(tidy(model, effects="fixed")) 
      
      }) %>% ungroup()
  
  
  #rbind all species and species-specific results
  Results.df = bind_rows(ModelResults.df, ModelResultsSpecies.df) %>%
    #add model and variable name and delete "scale()" from term column
    mutate(equation = 'full model',
           variable = variables[i],
           term     = gsub("scale","",term),
           term     = gsub("\\(|\\)","",term) ) %>%
    #delete intercept
    filter(!term %in% c("Intercept"))
  
  
  ##############################################################################################################################################
  
  
  #store dataframe in variable list
  DataList[[i]] = Results.df 
  
  #count
  print(paste0('...',i,' out of ',length(variables), ' (',variables[i],') done'))
}

#bind rows
FullModelAnalysis.df = bind_rows(DataList) 



##############################################################################################################################################
##############################################################################################################################################



##########################
## Monthly correlations ##
##########################



#Get sums for January to May photosynthesis parameters
PEP.monthly.df = PEP.df %>%
  mutate(GDDday3     = rowSums(select(.,c("GDDday1","GDDday2","GDDday3"))),
         GDDnight3   = rowSums(select(.,c("GDDnight1","GDDnight2","GDDnight3"))),
         SWrad3      = rowSums(select(.,c("SWrad1","SWrad2","SWrad3"))),
         Apm3        = rowSums(select(.,c("Apm1","Apm2","Apm3"))),
         ApmJmaxA3   = rowSums(select(.,c("ApmJmaxA1","ApmJmaxA2","ApmJmaxA3"))),
         ApmJmaxB3   = rowSums(select(.,c("ApmJmaxB1","ApmJmaxB2","ApmJmaxB3"))),
         Azani3      = rowSums(select(.,c("Azani1","Azani2","Azani3"))),
         AzaniJmaxA3 = rowSums(select(.,c("AzaniJmaxA1","AzaniJmaxA2","AzaniJmaxA3"))),
         AzaniJmaxB3 = rowSums(select(.,c("AzaniJmaxB1","AzaniJmaxB2","AzaniJmaxB3"))),
         GSI3        = rowSums(select(.,c("GSI1","GSI2","GSI3"))),
         GSIrad3     = rowSums(select(.,c("GSIrad1","GSIrad2","GSIrad3"))) )

#create List object to store results
DataList = replicate(length(variables), data.frame())
names(DataList) = variables


##############################################################################################################################################


#Loop through covariate groups
##############################

for (i in 1:length(variables)){

  #create explanatory variables
  #############################
  
  covariates.monthly  = paste0(variables[i], c(1:10))
  
  #---------------------------------------------------------
  
  #set equations
  ##############
  
  
  # Type 1: Temporal
  ##################
  
  if(variables[i] %in% c('Apm', 'ApmJmaxA', 'ApmJmaxB', 'Azani', 'AzaniJmaxA', 'AzaniJmaxB', 
                         'GSI', 'GSIrad',
                         'GDDday', 'GDDnight', 'SWrad')) {  
    equation1 = as.formula(paste("leaf_off ~ ", paste(covariates.monthly[3:10], collapse="+"), 
                                 '+ (1|timeseries) + (1|species)',
                                 collapse=""))
    equation1.species = as.formula(paste("leaf_off ~ ", paste(covariates.monthly[3:10], collapse="+"), 
                                         '+ (1|timeseries)',
                                         collapse="")) 
  } else {
    equation1 = as.formula(paste("leaf_off ~ ", paste(covariates.monthly, collapse="+"), 
                                 '+ (1|timeseries) + (1|species)',
                                 collapse=""))
    equation1.species = as.formula(paste("leaf_off ~ ", paste(covariates.monthly, collapse="+"), 
                                         '+ (1|timeseries)',
                                         collapse="")) }
  
  
  # Type 2: Temporal scaled
  #########################
  
  if(variables[i] %in% c('Apm', 'ApmJmaxA', 'ApmJmaxB', 'Azani', 'AzaniJmaxA', 'AzaniJmaxB', 
                         'GSI', 'GSIrad', 
                         'GDDday', 'GDDnight', 'SWrad')) {  
    equation2.scaled = as.formula(paste("scale(leaf_off) ~ ", paste('scale(', covariates.monthly[3:10], ')', collapse="+"), 
                                 '+ (1|timeseries) + (1|species)',
                                 collapse=""))
    equation2.species.scaled = as.formula(paste("scale(leaf_off) ~ ", paste('scale(', covariates.monthly[3:10], ')', collapse="+"), 
                                         '+ (1|timeseries)',
                                         collapse="")) 
  } else {
    equation2.scaled = as.formula(paste("scale(leaf_off) ~ ", paste('scale(', covariates.monthly, ')', collapse="+"), 
                                 '+ (1|timeseries) + (1|species)',
                                 collapse=""))
    equation2.species.scaled = as.formula(paste("scale(leaf_off) ~ ", paste('scale(', covariates.monthly, ')', collapse="+"), 
                                         '+ (1|timeseries)',
                                         collapse="")) }
  
    
  #---------------------------------------------------------
    
  #####################
  #mixed effects models
  #####################
    
  ModelResults.df = PEP.monthly.df %>%
      do({
        
        #run models
        ###########
        
        #Equation 1
        modelEq1 = lmer(equation1, data=.,
                        control = lmerControl(optimizer ="Nelder_Mead"))
        
        #Equation 2 (scaled)
        modelEq2 = lmer(equation2.scaled, data=.,
                        control = lmerControl(optimizer ="Nelder_Mead"))
    
        
        #create combined dataframe
        ##########################
        
        data.frame(rbind(
          
          #Equation 1
          tidy(modelEq1, effects="fixed") %>% 
            mutate(equation = 'monthly'),
          
          #Equation 2 (scaled)
          tidy(modelEq2, effects="fixed") %>% 
            mutate(equation = 'monthly.scaled') ) ) 
        
        }) %>%
    
    #add species name 
    mutate(species  = 'Aall')
    
    #----------------------------------------------------
    
    ######################################
    #species-specific mixed effects models 
    ######################################
    
    ModelResultsSpecies.df = PEP.df %>%
      
      #group by species
      group_by(species)%>%
      
      do({
        
        #run models
        ###########
        
        #Equation 1
        modelEq1 = lmer(equation1.species, data=.,
                        control = lmerControl(optimizer ="Nelder_Mead"))
        
        #Equation 2 (scaled)
        modelEq2 = lmer(equation2.species.scaled, data=.,
                        control = lmerControl(optimizer ="Nelder_Mead"))
      
        
        #create combined dataframe
        ##########################
        
        data.frame(rbind(
          
          #Equation 1
          tidy(modelEq1, effects="fixed") %>% 
            add_column(equation = 'monthly'),
          
          #Equation 2 (scaled)
          tidy(modelEq2, effects="fixed") %>% 
            add_column(equation = 'monthly.scaled') ) )
      
        }) %>% ungroup()

  #rbind all species and species-specific results
  Results.df = bind_rows(ModelResults.df, ModelResultsSpecies.df) %>%
    #delete intercept
    filter(!term %in% c("(Intercept)")) %>% 
    #add variable name and keep only numbers in month column 
    mutate(variable = variables[i],
           term = readr::parse_number(term))
  
  #---------------------------------------------------------
  
  #store dataframe in variable list
  DataList[[i]] = Results.df 
  
  #count
  print(paste0('...',i,' out of ',length(variables), ' (',variables[i],') done'))
}

#bind rows
MonthlyAnalysis.df = bind_rows(DataList) 



##############################################################################################################################################
##############################################################################################################################################



######################
## Seasonal drivers ##
######################


#Covariates
###########

#Variable length (leaf-out influenced):
#--------------------------------------
#Apm...Daily net photosynthesis (p-model)
#Azani...Daily net photosynthesis (Zani model)
#GSI...photoperiod-influenced growing-season index
#GSIrad...radiation-influenced growing-season index
#GDDday...daytime degree days
#GDDnight...nighttime degree days
#SWrad...Radiation sum

#Fixed length:
#-------------
#Tday...mean daytime temperature
#Tnight...mean daytime temperature


#-------------------------------------------------------------


## Define covariate groups
seasons    = c('LO.SOm30', 'LO.SO', 'LO.SOp30', 'LO.SOp60', 'LO.SE', 'SOm30.SE', 'SO.SE', 'SOp30.SE', 'SOp60.SE')
solstice   = c('solstice1', 'solstice2', 'solstice3', 'solstice4', 'solstice5', 'solstice6')

covariates1 = paste(rep(variables, each=length(seasons)),  seasons,  sep = '.')
covariates2 = paste(rep(variables, each=length(solstice)), solstice, sep = '.')
covariates  = c(covariates1,covariates2)

#Check if all variables are in dataframe
table(names(PEP.df) %in% covariates)[2]/length(covariates)==1

#-------------------------------------------------------------

## Create List object to store results
DataList = replicate(length(covariates), data.frame())
names(DataList) = covariates
i=1


##############################################################################################################################################


#Loop through covariates
########################

for (covariate in covariates){

  #---------------------------------------------------------
  
  #set equations
  ##############
  
  #univariate scaled
  equation1 = as.formula(paste("scale(leaf_off) ~ ", paste('scale(', covariate, ')', collapse="+"), 
                               '+ (1|timeseries) + (1|species)', collapse=""))
  equation1.species = as.formula(paste("scale(leaf_off) ~ ", paste('scale(', covariate, ')', collapse="+"), 
                               '+ (1|timeseries)', collapse=""))
  
  #univariate 
  equation2 = as.formula(paste("leaf_off ~ ", paste(covariate, collapse="+"), 
                               '+ (1|timeseries) + (1|species)', collapse=""))
  equation2.species = as.formula(paste("leaf_off ~ ", paste(covariate, collapse="+"), 
                               '+ (1|timeseries)', collapse=""))
  
  #autumn-temperature controlled scaled
  equation3 = as.formula(paste("scale(leaf_off) ~ ", paste('scale(', covariate, ')', collapse="+"), 
                                '+ scale(Tnight) + (1|timeseries) + (1|species)', collapse=""))
  equation3.species = as.formula(paste("scale(leaf_off) ~ ", paste('scale(', covariate, ')', collapse="+"), 
                                '+ scale(Tnight) + (1|timeseries)', collapse=""))
    
  #autumn-temperature controlled 
  equation4 = as.formula(paste("leaf_off ~ ", paste(covariate, collapse="+"), 
                                '+ Tnight + (1|timeseries) + (1|species)', collapse=""))
  equation4.species = as.formula(paste("leaf_off ~ ", paste(covariate, collapse="+"), 
                                '+ Tnight + (1|timeseries)', collapse=""))

   
  ##############################################################################################################################################

  
  ########################
  #Run mixed effect models
  ########################
  
  
  #All species
  ############
  
  ModelResults.df = PEP.df %>% 

    do({
      
      #run models
      ###########
      
      model1 = lmer(equation1, data=.,
                    control = lmerControl(optimizer ="Nelder_Mead"))
    
      model2 = lmer(equation2, data=.,
                    control = lmerControl(optimizer ="Nelder_Mead"))
    
      model3 = lmer(equation3, data=.,
                    control = lmerControl(optimizer ="Nelder_Mead"))
    
      model4 = lmer(equation4, data=.,
                    control = lmerControl(optimizer ="Nelder_Mead"))
    
      #create combined dataframe
      ##########################
      
      data.frame(rbind(
        
        #Equation 1
        tidy(model1, effects="fixed") %>% 
          add_column(equation = paste0(ifelse(str_contains(covariate, c("solstice"), logic="or"),"Solstice","Seasonal"),
                                       '.scaled') ) %>% 
          filter(term %in% paste0('scale(',covariate,')')), #delete intercept
          
        #Equation 2
        tidy(model2, effects="fixed") %>% 
          add_column(equation = ifelse(str_contains(covariate, c("solstice"), logic="or"),"Solstice","Seasonal") ) %>% 
          filter(term %in% covariate),
        
        #Equation 3
        tidy(model3, effects="fixed") %>% 
          add_column(equation = paste0(ifelse(str_contains(covariate, c("solstice"), logic="or"),"Solstice","Seasonal"),
                                       '.tempCon.scaled') ) %>% 
          filter(term %in% paste0('scale(',covariate,')')),
        
        #Equation 4
        tidy(model4, effects="fixed") %>% 
          add_column(equation = paste0(ifelse(str_contains(covariate, c("solstice"), logic="or"),"Solstice","Seasonal"),
                                       '.tempCon') ) %>% 
          filter(term %in% covariate) ))
      
      }) %>%
    
    #add variable and species name
    mutate(term = covariate,
           variable = sub("\\..*", "", covariate),
           species = 'Aall')
        
  #-----------------------------------------------------------------------------------------------------------------------
  
  #Species-specific
  #################
  
  ModelResultsSpecies.df = PEP.df %>%
    
    #group by species
    group_by(species)%>%
    
    do({
      
      #run models
      ###########
      
      model1 = lmer(equation1.species, data=.,
                    control = lmerControl(optimizer ="Nelder_Mead"))
      
      model2 = lmer(equation2.species, data=.,
                    control = lmerControl(optimizer ="Nelder_Mead"))
      
      model3 = lmer(equation3.species, data=.,
                    control = lmerControl(optimizer ="Nelder_Mead"))
      
      model4 = lmer(equation4.species, data=.,
                    control = lmerControl(optimizer ="Nelder_Mead"))
      
      #create combined dataframe
      ##########################
      
      data.frame(rbind(
        
        #Equation 1
        tidy(model1, effects="fixed") %>% 
          mutate(equation = paste0(ifelse(str_contains(covariate, c("solstice"), logic="or"),"Solstice","Seasonal"),
                                       '.scaled') ) %>% 
          filter(term %in% paste0('scale(',covariate,')')),
        
        #Equation 2
        tidy(model2, effects="fixed") %>% 
          mutate(equation = ifelse(str_contains(covariate, c("solstice"), logic="or"),"Solstice","Seasonal") ) %>% 
          filter(term %in% covariate),
        
        #Equation 3
        tidy(model3, effects="fixed") %>% 
          mutate(equation = paste0(ifelse(str_contains(covariate, c("solstice"), logic="or"),"Solstice","Seasonal"),
                                       '.tempCon.scaled') ) %>% 
          filter(term %in% paste0('scale(',covariate,')')),
        
        #Equation 4
        tidy(model4, effects="fixed") %>% 
          mutate(equation = paste0(ifelse(str_contains(covariate, c("solstice"), logic="or"),"Solstice","Seasonal"),
                                       '.tempCon') ) %>% 
          filter(term %in% covariate)
          ))
    }) %>%
    
    #add variable and species name
    mutate(term = covariate,
           variable = sub("\\..*", "", covariate) ) %>%
    ungroup()
  
      
  #rbind all species and species-specific results
  Results.df = bind_rows(ModelResults.df, ModelResultsSpecies.df)
  
  
  ##############################################################################################################################################
  
  
  #store dataframe in variable list
  DataList[[i]] = Results.df
  
  print(paste0('..... ',i, ' out of ', length(covariates), ' done'))
  i=i+1
}

#bind tables
SeasonalAnalysis.df = bind_rows(DataList) 



##############################################################################################################################################
##############################################################################################################################################



##########
## Safe ##
##########



#bind full model, monthly and seasonal analyses
Analysis.df = rbind(FullModelAnalysis.df,MonthlyAnalysis.df,SeasonalAnalysis.df)

#Safe tables
write.csv(Analysis.df, paste(output_path, "Mixed_effect_data.csv", sep="/"))
write.csv(PEP.df %>% dplyr::select(-V1), paste(PEP_drivers_path, "pep_drivers_data_preseason_predictions.csv", sep="/"))



##############################################################################################################################################
##############################################################################################################################################



#####################
## Reproducibility ##	
#####################


## date time
Sys.time()
#"2021-07-09 08:24:51 CEST"

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
#  [1] broom.mixed_0.2.6 car_3.0-11        carData_3.0-4     MuMIn_1.43.17     lme4_1.1-27.1     Matrix_1.3-3     
#[7] sjmisc_2.8.7      gmodels_2.18.1    broom_0.7.8       data.table_1.14.0 forcats_0.5.1     stringr_1.4.0    
#[13] dplyr_1.0.7       purrr_0.3.4       readr_1.4.0       tidyr_1.1.3       tibble_3.1.2      ggplot2_3.3.4    
#[19] tidyverse_1.3.1  

#loaded via a namespace (and not attached):
#  [1] nlme_3.1-152      fs_1.5.0          lubridate_1.7.10  insight_0.14.2    httr_1.4.2        tools_4.1.0      
#[7] TMB_1.7.20        backports_1.2.1   utf8_1.2.1        R6_2.5.0          sjlabelled_1.1.8  DBI_1.1.1        
#[13] colorspace_2.0-1  withr_2.4.2       tidyselect_1.1.1  curl_4.3.2        compiler_4.1.0    cli_2.5.0        
#[19] rvest_1.0.0       xml2_1.3.2        scales_1.1.1      digest_0.6.27     foreign_0.8-81    minqa_1.2.4      
#[25] rmarkdown_2.9     rio_0.5.27        pkgconfig_2.0.3   htmltools_0.5.1.1 dbplyr_2.1.1      rlang_0.4.11     
#[31] readxl_1.3.1      rstudioapi_0.13   generics_0.1.0    jsonlite_1.7.2    gtools_3.9.2      zip_2.2.0        
#[37] magrittr_2.0.1    Rcpp_1.0.6        munsell_0.5.0     fansi_0.5.0       abind_1.4-5       lifecycle_1.0.0  
#[43] stringi_1.6.2     yaml_2.2.1        MASS_7.3-54       plyr_1.8.6        grid_4.1.0        gdata_2.18.0     
#[49] crayon_1.4.1      lattice_0.20-44   haven_2.4.1       splines_4.1.0     hms_1.1.0         knitr_1.33       
#[55] pillar_1.6.1      boot_1.3-28       reshape2_1.4.4    stats4_4.1.0      reprex_2.0.0      glue_1.4.2       
#[61] evaluate_0.14     modelr_0.1.8      vctrs_0.3.8       nloptr_1.2.2.2    cellranger_1.1.0  gtable_0.3.0     
#[67] assertthat_0.2.1  xfun_0.24         openxlsx_4.2.4    coda_0.19-4       ellipsis_0.3.2   



##############################################################################################################################################
#############################################################THE END##########################################################################
##############################################################################################################################################


