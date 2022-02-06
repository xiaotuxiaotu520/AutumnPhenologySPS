


#############################################################################################################
############################################## R script for: ################################################
#############################################################################################################
##### Effect of climate warming on the timing of autumn leaf senescence reverses at the summer solstice #####
#############################################################################################################


#############################################################################################################
# Extended Data Fig. 2 and Supplementary Figs. S2 and S4 ####################################################
#############################################################################################################



#required packages
require(tidyverse)
require(data.table)
require(ggplot2)
require(patchwork)
require(gmodels)
require(wesanderson)
require(pracma)
require(lme4)
require(effects) #plot effects



##############################################################################################################################################
##############################################################################################################################################



#####################
## Set directories ##
#####################



# set the working directory
setwd("/Users/consti/Desktop/PhD/Publication_material/17_Autumn_phenology_tier2/Remote_sensing/Analysis")


# Paths

#input
Drivers_path      = "Analysis_input/Drivers_final/Merged_file"
Analysis_path     = "Analysis_output/Data"
photo_path        = "Analysis_input/Drivers" #Photoperiod file

#output
output_path       = "Analysis_output/Maps"



##############################################################################################################################################
##############################################################################################################################################



#################
## Import data ##
#################



#Spatial (pixel-level) models
#############################

Analysis.df = fread(paste(Analysis_path, "Spatial_effect_data.csv", sep="/")) %>%
  filter(!is.na(estimate))

#geometry: unique pixel identifier
#Lat: Latitude
#Lon: Longitude
#LC_type: All, DecB, DecN, EvgN, Mixed (Landcover type)
#term: monthly coefficients (1-10) and seasonal coefficients
#estimate: slopes or standardized coefficients of mixed effects models
#std.error: std.error of coefficients
#statistic: 
#equation: full model 1/2, monthly/seasonal/solstice, scaled/unscaled, tempCon (Tday controlled)
#variable: climate variable (LAI, GPP, Apm, Azani, Tday, Tnight, SWrad)


# get full model correlations
#############################

FullModel.df = Analysis.df %>%
  filter(equation == "full model1")

ReducedModel.df = Analysis.df %>%
  filter(equation == "full model2")


#-------------------------------------------------------------------------------------------------------


# get monthly correlations
##########################

#Summarize all pixels
MonthlyAnalysisAll.df = Analysis.df %>%
  filter(equation == "monthly") %>%
  group_by(term, variable) %>%
  summarise(mean  = mean(estimate), 
            lowCI = t.test(estimate)$conf.int[1],
            hiCI  = t.test(estimate)$conf.int[2]) %>%
  mutate(LC_Type = "All") %>%
  ungroup()
  
#Summarize by vegetation type
MonthlyAnalysisLCtype.df = Analysis.df %>%
  filter(equation == "monthly") %>%
  group_by(term, variable, LC_Type) %>%
  summarise(mean  = mean(estimate), 
            lowCI = t.test(estimate)$conf.int[1],
            hiCI  = t.test(estimate)$conf.int[2]) %>%
  ungroup()
  
#Rbind
MonthlyAnalysis.df = rbind(MonthlyAnalysisAll.df, MonthlyAnalysisLCtype.df) %>%
  #Add variable x equation identifier
  mutate(variable.type = paste(variable, LC_Type, sep='.'),
         term = as.numeric(term),
         LC_Type = factor(LC_Type, levels = c("All","Mixed", "DecB", "EvgN","DecN")))


#-------------------------------------------------------------------------------------------------------


# get seasonal correlations
###########################

SeasonalModel.df = Analysis.df %>%
  filter(equation == "Solstice.scaled") %>%
  #Summarize all pixels
  group_by(term, variable) %>%
  summarise(mean  = mean(estimate), 
            lowCI = t.test(estimate)$conf.int[1],
            hiCI  = t.test(estimate)$conf.int[2]) %>%
  ungroup() %>%
  #Add variable class identifier
  mutate(variable.class = gsub("^.*?\\.","", term) )


##############################################################################################################################################


#Phenology data
###############

Pheno.df = fread(paste(Drivers_path, "Remote_sensing_drivers_data_preseason.csv", sep="/"))



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
  strip.text        = element_text(colour = 'black'),
  plot.title        = element_text(hjust = 0.5))



##############################################################################################################################################
##############################################################################################################################################



######################
# Photoperiod figure #
######################


#get mean leaf-out and senescence dates
leaf_out = as.Date(mean(Pheno.df$Greenup_DOY), origin = "2016-12-31")
leaf_off = as.Date(mean(Pheno.df$MidGreendown_DOY), origin = "2016-12-31")

# dataframe of photoperiods
photo.df   = fread(paste(photo_path, "Photoperiod.csv", sep="/"))
phot.sub   = photo.df[475,3:367]
phot.sub   = rbind(as.data.frame(t(phot.sub)), as.data.frame(t(phot.sub)))
phot.sub$X = as.Date(1:nrow(phot.sub), origin = "2016-12-31")


# Plot of periods around solstice
#################################

#dataframe of periods
solstice.data = rbind(
  data.frame(X=as.Date(c("2017-05-14","2017-06-12")), Y=10, season = "A"),
  data.frame(X=as.Date(c("2017-05-24","2017-06-22")), Y=11, season = "B"),
  data.frame(X=as.Date(c("2017-06-02","2017-07-01")), Y=12, season = "C"),
  data.frame(X=as.Date(c("2017-06-12","2017-07-11")), Y=13, season = "D"),
  data.frame(X=as.Date(c("2017-06-22","2017-07-21")), Y=14, season = "E"),
  data.frame(X=as.Date(c("2017-07-03","2017-08-01")), Y=15, season = "F") )

#Plot
PhotoSolstice = ggplot() +
  #day length line
  geom_line(data=phot.sub, aes(x=X, y=V1, group=1),col="black") +
  #solstice
  geom_vline(xintercept = as.Date("2017-06-22"), size=1, alpha=0.4)+
  #periods
  geom_line(data=solstice.data, aes(x=X, y=Y, color=season), size=2.75)+
  scale_color_manual(values = rev(wes_palette(6, name = "Zissou1", type = "continuous")))+
  
  #plot settings
  coord_cartesian(xlim=c(as.Date(c('2017-03-01','2017-10-31'))), ylim=c(10,16))+
  ylab("Day length")+xlab("")+
  plotTheme1+
  theme(plot.background = element_rect(fill = "transparent", color = NA))



##############################################################################################################################################
##############################################################################################################################################



########################################
## Interpolation of monthly estimates ##
########################################



#Interpolation function
lin_interp = function(x, y, length.out=100) {
  approx(x, y, xout=seq(min(x), max(x), length.out=length.out))$y
}

#create identifier
variable.type = unique(MonthlyAnalysis.df$variable.type)

#create interpolation dataframe
df.interp = data.frame()
df.AUC = data.frame()

#loop over variable x equation x vegetation type vector
for (variable.name in variable.type){
  
  #subset table
  df.sub = MonthlyAnalysis.df  %>% 
    filter(variable.type == variable.name)
  
  # Interpolate data
  created.interp = lin_interp(df.sub$term, df.sub$term)
  score.interp   = lin_interp(df.sub$term, df.sub$mean)
  df.interp.sub  = data.frame(created=created.interp, score=score.interp)
  # Make a grouping variable for each pos/neg segment
  cat.rle        = rle(df.interp.sub$score < 0)
  df.interp.sub  = df.interp.sub %>%
    mutate(group = rep.int(1:length(cat.rle$lengths),  times=cat.rle$lengths),
           LC_Type  = unique(df.sub$LC_Type),
           variable = unique(df.sub$variable) )
  #rbind sub dataframes 
  df.interp = rbind(df.interp, df.interp.sub)
  
  #get Area under curve (%)
  df.AUC.sub = df.interp.sub %>%
    mutate(positive = ifelse(score<0, 0, score),
           negative = ifelse(score>0, 0, score))%>%
    summarise(sum.pos = trapz(created, positive), 
              sum.neg = abs(trapz(created, negative)))%>%
    mutate(percent.neg = round(sum.neg/(sum.pos+sum.neg)*100),
           percent.pos = round(sum.pos/(sum.pos+sum.neg)*100),
           LC_Type  = unique(df.sub$LC_Type),
           variable = unique(df.sub$variable) )
  #rbind sub dataframes 
  df.AUC = rbind(df.AUC, df.AUC.sub)
}



##############################################################################################################################################
##############################################################################################################################################



########################
# Mixed effects models #
########################



#Prepare data
#############

PhenoMixed.df <- Pheno.df %>%
  #delete outlier values
  filter(SWrad.LO.SO < quantile(.$SWrad.LO.SO, 0.99),
         SWrad.LO.SO > quantile(.$SWrad.LO.SO, 0.01),
         GPPstart.LO.SO < quantile(.$GPP.LO.SO, 0.99),
         GPPstart.LO.SO > quantile(.$GPP.LO.SO, 0.01),
         Tday.LO.SO < quantile(.$Tday.LO.SO, 0.99),
         Tday.LO.SO > quantile(.$Tday.LO.SO, 0.01)
  ) %>%
  mutate(GPPstart.LO.SO = GPPstart.LO.SO*0.1) %>%
  #delete pixels with less than 15 years
  group_by(geometry) %>%
  filter(n() >= 15) %>%
  ungroup()


#Effect of post-solstice temperature
summary(lmer(MidGreendown_DOY ~ Tday.SO.SE + (1|geometry), data=PhenoMixed.df))


##############################################################################################################################################


# Models
########

#list variables to loop through
variables = unique(Analysis.df$variable)

#create List object to store results
DataList1 = replicate(length(variables), data.frame())
DataList2 = replicate(length(variables), data.frame())
names(DataList1) = variables
names(DataList2) = variables

##############################################################################################################################################


#Loop through variables
#######################

for (i in 1:length(variables)){

  #extract variables
  Year             = as.numeric(PhenoMixed.df$Year)
  Pre.solstice     = as.numeric(PhenoMixed.df %>% pull(paste0(variables[i],".LO.SO")))
  MidGreendown_DOY = as.numeric(PhenoMixed.df$MidGreendown_DOY)
  geometry         = PhenoMixed.df$geometry
  
  
  #Multivariate
  fit_multi = lmer(MidGreendown_DOY ~ Pre.solstice + Year + (1 | geometry), 
                   na.action = "na.exclude", control = lmerControl(optimizer ="Nelder_Mead"))
  
  #year-only
  fit_year = lmer(MidGreendown_DOY ~ Year + (1 | geometry), 
                  na.action = "na.exclude", control = lmerControl(optimizer ="Nelder_Mead"))
  
  # Extract information for plotting
  plotMulti   = allEffects(fit_multi)
  plotYear    = allEffects(fit_year)
  
  # Extract coefficients
  df.coefficients = tibble(Coefficient = coef(summary(fit_multi))[ , "Estimate"][2:3],
                           std.error = coef(summary(fit_multi))[ , "Std. Error"][2:3],
                           variable = c(paste0(variables[i]),"Year"),
                           class = paste0(variables[i])) %>% 
    bind_rows(tibble(Coefficient = coef(summary(fit_year))[ , "Estimate"][2],
                     std.error = coef(summary(fit_multi))[ , "Std. Error"][2],
                     variable = c("Year"),
                     class = "Univariate"))
  
  # Final table
  df <- tibble(upper    = plotYear$Year$upper[,1],
               lower    = plotYear$Year$lower[,1],
               off      = plotYear$Year$fit[,1],
               xval     = plotYear$Year$x[,1],
               class    = "Univariate",
               variable = "Year") %>%
    #Multi
    bind_rows(
      tibble(upper    = plotMulti$Year$upper[,1],
             lower    = plotMulti$Year$lower[,1],
             off      = plotMulti$Year$fit[,1],
             xval     = plotMulti$Year$x[,1],
             class    = paste0(variables[i]),
             variable = "Year")
    )%>% 
    bind_rows(
      tibble(upper    = plotMulti$Pre.solstice$upper[,1],
             lower    = plotMulti$Pre.solstice$lower[,1],
             off      = plotMulti$Pre.solstice$fit[,1],
             xval     = plotMulti$Pre.solstice$x[,1],
             class    = paste0(variables[i]),
             variable = paste0(variables[i]))
    )
  
  
  # get phenology anomalies
  df = df %>%
    group_by(class, variable) %>%
    mutate(anomaly       = off - mean(off),
           anomaly.upper = upper - mean(off),
           anomaly.lower = lower - mean(off)) %>%
    ungroup()
  
  ##############################################################################################################################################
  
  #store data frame in variable list
  DataList1[[i]] = df 
  DataList2[[i]] = df.coefficients
  
  #count
  print(paste0('...',i,' out of ',length(variables), ' (',variables[i],') done'))
}

#bind rows
MixedModel.df   = bind_rows(DataList1) 
coefficients.df = bind_rows(DataList2) 



##############################################################################################################################################
##############################################################################################################################################



##############
# Map figure #
##############



#start loop
for(variable.name in variables) {

  #subset and reshape data 
  Analysis.df.sub = ReducedModel.df %>%
    filter(variable == variable.name) %>%
    dplyr::select(c(Lat, Lon, geometry, variable, term, estimate)) %>%
    pivot_wider(., names_from = term, values_from = estimate) %>%
    dplyr::rename('Post' = as.name(paste0(variable.name,'.SO.SE')),
                  'Pre'  = as.name(paste0(variable.name,'.LO.SO'))) %>%
    mutate(category = ifelse(Post>0 & Pre<0, "- +", 
                             ifelse(Post<0 & Pre>0, "+ -", 
                                    ifelse(Post>0 & Pre>0, "+ +", "- -"))))

  #data 2
  Analysis.df.sub2 = ReducedModel.df %>%
    filter(variable == variable.name) %>%
    mutate(term = factor(term, levels=c(paste0(variable.name,".SO.SE"),
                                        paste0(variable.name,".LO.SO") ), ordered=T),
           positive = ifelse(estimate>0,1,0),
           negative = ifelse(estimate<0,1,0),
           positive.sign = ifelse(estimate>0 & p.value<0.05,1,0),
           negative.sign = ifelse(estimate<0 & p.value<0.05,1,0))
  
  
  ##############################################################################################################################################
  
  
  #########
  # Barplot
  #########
  
  Analysis.df.sub = cbind(Analysis.df.sub, Analysis.df.sub2 %>%
    filter(term == paste0(variable.name,".LO.SO") ) %>%
    mutate(sig.level = ifelse(p.value<0.01,"<0.01",
                              ifelse(p.value<0.05,"0.01-0.05",
                                     ifelse(p.value<0.1,"0.05-0.1",
                                            ifelse(p.value<0.25,"0.1-0.25",
                                                   ifelse(p.value<0.5,"0.25-0.5",">0.5")))))) %>%
    dplyr::select(sig.level) 
    ) %>%
    mutate(BarPlotCat = paste(category, sig.level, sep="_"))
  
  #define groups
  BarPlot.df = as.data.frame(table(Analysis.df.sub$BarPlotCat) / sum(table(Analysis.df.sub$category)) ) %>%
    separate(Var1, into=c("Category", "Sig"), sep = "_") %>%#separate columns
    mutate(Category = factor(Category, levels=c("- +", "- -", "+ -", "+ +"))) %>%
    mutate(Sig = factor(Sig, levels=c("<0.01", "0.01-0.05", "0.05-0.1", "0.1-0.25", "0.25-0.5",">0.5")))
  
  #Plot
  BarPlot = ggplot(BarPlot.df, aes(x=Category, y=Freq*100, fill=Sig) )  +
    geom_bar(stat="identity")+
    xlab("Pre- / Post-solstice effects") +
    ylab("% pixels") +
    scale_fill_manual(values = c("grey5", "grey20", "grey35", "grey50", "grey65", "grey80")) +
    plotTheme1 +
    coord_cartesian(ylim=c(3,sum(BarPlot.df[BarPlot.df$Category=="- +",]$Freq)*100))+
    theme(legend.position = c(0.7,0.65))
    
  
  ##############################################################################################################################################
  
  
  #################
  # 2d density plot
  #################
  
  DensityPlot = ggplot(Analysis.df.sub, aes(x=Pre, y=Post) )  +
    stat_density_2d(aes(fill = ..level..), geom = "polygon") +
    scale_fill_distiller(palette=6, direction=1) +
    geom_hline(aes(yintercept=0))+
    geom_vline(aes(xintercept=0))+
    coord_cartesian(ylim = c(-.8, .8),xlim = c(-.9, .9))+
    xlab(paste0("Pre-solstice ",  variable.name)) +
    ylab(paste0("Post-solstice ", variable.name)) +
    plotTheme1
  
  
  ##############################################################################################################################################
  
  
  ###########
  # Histogram
  ###########

  #create summary info
  VariablesVector = c("estimate","p.value","positive","negative","positive.sign","negative.sign")
  data1 = Analysis.df.sub2 %>% 
    group_by(term) %>% 
    summarize_at(VariablesVector, mean, na.rm = TRUE) 
  
  #Plot
  HistoPlot = ggplot(Analysis.df.sub2, aes(x=estimate, fill=term)) +
    geom_histogram(binwidth=.01, alpha=.7, position="identity") +
    geom_vline(xintercept=0, colour="black",alpha=.8) +
    scale_fill_manual(values = c('#3B9AB2','#F21A00'))+
    #add pre-solstice text
    geom_text(data    = data1[data1$term==paste0(variable.name,".LO.SO"),],
              mapping = aes(x = -Inf, y = Inf, hjust = -0.1, vjust = 3.5, 
                            label = paste("Mean = ",round(estimate,2), "; ", 
                                          round(negative*100), "% (", round(negative.sign*100), '%)', sep="")), 
              size=3.5, color='#F21A00')+
    #add post-solstice text
    geom_text(data    = data1[data1$term==paste0(variable.name,".SO.SE"),],
              mapping = aes(x = Inf, y = Inf, hjust = 1.1, vjust = 3.5, 
                            label = paste("Mean = ",round(estimate,2), "; ", 
                                          round(positive*100), "% (", round(positive.sign*100), '%)', sep="")), 
              size=3.5, color='#3B9AB2')+
    xlab("Standardized effect") +
    ylab("Count (number of pixels)") +
    coord_cartesian(xlim = c(-.9, .9), ylim = c(11, 250))+
    plotTheme1 +
    theme(legend.position = c(0.1,0.5))
  
  
  ##############################################################################################################################################

  
  ###################
  # Latitudinal plots
  ###################
  
  #Pre-solstice
  LatPlotPre = Analysis.df.sub2[Analysis.df.sub2$term==paste0(variable.name,".LO.SO"),] %>%
    mutate(LatRound = round(Lat)) %>%
    group_by(term, LatRound) %>%
    summarise(mean = mean(estimate),
              lowCI = ci(estimate)[2],
              highCI = ci(estimate)[3]) %>% 
    ggplot(aes(x = LatRound, y= mean, group=term, color=term)) + 
    geom_ribbon(aes(ymin=lowCI, ymax=highCI, fill=term), alpha=0.4, color=NA)+
    geom_line()+
    geom_hline(yintercept=0)+
    scale_color_manual(values = c('#F21A00'))+
    scale_fill_manual(values = c('#F21A00'))+
    ylab("") +
    coord_flip(ylim = c(-0.35, 0.35),xlim=c(27,75))+
    plotTheme1 +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank())

  #Post-solstice
  LatPlotPost = Analysis.df.sub2[Analysis.df.sub2$term==paste0(variable.name,".SO.SE"),] %>%
    mutate(LatRound = round(Lat)) %>%
    group_by(term, LatRound) %>%
    summarise(mean = mean(estimate),
              lowCI = ci(estimate)[2],
              highCI = ci(estimate)[3]) %>% 
    ggplot(aes(x = LatRound, y= mean, group=term, color=term)) + 
    geom_ribbon(aes(ymin=lowCI, ymax=highCI, fill=term), alpha=0.4, color=NA)+
    geom_line()+
    geom_hline(yintercept=0)+
    scale_color_manual(values = c('#3B9AB2'))+
    scale_fill_manual(values = c('#3B9AB2'))+
    ylab("Standardized effect") +
    coord_flip(ylim = c(-0.35, 0.35),xlim=c(27,75))+
    plotTheme1 +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank())
 
  
  ##############################################################################################################################################
  
  
  #########
  # Mapping
  #########
  
  #subset and reshape data 
  Analysis.df.sub3 = ReducedModel.df %>%
    filter(variable == variable.name,
           term %in% c(paste0(variable.name,'.SO.SE'),paste0(variable.name,'.LO.SO'))) %>%
    mutate(estimate = ifelse(estimate>0.5, 0.5, ifelse(estimate < -0.5, -0.5, estimate)) ) %>%
    dplyr::select(c(Lat, Lon, geometry, variable, term, estimate)) %>%
    pivot_wider(., names_from = term, values_from = estimate) %>%
    dplyr::rename('Post' = as.name(paste0(variable.name,'.SO.SE')),
                  'Pre'  = as.name(paste0(variable.name,'.LO.SO')))
  
  #Get world map
  mp <- NULL
  mapWorld <- borders("world", colour="gray40", fill="gray40") # create a layer of borders
  mp <- ggplot() + mapWorld + plotTheme1
  
  #Add pre-solstice information
  MapPre <- mp + geom_tile(data = Analysis.df.sub3, 
                           show.legend=T,
                           aes(x = Lon, y = Lat, fill=Pre)) +
    scale_fill_gradient2(midpoint=0, low='#F21A00', mid="white",  
                         high='#3B9AB2', space ="Lab" ) +
    coord_cartesian(ylim = c(27, 75), xlim = c(-160, 175)) +
    xlab("") + ylab('Latitude') +
    theme(legend.position = c(0.08,0.33))
  
  #Add post-solstice information
  MapPost <- mp + geom_tile(data = Analysis.df.sub3, 
                             show.legend=T,
                             aes(x = Lon, y = Lat, fill=Post)) +
    scale_fill_gradient2(midpoint=0, low='#F21A00', mid="white",  
                         high='#3B9AB2', space ="Lab" ) +
    coord_cartesian(ylim = c(27, 75), xlim = c(-160, 175)) +
    xlab("") + ylab('Latitude') 
    
  
  ##############################################################################################################################################
  
  
  ################
  # Solstice plots
  ################
  
  #subset the data
  SolsticeModel.df.sub = SeasonalModel.df  %>% 
    filter(variable == variable.name)
  
  #set y ranges
  yRange=c(-0.18, 0.18)
  
  # Plot
  plotSolstice = ggplot(data = SolsticeModel.df.sub, aes(x = variable.class, y = mean, fill=variable.class)) + 
    geom_bar(stat = "identity")+
    geom_errorbar(aes(ymin = lowCI, ymax = hiCI), width=.2,
                  position=position_dodge(.9)) +
    geom_hline(yintercept=0)+
    xlab("") + ylab("") +
    coord_cartesian(ylim = yRange) +
    scale_fill_manual(values = rev(wes_palette(6, name = "Zissou1", type = "continuous")))+
    scale_x_discrete(labels=c("solstice1" = "May 13\nJun 11", "solstice2" = "May 23\nJun 21",
                              "solstice3" = "Jun 2\nJul 1", "solstice4"="Jun 12\nJul 11",
                              "solstice5"="Jun 22\nJul 21", "solstice6"="Jul 2\nJul 31"))+
    plotTheme1
  
  plotSolstice =  plotSolstice + annotation_custom(ggplotGrob(PhotoSolstice), xmin = 0.5, xmax = 3.5, 
                                    ymin = 0.0, ymax = yRange[2])
  
  
  ##############################################################################################################################################
  
  
  #######################################
  # Full model plots (Linear model means)
  #######################################
  
  #set y ranges
  yRange = c(-0.3,0.3)
  
  #-----------------------------------------------------------------------------------------------------------------------
  
  #All pixels
  ###########
  
  #Summarize table
  Analysis.df.sub = FullModel.df %>%
    filter(variable == variable.name) %>%
    group_by(term) %>%
    summarise(mean  = mean(estimate), 
              lowCI = gmodels::ci(estimate)[2],
              hiCI  = gmodels::ci(estimate)[3]) %>%
    mutate(term = factor(term, 
                         levels=c(paste0(variable.name, ".LO.SO"),
                                  "Prcp.LO.SO", 'Prcp.SO.SE', "CO2",
                                  paste0(variable.name, ".SO.SE"), "Tday"), ordered=T) )
  
  # Plot
  plotFull = ggplot(data = Analysis.df.sub, aes(x = term, y = mean, fill=term)) + 
    geom_bar(stat = "identity")+
    geom_errorbar(aes(ymin = lowCI, ymax = hiCI), width=.2,
                  position=position_dodge(.9)) +
    geom_hline(yintercept=0)+
    xlab("") + ylab("Standardized effect") +
    coord_cartesian(ylim = yRange) +
    scale_fill_manual(values = c('#F21A00','grey60','grey35','black','#3B9AB2','#78B7C5'))+
    scale_x_discrete(labels = c('Out-Sol','Prcp pre','Prcp post','CO2','Sol-Off','Autumn Tday'))+
    plotTheme1 +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
  
  #-----------------------------------------------------------------------------------------------------------------------
  
  #Vegetation-type-specific
  #########################
  
  #Summarize table
  Analysis.df.sub = FullModel.df %>%
    filter(variable == variable.name) %>%
    group_by(LC_Type, term) %>%
    summarise(mean  = gmodels::ci(estimate)[1], 
              lowCI = gmodels::ci(estimate)[2],
              hiCI  = gmodels::ci(estimate)[3]) %>%
    mutate(LC_Type  = factor(LC_Type, levels=c("Mixed","DecB","EvgN","DecN")),
           term = factor(term, 
                         levels=c(paste0(variable.name, ".LO.SO"),
                                  "Prcp.LO.SO", 'Prcp.SO.SE', "CO2",
                                  paste0(variable.name, ".SO.SE"),'Tday'), ordered=T) )
  
  # Plot
  plotFullLC = ggplot(data = Analysis.df.sub, aes(x = term, y = mean, fill=term)) + 
    geom_bar(stat = "identity")+
    geom_errorbar(aes(ymin = lowCI, ymax = hiCI), width=.2,
                  position=position_dodge(.9)) +
    geom_hline(yintercept=0)+
    xlab("") + ylab("Standardized effect") +
    coord_cartesian(ylim = c(-.35,.35)) +
    scale_fill_manual(values = c('#F21A00','grey60','grey35','black','#3B9AB2','#78B7C5'))+
    scale_x_discrete(labels = c('Out-Sol','Prcp pre','Prcp post','CO2','Sol-Off','Autumn Tday'))+
    plotTheme1 +
    facet_grid(LC_Type~1) +
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          strip.text  = element_blank())
  
  
  ##############################################################################################################################################
  
  
  ###############
  # Monthly plots
  ###############
  
  
  #subset the table
  #################
  
  Monthly.df.sub = MonthlyAnalysis.df  %>% 
    filter(variable == variable.name) 
  
  df.interp.sub = df.interp  %>% 
    filter(variable == variable.name)
  
  df.AUC.sub = df.AUC  %>% 
    filter(variable == variable.name)
  
  #-----------------------------------------------------------------------------------------------------------------------
  
  # Plots
  #######
  
  #set x and y ranges
  if(variable.name %in% c("Apm","Azani",'SWrad','GPPstart','LAIstart')){
    xRange=c(4.2, 9.8) } else {xRange=c(1.3, 9.7) }
  
  yRange=c(-0.2,0.2)
  yRange2=c(-0.25,0.25) 
  
  #-----------------------------------------------------------------------------------------------------------------------
  
  #All pixels
  plot.monthly = ggplot() + 
    geom_area(data = df.interp.sub[df.interp.sub$LC_Type=='All',], aes(x = created, y = score, fill=score>0, group=group)) + 
    scale_fill_manual(values = c('#F21A00', '#3B9AB2'))+
    geom_point(data=Monthly.df.sub[Monthly.df.sub$LC_Type=='All',], 
               aes(x=term, y=mean))+
    geom_errorbar(data=Monthly.df.sub[Monthly.df.sub$LC_Type=='All',], 
                  aes(x=term, ymin=lowCI, ymax=hiCI), width=.2,
                  position=position_dodge(.9)) +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=6.3, size=2, alpha=0.4)+
    geom_text(data = df.AUC.sub[df.AUC.sub$LC_Type=='All',], mapping = aes(x = -Inf, y = Inf, 
                                                                            hjust = -0.1, vjust = 1.5,
                                                                            label = paste0(percent.neg,'% / ',percent.pos, '%')))+
    coord_cartesian(xlim=xRange, ylim=yRange)+
    xlab("")+ylab("")+
    scale_x_continuous(breaks = seq(1,10,by=1),
                       labels = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct'))+
    plotTheme1
  
  #-----------------------------------------------------------------------------------------------------------------------
  
  #Vegetation-type-specific
  plot.monthly.LCtype = ggplot() + 
    geom_area(data = df.interp.sub[df.interp.sub$LC_Type!='All',], aes(x = created, y = score, fill=score>0, group=group)) + 
    scale_fill_manual(values = c('#F21A00', '#3B9AB2'))+
    geom_point(data=Monthly.df.sub[Monthly.df.sub$LC_Type!='All',], 
               aes(x=term, y=mean))+
    geom_errorbar(data=Monthly.df.sub[Monthly.df.sub$LC_Type!='All',], 
                  aes(x=term, ymin=lowCI, ymax=hiCI), width=.2,
                  position=position_dodge(.9)) +
    geom_hline(yintercept=0)+
    geom_vline(xintercept=6.3, size=2, alpha=0.4)+
    geom_text(data = df.AUC.sub[df.AUC.sub$LC_Type!='All',], 
              mapping = aes(x = -Inf, y = Inf, hjust = -.1, vjust = 1.5,
                            label = paste0(percent.neg,'% / ',percent.pos, '%')) ) +
    coord_cartesian(xlim=xRange,ylim=yRange2)+
    xlab("")+ylab('')+
    facet_grid(LC_Type~1)+
    scale_x_continuous(breaks = seq(1,10,by=2),
                       labels = c('Jan','Mar','May','Jul','Sep'))+
    plotTheme1 +
    theme(strip.text.x  = element_blank())
  
  
  ##############################################################################################################################################
  
  
  ###################
  # Mixed model plots
  ###################
  
  
  #Driver plots
  MixedModel.df.sub = MixedModel.df %>%
    filter(variable == variable.name)
  
  coefficients.df.sub = coefficients.df %>%
    filter(variable == variable.name)
  
  driver.plot = ggplot() + 
    geom_hline(yintercept = 0, linetype="dashed")+
    geom_ribbon(data = MixedModel.df.sub, aes(x = xval, ymin = anomaly.lower, ymax = anomaly.upper, fill=class), 
                alpha = 0.3) +
    geom_line(data=MixedModel.df.sub, aes(xval, anomaly, color=class)) +
    theme_classic() +
    geom_text(data=coefficients.df.sub, aes(label=paste0(round(Coefficient,2)," days per unit"),
                                            x=Inf, y=Inf,hjust = "inward", vjust = "inward"))+
    
    scale_color_manual(values = c('#F21A00'))+
    scale_fill_manual(values = c('#F21A00'))+
    
    coord_cartesian(ylim = c(-8,8))+
    
    ggtitle(paste("EOS ~",variable.name, " + Year"))+
    labs(x = "", y = "Senescence (days)")+
    plotTheme1
  
  
  #univariate year plots
  MixedModel.df.sub = MixedModel.df %>%
    filter(variable == "Year",
           class %in% c("Univariate")) %>%
    distinct()
  
  coefficients.df.sub = coefficients.df %>%
    filter(variable == "Year",
           class %in% c("Univariate"))%>%
    distinct() 
  
  year.plot = ggplot() + 
    geom_hline(yintercept = 0, linetype="dashed")+
    geom_ribbon(data = MixedModel.df.sub, aes(x = xval, ymin = anomaly.lower, ymax = anomaly.upper, fill=class), 
                alpha = 0.3) +
    geom_line(data=MixedModel.df.sub, aes(xval, anomaly, color=class)) +
    theme_classic() +
    
    geom_text(data=coefficients.df.sub, 
              aes(label=paste0(round(Coefficient*10,1)," days per decade"),
                  x=Inf, y=Inf,hjust = "inward", vjust = "inward"))+
    
    scale_color_manual(values = c('#F21A00'))+
    scale_fill_manual(values = c('#F21A00'))+
    
    coord_cartesian(ylim = c(-2.5,2.5))+
    
    ggtitle("EOS ~ Year")+
    labs(x = "", y = "Senescence (days)")+
    plotTheme1
  
  #Multivariate year plots
  MixedModel.df.sub = MixedModel.df %>%
    filter(variable == "Year",
           class %in% c(variable.name))
  
  coefficients.df.sub = coefficients.df %>%
    filter(variable == "Year",
           class %in% c(variable.name))
  
  year.multi.plot = ggplot() + 
    geom_hline(yintercept = 0, linetype="dashed")+
    geom_ribbon(data = MixedModel.df.sub, aes(x = xval, ymin = anomaly.lower, ymax = anomaly.upper, fill=class), 
                alpha = 0.3) +
    geom_line(data=MixedModel.df.sub, aes(xval, anomaly, color=class)) +
    theme_classic() +
    
    geom_text(data=coefficients.df.sub, 
              aes(label=paste0(round(Coefficient*10,1)," days per decade"),
                  x=Inf, y=Inf,hjust = "inward", vjust = "inward"))+
    
    scale_color_manual(values = c('#3B9AB2'))+
    scale_fill_manual(values = c('#3B9AB2'))+
    
    coord_cartesian(ylim = c(-2.5,2.5))+
    
    ggtitle(paste("EOS ~", variable.name, " + Year"))+
    labs(x = "", y = "Senescence (days)")+
    plotTheme1
  
  
  ##############################################################################################################################################
  
  
  ##########################
  # Arrange and safe plots #
  ##########################
  
  
  # 1. Map plots
  ##############
  
  #define plot layout
  layout <- "
AAAAAB
CCCCCD
EEEFFG
HHIIJJ
KKLLMM
"
  
  #Merge plots
  Fig_Plot = MapPre + LatPlotPre +
             MapPost + LatPlotPost + 
             HistoPlot + 
             DensityPlot +
             BarPlot +
             plotFull + plot.monthly + plotSolstice + 
             driver.plot + year.plot + year.multi.plot +
    plot_layout(design = layout) + plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(face = 'bold'))
  
  #save plots as .pdf
  ggsave(Fig_Plot, file=paste('Map_',variable.name, ".pdf", sep=''), 
         path=output_path,
         width=14, height=16)
  
  
  # 2. Vegetation-type-specific plots
  ###################################
  
  #define plot layout
  layout <- "AB"
  
  #Merge plots
  Fig_Plot = plotFullLC + plot.monthly.LCtype +
    plot_layout(design = layout) + plot_annotation(tag_levels = 'a') &
    theme(plot.tag = element_text(face = 'bold'))
  
  #save plots as .pdf
  ggsave(Fig_Plot, file=paste('LCtype_',variable.name, ".pdf", sep=''), 
         path=output_path,
         width=8, height=10)
  
  
  ##############################################################################################################################################
  
  #count
  print(variable.name)
}



##############################################################################################################################################
##############################################################################################################################################



#####################
## Reproducibility ##	
#####################



## datetime
Sys.time()
#"2021-07-01 13:19:31 CEST"

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
#  [1] wesanderson_0.3.6 gmodels_2.18.1    patchwork_1.1.1   data.table_1.14.0 forcats_0.5.1     stringr_1.4.0    
#[7] dplyr_1.0.7       purrr_0.3.4       readr_1.4.0       tidyr_1.1.3       tibble_3.1.2      ggplot2_3.3.4    
#[13] tidyverse_1.3.1  

#loaded via a namespace (and not attached):
#  [1] gtools_3.9.2       tidyselect_1.1.1   haven_2.4.1        colorspace_2.0-1   vctrs_0.3.8        generics_0.1.0    
#[7] utf8_1.2.1         rlang_0.4.11       isoband_0.2.4      pillar_1.6.1       glue_1.4.2         withr_2.4.2       
#[13] DBI_1.1.1          dbplyr_2.1.1       RColorBrewer_1.1-2 modelr_0.1.8       readxl_1.3.1       lifecycle_1.0.0   
#[19] munsell_0.5.0      gtable_0.3.0       cellranger_1.1.0   rvest_1.0.0        labeling_0.4.2     fansi_0.5.0       
#[25] broom_0.7.8        Rcpp_1.0.6         scales_1.1.1       backports_1.2.1    gdata_2.18.0       jsonlite_1.7.2    
#[31] farver_2.1.0       fs_1.5.0           digest_0.6.27      hms_1.1.0          stringi_1.6.2      grid_4.1.0        
#[37] cli_2.5.0          tools_4.1.0        maps_3.3.0         magrittr_2.0.1     crayon_1.4.1       pkgconfig_2.0.3   
#[43] ellipsis_0.3.2     MASS_7.3-54        xml2_1.3.2         reprex_2.0.0       lubridate_1.7.10   assertthat_0.2.1  
#[49] httr_1.4.2         rstudioapi_0.13    R6_2.5.0           compiler_4.1.0    



##############################################################################################################################################
#############################################################THE END##########################################################################
##############################################################################################################################################


