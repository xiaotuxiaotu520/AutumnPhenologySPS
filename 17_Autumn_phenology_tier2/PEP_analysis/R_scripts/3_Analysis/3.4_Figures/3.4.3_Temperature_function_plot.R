


#############################################################################################################
############################################## R script for: ################################################
#############################################################################################################
##### Effect of climate warming on the timing of autumn leaf senescence reverses at the summer solstice #####
#############################################################################################################


#############################################################################################################
# Supplementary Fig. S17: LPJ model temperature function plot ###############################################
#############################################################################################################



#required packages
require(ggplot2)
require(wesanderson)



##############################################################################################################################################
##############################################################################################################################################



# set the working dirctory
setwd("/Users/consti/Desktop/PhD/Publication_material/17_Autumn_phenology_tier2/PEP_analysis/Analysis")
output_path = "Analysis_output/Autumn/Temp_optimum_function"



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



# Temperate inhibition function from LPJ-GUESS 
##############################################

temp_opt.fun <- function(temp) {
  x1        <- 1
  x2        <- 18
  x3        <- 25
  x4        <- 45
  k1        <- 2.*log((1/0.99)-1.)/(x1-x2)
  k2        <- (x1+x2)/2
  low       <- 1/(1+exp(k1*(k2-temp)))
  k3        <- log(0.99/0.01)/(x4-x3)
  high      <- 1-0.01*exp(k3*(temp-x3))
  tstress   <- low*high 
  if(tstress>=0) {
    tstress <- tstress
  } else {
    tstress <- 0
  }
  return(tstress)
}


# Create dataframe
##################

temperature = c(-5:46)
temp.data = data.frame(
as.numeric(temperature),
index = temp_opt.fun(temperature) )


# Plot
######

Plot = ggplot(data=temp.data, aes(x=temperature, y=index, colour=index)) +
  
  geom_vline(xintercept = 1, linetype='dashed', col='grey30')+
  geom_vline(xintercept = 45, linetype='dashed', col='grey30')+
  geom_vline(xintercept = 18, linetype='dashed', col='grey30')+
  geom_vline(xintercept = 25, linetype='dashed', col='grey30')+
  geom_line(size=2) +

  scale_color_gradientn(colours = rev(rainbow(5)))+
  coord_cartesian(ylim=c(0.047,1),xlim=c(0,45))+
  xlab("Daytime temperature (??C)") + ylab("Photosynthesis response") +
  plotTheme1

#save plots as .pdf
ggsave(Plot, file="Temp_opt_curve.pdf", path=output_path,
       width=4, height=3)



##############################################################################################################################################
#############################################################THE END##########################################################################
##############################################################################################################################################


