


#############################################################################################################
############################################## R script for: ################################################
#############################################################################################################
##### Effect of climate warming on the timing of autumn leaf senescence reverses at the summer solstice #####
#############################################################################################################


#############################################################################################################
# Concept figure (not used) #################################################################################
#############################################################################################################



#required packages
require(ggplot2)
require(tidyverse)



##############################################################################################################################################
##############################################################################################################################################



# set the working dirctory
setwd("/Users/consti/Desktop/PhD/Publication_material/17_Autumn_phenology_tier2/PEP_analysis/Analysis")
output_path = "Analysis_output/Autumn/Concept_figure"



##############################################################################################################################################
##############################################################################################################################################



################
## Plot theme ##
################

plotTheme1 = theme(
  legend.position   = "top",
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



#######################
# Conceptual figure 1 #
#######################



## create dataframe
data.df = data.frame(month = c(1:12),
                     stress.hypothesis   = c(0,-0.2, -0.4,-0.4, 0.3, 0.7, 0.9, 0.9,0.7,0.3,0.04,0),
                     solstice.hypothesis = c(0,-0.05,-0.3,-0.6,-1,-1, 0.1,   0.8,0.9,0.3,0.04,0),
                     sink.hypothesis     = c(0,-0.05,-0.2,-0.4,-0.7,-0.9,-0.9, -0.5,0.9,0.3,0.04,0)
)
#long format
data.df = pivot_longer(data.df, -month)


#Plot
Concept.plot = ggplot(data=data.df, aes(x=month, y=value, group=name, colour=name)) +
  #horizontal line
  geom_hline(yintercept = 0, size=0.5)+
  #solstice
  geom_vline(xintercept = 6.2, size=3, alpha=1, color='grey50')+
  geom_line(stat="smooth",method='loess', se=F, span=0.4, size=2, alpha=0.9)+
  coord_cartesian(xlim=c(1.5,11))+
  scale_color_manual(values = c('#3B9AB2','#E1AF00','#F21A00'))+
  scale_x_continuous(breaks = seq(1,11,by=2),
                     labels = c('Jan','Mar','May','July','Sep','Nov'))+
  ylab("Effect of favourable growth condition")+xlab("")+
  plotTheme1


#save plots as .pdf
ggsave(Concept.plot, file="Concept.pdf", path=output_path,
       width=5, height=3.5)  



##############################################################################################################################################
#############################################################THE END##########################################################################
##############################################################################################################################################

  
 