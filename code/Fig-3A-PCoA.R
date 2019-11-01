# Plots a PCA plot on a phyloseq object made up with 16S rRNA gene
# amplicon NGS sequencing data

library(phyloseq)
library(ggplot2)
library(gtable)
library(grid)
# fetches the phylosect object
ps <- readRDS("C:/Users/stia/OneDrive - Norwegian University of Life Sciences/FOODSofNORWAY/FON_011/FON_011a/1_Manuscript/R_files_fon011/PS.APR.19/getting_there/ps.lulu.rds")

# transforms data to proportions as appropriate for Bray-Curtis distances
ps.prop <- transform_sample_counts(ps, function(otu) otu/sum(otu))
ord.nmds.bray <- ordinate(ps.prop,
                          method = "PCoA", 
                          distance = "bray" )


#change metadata SAMP_DAY variable from NUMERIC to FACTOR
ps.prop@sam_data[["day"]] <-
  as.factor(ps.prop@sam_data[["samp_day"]])
ps.prop@sam_data[["gut_site_2"]] <-as.factor(ps.prop@sam_data[["gut_site"]])
ps.prop@sam_data[["gut_site_2"]] <-factor(ps.prop@sam_data[["gut_site_2"]], labels = c("E","O","I"))
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
ps.prop@sam_data[["DIET"]] <-proper(ps.prop@sam_data[["diet"]])
ps.prop@sam_data[["DIET"]]
ps.prop@sam_data[["DIET_GS"]]<-paste0(ps.prop@sam_data[["DIET"]],
                                      ".",
                                      ps.prop@sam_data[["gut_site"]])
ps.prop@sam_data[["DIET_GS"]]<-as.factor(ps.prop@sam_data[["DIET_GS"]])
ps.prop@sam_data[["DIET_GS"]]
# plots ordination
ordPlot<-plot_ordination(ps.prop, 
                         ord.nmds.bray, 
                         color="DIET_GS", 
                         title="PCoA", 
                         shape = "day") +
  scale_shape_manual(values=c(19, 17, 15))+
  theme_minimal() +
 # scale_radius(trans = "log")+
  scale_color_manual(values=c("#FF9900",  
                              "#FF6600",
                              "#FFCC00",
                              "#FF9999",
                              "#CC6669",  
                              "#FFCCCC",
                              "#339999",  
                              "#336666",
                              "#33CCCC"
  ))+
  theme(legend.spacing.y = unit(1, 'mm'))+
 # theme(legend.position = c(0.650, 0.180))+ 
  theme(legend.position = 'top',legend.spacing.x = unit(1, 'mm') )+
  theme(legend.background = element_rect(fill="white",
                                         size = 0.05, 
                                         linetype = "blank",
                                         colour ="black"))+
  theme(legend.text = element_text(size=8,lineheight = unit(1, 'mm'), 
                                   family = "serif", vjust = 0.5, hjust = 0))+
  theme(legend.spacing.x = unit(1, 'mm'))+
  theme(legend.direction = "horizontal")+
  theme(legend.box.spacing = unit(2,"mm"))+
  theme(legend.key.size = unit(2,"mm"))+
  theme(legend.title = element_blank())+
  theme(legend.background=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.grid.minor =element_blank())+
  scale_x_continuous(breaks =  seq(0, 10, 1))+
  scale_y_continuous(breaks =  seq(0, 10, 1))+
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(colour = "grey"))+
  geom_point(size=2)+
  geom_point(alpha = 0.1 )+
  theme(plot.title = element_blank())+
  theme(axis.text = element_blank())+
  theme(axis.title = element_text(family = 'serif', size = 12))


ordPlot

#save the plot on the HDD
ggsave("PCoA_studySize_x3.png",device = "png", 
       plot = ordPlot, 
       dpi = 300, 
       width = 8.5, 
       height = 8.5, 
       units = "cm")

