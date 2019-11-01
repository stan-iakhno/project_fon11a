#fecth the file
ft <- read.csv("C:/Users/stia/OneDrive - Norwegian University of Life Sciences/FOODSofNORWAY/FON_011/FON_011a/1_Manuscript/Caroline_fon11a/FT2.csv")

#remove undesirable variables
ft <- ft[ft$variable != "mucus_fibrosis_atrophy",]
ft <- ft[ft$variable !="LP_vacuolisation_oedema",]

ft$variable
#manually reorder factor
ft$variable <- as.character(ft$variable)
ft$variable<- factor(ft$variable, levels=unique(ft$variable))
ft$variable <- factor(ft$variable, levels=c("diagnose",
                                            "LP_eosinophils",
                                            "LP_lymphocytes_&_plasma cells",
                                            "LP_macrophages", 
                                            "LP_neutrophils",
                                            "crypt_abscess",
                                            "intra-epithelial_lymphocytes",
                                            "epithelial_damage"))
library(plyr)
ft$variable<-revalue(ft$variable, c("diagnose"="DI", "LP_eosinophils"="LPE",
                       "LP_lymphocytes_&_plasma cells"="LPL",
                       "LP_macrophages"="LPM","LP_neutrophils"="LPN","crypt_abscess"= "CA",
                       "intra-epithelial_lymphocytes"="IEL", "epithelial_damage"="ED" ))


ft$value <- factor(ft$value, levels = c('none','half','one','two'))
ft$value<-forcats::fct_rev(ft$value)
# plot and flip the axises
library(ggplot2)
pl01<-ggplot(ft, increasing =T,aes(x=variable, y=Freq, fill=value))+
  geom_bar(stat = "identity", width = 0.8)+
  facet_grid(.~diet)+
  coord_flip()+
  labs( x="Histology analysis")+
  theme_minimal(base_size = 14,base_family = 'serif')+
#   theme(legend.title = element_text(hjust = 0), legend.title.align = 0.)+
  labs(fill="Histology score")+
  theme(axis.title.y = element_blank())+
theme(legend.direction = "horizontal",legend.position = c(0.5, -0.25))+
 # theme(legend.text = element_text(colour="black", size=10, hjust = 0.8),
 #       legend.key.size = unit(2, 'mm'))+
  guides(fill = guide_legend(reverse=T))+
  scale_fill_manual(values = c("#E69F00", "#009E73","#0090b2","#56B4E9"))+
  theme(axis.title.x = element_text(colour="white"))+
  theme(axis.text =  element_text(vjust = 0.5))+
  theme(axis.text.x.bottom = element_text(vjust = 1))

pl01 

ggsave("hist003.png",device = "png", 
       plot = pl01, 
       dpi = 300, 
       width = 17.4, 
       height = 6.28, units = 'cm')

