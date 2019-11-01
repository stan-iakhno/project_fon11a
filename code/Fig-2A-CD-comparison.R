library(readxl)
library(readr)
library(ggpval)
library(ggplot2)


#pull the metadata file
butyrate_CD <- read_excel("C:/Users/stia/OneDrive - Norwegian University of Life Sciences/FOODSofNORWAY/FON_011/FON_011a/1_Manuscript/Caroline_fon11a/butyrate-CD.xlsx")

# checks if the factors are indeed factors
is.factor(butyrate_CD$day)
is.factor(butyrate_CD$diet)

#its FALSE
#converts the factor variable to a proper format
butyrate_CD$day<- as.factor(butyrate_CD$day)
#makes a factor with proper labels
butyrate_CD$diet2<- factor(butyrate_CD$diet, labels = c(y="yeast", c="control"))
#sanity check
butyrate_CD$diet2

butyrate_CD$diet<-capitalize(butyrate_CD$diet)
butyrate_CD$diet

butyrate_CD$grouper <- paste0(butyrate_CD$day,".",butyrate_CD$diet)
butyrate_CD$grouper <- factor(butyrate_CD$grouper, levels = c('7.C', '7.Y', '14.C', '14.Y' ))
butyrate_CD$grouper
butyrate_CD$CD

plt <- ggplot(butyrate_CD, aes(x=grouper, y=CD, fill=grouper)) +
  geom_boxplot(fill=c("#CC79A7","#009E73","#CC79A7","#009E73"),
               color=c("#CC79A7","#009E73","#CC79A7","#009E73"),
               alpha=0.25, 
               lwd=0.4,
               outlier.shape = NA,
               fatten = 1.5)
plt
pltm<-add_pval(plt, pairs = list(c(1,2),c(3,4)), 
               test='wilcox.test', 
               barheight = 20, 
               pval_star = F, 
               heights = c(850,850),
               textsize = 9, pval_text_adj = 30)

n<-pltm + theme_bw(base_size = 12, base_family = 'serif')+
    labs(title ='Colonic CD in µm')+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y = element_text(face = 'plain'))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.border = element_blank()) +
  theme(panel.grid.minor = element_blank())+
  theme(panel.grid.major.y =element_line(linetype = NULL))+
  theme(axis.line.y.left =  element_blank())+
  theme(axis.line.y = element_line(colour = "grey"))+
  theme(axis.ticks.y = element_line(colour = "grey"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title = element_blank(), plot.title = element_text(size = 12))

n
ggsave("diet-CD.png",device = "png", 
       plot = n, 
       dpi = 300, 
       width = 7.5, height = 5.5, 
       units = "cm")

















