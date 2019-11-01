# plots the boxplots of alpha diversity measures

library(readr)
library(ggpval)
library(ggplot2)

# fetches the dataset
shannonOBS<-read_csv("C:/Users/stia/OneDrive - Norwegian University of Life Sciences/FOODSofNORWAY/FON_011/FON_011a/1_Manuscript/Figures/shannonOBS.csv")

# reorders factors
shannonOBS$groups <- factor(shannonOBS$new2, levels = c("14.CO.Y",
                                                        "14.CO.C",
                                                        "7.CO.Y",
                                                        "7.CO.C",
                                                        "0.CO.B",
                                                        "14.CE.Y",
                                                        "14.CE.C",
                                                        "7.CE.Y",
                                                        "7.CE.C",
                                                        "0.CE.B",
                                                        "14.IL.Y",
                                                        "14.IL.C",
                                                        "7.IL.Y",
                                                        "7.IL.C",
                                                        "0.IL.B"))

# creates the body of the plot
plt <- ggplot(shannonOBS, aes(x=groups, y=value, color=feed)) +
  geom_boxplot(fill=c("#009E73","#CC79A7","#009E73","#CC79A7","#E69F00",
                      "#009E73","#CC79A7","#009E73","#CC79A7","#E69F00",
                      "#009E73","#CC79A7","#009E73","#CC79A7","#E69F00"),
               color=c("#009E73","#CC79A7","#009E73","#CC79A7","#E69F00",
                       "#009E73","#CC79A7","#009E73","#CC79A7","#E69F00",
                       "#009E73","#CC79A7","#009E73","#CC79A7","#E69F00"),
               alpha=0.25, 
               lwd=0.4,
               outlier.shape = NA,
               fatten = 1.5)  + 
  ylim(1.6,8) + 
  coord_flip()

# adds p-values
m<-add_pval(plt, pairs = list(c(1,2), 
                              c(3,4),
                              c(6,7),
                              c(8,9),
                              c(11,12),
                              c(13,14)), 
            test='wilcox.test', 
            barheight = 0.1,
            pval_star = F,
            pval_text_adj = 1.5, textsize = 9) +
  theme_bw(base_size = 12, base_family = 'serif')+
  labs(title ='Shannon diversity index')+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y = element_text(face = 'plain'))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.border = element_blank()) +
  theme(panel.grid.minor = element_blank())+
  theme(panel.grid.major.y =element_line(linetype = 2))+
  theme(axis.line.y.left =  element_blank())+
  theme(axis.line.y = element_line(colour = "grey"))+
  theme(axis.text.y = element_text(color = c("#009E73","#CC79A7","#009E73","#CC79A7","#E69F00",
                                             "#009E73","#CC79A7","#009E73","#CC79A7","#E69F00",
                                             "#009E73","#CC79A7","#009E73","#CC79A7","#E69F00"),
                                   hjust = 1,family = 'serif', size = 10))+
  theme(axis.ticks.y = element_line(colour = "grey"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title = element_blank(), plot.title = element_text(size = 12))
m

# saves the plot to HDD
ggsave("diversity-colours.png",device = "png", 
       plot = m, 
       dpi = 300, 
       width = 8.5, height = 8.5, 
       units = "cm")
