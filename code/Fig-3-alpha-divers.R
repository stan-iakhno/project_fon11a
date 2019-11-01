# Plots the alpha-diversiy comparison between the two
# diets; baseline figure follows
# "PS" - a phyloseq object
# "df" - a dataframe


library(phyloseq)
library(reshape2)
library(ggplot2)
library(extrafont)
library(extrafontdb)

# fetches the data
ps<-  readRDS("C:/Users/stia/OneDrive - Norwegian University of Life Sciences/FOODSofNORWAY/FON_011/FON_011a/1_Manuscript/R_files_fon011/PS.APR.19/getting_there/ps.lulu.rds")

#calculates alpha-diversity for each sample
diversity <- estimate_richness(ps, measures = c("Observed", "Shannon", "Simpson"))

# checks if the sample order matches between the PS and the new df 
x <- rownames(diversity)
y <- rownames(ps@sam_data)
identical(x,y)

# merges the diversity estimates with the metadata
ps.div <- merge_phyloseq(ps, sample_data(diversity))
ps <- ps.div  
ps.df <- as.data.frame(ps@sam_data)

# saves metadata df to the HDD  
saveRDS(ps.df, "ps.lulu.with.diversity.rds")
str(ps.df)

# subsets the df
ps.df2<-ps.df[,c(1,2,18,31:33)]
str(ps.df2)

# factorizes the selected variables
ps.df2$gut_site <- as.factor(ps.df2$gut_site) 
ps.df2$samp_day <- as.factor(ps.df2$samp_day) 
ps.df2$feed <- as.factor(ps.df2$feed) 

str(ps.df2)

# changes df format from wide to long

ps.m <- melt(ps.df2, measure.vars = c("Observed", "Shannon", "Simpson") )
str(ps.m)

# rearranges order and names of the variable for better graphics
shannon <- ps.m[ps.m$variable != "Simpson",]
str(shannon)
shannon$gut_site <- factor(shannon$gut_site, 
                           levels = c("IL","CE","CO"), 
                           labels = c("Ileum","Caecum","Colon"))

# plots the figure
pl01 <- ggplot(shannon,aes(x=samp_day,y=value, color=feed, fill = feed)) + 
  geom_boxplot(width=0.8, 
               alpha=0.2, 
               show.legend = F, 
               outlier.size = 0.5, 
               lwd=0.3,
               fatten = 0.7) + 
  
  theme(plot.title = element_text(hjust=0.5, size=25)) +
  scale_color_manual(values=c("#E69F00","#CC79A7","#009E73"))+ 
  scale_fill_manual(values=c("#E69F00","#CC79A7","#009E73"))+ 
  #  geom_jitter(width = 0.05) +
  facet_grid(variable~gut_site, scales = "free_y") +
  theme_minimal(base_family = "serif", base_size = 8) +
  theme(legend.title = element_text(hjust = 0.5, family = "serif")) + 
  theme(legend.background = element_rect(fill="white",
                                         size=0.3, 
                                         linetype="solid",
                                         colour ="black"))+
  theme(panel.grid.minor =element_blank())+
  theme(panel.grid.major = element_blank()) +
  theme(axis.ticks.y = element_line(color = 'black'),
        axis.line.y = element_line(color = 'black', linetype = 1)) +
  theme(axis.line.x.top = element_line(colour = "black")) +
  theme(panel.border = element_rect(color = "azure3",fill = NA,linetype = 1))+
  theme(panel.spacing.x = unit(0.0, "mm")) +
  labs(y="Alpha-diversity units ", x="day PW")+
  theme(axis.ticks.length.y = unit(0.5, "mm"))+
  theme(axis.ticks = element_line(colour = "grey")) +
  theme(axis.line.y.left = element_line(colour = "black", linetype = "solid"))+
  theme(aspect.ratio = 1)

pl01

# saves the figure to HDD
ggsave("diversity.png",device = "png", 
       plot = pl01, 
       dpi = 300, 
       width = 9, height = 5, 
       units = "cm")



#save table for the manuscript
write.csv((group_by(shannon, samp_day, feed, gut_site) %>%
             summarise(median_shannon = median(value), IQR_shannon = IQR(value))), "median_iqr_shannon_lulu.csv")

#do statistics 
s_stat<-shannon[shannon$gut_site == "Ileum" & 
                  shannon$samp_day != "14" & 
                  shannon$feed != "yeast",]
s_stat
wilcox.test(s_stat$value ~ s_stat$samp_day,
            paired = F, 
            conf.int=T)
