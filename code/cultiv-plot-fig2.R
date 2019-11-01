# plots the boxplot caparing cutivation results between the diets


library(ggplot2)
library(extrafont)
library(extrafontdb)
library(readr)
library(ggpval)
library(data.table)

# fetches the data
df<-read_csv("C:/Users/stia/OneDrive - Norwegian University of Life Sciences/FOODSofNORWAY/FON_011/FON_011a/1_Manuscript/Figures/cultiv_LAB.csv")
#df$groups = paste0(df$day, ".",df$gut_site,".",df$diet);df$grouper = paste0(df$sorter2,df$sorter1,df$diet);df$grouper <-as.factor(df$grouper);df$gut_site<-factor(df$gut_site)

df.jej
##########
plt <- ggplot(df, aes(groups, value,fill= diet)) +
  geom_boxplot() +
  geom_jitter() + theme_minimal() +coord_flip() 
plt
plt<-add_pval(plt, pairs = list(c(1, 2))) 
plt
