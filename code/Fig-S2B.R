# plots correaltion scatter-plot with the abline: colonic butyrate ~ liver 
# CONTROLS
#  Coefficients:
#                       Estimate   SE       t-value  Pr(>|t|)    
#  (Intercept)          2.130286   0.089043   23.92  9.36e-13 ***
#  butyrate_CD$butyrate 0.045983   0.005872    7.83  1.76e-06 *** 
#  Adjusted R-squared:  0.80 at 14 DF


#fetches the data from an excel file
butyrate_CD<-
  read_xlsx("C:/Users/stia/OneDrive - Norwegian University of Life Sciences/FOODSofNORWAY/FON_011/FON_011a/1_Manuscript/Caroline_fon11a/butyrate-CD.xlsx")

#subset the controls
butyrate_CD<-butyrate_CD[butyrate_CD$diet=="y",]

# Fit regression line
corr_eqn <- function(x,y, digits = 1) {
  corr_coef <- round(cor(x, y), digits = digits)
  paste("italic(r) == ", corr_coef)
}

pl5<-ggplot(butyrate_CD, aes(x = butyrate, y = liver)) +
  geom_point(shape = 19, size = 2, aes(colour = diet)) +
  scale_color_manual(values=c("#009E73"))+
  # scale_fill_manual(values=c("#CC79A7","#009E73"))
  geom_smooth(colour = "#009E73", fill = "lightgreen", method = 'lm') +
  #  ggtitle("Example") +
  ylab("liver index") +
  xlab("colonic butyrate, µM/g digesta") +
  theme(plot.title = element_text(lineheight = .8, face = "bold", vjust = 1),
        axis.text.x = element_text(size = 11, vjust = 0.5,
                                   hjust = 1, colour = 'black'),
        axis.text.y = element_text(size = 11, colour = 'black'),
        axis.title = element_text(size = 10, face = 'bold'),
        axis.line = element_line(colour = "black"),
        plot.background = element_rect(colour = 'black', size = 1),
        panel.background = element_blank()) +
  
  theme_minimal(base_size = 12,base_family = 'serif')
pl<-pl5 + theme(legend.position = 'none') +
  theme(panel.grid.minor = element_blank())
pl
cor.test(butyrate_CD$liver,butyrate_CD$butyrate)
linmod<-lm(butyrate_CD$liver~butyrate_CD$butyrate+butyrate_CD$day)
summary(linmod)

ggsave("FIgure-S2B.png",device = "png", 
       plot = pl, 
       dpi = 300, 
       width = 11, 
       height = 10, 
       units = "cm")
