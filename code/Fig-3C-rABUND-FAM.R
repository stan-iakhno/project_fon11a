ps.top20

library(ggplot2)
library(reshape2)
################
rab_LLmrgMEDIANaggFamily <- readRDS("C:/Users/stia/OneDrive - Norwegian University of Life Sciences/FOODSofNORWAY/FON_011/FON_011a/1_Manuscript/R_files_fon011/PS.APR.19/getting_there/ABUND/rab_LLmrgMEDIANaggFamily.rds")

rab_LLmrgMEDIANaggFamily

top20 <- names(sort(taxa_sums(rab_LLmrgMEDIANaggFamily), decreasing=TRUE))[1:10]
ps.top20 <- transform_sample_counts(rab_LLmrgMEDIANaggFamily, function(OTU) OTU/sum(OTU))
ps.top20 <- prune_taxa(top20, ps.top20)
ps.top20

otu.top6.df <- as.data.frame(otu_table(ps.top20))
tax.top6.df <- as.data.frame(ps.top20@tax_table@.Data)
colnames(otu.top6.df)<- paste0(tax.top6.df$Family)
meta.top6.df <- as.data.frame(ps.top20@sam_data@.Data)
colnames(meta.top6.df) <- ps.top20@sam_data@names

rownames(otu.top6.df) = c(6,11,1,9,7,14,12,4,2,10,8,15,13,5,3)
# removes redundant p__ in the taxonomy colnames
colnames(otu.top6.df)<-sub("f__*", "", colnames(otu.top6.df))
names(otu.top6.df)[10]<-"un.Bacteroidales"

# adds metadata to the df
otu.table.top6.df <-cbind(otu.top6.df, meta.top6.df[,c(1,2,5)])
otu.table.top6.df
# converts the matrix from wide to long format
otu.table.top6.df.long <-melt(data = otu.table.top6.df, id.vars = c(11:13))

# renames the df for ease
otu <- otu.table.top6.df.long

# adds a grouping variable for each day/gutSite/feed
otu$diet <- as.character(otu$diet)
otu$diet[otu$diet == "baseline"] <- "B"
otu$diet[otu$diet == "yeast"] <- "Y"
otu$diet[otu$diet == "control"] <- "C"

otu$groups <- paste0(otu$samp_day,".",otu$gut_site, ".",otu$diet)
otu$groups<-factor(otu$groups, levels = c("14.CO.Y",
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

# declares palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#builds the plot
p <- ggplot(data = otu, x=groups, y=value)+
  geom_bar(aes(x=groups, y=value, fill=variable), 
           position="stack", 
           stat = "identity", alpha=0.7)+
  coord_flip()+
  theme_bw(base_size = 12, base_family = 'serif') +
  theme(legend.position = "bottom")+
  theme(axis.ticks.y = element_line(colour = "grey"))+
  theme(axis.title.y = element_blank())+
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
  theme(legend.position="bottom",legend.text = element_text(), legend.direction = 'horizontal')+
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                               "#000E73", "#9072D5", "#AA99B0"))+
  theme(legend.position = "top", legend.box.spacing = unit(1,"mm"), legend.spacing.x = unit(1,"mm"),
        legend.text = element_text(size = 8), 
        legend.key.size = unit(2,"mm"), legend.title = element_blank(),legend.key.height = unit(2,"mm"))+
  theme(axis.title = element_blank())

p<-p+guides(fill=guide_legend(nrow=4,byrow=F ))
p
ggsave("RABF.png",device = "png", 
       plot = p, 
       dpi = 300, 
       width = 8.5, height = 8.5, 
       units = "cm")           
           
           
           
           
           
           
           
           
           
           
           
