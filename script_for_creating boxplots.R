# script using ggplot, reshape2 to create boxplot
NCM=read.csv(file.choose(),header=TRUE)
NCM.df=as.data.frame(NCM)
NCM.df$Celltype=factor(NCM.df$Celltype,levels=c("CBCT","CTPV","CBPV","CBCTPV"))
NCM.df$Learner_type=factor(NCM.df$Learner_type,levels=c("Good","Bad"))
HVC=read.csv(file.choose(),header=TRUE)
HVC.df=as.data.frame(HVC)
HVC.df$Cell_type=factor(HVC.df$Cell_type,levels=c("CB","CT","aPV"))
HVC.df$Learner_type=factor(HVC.df$Learner_type,levels=c("Good","Bad"))
library(ggplot2)

ggplot(NCM.df,aes(Learner_type,Density,fill=Hemisphere))+geom_boxplot(color="black",outlier.shape = NA) +geom_point(position=position_dodge(width=0.75),aes(group=Hemisphere))+facet_wrap(~Celltype,ncol=7)+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black",size=1))+scale_fill_brewer(palette="Greys")+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+  theme(axis.ticks.length=unit(.25, "cm"))+theme(axis.ticks = element_line(size = 1)+theme(strip.text.x = element_text(size = 14)))
ggplot(HVC.df,aes(Learner_type,Density_interneurons,fill=Hemisphere))+geom_boxplot(color="black",outlier.shape = NA) +geom_point(position=position_dodge(width=0.75),aes(group=Hemisphere))+facet_wrap(~Cell_type,ncol=7)+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black",size=1))+scale_fill_brewer(palette="Greys")+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+  theme(axis.ticks.length=unit(.25, "cm"))+theme(axis.ticks = element_line(size = 1)+theme(strip.text.x = element_text(size = 14)))
#without the top labels (no cb,ct,pv labels)
ggplot(NCM.df,aes(Learner_type,Density,fill=Hemisphere))+geom_boxplot(color="black",outlier.shape = NA,coef=0) +geom_point(position=position_dodge(width=0.75),aes(group=Hemisphere))+facet_wrap(~Celltype,ncol=7)+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank(),axis.line=element_line(colour="black",size=1))+scale_fill_brewer(palette="Greys")+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+  theme(axis.ticks.length=unit(.25, "cm"))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())

