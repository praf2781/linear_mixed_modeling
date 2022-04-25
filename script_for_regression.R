NCM.lat=read.csv(file.choose(),header=TRUE)
NCM.lat.df= as.data.frame(NCM.lat)
oldpar=par(no.readonly=TRUE)
par(yaxt="l",xaxt="l",bty="l", tcl="n")
plot(NCM.lat.df$Learning_score,NCM.lat.df$Lat_ratio_good_learners,xlim=c(0,100),ylim=c(-0.2,0.2),xaxt="n",yaxt="n",axes=FALSE,cex=1.3,pch=c(21,24)[as.numeric(NCM.lat.df$Learner_type)],col="black",bg="black")+axis(1,pos=0,labels=FALSE,lwd=2)+axis(2,pos=0,labels=FALSE,lwd=2)
abline(lm(NCM.lat.df$Lat_ratio_good_learners~NCM.lat.df$Learning_Score),lwd=2)
plot(NCM.lat.df$Learning_Score,NCM.lat.df$Lat_ratio_good_learners,xlim=c(0,100),ylim=c(-0.2,0.2),xaxt="n",yaxt="n",axes=FALSE,cex=1.3,pch=c(21,24)[as.numeric(NCM.lat.df$Learner_type)],col="black",bg="black")+axis(1,pos=0,labels=FALSE,lwd=2)+axis(2,pos=0,labels=FALSE,lwd=2,tick=FALSE)
par(yaxt="l",xaxt="l",bty="l", tcl=-0.3)
plot(NCM.lat.df$Learning_score,NCM.lat.df$Lat_ratio_good_learners,xlim=c(0,80),ylim=c(-0.2,0.2),xaxt="n",yaxt="n",axes=FALSE,cex=1.3,pch=c(21,24)[as.numeric(NCM.lat.df$Learner_type)],col="black",bg="black")+axis(1,pos=0,labels=FALSE,lwd=2)+axis(2,pos=0,labels=FALSE,lwd=2,tick=FALSE)
par(yaxt="l",xaxt="l",bty="l")
plot(NCM.lat.df$Learning_score,NCM.lat.df$Lat_ratio_good_learners,xlim=c(0,80),ylim=c(-0.2,0.2),xaxt="n",yaxt="n",axes=FALSE,cex=2,pch=c(21,24)[as.numeric(NCM.lat.df$Learner_type)],col="black",bg="black")

abline(lm(NCM.lat.df$Lat_ratio_good_learners~NCM.lat.df$Learning_score),lwd=3)

NCM.Lat=read.csv(file.choose(),header=TRUE)
NCM.Lat.df= as.data.frame(NCM.Lat)
bar=barplot(height=NCM.Lat.df$means,names.arg=NCM.Lat.df$Learner_type,las=3,ylim=c(-0.2,0.2),lwd=2.5,axes=TRUE,col=c("#C0C0C0","#2C2C2C"))
#segments=(bar,NCM.Lat.df$means-NCM.Lat.df$se *2,bar ,NCM.Lat.df$means-NCM.Lat.df$se *2,lwd=2)
arrows(bar,NCM.Lat.df$means-NCM.Lat.df$se,bar,NCM.Lat.df$means+NCM.Lat.df$se ,lwd=1.8,angle=90,code=3,length=0.08)

sim=read.csv(file.choose(),header=TRUE)
sim.df=as.data.frame(sim)

bar=barplot(counts,beside=T,ylim=c(-2,2),xlim=c(0,50),las=3,width=4.5,space=0.4,lwd=2.5,axes=TRUE,col=c("#FFFFFF","#363535", "#FFFFFF","#363535"))
arrows(bar,sim.df$means-sim.df$se,bar,sim.df$means+sim.df$se ,lwd=1.8,angle=90,code=3,length=0.06)
plot= plot(sim.df$means,ylim=c(-1,1),las=3,lwd=2.5,axes=TRUE,pch=19)
arrows(sim.df$Group,sim.df$means-sim.df$se,sim.df$Group,sim.df$means+sim.df$se ,lwd=1.8,angle=90,code=3,length=0.03)


#ggplot to make bargraph with sem
library(ggplot2)
sim=read.csv(file.choose(),header=TRUE)
sim.df=as.data.frame(sim)
sim.df$Tutor=factor(sim.df$Tutor,levels=c("TUT1","TUT2","CON"))
sim.df$Age=factor(sim.df$Age,levels=c("55","90"))
ggplot(sim.df,aes(Tutor,BOLD,group=Tutor))+geom_bar(aes(x=Tutor,y=BOLD,fill=Tutor),stat="identity",position="dodge")+ geom_bar(aes(x=Tutor,y=BOLD,fill=Tutor),stat="identity",color="black",show_guide=FALSE, position="dodge")+ geom_errorbar(aes(ymin=BOLD,ymax=BOLD+SE),width=0.1,position=position_dodge(0.9))+scale_y_continuous(expand=c(0,0))+ scale_fill_manual(values = c( "#736F6E","#282828","#898181"))+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank())+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+  theme(axis.ticks.length=unit(.25, "cm"))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())
ggplot(sim.df,aes(Tutor,BOLD,group=Tutor))+geom_bar(aes(x=Tutor,y=BOLD,fill=Tutor),stat="identity",position="dodge")+ geom_bar(aes(x=Tutor,y=BOLD,fill=Tutor),stat="identity",color="black", position="dodge")+ geom_errorbar(aes(ymin=SD,ymax=BOLD+SE),width=0.1,position=position_dodge(0.9))+scale_y_continuous(expand=c(-0.2,0.4))+ scale_fill_manual(values = c( "#736F6E","#282828","#898181"))+theme(strip.text.x=element_blank())+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background=element_blank())+theme(legend.title=element_text(size=14),legend.text =element_text(size = 12))+  theme(axis.ticks.length=unit(.25, "cm"))+theme(axis.ticks.x = element_blank())+theme(axis.ticks.y = element_blank())
