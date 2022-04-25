#PLOT REGRESSION PLOT FIG5 IN GABA ARTICLE

NCM.lat=read.csv(file.choose(),header=TRUE)
NCM.lat.df= as.data.frame(NCM.lat)
oldpar=par(no.readonly=TRUE)
par(yaxt="l",xaxt="l",bty="l", tcl="n")
plot(NCM.lat.df$Learning_score,NCM.lat.df$Lat_ratio_good_learners,xlim=c(0,100),ylim=c(-0.2,0.2),xaxt="n",yaxt="n",axes=FALSE,cex=2,pch=c(21,24)[as.numeric(NCM.lat.df$Learner_type)],col="black",bg="black")
clip(0,100,0.2,-0.2)
abline(lm(NCM.lat.df$Lat_ratio_good_learners~NCM.lat.df$Learning_score),lwd=3)

#regression plot with axis intersecting at 0
plot(NCM.lat.df$Learning_score,NCM.lat.df$Lat_ratio_good_learners,xlim=c(0,100),ylim=c(-0.2,0.2),xaxt="n",yaxt="n",axes=FALSE,cex=1.3,pch=c(21,24)[as.numeric(NCM.lat.df$Learner_type)],col="black",bg="black")+axis(1,pos=0,labels=FALSE,lwd=2)+axis(2,pos=0,labels=FALSE,lwd=2)
clip(0,100,0.2,-0.2)
abline(lm(NCM.lat.df$Lat_ratio_good_learners~NCM.lat.df$Learning_score),lwd=3)

LI.lat=read.csv(file.choose(),header=TRUE)
LI.lat.df= as.data.frame(LI.lat)
oldpar=par(no.readonly=TRUE)
par(yaxt="l",xaxt="l",bty="l", tcl="n")
plot(LI.lat.df$Song,LI.lat.df$T2_T1,xlim=c(0,100),ylim=c(-3,3),xaxt="n",yaxt="n",axes=TRUE,cex=2,pch=c(21,24)[as.numeric(LI.lat.df$Learner)],col="black",bg="black")
clip(0,100,0.2,-0.2)
abline(lm(LI.lat.df$T2_T1~LI.lat.df$Song),lwd=3)

cor.test(LI.lat.df$Learning_Score,LI.lat.df$T2_T1,method=c("pearson"))
cor.test(LI.lat.df$Learning_Score,LI.lat.df$LI_.T2.T1_cluster,method=c("pearson"))

model=lm(T2_T1~Learner,LI.lat.df)
summary(model)
anova(model)
