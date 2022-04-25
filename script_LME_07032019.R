library(nlme)
library(lme4)
library(car)
library(lsmeans)
library(ggplot2)

#using effect coding to do linear mixed modeling

NCM=read.csv(file.choose(),header=TRUE)# used alexa_interneurons_NCM_updated csv file
NCM.df=as.data.frame(NCM)
#log transformation
attach(NCM.df)
subject=factor(BIRD)
Preference=factor(Preference)
Hemisphere=factor(Hemisphere)
Brain_region=factor(Brain_Region)
Preference=as.factor(1*(Preference=="NO")+2*(Preference=="Pref"))
Hemisphere=as.factor(1*(Hemisphere=="Left")+2*(Hemisphere=="Right"))
View(NCM1)
Brain_region= as.factor(1*(Brain_Region=="NCM")+2*(Brain_Region=="CMM"))
contrasts(Preference)=contr.sum
contrasts(Brain_region)=contr.sum
contrasts(Brain_region)
contrasts(Hemisphere)=contr.sum
modellme=lme(BOLD~Preference*Hemisphere,random=~1|subject,data=NCM.df,na.action=na.omit)
modellmer=lmer(Density~Group*Hemisphere*Brain_region +(1|subject),data=NCM.df,na.action=na.omit)
summary(modellmer)
summary(modellme)
Anova(modellme,test.statistic = "F")
Anova(modellmer,test.statistic = "F")
# testing normality of residuals
qqnorm(resid(modellme))
qqline(resid(modellme))
plot(resid(modellmer))
lsm=lsmeans(modellme,~Preference*Hemisphere)
contrast(lsm,interaction="pairwise",adjust="Bonferroni")
lsm1=lsmeans(modellme,~Group*Celltype|Hemisphere)
contrast(lsm1,interaction="pairwise",adjust="Bonferroni")
lsm2= lsmeans(modellme,~Celltype|Hemisphere)
contrast(lsm2,interaction="pairwise",adjust="Bonferroni")
lsm3=lsmeans(modellme,~Celltype)
contrast(lsm3,interaction="pairwise",adjust="Bonferroni")
# same lme for HVC

#using effect coding to do linear mixed modeling

HVC=read.csv(file.choose(),header=TRUE)# used alexa_interneurons_NCM_updated csv file
HVC.df=as.data.frame(HVC)
#log transformation
attach(HVC.df)
subject=factor(Bird_Id)
Group=factor(Learner_type)
Hemisphere=factor(Hemisphere)
Celltype=factor(Cell_type)
GroupGB=as.factor(1*(Group=="Bad")+2*(Group=="Good"))
Hemisphere=as.factor(1*(Hemisphere=="RH")+2*(Hemisphere=="LH"))
#Region=as.factor(1*(Region=="Lateral")+2*(Region=="Medial"))

Celltype= as.factor(1*(Celltype=="CB")+2*(Celltype=="aPV")+3*(Celltype=="CT"))

contrasts(Group)=contr.sum
contrasts(Celltype)=contr.sum
contrasts(Celltype)
contrasts(Hemisphere)=contr.sum
contrasts(Region)=contr.sum
modellme=lme(log(Density_interneurons)~Group*Hemisphere*Celltype,random=~1|subject,data=HVC.df,na.action=na.omit)
modellmer=lmer(log(Density_interneurons)~Group*Hemisphere*Celltype+(1|subject),data=HVC.df,na.action=na.omit)
summary(modellme)
Anova(modellmer,test.statistic = "F")
Anova(modellme,test.statistic = "F")
# testing normality of residuals
qqnorm(resid(modellme))
qqline(resid(modellme))
shapiro.test(resid(modellme))
lsm=lsmeans(modellmer,~Group*Hemisphere |Celltype)
contrast(lsm,interaction="pairwise",adjust="Bonferroni")
lsm1=lsmeans(modellme,~Group*Celltype|Hemisphere)
contrast(lsm1,interaction="pairwise",adjust="Bonferroni")
