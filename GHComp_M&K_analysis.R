setwd("~/2020COVID-19")

#pupal weight

fit<-read.csv("GHComp_M&K.csv", header=TRUE)
head(fit)

fit$Trial=as.character(fit$Trial)
fit$Block=as.character(fit$Block)
fit$Treatment=as.character(fit$Treatment)

# Poisson glm
pmass <- glm(PupalMass ~ Block + Trial + Treatment, data=fit, family = Gamma(link = "inverse"))
#not useful, just make sure there isn't a bunch of NA
summary(pmass)

# Goodness of fit test with Resids
Resids <- residuals(pmass, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(pmass), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(pmass), df.residual(pmass), lower.tail = F)

#Test for overdispersion
summary(pmass)$deviance/summary(pmass)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

library(emmeans)
dtm.emm <- emmeans(pmass, c("Trial", "Block", "Treatment"), type='response')
joint_tests(dtm.emm)

# Test for significance of NumPlants
dtm.emm2 <- emmeans(pmass, c("Treatment"), type='response')
joint_tests(dtm.emm2)

pairs(dtm.emm2)
dtm.emm2
CLD(dtm.emm2)

#average by number of plants
library(plyr)
ddply(fit,~Treatment,summarise,mean=mean(PupalMass),sd=sd(PupalMass))

#Graph for manuscript
library(lattice)
library(Rmisc)
SE<- summarySE(fit, measurevar="PupalMass", groupvars=c("Treatment"))
SE$Sig<- NA
SE$Sig<- c("b,c","a,b","d","a","c,d")
SE

SE$Treatment<-c("Control","Opposite Plants - Small", "Opposite Plants - Large", "Same Plant - Small", "Same Plant - Large")

library(ggplot2)
ggplot(SE, aes(x=reorder(Treatment,PupalMass), y=PupalMass, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Group") +
  ylab("Pupal Mass (mg)")+
  theme_bw()+
  geom_errorbar(aes(ymin=PupalMass-sd, ymax=PupalMass+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=PupalMass+(sd)),vjust=-1.5, size=8)+
  scale_y_continuous(expand=c(0,0), limits=c(0,2.2))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16, angle = 55, hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 20, face="bold"))





#observations to pupation

fit2<-read.csv("GHComp_M&K2.csv", header=TRUE)
head(fit2)

fit2$Trial=as.character(fit2$Trial)
fit2$Block=as.character(fit2$Block)
fit2$Treatment=as.character(fit2$Treatment)


# Poisson glm
days <- glm(DaysToP ~ Block + Trial + Treatment, data=fit2, family = Gamma(link = "inverse"))
#not useful, just make sure there isn't a bunch of NA
summary(days)

# Goodness of fit test with Resids
Resids <- residuals(days, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(days), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(days), df.residual(days), lower.tail = F)

#Test for overdispersion
summary(days)$deviance/summary(days)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

library(emmeans)
dtm.emm <- emmeans(days, c("Trial", "Block", "Treatment"), type='response')
joint_tests(dtm.emm)

# Test for significance of NumPlants
dtm.emm2 <- emmeans(days, c("Treatment"), type='response')
joint_tests(dtm.emm2)

pairs(dtm.emm2)
dtm.emm2
CLD(dtm.emm2)

#average by number of plants
library(plyr)
ddply(fit2,~Treatment,summarise,mean=mean(DaysToP),sd=sd(DaysToP))

#Graph for manuscript
library(lattice)
library(Rmisc)
SE2<- summarySE(fit2, measurevar="DaysToP", groupvars=c("Treatment"))
SE2$Sig<- NA
SE2$Sig<- c("b","a,b","b,c","a","b")
SE2$order<-c("3","2","5","1","4")
SE2$order<-as.numeric(SE2$order)
SE2

SE2$Treatment<-c("Control","Opposite Plants - Small", "Opposite Plants - Large", "Same Plant - Small", "Same Plant - Large")

library(ggplot2)
ggplot(SE2, aes(x=reorder(Treatment,order), y=DaysToP, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Group") +
  ylab("Observations Until Pupation")+
  theme_bw()+
  geom_errorbar(aes(ymin=DaysToP-sd, ymax=DaysToP+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=DaysToP+(sd)),vjust=-1.5, size=8)+
  scale_y_continuous(expand=c(0,0), limits=c(0,37))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16, angle = 55, hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 20, face="bold"))




#survival based on treatment
sur<-c(0.852,0.806,0.722)
trt<-c("control","same","opposite")
survival<-data.frame(trt,sur)
survival

chisq.test(survival$trt, survival$sur, correct=FALSE)


















