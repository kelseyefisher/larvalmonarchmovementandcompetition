
##################################################
### Portion of the plant (top, middle, bottom) ###
##################################################

loc<- read.csv("031319_LocationOnStem.csv",header=TRUE)

loc$Trial=factor(loc$Trial)
loc$Block=factor(loc$Block)
loc$NumPlants=factor(loc$NumPlants)
loc$Year=factor(loc$Year)
loc$YearBlock <- as.numeric(as.factor(paste(loc$Year, loc$Block, sep = "-")))
loc$YearBlock <- as.factor(loc$YearBlock)

head(loc)

location <- glm(LocRelativeTime ~ Location + NumPlants + Trial + Trial:NumPlants + YearBlock, data=loc, family = binomial, na.action = "na.omit")
#warning not error
summary(location)

Resids <- residuals(location, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(location), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(location), df.residual(location), lower.tail = F)

#Test for overdispersion
summary(location)$deviance/summary(location)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

#Test for overdispersion
summary(location)$deviance/summary(location)$df.residual
#Way underdispersed - inference is very conservative

#Test for significance
library(emmeans)
dtm.emm <- emmeans(location, c("Location", "NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)

# Test for significance of NumPlants
dtm.emm2 <- emmeans(location, c("Location"), type='response')
joint_tests(dtm.emm2)

pairs(dtm.emm2)
dtm.emm2
CLD(dtm.emm2)

#Graph for manuscript
library(lattice)
library(Rmisc)
SEloc<- summarySE(loc, measurevar="LocRelativeTime", groupvars=c("Location"))
SEloc$Sig<- NA
SEloc$Sig<- c("a","b","c")
SEloc
SEloc$sdL = SEloc$sd
SEloc[1, 8] = 0.05629427
SEloc


library(ggplot2)
ggplot(SEloc, aes(x=reorder(Location,-LocRelativeTime), y=LocRelativeTime, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Portion of Milkweed Ramet") +
  ylab("Relative Time")+
  theme_bw()+
  geom_errorbar(aes(ymin=LocRelativeTime-sdL, ymax=LocRelativeTime+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=LocRelativeTime+(sd)),vjust=-1.5, size=8)+
  scale_y_continuous(expand=c(0,0), limits=c(0,1.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 15))


#######################################################
### Portion of the leaf (top, under, new veg, stem) ###
#######################################################

sur<- read.csv("031319_LocationOnLeaf.csv",header=TRUE)

sur$Trial=factor(sur$Trial)
sur$Block=factor(sur$Block)
sur$NumPlants=factor(sur$NumPlants)
sur$Year=factor(sur$Year)
sur$YearBlock <- as.numeric(as.factor(paste(sur$Year, sur$Block, sep = "-")))
sur$YearBlock <- as.factor(sur$YearBlock)

surface <- glm(SurRelativeTime ~ LeafSurf + NumPlants + Trial + Trial:NumPlants + YearBlock, data=sur, family = binomial, na.action = "na.omit")
#warning not error
summary(surface)

Resids <- residuals(surface, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(surface), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(surface), df.residual(surface), lower.tail = F)

#Test for overdispersion
summary(surface)$deviance/summary(surface)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

#Test for overdispersion
summary(surface)$deviance/summary(surface)$df.residual
#Way underdispersed - inference is very conservative

#Test for significance
library(emmeans)
dtm.emm <- emmeans(surface, c("LeafSurf", "NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)

# Test for significance of NumPlants
dtm.emm2 <- emmeans(surface, c("LeafSurf"), type='response')
joint_tests(dtm.emm2)

pairs(dtm.emm2)
dtm.emm2
CLD(dtm.emm2)

#Graph for manuscript
library(lattice)
library(Rmisc)
SEsur<- summarySE(sur, measurevar="SurRelativeTime", groupvars=c("LeafSurf"))
SEsur$Sig<- NA
SEsur$Sig<- c("a","c","a", "b")
SEsur

SEsur$sdL = SEsur$sd
SEsur[2, 8] = 0.02868232
SEsur



library(ggplot2)
order <- c("New Growth", "Top", "Underside", "Stem")
ggplot(SEsur, aes(x=LeafSurf, y=SurRelativeTime, width=.6))+
  scale_x_discrete(limits = order)+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Portion of Leaf Surface") +
  ylab("Relative Time")+
  theme_bw()+
  geom_errorbar(aes(ymin=SurRelativeTime-sdL, ymax=SurRelativeTime+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=SurRelativeTime+(sd)),vjust=-1.5, size=8)+
  scale_y_continuous(expand=c(0,0), limits=c(0,1.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 15))



#################################
### On Stem Movement by Instar###
#################################

Mvt<-read.csv("032519_OnRametMovement2018.csv", header=TRUE)

Mvt$Trial=factor(Mvt$Trial)
Mvt$Block=factor(Mvt$Block)
Mvt$NumPlants=factor(Mvt$NumPlants)
Mvt$Year=factor(Mvt$Year)
# Create Year-BMvtk variable with 12 levels
Mvt$YearBlock <- as.numeric(as.factor(paste(Mvt$Year, Mvt$Block, sep = "-")))
Mvt$YearBlock <- as.factor(Mvt$YearBlock)
Mvt$MoveInstar <- as.factor(Mvt$MoveInstar)

# Poisson glm
move <- glm(On.RametMvt ~ YearBlock + Trial + NumPlants + MoveInstar + Trial:NumPlants, data=Mvt, family = poisson(link = "log"))
#not useful, just make sure there isn't a bunch of NA
summary(move)

# Goodness of fit test with Resids
Resids <- residuals(move, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(move), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(move), df.residual(move), lower.tail = F)

#Test for overdispersion
summary(move)$deviance/summary(move)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

# Test for significance of NumPlants
library(emmeans)
dtm.emm <- emmeans(move, c("NumPlants", "Trial", "YearBlock", "MoveInstar"), type='response')
joint_tests(dtm.emm)

# Test for significance of NumPlants
dtm.emm2 <- emmeans(move, c("MoveInstar"), type='response')
joint_tests(dtm.emm2)

pairs(dtm.emm2)
dtm.emm2
CLD(dtm.emm2)

#average by number of plants
library(plyr)
ddply(Mvt,~MoveInstar,summarise,mean=mean(On.RametMvt),sd=sd(On.RametMvt))

#Graph for manuscript
library(lattice)
library(Rmisc)
SEB<- summarySE(Mvt, measurevar="On.RametMvt", groupvars=c("MoveInstar"))
SEB$Sig<- NA
SEB$Sig<- c("a","a","a","b")
SEB

SEB$sdL = SEB$sd
SEB
SEB[1, 8] = 0.875000
SEB[2, 8] = 1.642857
SEB

library(ggplot2)
ggplot(SEB, aes(x=MoveInstar, y=On.RametMvt, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Instar At Natal Plant Abandonment") +
  ylab("On-Ramet Movements \nBefore Natal Abandonment")+
  theme_bw()+
  geom_errorbar(aes(ymin=On.RametMvt-sdL, ymax=On.RametMvt+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=On.RametMvt+(sd)),vjust=-1.5, size=8)+
  scale_y_continuous(expand=c(0,0), limits=c(0,12.2))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 15))

#stats for manuscript
Bio2=Bio$Biomass
mean(Bio2)
sd(Bio2)
max(Bio2)
min(Bio2)

#############################################################################################################
###                                             Only 2017 Data                                            ###
###########################################################################################################

#############################
### Among Plant Movements ###
#############################

GH1718<- read.csv("011819_GH2017&2018_FullDevelopment_ForAnalysis.csv")

GH1718$Trial=factor(GH1718$Trial)
GH1718$Block=factor(GH1718$Block)
GH1718$NumPlants=factor(GH1718$NumPlants)
GH1718$Year=factor(GH1718$Year)
GH1718$YearBlock <- as.numeric(as.factor(paste(GH1718$Year, GH1718$Block, sep = "-")))
GH1718$YearBlock <- as.factor(GH1718$YearBlock)

GH1718.complete <- GH1718[which(GH1718$Trial == 1 | GH1718$Trial == 2 | GH1718$Trial == 3 | GH1718$Trial == 4),]
nrow(GH1718)
nrow(GH1718.complete)

head(GH1718.complete)

# Poisson glm
among <- glm(TimesMoved ~ YearBlock + Trial + NumPlants + Trial:NumPlants, data=GH1718.complete, family = poisson(link = "log"))
#not useful, just make sure there isn't a bunch of NA
summary(among)

# Goodness of fit test with Resids
Resids <- residuals(among, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(among), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(among), df.residual(among), lower.tail = F)

#Test for overdispersion
summary(among)$deviance/summary(among)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

# Test for significance of NumPlants
dtm.emm <- emmeans(among, c("NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)

#Graph for manuscript
library(lattice)
library(Rmisc)
SEamong<- summarySE(GH1718.complete, measurevar="TimesMoved", groupvars=c("NumPlants"))
SEamong

library(ggplot2)
ggplot(SEamong, aes(x=NumPlants, y=TimesMoved, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Number of Plants") +
  ylab("Total Among Plant Movements")+
  theme_bw()+
  geom_errorbar(aes(ymin=TimesMoved-sd, ymax=TimesMoved+sd), width=.2,
                position = position_dodge(.9))+
  scale_y_continuous(expand=c(0,0), limits=c(0,6.2))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 15))

#stats for manuscript
among2=GH1718.complete$TimesMoved
length(among2)
mean(among2)
sd(among2)
max(among2)
min(among2)

getmode <- function(among2) {
  uniqv <- unique(among2)
  uniqv[which.max(tabulate(match(among2, uniqv)))]
}

result<-getmode(among2)
print(result)

################################
### Total Plant Abandonments ###
################################

# Poisson glm
aban <- glm(PlantAbandonment ~ YearBlock + Trial + NumPlants + Trial:NumPlants, data=GH1718.complete, family = poisson(link = "log"))
#not useful, just make sure there isn't a bunch of NA
summary(aban)

# Goodness of fit test with Resids
Resids <- residuals(aban, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(aban), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(aban), df.residual(aban), lower.tail = F)

#Test for overdispersion
summary(aban)$deviance/summary(aban)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

# Test for significance of NumPlants
dtm.emm <- emmeans(aban, c("NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)

#Graph for manuscript
library(lattice)
library(Rmisc)
SEaban<- summarySE(GH1718.complete, measurevar="PlantAbandonment", groupvars=c("NumPlants"))
SEaban

library(ggplot2)
ggplot(SEaban, aes(x=NumPlants, y=PlantAbandonment, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Number of Plants") +
  ylab("Total Plant Abandonments")+
  theme_bw()+
  geom_errorbar(aes(ymin=PlantAbandonment-sd, ymax=PlantAbandonment+sd), width=.2,
                position = position_dodge(.9))+
  scale_y_continuous(expand=c(0,0), limits=c(0,5.2))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 15))

#stats for manuscript
aban2=GH1718.complete$PlantAbandonment
length(aban2)
mean(aban2)
sd(aban2)
max(aban2)
min(aban2)

getmode <- function(aban2) {
  uniqv <- unique(aban2)
  uniqv[which.max(tabulate(match(aban2, uniqv)))]
}

result<-getmode(aban2)
print(result)

#############################
### Abandonment by Instar ###
#############################

mvt<- read.csv("032519_Move&AbanByInstar2017.csv", header=TRUE)

mvt$Trial=factor(mvt$Trial)
mvt$Block=factor(mvt$Block)
mvt$NumPlants=factor(mvt$NumPlants)
mvt$Year=factor(mvt$Year)
mvt$YearBlock <- as.numeric(as.factor(paste(mvt$Year, mvt$Block, sep = "-")))
mvt$YearBlock <- as.factor(mvt$YearBlock)
mvt$Instar <- as.factor(mvt$Instar)

move<-glm(Abandonments ~ YearBlock + Trial + NumPlants + Instar + Trial:NumPlants + NumPlants:Instar, data=mvt, family = poisson(link = "log"))
summary

# Goodness of fit test with Resids
Resids <- residuals(move, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(move), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(move), df.residual(move), lower.tail = F)

#Test for overdispersion
summary(move)$deviance/summary(move)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

# Test for significance of NumPlants
dtm.emm <- emmeans(move, c("NumPlants", "Trial", "YearBlock", "Instar"), type='response')
joint_tests(dtm.emm)

# Test for significance of NumPlants
dtm.emm2 <- emmeans(move, c("Instar"), type='response')
joint_tests(dtm.emm2)

pairs(dtm.emm2)
dtm.emm2
CLD(dtm.emm2)

#average by number of plants
library(plyr)
ddply(mvt,~Instar,summarise,mean=mean(Abandonments),sd=sd(Abandonments))

#Graph for manuscript
library(lattice)
library(Rmisc)
SEB<- summarySE(mvt, measurevar="Abandonments", groupvars=c("Instar"))
SEB$Sig<- NA
SEB$Sig<- c("a","a","a","b","c")

SEB$sdL = SEB$sd
SEB
SEB[1, 8] = 0
SEB[2, 8] = 0.03
SEB[3, 8] = 0.23376623
SEB[4, 8] = 0.68
SEB


library(ggplot2)
ggplot(SEB, aes(x=Instar, y=Abandonments, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Instar") +
  ylab("Ramet Abandonments")+
  theme_bw()+
  geom_errorbar(aes(ymin=Abandonments-sdL, ymax=Abandonments+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=Abandonments+(sd)),vjust=-1.5, size=8)+
  scale_y_continuous(expand=c(0,0), limits=c(0,4.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 15))

#######################################
### Among Plant Movements by Instar ###
#######################################

mvt<- read.csv("032519_Move&AbanByInstar2017.csv", header=TRUE)

mvt$Trial=factor(mvt$Trial)
mvt$Block=factor(mvt$Block)
mvt$NumPlants=factor(mvt$NumPlants)
mvt$Year=factor(mvt$Year)
mvt$YearBlock <- as.numeric(as.factor(paste(mvt$Year, mvt$Block, sep = "-")))
mvt$YearBlock <- as.factor(mvt$YearBlock)
mvt$Instar <- as.factor(mvt$Instar)

move<-glm(Movements ~ YearBlock + Trial + NumPlants + Instar + Trial:NumPlants + NumPlants:Instar, data=mvt, family = poisson(link = "log"))
summary

# Goodness of fit test with Resids
Resids <- residuals(move, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(move), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(move), df.residual(move), lower.tail = F)

#Test for overdispersion
summary(move)$deviance/summary(move)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

# Test for significance of NumPlants
dtm.emm <- emmeans(move, c("NumPlants", "Trial", "YearBlock", "Instar"), type='response')
joint_tests(dtm.emm)

# Test for significance of NumPlants
dtm.emm2 <- emmeans(move, c("Instar"), type='response')
joint_tests(dtm.emm2)

pairs(dtm.emm2)
dtm.emm2
CLD(dtm.emm2)

#average by number of plants
library(plyr)
ddply(mvt,~Instar,summarise,mean=mean(Movements),sd=sd(Movements))

#Graph for manuscript
library(lattice)
library(Rmisc)
SEB<- summarySE(mvt, measurevar="Movements", groupvars=c("Instar"))
SEB$Sig<- NA
SEB$Sig<- c("a","a","b","c","d")
SEB

SEB$sdL = SEB$sd
SEB
SEB[1, 8] = 0.01298701
SEB[2, 8] = 0.03
SEB[3, 8] = 0.25
SEB[4, 8] = 0.8
SEB


library(ggplot2)
ggplot(SEB, aes(x=Instar, y=Movements, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Instar") +
  ylab("Among Ramet Movements")+
  theme_bw()+
  geom_errorbar(aes(ymin=Movements-sdL, ymax=Movements+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=Movements+(sd)),vjust=-1.5, size=8)+
  scale_y_continuous(expand=c(0,0), limits=c(0,5.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 15))

#############################
##Movement at day vs night

ampm<- read.csv("012219_AMPM_Movement_GH17.csv", header=TRUE)

ampm$MoveAtNight<-ampm$AM
ampm$MoveDuringDay<-ampm$PM

length(ampm$MoveDuringDay)
mean(ampm$MoveDuringDay)
max(ampm$MoveDuringDay)
min(ampm$MoveDuringDay)
sum(ampm$MoveDuringDay)

length(ampm$MoveAtNight)
mean(ampm$MoveAtNight)
max(ampm$MoveAtNight)
min(ampm$MoveAtNight)
sum(ampm$MoveAtNight)



ampm2<-read.csv("030719_AMPM_Movement_GH17.csv", header=TRUE)
ampm2$Movements<-as.numeric(ampm2$Movements)

ampmplot<- summarySE(ampm2, measurevar="Movements", groupvars=c("Time", "Plants"))
ampmplot

ampmplot2<- summarySE(ampm2, measurevar="Movements", groupvars=c("Time"))
ampmplot2

wilcox.test(ampm$AM, ampm$PM, paired = TRUE, alternative = "two.sided")

library(ggplot2)
ggplot(ampmplot2, aes(x=Time, y=Movements, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Time") +
  ylab("Number of Movements")+
  theme_bw()+
  geom_errorbar(aes(ymin=Movements-sd, ymax=Movements+sd), width=.2,
                position = position_dodge(.9))+
  scale_y_continuous(expand=c(0,0), limits=c(0,4.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 22))

