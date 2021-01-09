setwd("C:/Users/kefisher/Box Sync/Publications/Monarch Larval Biomass Consumption - GH 2017 & 2018/Analysis_022019")

##Leaves in first 24 hours

firstobs<-read.csv("012119_GH2018_FirstObservations_ForAnalysis.csv")
head(firstobs)

#average by number of plants
library(plyr)
ddply(firstobs,~NumPlants,summarise,mean=mean(Leaves),sd=sd(Leaves))
#Graph for manuscript
library(lattice)
library(Rmisc)
foleaves<- summarySE(firstobs, measurevar="Leaves", groupvars=c("NumPlants"))
foleaves

table(firstobs$Leaves)
with(firstobs, table(Trial, NumPlants))


#################################################
### Biomass Consumed by Instar at Abandonment ###
#################################################

Bio<-read.csv("012219_GH18_BiomassByInstar_NatalStem.csv", header=TRUE)

Bio$Trial=factor(Bio$Trial)
Bio$Block=factor(Bio$Block)
Bio$NumPlants=factor(Bio$NumPlants)
Bio$Year=factor(Bio$Year)
# Create Year-Block variable with 12 levels
Bio$YearBlock <- as.numeric(as.factor(paste(Bio$Year, Bio$Block, sep = "-")))
Bio$YearBlock <- as.factor(Bio$YearBlock)
Bio$MoveInstar <- as.factor(Bio$MoveInstar)
Bio$Biomass <- as.numeric(Bio$Biomass)

# Poisson glm
biomass <- glm(Biomass ~ YearBlock + Trial + NumPlants + MoveInstar + Trial:NumPlants + NumPlants:MoveInstar, data=Bio, family = Gamma(link = "inverse"))
#not useful, just make sure there isn't a bunch of NA
summary(biomass)

# Goodness of fit test with Resids
Resids <- residuals(biomass, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(biomass), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(biomass), df.residual(biomass), lower.tail = F)

#Test for overdispersion
summary(biomass)$deviance/summary(biomass)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

library(emmeans)
# Test for significance of NumPlants
dtm.emm <- emmeans(biomass, c("NumPlants", "Trial", "YearBlock", "MoveInstar"), type='response')
joint_tests(dtm.emm)

# Test for significance of NumPlants
dtm.emm2 <- emmeans(biomass, c("MoveInstar"), type='response')
joint_tests(dtm.emm2)

pairs(dtm.emm2)
dtm.emm2
CLD(dtm.emm2)

#average by number of plants
library(plyr)
ddply(Bio,~MoveInstar,summarise,mean=mean(Biomass),sd=sd(Biomass))

#Graph for manuscript
library(lattice)
library(Rmisc)
SEB<- summarySE(Bio, measurevar="Biomass", groupvars=c("MoveInstar"))
SEB$Sig<- NA
SEB$Sig<- c("*","*","**","***")
SEB

library(ggplot2)
ggplot(SEB, aes(x=MoveInstar, y=Biomass, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Instar At Natal Plant Abandonment") +
  ylab("Milkweed Biomass (mg dry biomass)")+
  theme_bw()+
  geom_errorbar(aes(ymin=Biomass-sd, ymax=Biomass+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=Biomass+(sd)),vjust=-1.5, size=8)+
  scale_y_continuous(expand=c(0,0), limits=c(0,1525))+
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

#################################################
###  Leaves Consumed by Instar at Abandonment ###
#################################################

Leaf<-read.csv("012219_GH18_LeavesByInstar_NatalStem.csv", header=TRUE)

Leaf$Trial=factor(Leaf$Trial)
Leaf$Block=factor(Leaf$Block)
Leaf$NumPlants=factor(Leaf$NumPlants)
Leaf$Year=factor(Leaf$Year)
# Create Year-Block variable with 12 levels
Leaf$YearBlock <- as.numeric(as.factor(paste(Leaf$Year, Leaf$Block, sep = "-")))
Leaf$YearBlock <- as.factor(Leaf$YearBlock)
Leaf$MoveInstar <- as.factor(Leaf$MoveInstar)

# Poisson glm
leaf <- glm(Leaves ~ YearBlock + Trial + NumPlants + MoveInstar + Trial:NumPlants + NumPlants:MoveInstar, data=Leaf, family = Gamma(link = "inverse"))
#not useful, just make sure there isn't a bunch of NA
summary(leaf)

# Goodness of fit test with Resids
Resids <- residuals(leaf, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(leaf), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(leaf), df.residual(leaf), lower.tail = F)

# Test for significance of NumPlants
dtm.emm <- emmeans(leaf, c("NumPlants", "Trial", "YearBlock", "MoveInstar"), type='response')
joint_tests(dtm.emm)

# Test for significance of NumPlants
dtm.emm2 <- emmeans(leaf, c("MoveInstar"), type='response')
joint_tests(dtm.emm2)

pairs(dtm.emm2)
dtm.emm2
CLD(dtm.emm2)

#average by number of plants
library(plyr)
ddply(Leaf,~MoveInstar,summarise,mean=mean(Biomass),sd=sd(Biomass))
head(Leaf)

#Graph for manuscript
library(lattice)
library(Rmisc)
SEL<- summarySE(Leaf, measurevar="Leaves", groupvars=c("MoveInstar"))
head(SEL)
SEL$Sig<- NA
SEL$Sig<- c("ab","a","b","c")
SEL

library(ggplot2)
ggplot(SEL, aes(x=MoveInstar, y=Leaves, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Instar At Natal Plant Abandonment") +
  ylab("Leaves with Feeding")+
  theme_bw()+
  geom_errorbar(aes(ymin=Leaves-sd, ymax=Leaves+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=Leaves+(sd)),vjust=-1.5, size=8)+
  scale_y_continuous(expand=c(0,0), limits=c(0,21))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 15))

#stats for manuscript
leaf2=Leaf$Leaves
mean(leaf2)
sd(leaf2)
max(leaf2)
min(leaf2)

getmode <- function(Neo) {
  uniqv <- unique(Neo)
  uniqv[which.max(tabulate(match(Neo, uniqv)))]
}

result<-getmode(Neo)
print(result)


#Full Development Biomass Consumption

Full<-read.csv('012219_GH18_Biomass_FullDevelopment.csv')
head(Full)

Full$Trial=factor(Full$Trial)
Full$Block=factor(Full$Block)
Full$TotalBiomass=as.numeric(Full$TotalBiomass)

#Total Biomass
biomass2 = Full$TotalBiomass
length(biomass2)
mean(biomass2)
sd(biomass2)
max(biomass2)
min(biomass2)

#Leaves with feeding
leaves2=Full$TotalLeaves
length(leaves2)
mean(leaves2)
sd(leaves2)
max(leaves2)
min(leaves2)

#Biomass from natal
bionatal=Full$NatalBiomass
length(bionatal)
mean(bionatal)
sd(bionatal)
max(bionatal)
min(bionatal)

#Biomass from Subsequent
biosub=Full$SubsequentBiomass
length(biosub)
mean(biosub)
sd(biosub)
max(biosub)
min(biosub)

#is natal and subsequent different biomass?
fullt<- read.csv("012219_GH18_Biomass_FullDevelopment_ttest.csv")
t.test(Biomass ~ Plant, fullt)
t.test(Leaves ~ Plant, fullt)
head(fullt)

library(lattice)
library(Rmisc)
SET<- summarySE(fullt, measurevar="Biomass", groupvars=c("Plant"))
SET$Sig<- NA
SET$Sig<- c("a","b")
SET

##Number of plants with feeding 2018 4 plants
head(Full)
colMeans(Full)

plants = Full$PlantsWithFeeding
length(plants)
mean(plants)
sd(plants)
max(plants)
min(plants)

#Number of Plants Visited
GH1718<-read.csv("011819_GH2017&2018_FullDevelopment_ForAnalysis.csv", header=TRUE)
head(GH1718)

library(plyr)
ddply(GH1718,~NumPlants,summarise,mean=mean(NumPlantsVisited),sd=sd(NumPlantsVisited))
library(lattice)
library(Rmisc)
numplantsv<- summarySE(GH1718, measurevar="NumPlantsVisited", groupvars=c("NumPlants","Year"))
numplantsv

numplantsv<- summarySE(GH1718, measurevar="NumPlantsVisited", groupvars=c("NumPlants"))
numplantsv


GH1718<- read.csv("011819_GH2017&2018_FullDevelopment_ForAnalysis.csv")

GH1718$Trial=factor(GH1718$Trial)
GH1718$Block=factor(GH1718$Block)
GH1718$NumPlants=factor(GH1718$NumPlants)
GH1718$Year=factor(GH1718$Year)
GH1718$YearBlock <- as.numeric(as.factor(paste(GH1718$Year, GH1718$Block, sep = "-")))
GH1718$YearBlock <- as.factor(GH1718$YearBlock)

#Reduce data frame
GH1718.complete <- GH1718[which(GH1718$Trial == 1 | GH1718$Trial == 2 | GH1718$Trial == 3 | GH1718$Trial == 4),]
nrow(GH1718)
nrow(GH1718.complete)

numplantsv<- summarySE(GH1718.complete, measurevar="NumPlantsVisited", groupvars=c("NumPlants"))
numplantsv

library(ggplot2)
ggplot(numplantsv, aes(x=NumPlants, y=NumPlantsVisited, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Number of Plants") +
  ylab("Number of plants Visited")+
  theme_bw()+
  geom_errorbar(aes(ymin=NumPlantsVisited-sd, ymax=NumPlantsVisited+sd), width=.2,
                position = position_dodge(.9))+
  scale_y_continuous(expand=c(0,0), limits=c(0,4.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 22))


library(ggplot2)
library(emmeans)

###  Proportion Analyses

#Observed on Plant Material

GH1718<- read.csv("011819_GH2017&2018_FullDevelopment_ForAnalysis.csv")

GH1718$Trial=factor(GH1718$Trial)
GH1718$Block=factor(GH1718$Block)
GH1718$NumPlants=factor(GH1718$NumPlants)
GH1718$Year=factor(GH1718$Year)
GH1718$YearBlock <- as.numeric(as.factor(paste(GH1718$Year, GH1718$Block, sep = "-")))
GH1718$YearBlock <- as.factor(GH1718$YearBlock)

#Reduce data frame
GH1718.complete <- GH1718[which(GH1718$Trial == 1 | GH1718$Trial == 2 | GH1718$Trial == 3 | GH1718$Trial == 4),]
write.csv(GH1718.complete, "GH17_ONLY.csv")
GH17<- read.csv("GH17_ONLY.csv", header = TRUE)
nrow(GH17)

GH17$Trial=factor(GH17$Trial)
GH17$Block=factor(GH17$Block)
GH17$NumPlants=factor(GH17$NumPlants)
GH17$Year=factor(GH17$Year)
GH17$YearBlock <- as.numeric(as.factor(paste(GH17$Year, GH17$Block, sep = "-")))
GH17$YearBlock <- as.factor(GH17$YearBlock)


table(GH17$RelPlant)
with(GH17, table(Trial, NumPlants))

#This is the correct model.  Your response needs to be c(succ,fail).  Becuase the model is overdispersed, you should use a family = quasibinomial rather than a simple binomial.
relplant2 <- glm(cbind(ObsOnPlant, TimesObs-ObsOnPlant) ~ Trial + NumPlants, data=GH17, family = quasibinomial, na.action = "na.omit")
summary(relplant2)

relplant2.emm <- emmeans(relplant2, c('NumPlants', 'Trial'))
relplant2.emm
joint_tests(relplant2.emm)

relplant2.emm2 <- emmeans(relplant2, 'NumPlants', type = "response")
relplant2.emm2
pairs(relplant2.emm2)

pairs(relplant2.emm2)
relplant2.emm2
CLD(relplant2.emm2)

#Graph for manuscript
library(lattice)
library(Rmisc)
SErp<- summarySE(GH17, measurevar="RelPlant", groupvars=c("NumPlants"))
SErp
SErp$Sig<- NA
SErp$Sig<- c("a","ab","b")
SErp



library(ggplot2)
ggplot(SErp, aes(x=NumPlants, y=RelPlant, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Number of Plants") +
  ylab("Proportion of Time on Plant Material")+
  theme_bw()+
  geom_errorbar(aes(ymin=RelPlant-sd, ymax=RelPlant+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=RelPlant+(sd)),vjust=-1.5, size=8)+
  scale_y_continuous(expand=c(0,0), limits=c(0,1.125))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 15))


#stats for manuscript
relp=GH1718.complete$RelPlant
length(relp)
mean(relp)
sd(relp)
max(relp)
min(relp)




