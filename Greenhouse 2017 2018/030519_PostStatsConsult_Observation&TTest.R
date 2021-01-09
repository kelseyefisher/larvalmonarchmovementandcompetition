##################################################
###### 2017 and 2018 Observation Statistics ######
##################################################

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

#Full Development Biomass Consumption

Full<-read.csv('012219_GH18_Biomass_FullDevelopment.csv')

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
head(fullt)

library(lattice)
library(Rmisc)
SET<- summarySE(fullt, measurevar="Biomass", groupvars=c("Plant"))
SET$Sig<- NA
SET$Sig<- c("a","b")
SET

test<-read.csv('TEST.csv', header=TRUE)
head(test)

library(ggplot2)
ggplot(test, aes(x=Plant, y=Biomass, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Plant with Feeding") +
  ylab("Dry Plant Biomass (mg)")+
  theme_bw()+
  geom_errorbar(aes(ymin=Biomass-sdl, ymax=Biomass+sdu), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=Biomass+(sdu)),vjust=-1.5, size=8)+
  scale_y_continuous(expand=c(0,0), limits=c(0,1700))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=20))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=20))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 24))






#leaves from natal
leavesnat=Full$NatalLeaves
length(leavesnat)
mean(leavesnat)
sd(leavesnat)
max(leavesnat)
min(leavesnat)

#leaves from subsequent
leavessub=Full$SubseLeaves
length(leavessub)
mean(leavessub)
sd(leavessub)
max(leavessub)
min(leavessub)

#leaves?
t.test(Leaves ~ Plant, fullt)

#Number of Plants with Feeding
plants2=Full$PlantsWithFeeding
length(plants2)
mean(plants2)
sd(plants2)
max(plants2)
min(plants2)

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




#############
##Larval detection error is currently in excel sheets
#############


