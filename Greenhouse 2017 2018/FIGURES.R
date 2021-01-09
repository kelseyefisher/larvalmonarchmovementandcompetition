setwd("C:/Users/kefisher/Box Sync/Publications/Monarch Larval Biomass Consumption - GH 2017 & 2018/Analysis_022019")
library(lattice)
library(Rmisc)
library(ggplot2)

##################################################
###                  Figure 2a                 ###
### Portion of the plant (top, middle, bottom) ###
##################################################

loc<- read.csv("031319_LocationOnStem.csv",header=TRUE)

#Graph for manuscript
SEloc<- summarySE(loc, measurevar="LocRelativeTime", groupvars=c("Location"))
SEloc$Sig<- NA
SEloc$Sig<- c("***","**","*")
SEloc
SEloc$sdL = SEloc$sd
SEloc[1, 8] = 0.05629427
SEloc

a<-ggplot(SEloc, aes(x=reorder(Location,-LocRelativeTime), y=LocRelativeTime, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Portion of Milkweed Ramet") +
  ylab("Relative Time")+
  theme_bw()+
  geom_errorbar(aes(ymin=LocRelativeTime-sdL, ymax=LocRelativeTime+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=LocRelativeTime+(sd)),vjust=-.5, size=7)+
  scale_y_continuous(expand=c(0,0), limits=c(0,1.0))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 12))

a

ggsave("fig2a_format.png", plot=a, width=4.68, height=4.68, units="in")


#######################################################
###                     Figure 2b                   ###
### Portion of the leaf (top, under, new veg, stem) ###
#######################################################

sur<- read.csv("031319_LocationOnLeaf.csv",header=TRUE)

#Graph for manuscript
SEsur<- summarySE(sur, measurevar="SurRelativeTime", groupvars=c("LeafSurf"))
SEsur$Sig<- NA
SEsur$Sig<- c("*","***","*", "**")
SEsur

SEsur$sdL = SEsur$sd
SEsur[2, 8] = 0.02868232
SEsur


order <- c("New Growth", "Top", "Underside", "Stem")
b<-ggplot(SEsur, aes(x=LeafSurf, y=SurRelativeTime, width=.6))+
  scale_x_discrete(limits = order)+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Leaf Surface") +
  ylab("Relative Time")+
  theme_bw()+
  geom_errorbar(aes(ymin=SurRelativeTime-sdL, ymax=SurRelativeTime+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=SurRelativeTime+(sd)),vjust=-.5, size=7)+
  scale_y_continuous(expand=c(0,0), limits=c(0,1.0))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 12))

b

ggsave("fig2b_format.png", plot=b, width=4.68, height=4.68, units="in")

#################################
###         Figure 3          ###
### On Stem Movement by Instar###
#################################

Mvt<-read.csv("032519_OnRametMovement2018.csv", header=TRUE)

#Graph for manuscript
SEB<- summarySE(Mvt, measurevar="On.RametMvt", groupvars=c("MoveInstar"))
SEB$Sig<- NA
SEB$Sig<- c("*","*","*","**")
SEB

SEB$sdL = SEB$sd
SEB
SEB[1, 8] = 0.875000
SEB[2, 8] = 1.642857
SEB

c<-ggplot(SEB, aes(x=MoveInstar, y=On.RametMvt, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Instar At Natal Plant Abandonment") +
  ylab("On-Ramet Movements \nBefore Natal Abandonment")+
  theme_bw()+
  geom_errorbar(aes(ymin=On.RametMvt-sdL, ymax=On.RametMvt+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=On.RametMvt+(sd)),vjust=-.5, size=7)+
  scale_y_continuous(expand=c(0,0), limits=c(0,12.0))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 12))

c

ggsave("fig3_format.png", plot=c, width=4.68, height=4.68, units="in")

#############################
###       Figure 4A       ###
### Abandonment by Instar ###
#############################

mvt<- read.csv("032519_Move&AbanByInstar2017.csv", header=TRUE)

#Graph for manuscript
SEB<- summarySE(mvt, measurevar="Abandonments", groupvars=c("Instar"))
SEB$Sig<- NA
SEB$Sig<- c("*","*","*","**","***")

SEB$sdL = SEB$sd
SEB
SEB[1, 8] = 0
SEB[2, 8] = 0.03
SEB[3, 8] = 0.23376623
SEB[4, 8] = 0.68
SEB

d<-ggplot(SEB, aes(x=Instar, y=Abandonments, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Instar") +
  ylab("Ramet Abandonments")+
  theme_bw()+
  geom_errorbar(aes(ymin=Abandonments-sdL, ymax=Abandonments+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=Abandonments+(sd)),vjust=-.5, size=7)+
  scale_y_continuous(expand=c(0,0), limits=c(0,5.0))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 12))

d

ggsave("fig4b_format.png", plot=d, width=4.68, height=4.68, units="in")

#######################################
###            Figure 4B            ###
### Among Plant Movements by Instar ###
#######################################

mvt<- read.csv("032519_Move&AbanByInstar2017.csv", header=TRUE)

#Graph for manuscript
SEB<- summarySE(mvt, measurevar="Movements", groupvars=c("Instar"))
SEB$Sig<- NA
SEB$Sig<- c("*","*","**","***","****")
SEB

SEB$sdL = SEB$sd
SEB
SEB[1, 8] = 0.01298701
SEB[2, 8] = 0.03
SEB[3, 8] = 0.25
SEB[4, 8] = 0.8
SEB

e<-ggplot(SEB, aes(x=Instar, y=Movements, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Instar") +
  ylab("Among Ramet Movements")+
  theme_bw()+
  geom_errorbar(aes(ymin=Movements-sdL, ymax=Movements+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=Movements+(sd)),vjust=-.5, size=7)+
  scale_y_continuous(expand=c(0,0), limits=c(0,5.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 12))

ggsave("figure4a.png", plot=e, width=4.68, height=4.68, units="in")


#################################################
###                 Figure 5A                 ###
### Biomass Consumed by Instar at Abandonment ###
#################################################

Bio<-read.csv("012219_GH18_BiomassByInstar_NatalStem.csv", header=TRUE)

#Graph for manuscript
SEB<- summarySE(Bio, measurevar="Biomass", groupvars=c("MoveInstar"))
SEB$Sig<- NA
SEB$Sig<- c("*","*","**","***")
SEB

f<-ggplot(SEB, aes(x=MoveInstar, y=Biomass, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Instar At Natal Ramet Abandonment") +
  ylab("Milkweed Biomass (mg dry weight)")+
  theme_bw()+
  geom_errorbar(aes(ymin=Biomass-sd, ymax=Biomass+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=Biomass+(sd)),vjust=-.5, size=6)+
  scale_y_continuous(expand=c(0,0), limits=c(0,1525))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 12))

f

ggsave("fig5a_format.png", plot=f, width=4.68, height=4.68, units="in")


#################################################
###                 Figure 5B                 ###
###  Leaves Consumed by Instar at Abandonment ###
#################################################

Leaf<-read.csv("012219_GH18_LeavesByInstar_NatalStem.csv", header=TRUE)

#Graph for manuscript
SEL<- summarySE(Leaf, measurevar="Leaves", groupvars=c("MoveInstar"))
head(SEL)
SEL$Sig<- NA
SEL$Sig<- c("*","*","**","***")
SEL$Sig2<-c("**", "", "", "")
SEL

g<- ggplot(SEL, aes(x=MoveInstar, y=Leaves, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Instar At Natal Ramet Abandonment") +
  ylab("Number of Milkweed Leaves \nwith Feeding Injury")+
  theme_bw()+
  geom_errorbar(aes(ymin=Leaves-sd, ymax=Leaves+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=Leaves+(sd)),vjust=-.5, size=6)+
  geom_text(aes(label=Sig2, y=Leaves+(sd*2)),vjust=-.5, size=6)+
  scale_y_continuous(expand=c(0,0), limits=c(0,21))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 12))

g

ggsave("fig5b_format.png", plot=g, width=4.68, height=4.68, units="in")


###################################
###                 Figure 6    ###
###  Observed on Plant Material ###
###################################

GH17<- read.csv("GH17_ONLY.csv", header = TRUE)

#Graph for manuscript
SErp<- summarySE(GH17, measurevar="RelPlant", groupvars=c("NumPlants"))
SErp
SErp$Sig<- NA
SErp$Sig<- c("*","*","**")
SErp$Sig2<- c("","**","")
SErp

h<-ggplot(SErp, aes(x=NumPlants, y=RelPlant, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Number of Ramets") +
  ylab("Proportion of Observations on Ramet")+
  theme_bw()+
  geom_errorbar(aes(ymin=RelPlant-sd, ymax=RelPlant+sd), width=.2,
                position = position_dodge(.9))+
  geom_text(aes(label=Sig, y=RelPlant+(sd)),vjust=-.5, size=6)+
  geom_text(aes(label=Sig2, y=RelPlant+(sd*2)),vjust=-.5, size=6)+
  scale_y_continuous(expand=c(0,0), limits=c(0,1.15), breaks=c(0.25,0.50,0.75,1.00))+
  scale_x_continuous(breaks=c(2,3,4))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 12))

h

ggsave("fig6_format.png", plot=h, width=4.68, height=4.68, units="in")
