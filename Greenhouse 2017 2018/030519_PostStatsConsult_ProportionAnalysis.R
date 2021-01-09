###  Proportion Analyses

#Observed on Plant Material

GH1718<- read.csv("011819_GH2017&2018_FullDevelopment_ForAnalysis.csv")

head(GH1718)

GH1718$Trial=factor(GH1718$Trial)
GH1718$Block=factor(GH1718$Block)
GH1718$NumPlants=factor(GH1718$NumPlants)
GH1718$Year=factor(GH1718$Year)
GH1718$YearBlock <- as.numeric(as.factor(paste(GH1718$Year, GH1718$Block, sep = "-")))
GH1718$YearBlock <- as.factor(GH1718$YearBlock)

#Reduce data frame
GH1718.complete <- GH1718[which(GH1718$Trial == 1 | GH1718$Trial == 2 | GH1718$Trial == 3 | GH1718$Trial == 4),]
head(GH1718.complete)
nrow(GH1718)
nrow(GH1718.complete)

relplant <- glm(RelPlant ~ NumPlants + Trial + Trial:NumPlants + YearBlock, data=GH1718.complete, family = binomial, na.action = "na.omit")
#warning not error
summary(relplant)

#Test for overdispersion
summary(relplant)$deviance/summary(relplant)$df.residual
#Way underdispersed - inference is very conservative

#Test for significance
dtm.emm <- emmeans(relplant, c("NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)

#Graph for manuscript
library(lattice)
library(Rmisc)
SErp<- summarySE(GH1718.complete, measurevar="RelPlant", groupvars=c("NumPlants"))
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
  scale_y_continuous(expand=c(0,0), limits=c(0,1.1))+
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


