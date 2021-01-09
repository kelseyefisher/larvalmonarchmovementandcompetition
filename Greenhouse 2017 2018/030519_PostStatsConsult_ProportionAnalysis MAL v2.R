library(ggplot2)
library(emmeans)

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

table(GH1718$RelPlant)
with(GH1718, table(Trial, NumPlants))

#This is the correct model.  Your response needs to be c(succ,fail).  Becuase the model is overdispersed, you should use a family = quasibinomial rather than a simple binomial.
relplant2 <- glm(cbind(ObsOnPlant, TimesObs-ObsOnPlant) ~ Trial + NumPlants, data=GH1718, family = quasibinomial, na.action = "na.omit")
summary(relplant2)

relplant2.emm <- emmeans(relplant2, c('NumPlants', 'Trial'))
relplant2.emm
joint_tests(relplant2.emm)

relplant2.emm2 <- emmeans(relplant2, 'NumPlants', type = "response")
relplant2.emm2
pairs(relplant2.emm2)

