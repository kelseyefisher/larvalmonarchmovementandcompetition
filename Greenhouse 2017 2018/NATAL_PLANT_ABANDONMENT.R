Natal<-read.csv("011819_GH17&18_Abandonment.csv")

Natal$Trial=factor(Natal$Trial)
Natal$Block=factor(Natal$Block)
Natal$NumPlants=factor(Natal$NumPlants)
Natal$Year=factor(Natal$Year)
# Create Year-Block variable with 12 levels
Natal$YearBlock <- as.numeric(as.factor(paste(Natal$Year, Natal$Block, sep = "-")))
Natal$YearBlock <- as.factor(Natal$YearBlock)


#################################
### Days to Natal Abandonment ###
#################################

# Poisson glm
daystomove <- glm(DaysToMove ~ YearBlock + Trial + NumPlants + Trial:NumPlants, data=Natal, family = poisson(link = "log"))
#not useful, just make sure there isn't a bunch of NA
summary(daystomove)

# Goodness of fit test with Resids
Resids <- residuals(daystomove, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(daystomove), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(daystomove), df.residual(daystomove), lower.tail = F)

# Test for significance of NumPlants
dtm.emm <- emmeans(daystomove, c("NumPlants", "Trial", "YearBlock"), type='response')
dtm.emm
joint_tests(dtm.emm)

#average by number of plants
library(plyr)
ddply(Natal,~NumPlants,summarise,mean=mean(DaysToMove),sd=sd(DaysToMove))

#Graph for manuscript
library(lattice)
library(Rmisc)
SEDays<- summarySE(Natal, measurevar="DaysToMove", groupvars=c("NumPlants"))
SEDays

library(ggplot2)
ggplot(SEDays, aes(x=NumPlants, y=DaysToMove, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Number of Plants") +
  ylab("Days Until Natal Plant Abandonment")+
  theme_bw()+
  geom_errorbar(aes(ymin=DaysToMove-sd, ymax=DaysToMove+sd), width=.2,
                position = position_dodge(.9))+
  scale_y_continuous(expand=c(0,0), limits=c(0,10.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 15))

Days=Natal$DaysToMove
mean(Days)
sd(Days)
max(Days)
min(Days)


###################################
### Instar at Natal Abandonment ###
###################################

# Poisson glm
instar <- glm(InstarToMove ~ YearBlock + Trial + NumPlants + Trial:NumPlants, data=Natal, family = poisson(link = "log"))
#not useful, just make sure there isn't a bunch of NA
summary(instar)

Resids <- residuals(instar, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(instar), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(instar), df.residual(instar), lower.tail = F)

#Test for overdispersion
summary(instar)$deviance/summary(instar)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

# Test for significance of NumPlants
dtm.emm <- emmeans(instar, c("NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)

#average by number of plants
library(plyr)
ddply(Natal,~NumPlants,summarise,mean=mean(InstarToMove),sd=sd(InstarToMove))

#Graph for manuscript
library(lattice)
library(Rmisc)
SEInstar<- summarySE(Natal, measurevar="InstarToMove", groupvars=c("NumPlants"))
SEInstar

library(ggplot2)
ggplot(SEInstar, aes(x=NumPlants, y=InstarToMove, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Number of Plants") +
  ylab("Instar at Natal Plant Abandonment")+
  theme_bw()+
  geom_errorbar(aes(ymin=InstarToMove-sd, ymax=InstarToMove+sd), width=.2,
                position = position_dodge(.9))+
  scale_y_continuous(expand=c(0,0), limits=c(0,5.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 15))

#stats for manuscript
Instar=Natal$InstarToMove
mean(Instar)
sd(Instar)
max(Instar)
min(Instar)

getmode <- function(Instar) {
  uniqv <- unique(Instar)
  uniqv[which.max(tabulate(match(Instar, uniqv)))]
}

result<-getmode(Instar)
print(result)


#################################
### Plant Rank at Abandonment ###
#################################

Rank<-read.csv("011819_GH18_PlantRank_NatalAbandonment.csv")

Rank$Trial=factor(Rank$Trial)
Rank$Block=factor(Rank$Block)
Rank$NumPlants=factor(Rank$NumPlants)
Rank$Year=factor(Rank$Year)
# Create Year-Block variable with 12 levels
Rank$YearBlock <- as.numeric(as.factor(paste(Rank$Year, Rank$Block, sep = "-")))
Rank$YearBlock <- as.factor(Rank$YearBlock)

# Poisson glm
rank <- glm(PlantRank ~ YearBlock + Trial + NumPlants + Trial:NumPlants, data=Rank, family = poisson(link = "log"))
#not useful, just make sure there isn't a bunch of NA
summary(rank)

# Goodness of fit test with Resids
Resids <- residuals(rank, type = "pearson")
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(rank), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(rank), df.residual(rank), lower.tail = F)

