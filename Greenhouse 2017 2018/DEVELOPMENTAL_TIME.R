#############################################################################################################
###                                             Only 2017 Data                                            ###
#############################################################################################################

########################
### Days to Pupation ###
########################

GH17<- read.csv("012119_GH2017_ForAnalysis.csv")

GH17$Trial=factor(GH17$Trial)
GH17$Block=factor(GH17$Block)
GH17$NumPlants=factor(GH17$NumPlants)
GH17$Year=factor(GH17$Year)
# Create Year-Block variable with 12 levels
GH17$YearBlock <- as.numeric(as.factor(paste(GH17$Year, GH17$Block, sep = "-")))
GH17$YearBlock <- as.factor(GH17$YearBlock)

# Poisson glm
pday <- glm(PDay ~ YearBlock + Trial + NumPlants + Trial:NumPlants, data=GH17, family = poisson(link = "log"))
#not useful, just make sure there isn't a bunch of NA
summary(pday)

# Goodness of fit test with Resids
Resids <- residuals(pday, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(pday), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(pday), df.residual(pday), lower.tail = F)

#Test for overdispersion
summary(pday)$deviance/summary(pday)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

# Test for significance of NumPlants
dtm.emm <- emmeans(pday, c("NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)

#average by number of plants
library(plyr)
ddply(GH17,~NumPlants,summarise,mean=mean(PDay),sd=sd(PDay))

#Graph for manuscript
library(lattice)
library(Rmisc)
SEPDay<- summarySE(GH17, measurevar="PDay", groupvars=c("NumPlants"))
SEPDay

library(ggplot2)
ggplot(SEPDay, aes(x=NumPlants, y=PDay, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Number of Plants") +
  ylab("Day of Pupal Formation")+
  theme_bw()+
  geom_errorbar(aes(ymin=PDay-sd, ymax=PDay+sd), width=.2,
                position = position_dodge(.9))+
  scale_y_continuous(expand=c(0,0), limits=c(0,15.1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 15))

#stats for manuscript
PDay=GH17$PDay
mean(PDay)
sd(PDay)
max(PDay)
min(PDay)

getmode <- function(PDay) {
  uniqv <- unique(PDay)
  uniqv[which.max(tabulate(match(PDay, uniqv)))]
}

result<-getmode(PDay)
print(result)


####################
### Pupal Weight ###
####################

Weight<- read.csv("012119_GH2017_ForAnalysis_pweight.csv", header=TRUE)

Weight$Trial=factor(Weight$Trial)
Weight$Block=factor(Weight$Block)
Weight$NumPlants=factor(Weight$NumPlants)
Weight$Year=factor(Weight$Year)
# Create Year-Block variable with 12 levels
Weight$YearBlock <- as.numeric(as.factor(paste(Weight$Year, Weight$Block, sep = "-")))
Weight$YearBlock <- as.factor(Weight$YearBlock)
Weight$PWeight <- as.numeric(Weight$PWeight)

# Poisson glm
pweight <- glm(PWeight ~ YearBlock + Trial + NumPlants + Trial:NumPlants, data=Weight, family = poisson(link = "log"))
#not useful, just make sure there isn't a bunch of NA
summary(pweight)

# Goodness of fit test with Resids
Resids <- residuals(pweight, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(pweight), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(pweight), df.residual(pweight), lower.tail = F)

#Test for overdispersion
summary(pweight)$deviance/summary(pweight)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

# Test for significance of NumPlants
dtm.emm <- emmeans(pweight, c("NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)

#average by number of plants
library(plyr)
ddply(Weight,~NumPlants,summarise,mean=mean(PWeight),sd=sd(PWeight))

#Graph for manuscript
library(lattice)
library(Rmisc)
SEPWeight<- summarySE(Weight, measurevar="PWeight", groupvars=c("NumPlants"))
SEPWeight

library(ggplot2)
ggplot(SEPWeight, aes(x=NumPlants, y=PWeight, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Number of Plants") +
  ylab("Pupal Weight")+
  theme_bw()+
  geom_errorbar(aes(ymin=PWeight-sd, ymax=PWeight+sd), width=.2,
                position = position_dodge(.9))+
  scale_y_continuous(expand=c(0,0), limits=c(0,1.55))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 15))

#stats for manuscript
PWeight=Weight$PWeight
mean(PWeight)
sd(PWeight)
max(PWeight)
min(PWeight)

getmode <- function(PWeight) {
  uniqv <- unique(PWeight)
  uniqv[which.max(tabulate(match(PWeight, uniqv)))]
}

result<-getmode(PWeight)
print(result)

######################
### Pupal Duration ###
######################

PDur<- read.csv("012119_GH2017_ForPdur.csv", header=TRUE)

PDur$Trial=factor(PDur$Trial)
PDur$Block=factor(PDur$Block)
PDur$NumPlants=factor(PDur$NumPlants)
PDur$Year=factor(PDur$Year)
# Create Year-Block variable with 12 levels
PDur$YearBlock <- as.numeric(as.factor(paste(PDur$Year, PDur$Block, sep = "-")))
PDur$YearBlock <- as.factor(PDur$YearBlock)

# Poisson glm
pdur <- glm(PDuration ~ YearBlock + Trial + NumPlants + Trial:NumPlants, data=PDur, family = poisson(link = "log"))
#not useful, just make sure there isn't a bunch of NA
summary(pdur)

# Goodness of fit test with Resids
Resids <- residuals(pdur, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(pdur), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(pdur), df.residual(pdur), lower.tail = F)

#Test for overdispersion
summary(pdur)$deviance/summary(pdur)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

# Test for significance of NumPlants
dtm.emm <- emmeans(pdur, c("NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)

#average by number of plants
library(plyr)
ddply(PDur,~NumPlants,summarise,mean=mean(PDuration),sd=sd(PDuration))

#Graph for manuscript
library(lattice)
library(Rmisc)
SEPDur<- summarySE(PDur, measurevar="PDuration", groupvars=c("NumPlants"))
SEPDur

library(ggplot2)
ggplot(SEPDur, aes(x=NumPlants, y=PDuration, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Number of Plants") +
  ylab("Pupal Duration (days)")+
  theme_bw()+
  geom_errorbar(aes(ymin=PDuration-sd, ymax=PDuration+sd), width=.2,
                position = position_dodge(.9))+
  scale_y_continuous(expand=c(0,0), limits=c(0,12.2))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 15))

#stats for manuscript
PDur2=PDur$PDuration
mean(PDur2)
sd(PDur2)
max(PDur2)
min(PDur2)

getmode <- function(PDur2) {
  uniqv <- unique(PDur2)
  uniqv[which.max(tabulate(match(PDur2, uniqv)))]
}

result<-getmode(PDur2)
print(result)

################################
### Neo to Adult Development ###
################################

neo <- glm(NeoToAdult ~ YearBlock + Trial + NumPlants + Trial:NumPlants, data=PDur, family = poisson(link = "log"))
#not useful, just make sure there isn't a bunch of NA
summary(neo)

# Goodness of fit test with Resids
Resids <- residuals(neo, type = "pearson")
Resids
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(neo), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(neo), df.residual(neo), lower.tail = F)

#Test for overdispersion
summary(neo)$deviance/summary(neo)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

# Test for significance of NumPlants
dtm.emm <- emmeans(neo, c("NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)

#average by number of plants
library(plyr)
ddply(PDur,~NumPlants,summarise,mean=mean(NeoToAdult),sd=sd(NeoToAdult))

#Graph for manuscript
library(lattice)
library(Rmisc)
SEPDur<- summarySE(PDur, measurevar="NeoToAdult", groupvars=c("NumPlants"))
SEPDur

library(ggplot2)
ggplot(SEPDur, aes(x=NumPlants, y=NeoToAdult, width=.6))+
  geom_bar(stat="identity", color="black",
           position=position_dodge())+
  xlab("Number of Plants") +
  ylab("Duration of Neonate to Adult Eclosion (days)")+
  theme_bw()+
  geom_errorbar(aes(ymin=NeoToAdult-sd, ymax=NeoToAdult+sd), width=.2,
                position = position_dodge(.9))+
  scale_y_continuous(expand=c(0,0), limits=c(0,25.4))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(axis.text.x = element_text(size=16))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y = element_text(size=16))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(axis.title = element_text(size = 15))

#stats for manuscript
Neo=PDur$NeoToAdult
mean(Neo)
sd(Neo)
max(Neo)
min(Neo)

getmode <- function(Neo) {
  uniqv <- unique(Neo)
  uniqv[which.max(tabulate(match(Neo, uniqv)))]
}

result<-getmode(Neo)
print(result)


