##work directory
setwd("C:/Users/kefisher/Box Sync/Publications/Monarch Larval Biomass Consumption - GH 2017 & 2018/Analysis_022019")
##home directory
setwd("C:/Users/Kelsey/Box/Publications/Monarch Larval Biomass Consumption - GH 2017 & 2018/Analysis_022019")

library(ggplot2)
library(emmeans)

#################################################################################################################

##########################################
### Days Until Natal Plant Abandonment ###
##########################################

Natal<-read.csv("011819_GH17&18_Abandonment.csv")

Natal$Trial=factor(Natal$Trial)
Natal$Block=factor(Natal$Block)
Natal$NumPlants=factor(Natal$NumPlants)
Natal$Year=factor(Natal$Year)
# Create Year-Block variable with 12 levels
Natal$YearBlock <- as.numeric(as.factor(paste(Natal$Year, Natal$Block, sep = "-")))
Natal$YearBlock <- as.factor(Natal$YearBlock)

# Poisson glm
daystomovePoisson <- glm(DaysToMove ~ YearBlock + Trial + NumPlants + Trial:NumPlants, data=Natal, family = poisson(link = "log"))
#not useful, just make sure there isn't a bunch of NA
summary(daystomovePoisson)

# Goodness of fit test with Resids
Resids <- residuals(daystomovePoisson, type = "pearson")
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(daystomovePoisson), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(daystomovePoisson), df.residual(daystomovePoisson), lower.tail = F)

#Test for overdispersion
summary(daystomovePoisson)$deviance/summary(daystomovePoisson)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

# Test for significance of NumPlants
dtm.emm <- emmeans(daystomovePoisson, c("NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)


#######################################################
### 2017 and 2018 Instar at Natal Plant Abandonment ###
#######################################################

instartomove<-glm(InstarToMove ~ YearBlock + Trial + NumPlants + Trial:NumPlants, data=Natal, family = poisson(link = "log"))
summary(instartomove)

# Goodness of fit test with Resids
Resids <- residuals(instartomove, type = "pearson")
#Change "type" to "deviance" for deviance resids (get the same results)
plot(fitted(instartomove), Resids,
     xlab = "estimated mean",
     ylab = "residual",
     cex.lab = 1.4,
     pch = 16, col = 4)
abline(h = 0, lty = 2)  #Resids look okay

hist(Resids, prob = T)
lines(density(Resids), col='orange') #Should look like a standard normal - does

#Using deviance residuals
#If the p-value is less than 0.05, you have a problem with model fit
pchisq(deviance(instartomove), df.residual(instartomove), lower.tail = F)

#Test for overdispersion
summary(instartomove)$deviance/summary(instartomove)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

# Test for significance of NumPlants
dtm.emm <- emmeans(instartomove, c("NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)

##############################################################################################################

######################################
### PlantRank At Natal Abandonment ###
######################################

Rank<-read.csv("011819_GH18_PlantRank_NatalAbandonment.csv")

Rank$Trial=factor(Rank$Trial)
Rank$Block=factor(Rank$Block)
Rank$NumPlants=factor(Rank$NumPlants)
Rank$Year=factor(Rank$Year)
# Create Year-Block variable with 12 levels
Rank$YearBlock <- as.numeric(as.factor(paste(Rank$Year, Rank$Block, sep = "-")))
Rank$YearBlock <- as.factor(Rank$YearBlock)

rank<-glm(PlantRank ~ YearBlock + Trial + NumPlants + Trial:NumPlants, data=Rank, family = poisson(link = "log"))
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

#Test for overdispersion
summary(rank)$deviance/summary(rank)$df.residual
#0.82 underdispersed by a bit - that's okay (inference is conservative)

# Test for significance of NumPlants
dtm.emm <- emmeans(instartomove, c("NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)
