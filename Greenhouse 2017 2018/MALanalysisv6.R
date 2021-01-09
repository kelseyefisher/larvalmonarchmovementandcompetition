#setwd("D:/Iowa State University/Statistics department/Consulting/Kelsey Fisher/Analysis_011819")
setwd("C:/Users/Audrey McCombs/Desktop/Kelsey Fisher/Analysis_011819")

library(ggResidpanel)
library(ggplot2)
library(emmeans)

###Days to Natal Abandonment
      ### No issues here - just making sure everything works

Natal<-read.csv("011819_GH17&18_Abandonment.csv")

Natal$Trial=factor(Natal$Trial)
Natal$Block=factor(Natal$Block)
Natal$NumPlants=factor(Natal$NumPlants)
Natal$Year=factor(Natal$Year)
  # Create Year-Block variable with 12 levels
Natal$YearBlock <- as.numeric(as.factor(paste(Natal$Year, Natal$Block, sep = "-")))
Natal$YearBlock <- as.factor(Natal$YearBlock)

# Linear model assuming Normality
  #use this order of model terms for anova:
  #account for everything else before you account for treatment of interest
daystomove<-lm(DaysToMove ~ YearBlock + Trial + NumPlants + Trial:NumPlants, data=Natal)
    
summary(daystomove)
plot(daystomove)

# Histogram of residuals - should look like a standard normal for lm
Resids <- residuals(daystomove, type = "pearson")
hist(Resids, prob = T)
lines(density(Resids), col='orange') #Bad

#resid_panel(daystomove)  #Normal model looks okay

anova(daystomove) #NumPlants not significant

# Poisson glm
daystomovePoisson <- glm(DaysToMove ~ YearBlock + Trial + NumPlants + Trial:NumPlants, data=Natal, family = poisson(link = "log"))
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

remove(list = ls())

### Survival

Survive<-read.csv("011819_GH17&18_Survival.csv")

Survive$Trial=factor(Survive$Trial)
Survive$Block=factor(Survive$Block)
Survive$NumPlants=factor(Survive$NumPlants)
Survive$Year=factor(Survive$Year)
Survive$YearBlock <- as.numeric(as.factor(paste(Survive$Year, Survive$Block, sep = "-")))
Survive$YearBlock <- as.factor(Survive$YearBlock)

table(Survive$Survive)
with(Survive, table(Trial, NumPlants))
  #missing cells - this is a problem.  2 options:
    # 1) fit only the complete data (reduce data frame using only Trials 1-4)
    # 2) fit model without interaction term

#Reduce data frame
Survive.complete <- Survive[which(Survive$Trial == 1 | Survive$Trial == 2 | Survive$Trial == 3 | Survive$Trial == 4),]

survival <- glm(Survive ~ YearBlock + Trial + NumPlants + Trial:NumPlants, data=Survive.complete, family = binomial(link = "logit"))
summary(survival)

#Test for overdispersion
summary(survival)$deviance/summary(survival)$df.residual
  #1.12 is right on the edge of overdispersion

#Test for significance
dtm.emm <- emmeans(survival, c("NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)


## Fit model without interaction
survival <- glm(Survive ~ Trial + YearBlock + NumPlants, data=Survive, family = binomial(link = "logit"))
summary(survival)

#Test for overdispersion
summary(survival)$deviance/summary(survival)$df.residual
    #1.23 this is worse - code below just for example - use above analysis

#Test for significance
dtm.emm <- emmeans(survival, c("NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)
  #The 0.08 p-value underestimates the variance - don't use this.

remove(list = ls())

###  Percent Plants Vistited

GH1718<- read.csv("011819_GH2017&2018_FullDevelopment_ForAnalysis.csv")

GH1718$Trial=factor(GH1718$Trial)
GH1718$Block=factor(GH1718$Block)
GH1718$NumPlants=factor(GH1718$NumPlants)
GH1718$Year=factor(GH1718$Year)
GH1718$PercPlantsVisited <- GH1718$PercPlantsVisited/100
GH1718$YearBlock <- as.numeric(as.factor(paste(GH1718$Year, GH1718$Block, sep = "-")))
GH1718$YearBlock <- as.factor(GH1718$YearBlock)

table(GH1718$PercPlantsVisited)
with(GH1718, table(Trial, NumPlants))
  #missing cells - same options as before

#mean of NumPlantsVisited for NumPlants = 2
mean(GH1718$NumPlantsVisited[which(GH1718$NumPlants == "3")])

#Reduce data frame
GH1718.complete <- GH1718[which(GH1718$Trial == 1 | GH1718$Trial == 2 | GH1718$Trial == 3 | GH1718$Trial == 4),]

percvisitGLM <- glm(PercPlantsVisited ~ NumPlants + Trial + Trial:NumPlants + YearBlock, data=GH1718.complete, family = binomial, na.action = "na.omit")
summary(percvisitGLM)

#Test for overdispersion
summary(percvisitGLM)$deviance/summary(percvisitGLM)$df.residual
  #Way underdispersed - inference is very conservative

#Test for significance
dtm.emm <- emmeans(percvisitGLM, c("NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)


#Fit without interaction term
percvisitGLM <- glm(PercPlantsVisited ~ NumPlants + Trial + YearBlock, data=GH1718, family = binomial, na.action = "na.omit")
summary(percvisitGLM)

# Goodness of fit test with Resids
Resids <- residuals(percvisitGLM, type = "pearson")
hist(Resids, prob = T)
lines(density(Resids), col='orange') #Not too bad

pchisq(deviance(percvisitGLM), df.residual(percvisitGLM), lower.tail = F)

#Test for overdispersion
summary(percvisitGLM)$deviance/summary(percvisitGLM)$df.residual
  # 0.26 still pretty underdispersed

# Test for significance of NumPlants
dtm.emm <- emmeans(percvisitGLM, c("NumPlants", "Trial", "YearBlock"), type='response')
joint_tests(dtm.emm)
