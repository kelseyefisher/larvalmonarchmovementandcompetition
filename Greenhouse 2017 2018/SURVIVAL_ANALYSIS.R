setwd("C:/Users/kefisher/Box Sync/Publications/Monarch Larval Biomass Consumption - GH 2017 & 2018/Analysis_022019")

library(emmeans)

### Munge the dataset

surv.dat<-read.csv("011819_GH17&18_Survival.csv")

head(surv.dat)

surv.dat$Trial=factor(surv.dat$Trial)
surv.dat$Block=factor(surv.dat$Block)
surv.dat$NumPlants=factor(surv.dat$NumPlants)
surv.dat$Year=factor(surv.dat$Year)
surv.dat$YearBlock <- as.numeric(as.factor(paste(surv.dat$Year, surv.dat$Block, sep = "-")))
surv.dat$YearBlock <- as.factor(surv.dat$YearBlock)

table(surv.dat$Survive)
with(surv.dat, table(Trial, NumPlants))

surv.mod3 <- glm(Survive ~ Trial + NumPlants, data = surv.dat, family = binomial(link = "logit"))
summary(surv.mod3)
surv.emm3 <- emmeans(surv.mod3, c("Trial", "NumPlants"))
joint_tests(surv.emm3)
emmeans(surv.mod3, "NumPlants", type = "response")

Resids <- residuals(surv.mod3, type = "pearson")
Resids

surv.emm4 <- emmeans(surv.mod3, "NumPlants")
pairs(surv.emm4, type = "response")


######instar mortality
setwd("C:/Users/kefisher/Box/Publications/Monarch Larval Biomass Consumption - GH 2017 & 2018/Analysis_022019")

mort<-read.csv("110419_instarsurvival.csv")

chisq.test(mort)


