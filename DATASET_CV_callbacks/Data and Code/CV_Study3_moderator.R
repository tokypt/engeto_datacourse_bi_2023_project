####################################
###########  CV Study 3 ############
########### Ariella Kristal ########
## Code Finalized: October 1, 2021 ##
####################################

if (!require("pacman")) {install.packages("pacman"); require("pacman")}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
p_load(rstudioapi, Rmisc, data.table, psy, psych, nlme, lme4, varhandle, stringr, effects, ggsignif, lmerTest, tidyverse, data.table)

d <- read.csv("study3_moderator.csv")

##------------Data Cleaning-----------------#

d <- d[d$Cond!="",]

library(stringr)
d$treat <- word(d$Cond,2)
d$experience <- ifelse(d$Dates1=="5 years"|d$Dates1=="Jan 2016 - Present", "many years", "fewer years")

#gender check
d$fail_check <- ifelse(d$applicant_male==0,0,1)
table(d$fail_check)
d <- d[d$fail_check==0,]
d$age <-ifelse(d$birth_year==84,NA,2020-(d$birth_year+1919))
summary(d$age)
sd(d$age, na.rm=T)

d%>%
  group_by(d$Cond, d$experience) %>%
  summarise(hire.m = mean(hire_10),
            hire.sd = sd(hire_10),
            years_experience.m = mean(years_experience),
            years_experience.sd=sd(years_experience),
            N=n())

d$hire <- d$hire_10

summary(d$years_experience)
#d <- d[d$years_experience<22,]
#d <- d[d$years_experience<50,]

##------------------Visualization-------------------------
sum.value = summarySE(d, measurevar = "hire", groupvars = c("treat", "experience"), na.rm=TRUE)
ggplot(sum.value, aes(x=treat, y=hire, fill = experience)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=hire-se, ymax=hire+se),
                width=.2,                    
                position=position_dodge(.9)) +
  coord_cartesian(ylim = c(1,100))

sum.value = summarySE(d, measurevar = "hire", groupvars = c("Cond"), na.rm=TRUE)
ggplot(sum.value, aes(x=Cond, y=hire)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=hire-se, ymax=hire+se),
                width=.2,                   
                position=position_dodge(.9)) +
  coord_cartesian(ylim = c(1,100))

sum.value = summarySE(d, measurevar = "hire", groupvars = c("treat"), na.rm=TRUE)
ggplot(sum.value, aes(x=treat, y=hire)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=hire-se, ymax=hire+se),
                width=.2,                    
                position=position_dodge(.9)) +
  coord_cartesian(ylim = c(1,100))

sum.value = summarySE(d, measurevar = "years_experience", groupvars = c("treat", "experience"), na.rm=TRUE)
ggplot(sum.value, aes(x=treat, y=years_experience, fill = experience)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=years_experience-se, ymax=years_experience+se),
                width=.2,                    
                position=position_dodge(.9)) +
  coord_cartesian(ylim = c(1,20))

##--------------Pre-registered analyses---------------##

library(lfe)
summary(felm(hire~treat, d[d$experience=="fewer years",]))
summary(felm(hire~treat, d[d$experience=="many years",]))
summary(felm(years_experience~treat|0|0|0, d[d$experience=="fewer years",]))
summary(felm(years_experience~treat|0|0|0, d[d$experience=="many years",]))

summary(felm(years_experience~treat*experience|0|0|0, d))
summary(felm(hire~treat*experience|0|0|0, d))
summary(felm(hire~treat+experience|0|0|0, d))

d%>%
  group_by(d$Cond, d$experience) %>%
  summarise(hire.m = mean(hire_10),
            hire.sd = sd(hire_10),
            years_experience.m = mean(years_experience),
            years_experience.sd=sd(years_experience),
            N=n())

library(rstatix)
CohenD(d$hire[d$Cond=="Female Normal" & d$experience=="many years"], d$hire[d$Cond=="Female Treat" & d$experience=="many years"], na.rm = TRUE, conf.level = 0.95)
CohenD(d$hire[d$Cond=="Female Normal" & d$experience=="fewer years"], d$hire[d$Cond=="Female Treat" & d$experience=="fewer years"], na.rm = TRUE, conf.level = 0.95)
cohen.d.ci(0.16,379,378)
cohen.d.ci(0.17,382,382)

library(lavaan)
set.seed(123456)
X <- d$treat
M1 <- d$years_experience
Y <- d$hire
Data <- data.frame(X = X, Y = Y, M1 = M1)
mod <- ' # direct effect
Y ~ c*X
# mediator
M1 ~ a1*X
Y ~ b1*M1
# indirect effect (a*b)
ab1 := a1*b1
# total effect
total := c + (a1*b1)
'
fit <- sem(mod, data = Data)
summary(fit)



