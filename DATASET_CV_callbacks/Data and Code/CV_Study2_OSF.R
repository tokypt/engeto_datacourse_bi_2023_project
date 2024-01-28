####################################
###########  CV Study 2 ############
########### Ariella Kristal ########
### Code Finalized: March 7, 2021 ##
####################################
if (!require("pacman")) {install.packages("pacman"); require("pacman")}
p_load(rstudioapi, tidyverse, Rmisc, data.table, psy, psych, nlme, lme4, lfe, varhandle, stringr, effects, ggsignif, lmerTest, tidyverse, data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

d <- read.csv("CV_Study2_OSF.csv")


##------------Data Cleaning----------------------
d <- d[d$Cond!="",] #exclude those who failed attention check before survey

d <- d %>%
  mutate(hire = coalesce(hireability_HR_10,hireability_Software_10),
         years_experience = coalesce(years_experience_H, years_experience_S),
         prev_jobs = coalesce(prev_jobs_H, prev_jobs_S),
         app_male = coalesce(applicant_male, Q226),
         app_age = coalesce(applicant_age, Q227),
         app_race = coalesce(applicant_ethnicity, Q228))

library(stringr)
d$gender <- word(d$Cond,1)
d$treat <- word(d$Cond,2)

#Exclude those who failed the gender manipulation check
d$fail_check <- ifelse(d$gender=="Male"&d$app_male==1|d$gender=="Female"&d$app_male==0,0,1)
table(d$fail_check)
d <- d[d$fail_check==0,]

#Participant demographics:
d$age <-ifelse(d$birth_year==84,NA,2020-(d$birth_year+1919))
summary(d$age)
table(d$male)

##------------Examine the data------------------
d%>%
  group_by(d$Cond) %>%
  summarise(hire.m = mean(hire),
            hire.sd = sd(hire),
            years_experience.m = mean(years_experience),
            years_experience.sd=sd(years_experience),
            N=n())

#There are some serious outliers 
summary(d$years_experience) #maximum recalled years = 110
quantile(d$years_experience,c(0.10,0.5,0.9,0.99)) #99th percentile is 17.2,
#will conduct the analysis both with and without excluding years of experience >= 17.2
d2 <- d[d$years_experience<17.2,]

###----------------------Data visualization--------------------
sum.value = summarySE(d, measurevar = "hire", groupvars = c("Cond", "Job"), na.rm=TRUE)
ggplot(sum.value, aes(x=Cond, y=hire)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=hire-se, ymax=hire+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  facet_grid(~Job) +
  coord_cartesian(ylim = c(1,100))

sum.value = summarySE(d, measurevar = "hire", groupvars = c("Cond"), na.rm=TRUE)
ggplot(sum.value, aes(x=Cond, y=hire)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=hire-se, ymax=hire+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  coord_cartesian(ylim = c(1,100))

sum.value = summarySE(d, measurevar = "years_experience", groupvars = c("Cond"), na.rm=TRUE)
ggplot(sum.value, aes(x=Cond, y=years_experience)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=years_experience-se, ymax=years_experience+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  coord_cartesian(ylim = c(1,12))


##------------Main Analysis----------------------
#Table S5 without excluding the outlier
summary(felm(hire~treat+gender|0|0|0, d))
summary(felm(hire~treat+gender|Job|0|0, d))
summary(felm(hire~treat*gender|0|0|0, d))
summary(felm(hire~treat*gender|Job|0|0, d))

#Robustness WITH excluding the outliner
summary(felm(hire~treat+gender|0|0|0, d2))
summary(felm(hire~treat+gender|Job|0|0, d2))
summary(felm(hire~treat*gender|0|0|0, d2))
summary(felm(hire~treat*gender|Job|0|0, d2))

#Doesn't really make a difference for hireability

#Table S5 without excluding the outlier
summary(felm(years_experience~treat+gender|0|0|0, d))
summary(felm(years_experience~treat+gender|Job|0|0, d))
summary(felm(years_experience~treat*gender|0|0|0, d))
summary(felm(years_experience~treat*gender|Job|0|0, d))

#Robustness WITH excluding the outliner
summary(felm(years_experience~treat+gender|0|0|0, d2))
summary(felm(years_experience~treat+gender|Job|0|0, d2))
summary(felm(years_experience~treat*gender|0|0|0, d2))
summary(felm(years_experience~treat*gender|Job|0|0, d2))

#Magnitude changes slightly, but treatment effect remains equally significant and still no impact of gender
