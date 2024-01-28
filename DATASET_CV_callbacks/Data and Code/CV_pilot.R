####################################
###########  CV Pilot Study ############
########### Ariella Kristal ########
### Code Finalized: March 7, 2021 ##
####################################
if (!require("pacman")) {install.packages("pacman"); require("pacman")}
p_load(rstudioapi, tidyverse, Rmisc, data.table, psy, psych, nlme, lme4, lfe, varhandle, stringr, effects, ggsignif, lmerTest, tidyverse, data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

d <- read.csv("pilot2_OSF.csv")


##------------Data Cleaning----------------------
d <- d[!is.na(d$experience_overall),] #exclude those who failed attention check before survey
d2 <- d[d$Q274==0 & d$novel_acheck==4,] #exclude those who failed the gender manipulation check and comprehension check of the word "novel" in this context
d3 <- d2[d2$job!="Call Centre",] #exclude those from the CV with the technical issue

#Participant demographics:
table(d3$male)
d3$age <- 2020 - (d3$birth_year+1919)
mean(d3$age) #34
sd(d3$age) #12
table(d3$race)

##------------Main Analysis----------------------
#Table S4
library(stargazer)
m1<- lm(experience_overall~condition, data=d3)
m2<-lm(years_experience~condition, data=d3)
m3<-lm(CV_easy~condition, data=d3)
m4<-lm(CV_novel~condition, data=d3)
stargazer(m1,m2,m3,m4, type = "text")

##-------Further exploratory research-----
#Does CRT score moderate? 
#new crt scores (adapted from Thomson & Oppenheimer, 2016)
d3$CRT1 <- ifelse(d3$nCRT1=="2" | d3$nCRT1=="2nd"|d3$nCRT1=="Second"| d3$nCRT1=="second" | d3$nCRT1=="second place" |
                    d3$nCRT1==" Second" | d3$nCRT1=="2nd place" | d3$nCRT1=="second " | d3$nCRT1=="Second place" |
                    d3$nCRT1=="Second place " | d3$nCRT1=="Second " | d3$nCRT1=="Second place.",1,0)
d3$CRT2 <- ifelse(d3$nCRT2==8,1,0)
d3$CRT3 <- ifelse(d3$nCRT3=="Emily" | d3$nCRT3=="Emily " |d3$nCRT3=="emily" |d3$nCRT3=="Emily probably", 1,0)
d3$CRT_sum <- d3$CRT1 + d3$CRT2 + d3$CRT3
table(d3$CRT_sum)

m5<- lm(experience_overall~condition*CRT_sum, data=d3)
m6<-lm(years_experience~condition*CRT_sum, data=d3)
m7<-lm(CV_easy~condition*CRT_sum, data=d3)
m8<-lm(CV_novel~condition*CRT_sum, data=d3)
stargazer(m5,m6,m7,m8, type = "text")
#Doesn't seem like it. 