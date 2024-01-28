####################################
###########  CV Study 1 ############
########### Ariella Kristal ########
### Code Finalized: March 7, 2021 ##
####################################
if (!require("pacman")) {install.packages("pacman"); require("pacman")}
p_load(rstudioapi, tidyverse, Rmisc, data.table, psy, psych, nlme, lme4, stargazer, lfe, varhandle, stringr, effects, ggsignif, lmerTest, tidyverse, data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

d <- read.csv("CV_Study1.csv")

##------------Data Cleaning----------------------
d$cond[d$experimental.condition==1] <- "Unexplained gap"
d$cond[d$experimental.condition==2] <- "No gap"
d$cond[d$experimental.condition==3] <- "Explained gap"
d$cond[d$experimental.condition==4] <- "Years"
table(d$cond)
d$cond <- factor(d$cond, levels = c("Years", "No gap", "Explained gap", "Unexplained gap"))

##------------Balance Checks----------------------
#Table S1
d %>%
  group_by(cond) %>%
  count(working.pattern)

d %>% 
  group_by(record, cond) %>% 
  summarize(size=n()) %>% 
  print(nrow=32)

d %>% 
  group_by(cond) %>% 
  summarize(salary = mean(salary, na.rm=T))

##------------Pre-registered analysis----------------------
summary(felm(positive.callback~cond|working.pattern+region|0|0, data = d))

##-----------Robustness-------------------------------------
#Table S2
m1 <-felm(positive.callback~cond|working.pattern|0|0, data = d)
m2 <-felm(positive.callback~cond|working.pattern+record|0|0, data = d)
m3 <- felm(positive.callback~cond|working.pattern+region|0|0, data = d)
m4 <- felm(positive.callback~cond|working.pattern+region+record|0|0, data = d)
stargazer(m1,m2,m3,m4, type = "text")

#Table S3
m1.2 <- glmer(positive.callback~cond+(1|working.pattern), data = d, family = "binomial")
m2.2 <-glmer(positive.callback~cond+(1|working.pattern)+(1|record), data = d, family = "binomial")
m3.2 <- glmer(positive.callback~cond+(1|working.pattern)+(1|region), data = d, family = "binomial")
m4.2 <- glmer(positive.callback~cond+(1|working.pattern)+(1|record)++(1|region), data = d, family = "binomial")
summary(m1.2)
summary(m2.2)
summary(m3.2)
summary(m4.2)

#Figure 1
d$cond <- factor(d$cond, levels = c("Years", "No gap", "Explained gap", "Unexplained gap"))
cbPalette <-c("cornflower blue", "#2C579B")
sum.pos = summarySE(d, measurevar = "positive.callback", groupvars = c("cond"), na.rm=TRUE)
sum.pos$treat <-ifelse(sum.pos$cond=="Years",1,0)
ggplot(sum.pos, aes(x=cond, y=positive.callback, fill=as.factor(treat))) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=positive.callback-se, ymax=positive.callback+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  theme_bw() +
 theme(panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank(),
       axis.text.x = element_text(size=17),
       axis.text.y = element_text(size=17),
       axis.title = element_text(size=20)) +
  scale_fill_manual(name = "Type", labels = c("Traditional", "Treatment"), values=cbPalette) +
  theme(legend.title = element_text(size=14),
        legend.text = element_text(size=12)) +
  labs(x = "Condition", y = "Callback rate") 

