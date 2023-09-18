#if(!require("DHARMa")){install.packages("DHARMa")}
if(!require("merTools")){install.packages("merTools")}
if(!require(caTools)){install.packages("caTools")}
if(!require(devtools)){install.packages("devtools")}
if(!require("ggprism")){install.packages("ggprism")}
install.packages("gtable")
install.packages("modelsummary")


remove.packages("lme4")
remove.packages("lmerTest")
remove.packages("DHARMa")
install.packages("lme4")
install.packages("lmerTest")
install.packages("DHARMa")


library(DHARMa)
library(emmeans)
library(merTools)
library(ggplot2)
library('ggpubr')
library(tidyverse)
library(rstatix)
library('readxl')
library(dplyr)
library(ggprism)
library(ggstatsplot)
library(modelsummary)
remove.packages("rlang")
remove.packages("ggplot2")
install.packages("ggplot2", dependencies = TRUE)
library("ggplot2")
install.packages("emmeans")
library("emmeans")
install.packages('afex')
library(afex) 

####--------------------------------------------- SINGLE PHASE FACTOR ANALYSIS--------------------------
df <- read_excel('C:\\Users\\burka\\OneDrive\\Desktop\\Phd2\\Publications\\Sophia\\Sophia_Stats_CP.xlsx',
                 sheet = 'CP_single')
head(df)

df$Session = factor(df$Session) #=> setting of factors from char
df$Sequence = factor(df$Sequence)
#df$Sugar = factor(df$Sugar, ordered = TRUE, levels = c("LOW","MEDIUM","HIGH"))
#df$Oil = factor(df$Oil, ordered = TRUE, levels = c("LOW","MEDIUM","HIGH"))

summary(df)

###---------------------Check Effect of each Variable

#### PSD


###Important - Triplicates are of one batch - so no independence, but three different recipes I productions
## to create independence of observations 


df_psd <- read_excel('C:\\Users\\burka\\OneDrive\\Desktop\\Phd2\\Publications\\Sophia\\Sophia_Stats_CP.xlsx',
                     sheet = 'psd_summary')

summary(df_psd)
model_psd <- lmer(d90 ~ Sugar * Oil + (1 | Repetition), data = df_psd)
anova(model_psd)
summary(model_psd)

df_psd$Sample <- factor(df_psd$Sample, levels = c('04-0','04-18','23-0','23-9','23-18','42-0','42-18'))
df_psd
p <- ggplot(df_psd, aes(x=Sample, y=d90, color = Sample))+
  geom_violin(lwd = 1) +  labs(title = 'd90')
p + geom_jitter(shape=16, position=position_jitter(0.2)) + theme_bw()


model_psd <- lmer(span ~ Sugar * Oil + (1 | Repetition), data = df_psd)
anova(model_psd)
summary(model_psd)

df_psd$Sample <- factor(df_psd$Sample, levels = c('04-0','04-18','23-0','23-9','23-18','42-0','42-18'))
p <- ggplot(df_psd, aes(x=Sample, y=span, color = Sample))+
  geom_boxplot(lwd = 1) +  labs(title = 'span')
p + geom_jitter(shape=16, position=position_jitter(0.2)) + theme_bw()


#### DSC


df_dsc <- read_excel('C:\\Users\\burka\\OneDrive\\Desktop\\Phd2\\Publications\\Sophia\\Sophia_Stats_CP.xlsx',
                     sheet = 'DSC_New')

model_dsc <- lmer(Enthalpy ~ Sugar * Oil + (1 | Repetitions), data = df_dsc)
anova(model_dsc)
summary(model_dsc)

aggregate(Enthalpy ~ Sample, df_dsc, function(x) c(mean=mean(x), sd=sd(x))) #Show Averages


#df_dsc$Sample <- factor(df_dsc$Sample, levels = c('04-0','04-18','23-0','23-9','23-18','42-0','42-18'))
p <- ggplot(df_dsc, aes(x=Sample, y=Enthalpy, col = Sample))+
  geom_boxplot(lwd = 1) +  labs(title = 'enthalpy')
p + geom_jitter(shape=16, position=position_jitter(0.2)) + theme_bw()




#modelsummary(model_dsc)
emmeans(model_dsc, list(pairwise ~ Sugar * Oil), adjust = 'tukey')

simulateResiduals(model_dsc, plot = T) #Check for Normality w QQ and KS 

model_dsc <- lmer(Enthalpy ~ Sample + (1 | Repetitions), data = df_dsc)
emmeans(model_dsc, list(pairwise ~ Sample), adjust = 'tukey')







### -------------------- Factorial Design -- check significance of Oil I Sugar on Sweetness ##
## Experimental Design: 1 Sample (e.g 04-0) can unambiguously be described by the two fixed factors Oil and Sugar (the first number refers to the Sugar content and the
# second to the Oil content). Each of the seven samples (04-0,04-18,23-0,23-9,23-18,42-0,42-18) was tested twice by each of the eight panelists. During each session and for each
# panelist the sequence was balanced. The panel was asked to rate three different attributes Smoothness, Sweetness and Bitterness on a scale from 0 - 100. The consumption protocol and
# the rating of the samples was learnt during three previous sessions

#Based on our Research Question, the fixed factors Sugar and Oil were suggested, with the random factors Session, Panelist and Sequence - do we assume the coefficients of the random
#factors to be the same ? 

model <- lmer(Sweetness ~ Sugar * Oil + Session:Sequence + (1 | Panelist), data = df)
anova(model)
summary(model)
vcov(model)
simulateResiduals(model, plot = T) #Check for Normality w QQ and KS 
emmeans(model, list(pairwise ~ Sugar * Oil), adjust = 'tukey') #to check w/ pairwise post-hoc test only amongst the samples

install.packages("car")
library('car')
vif(model)
values <- vif(model)
print(values)

tol <- 1/values
print(tol)
#Visualisation
df$Sample <- factor(df$Sample, levels = c('04-0','04-18','23-0','23-9','23-18','42-0','42-18'))
p <- ggplot(df, aes(x=Sample, y=Sweetness, color = Sample))+
  geom_boxplot(lwd = 1) +  labs(title = 'Sweetness of Single Masses')
p + geom_jitter(shape=16, position=position_jitter(0.2)) + theme_bw()




##  -------------------- Correlation Plot b/t different DV -- ##
# Hereby the different Dependent Variables Smoothness, Bitterness and Sweetness from above are compared 
#aggregate df columns - to aggregate all replicate data - e.g. J01 Product01 tested in Session 1 and 2 --> mean of both 

#1. Concatenate Sample and Judge 
df$conc <- paste(df$Sample, df$Panelist)
df2 <- df %>% group_by(conc) %>% summarize(Sweetness = mean(Sweetness), Bitterness = mean(Bitterness), Smoothness = mean(Smoothness))

view(df2)


plot(df2$Sweetness,df2$Smoothness)

cor.test(df2$Sweetness, df2$Smoothness, method = 'pearson')
ggscatterstats(data = df2, x = Sweetness, y = Smoothness)


cor.test(df2$Sweetness, df2$Bitterness, method = 'pearson')
ggscatterstats(data = df2, x = Sweetness, y = Bitterness)



### -------------------- Correlation b/t Instrumental (e.g. d90 in psd_summary sheet and smoothness)

#### How can I summarize the different data to conduct a correlation ? best would be a correlation matrix -- HELP REQUIRED 
#with different instrumental measures (e.g. d90, span (in psd_summary sheet) and Peak, Enthalpy, Onset, and Offset (in DSC_final sheet) to IV I Fixed Factors Sugar and Oil
# plus the different Dependent Variables Sweetness I Smoothness I Melting I Bitterness in the sheet CP_single)


library(GGally)
library(psych)
library(Hmisc)
df_multicomp <- read_excel('C:\\Users\\burka\\OneDrive\\Desktop\\Phd2\\Publications\\Sophia\\Sophia_Stats_CP.xlsx',
                           sheet = 'multicomparison')

ggplot(df_multicomp,aes(x=Melting,y=enthalpy, col=Sample, cex = 5)) + geom_point() +theme_bw()


df_multicomp <-  df_multicomp[,-1:-2] #delete first and second column
#df_multicomp <- df_multicomp[,-7:-8]
head(df_multicomp)
summary(df_multicomp)
ggcorr(df_multicomp, method = c("pairwise","spearman"), hjust = 0.9, size = 5, label = TRUE, angle = 0)

multicomp_cor = rcorr(as.matrix(df_multicomp), type = 'spearman')
multicomp_cor$n
multicomp_cor_r  = multicomp_cor$r
multicomp_cor_p = multicomp_cor$P

multicomp_cor_p


##  -------------------- Factorial Design -- Check Significance of Oil I Sugar I Layers on Sweetness in Multiphase Systems -- ##
### Experimental Design: In the Multiphase Systems there were 8 Assessors that evaluated 7 Products twice in two separate sessions 
### One Sample can unambiguously described by 3 different attributes: Sugar, Oil and number of Layers  - hence those factors were set as fixed factors
## The nested factors Session, Panelist and Sequence were set as random factors and the coefficient was not varied between the different random factors 


df_multi <- read_excel('C:\\Users\\burka\\OneDrive\\Desktop\\Phd2\\Publications\\Sophia\\Sophia_Stats_CP.xlsx',
                 sheet = 'CP_multiple')

df_multi$Session = factor(df_multi$Session)
df_multi$Sequence = factor(df_multi$Sequence)
df_multi$Sweetness = as.numeric(df_multi$Sweetness)
df_multi$Smoothness = as.numeric(df_multi$Smoothness)
df_multi$Layers = factor(df_multi$Layers)
df_multi$Sugar = factor(df_multi$Sugar, ordered = TRUE, levels = c("LOW","MEDIUM","HIGH"))
df_multi$Oil = factor(df_multi$Oil, ordered = TRUE, levels = c("LOW","MEDIUM","HIGH"))

summary(df_multi)

model_multi <- lmer(Sweetness ~ Sugar * Oil * Layers + Sequence:Session + (1|Panelist), data = df_multi)

summary(model_multi)
anova(model_multi)
vcov(model_multi)
simulateResiduals(model_multi, plot = T) #Check for Normality w QQ and KS 

emmeans(model_multi, list(pairwise ~ Sugar * Oil), adjust = 'tukey') #to check w/ pairwise post-hoc test only amongst the samples 


aggregate(Sweetness ~ Sample, df_multi, function(x) c(mean=mean(x), sd=sd(x))) #Show Averages



#df_multi$SAMPLE <- factor(df_multi$SAMPLE, levels = c('230 2318 3','239 0  1','2318 230 3','420 418 3','420 418 5','4218 40 3','4218 40 5','230 2318 5'))
#view(df_multi)
p <- ggplot(df_multi, aes(x=SAMPLE, y=Sweetness, color = SAMPLE))+
  geom_boxplot(lwd = 1) +  labs(title = 'Sweetness of Multi Masses')
p + geom_jitter(shape=16, position=position_jitter(0.2)) + theme_bw()



























