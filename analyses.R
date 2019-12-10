## Analyses for Ch. 2 Lassen Phenology
## Elena Suglia

library(scales)
library(vistime)
library(directlabels)
library(export)
library(gridExtra)
library(arm)
library(lme4)
library(lattice)
library(vioplot)
library(lme4)
library(arm)
library(bbmle) # Ben Bolker's library of mle functions
library(MASS)
library(MuMIn)
library(nlme)
library(tidyverse)
library(dplyr)

setwd("~/Box Sync/Graduate School/PhD Research/Ch. 2) Lassen Snowmelt & Phenology/Data analysis/lassen-phenology")
source("plots.R")

# 2.1 Relationship between environment and phenology ----
# peakflr ~ jmeltdate model selection ----

# peakflr ~ jmeltdate fixed effects model & anova
peakflrlm = lm(peakflr ~ jmeltdate, data = flrsnow0)
summary(peakflrlm)
anova(peakflrlm)

# mixed models for peakflr ~ jmeltdate
m6 = lmer(peakflr ~ jmeltdate + (1|site), data = flrsnow0, REML = FALSE)
m7 = lmer(peakflr ~ jmeltdate + (1 + jmeltdate|site), data = flrsnow0, REML = FALSE)
m8 = lmer(peakflr ~ jmeltdate + (1|year), data = flrsnow0, REML = FALSE)
m9 = lmer(peakflr ~ jmeltdate + (1|site) + (1|year), data = flrsnow0, REML = FALSE)
m10 = lmer(peakflr ~ jmeltdate*year + (1|site), data = flrsnow0, REML = FALSE) # best fit
m11 = lmer(peakflr ~ jmeltdate*site + (1|year), data = flrsnow0, REML = FALSE)
m12 = lmer(peakflr ~ jmeltdate*site + (1|year) + (1|site), data = flrsnow0, REML = FALSE)
m13 = lmer(peakflr ~ jmeltdate*site + (1 + site|year), data = flrsnow0, REML = FALSE)

AIC(peakflrlm, m6, m7, m8, m9, m10, m11, m12, m13)
AICc(peakflrlm, m6, m7, m8, m9, m10, m11, m12, m13)
BIC(peakflrlm, m6, m7, m8, m9, m10, m11, m12, m13)

r.squaredGLMM(m6)
r.squaredGLMM(m7) 
r.squaredGLMM(m8) 
r.squaredGLMM(m9) 
r.squaredGLMM(m10) 
r.squaredGLMM(m11) 
r.squaredGLMM(m12) 
r.squaredGLMM(m13) 
# including random effects improves all models

# firstflr ~ jmeltdate model selection ----

# firstflr ~ jmeltdate fixed model & anova
firstflrlm = lm(firstflr ~ jmeltdate, data = flrsnow0)
summary(firstflrlm)
anova(firstflrlm)
# later snowmelt significantly delays first flowering 

m14 = lmer(firstflr ~ jmeltdate + (1|site), data = flrsnow0, REML = FALSE)
m15 = lmer(firstflr ~ jmeltdate + (1 + jmeltdate|site), data = flrsnow0, REML = FALSE)
m16 = lmer(firstflr ~ jmeltdate + (1|year), data = flrsnow0, REML = FALSE)
m17 = lmer(firstflr ~ jmeltdate + (1|site) + (1|year), data = flrsnow0, REML = FALSE)
m18 = lmer(firstflr ~ jmeltdate*year + (1|site), data = flrsnow0, REML = FALSE)
m19 = lmer(firstflr ~ jmeltdate*site + (1|year), data = flrsnow0, REML = FALSE) # best fit
m20 = lmer(firstflr ~ jmeltdate*site + (1|year) + (1|site), data = flrsnow0, REML = FALSE)
m21 = lmer(firstflr ~ jmeltdate*site + (1 + site|year), data = flrsnow0, REML = FALSE)
# some warning messages

AIC(firstflrlm, m14, m15, m16, m17, m18, m19, m20, m21)
AICc(firstflrlm, m14, m15, m16, m17, m18, m19, m20, m21)
BIC(firstflrlm, m14, m15, m16, m17, m18, m19, m20, m21)

r.squaredGLMM(m14)
r.squaredGLMM(m15) 
r.squaredGLMM(m16) 
r.squaredGLMM(m17) 
r.squaredGLMM(m18) 
r.squaredGLMM(m19) 
r.squaredGLMM(m20) 
r.squaredGLMM(m21) 
# including random effects does not necessarily appear to help
# why are the last three identical? Ask Andrew about error messages



# lastflr ~ jmeltdate model selection ----

# lastflr ~ jmeltdate fixed effects model
lastflrlm = lm(lastflr ~ jmeltdate, data = flrsnow0)
summary(lastflrlm)
anova(lastflrlm)    
# later snowmelt significantly delays last flowering, but to a lesser degree than it delays first flowering. Implies there must be some mechanism for plants to "catch up", OR that flowering just has a hard end date beyond which the plant can't produce more (late season drought? (THIS MAY NOT BE TRUE ANYMORE ONCE 2019 DATA INCLUDED)

# mixed models for meltdate ~ lastflr
m22 = lmer(lastflr ~ jmeltdate + (1|site), data = flrsnow0, REML = FALSE)
m23 = lmer(lastflr ~ jmeltdate + (1 + jmeltdate|site), data = flrsnow0, REML = FALSE)
m24 = lmer(lastflr ~ jmeltdate + (1|year), data = flrsnow0, REML = FALSE)
m25 = lmer(lastflr ~ jmeltdate + (1|site) + (1|year), data = flrsnow0, REML = FALSE)
m26 = lmer(lastflr ~ jmeltdate*year + (1|site), data = flrsnow0, REML = FALSE) # best fit BIC
m27 = lmer(lastflr ~ jmeltdate*site + (1|year), data = flrsnow0, REML = FALSE) # best fit AIC
m28 = lmer(lastflr ~ jmeltdate*site + (1|year) + (1|site), data = flrsnow0, REML = FALSE)
m29 = lmer(lastflr ~ jmeltdate*site + (1 + site|year), data = flrsnow0, REML = FALSE)

AIC(lastflrlm, m22, m23, m24, m25, m26, m27, m28, m29)
AICc(lastflrlm, m22, m23, m24, m25, m26, m27, m28, m29)
BIC(lastflrlm, m22, m23, m24, m25, m26, m27, m28, m29)
# BIC says model 26 is best

r.squaredGLMM(m22)
r.squaredGLMM(m23) 
r.squaredGLMM(m24) 
r.squaredGLMM(m25) 
r.squaredGLMM(m26) 
r.squaredGLMM(m27) 
r.squaredGLMM(m28) 
r.squaredGLMM(m29) 


# peakflr ~ elevation ----
elevpeakflrlm = lm(peakflr ~ elevation, data = flrsnow0)
summary(elevpeakflrlm)
anova(elevpeakflrlm)
# significant, but less so than snowmelt date

# flowering duration ~ jmeltdate ----
meltdatedurationlm = lm(flrduration ~ jmeltdate, data = snowfirstflr)
summary(meltdatedurationlm)
anova(meltdatedurationlm)
# this is a pretty interesting result. Flowering duration is more influenced when snowmelt is later (or is it something else about that year?)
# days from snowmelt to first flr ~ jmeltdate ----
meltdatesnowtoflrlm = lm(snowtoflr ~ jmeltdate, data = snowfirstflr)
summary(meltdatesnowtoflrlm)
anova(meltdatesnowtoflrlm)
AIC(meltdatesnowtoflrlm)
# flower much faster when snow melts later. This could either be developmental (growth is faster in warmer temps) or due to anticipatory/photoperiod cuing to synchronize flowering with other plants.

# ---------------------------------------------------------------------------
# 2.2 Relationships between environment, phenology, and fitness ----
# fitness ~ snowmelt date ----

# fixed effects model & anova
fitnesssnowlm = lm(fitness ~ jmeltdate, data = fitnesssnow)
summary(fitnesssnowlm)
anova(fitnesssnowlm)
# site is important. not sure how to interpret this result

# mixed model
m1 = lmer(fitness ~ jmeltdate + (1 + jmeltdate|year), data = fitnesssnow, REML = FALSE)
m2 = lmer(fitness ~ jmeltdate + (1|site), data = fitnesssnow, REML = FALSE)
m2.5 = lmer(fitness ~ jmeltdate + (1|jmeltdate), data = fitnesssnow, REML = FALSE)
m3 = lmer(fitness ~ jmeltdate + (1|year), data = fitnesssnow, REML = FALSE)
m4 = lmer(fitness ~ jmeltdate + (1 + jmeltdate|year) + (1|site), data = fitnesssnow, REML = FALSE)
m5 = lmer(fitness ~ jmeltdate*year +  (1|site), data = fitnesssnow, REML=FALSE)

display(m1)
display(m2)
AIC(fitnesssnowlm, m1, m2, m2.5, m3, m4, m5)
AICc(fitnesssnowlm, m1, m2, m2.5, m3, m4, m5)
BIC(fitnesssnowlm, m1, m2, m2.5, m3, m4, m5)

# Do we include the random effects or not?
# Notes for me from the help page:
# The marginal R2 value represents the variance explained by the fixed effects, defined as: 
# R_GLMM(m)² = (σ_f²) / (σ_f² + σ_α² + σ_ε²)
# The conditional R2 value is interpreted as the variance explained by the entire model, including both fixed and random effects, and is calculated according to the equation: 
# R_GLMM(c)² = (σ_f² + σ_α²) / (σ_f² + σ_α² + σ_ε²)
# where σ_f² is the variance of the fixed effect components, σ_α² is the variance of the random effects, and σ_ε² is the “observation-level” variance.

r.squaredGLMM(m1) # R2m        R2c
# 0.0560748  0.1495234
r.squaredGLMM(m2) 
r.squaredGLMM(m2.5)
r.squaredGLMM(m3) 
r.squaredGLMM(m4) 
r.squaredGLMM(m5) 
# all models perform better when including random effects

# fitness ~ peakflr ----
fitnesspeakflrlm = lm(fitness ~ peakflr, data = flr)
summary(flrlm)
anova(flrlm)

# fitness ~ firstflr ----
fitnessfirstflrlm = lm(fitness ~ firstflr, data = flr)
summary(flrlm)
anova(flrlm)

# fitness ~ days from snowmelt to first flr ----
snowfirstflrlm = lm(fitness ~ snowtoflr*year, data = snowfirstflr)
summary(snowfirstflrlm)
anova(snowfirstflrlm)
# relationship between fitness and days from snowmelt to first flowering are totally dependent on year!


# fitness ~ branchiness ----
branchfitnesslm = lm(fitness ~ branchiness, data = fruits)
summary(branchfitnesslm)
anova(branchfitnesslm)
# Those with more (max) total branches had significantly higher fecundity.
# ---------------------------------------------------------------------------
# 2.3 Relationships between environment, flowering synchronization, and fitness ----

# what relationships here?
# ---------------------------------------------------------------------------
# 2.4 Relationships between herbivory, snowmelt, and fitness ----

# ---------------------------------------------------------------------------
# presence of herbivory ~ snowmelt date ----
## LOGISTIC REGRESSION FOR BINARY DATA
snowherbpreslm = lm(jmeltdate ~ herbpres, data = snowherb)
summary(snowherbpreslm)
anova(snowherbpreslm)
# meltdate does not affect likelihood of herbivory

# anova
snowfirstherblm = lm(jmeltdate ~ firstherb, data = snowherb)
summary(snowfirstherblm)
anova(snowfirstherblm)
# later snowmelt significantly delays first occurrence of herbivory

# lastflr ~ presence of herbivory ----
herbpreslastflrlm = lm(lastflr ~ herbpres, data = herbphen)
summary(herbpreslastflrlm)
anova(herbpreslastflrlm)
# flrduration ~ herbpres ----
herbpresflrdurationlm = lm(flrduration ~ herbpres, data = herbphen)
summary(herbpresflrdurationlm)
anova(herbpresflrdurationlm)
# lengthens flowering duration significantly
# snowmelt*firstherb ~ fitness ----

# anova
snowfirstherbfitnesslm = lm(jmeltdate*firstherb ~ fitness, data = snowherb)
summary(snowfirstherbfitnesslm)
anova(snowfirstherbfitnesslm)
# no significant effects of interaction between snowmelt date and date of first herbivory occurrence on fitness (fecundity)

# snowmelt*midherb ~ fitness ----

# anova
snowmidherbfitnesslm = lm(jmeltdate*midherb ~ fitness, data = snowherb)
summary(snowmidherbfitnesslm)
anova(snowmidherbfitnesslm)
# no significant effects of interaction between snowmelt date and median date of herbivory occurrence on fitness (fecundity)

# snowmelt*herbpres ~ fitness ----

snowherbpresfitnesslm = lm(jmeltdate*herbpres ~ fitness, data = snowherb)
summary(snowherbpresfitnesslm)
anova(snowherbpresfitnesslm)
# 2018 fitness ~ herbpres
herbphen18 = herbphen %>% filter(year == 2018)
herbpresfitnesslm18 = lm(fitness ~ herbpres, data = herbphen18)
summary(herbpresfitnesslm18)
anova(herbpresfitnesslm18)
# SUPER different results depending on the year. In 2017, herbivory depresses fitness, but in 2018, it increases it mostly because of LVTR2
# fitness ~ firstherb + year ----
firstherbfitnesslm = lm(fitness ~ firstherb, data = herbphen)
summary(firstherbfitnesslm)
anova(firstherbfitnesslm)
# borderline significant

