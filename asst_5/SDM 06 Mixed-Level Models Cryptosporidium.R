#' Multi-Level Analysis of Cryptosporidium Infection
#' 5074 weight observations from 1064 children over multiple days, with suspected infection.
#' Level 1: day, Level 2: Patient

setwd("C:/Users/abhatt/Desktop/SDM/Data")
setwd("/Users/ab/Desktop/SDM/Data")
d <- read.csv("Cryptosporidium.csv")
str(d)
View(d)

colSums(is.na(d))                                 # Check for missing data
d <- d[complete.cases(d),]
length(unique(d$ID))

#' Data visualizations

library(lattice)
histogram(~Weight, data=d)                        # Misleading histogram: data autocorrelated
densityplot(~Weight, data=d)
densityplot(~Weight | Gender, data=d)
densityplot(~Weight | Gender*Infected, data=d)

bwplot(Weight ~ Gender, data=d)
bwplot(Weight ~ Gender | Infected, data=d)

xyplot(Weight ~ Age | Infected, data=d)
xyplot(Weight ~ Age | ID, data=d[1:43,])

#' OLS model (pooled: does not control for individual-level differences)

ols <- lm(Weight ~ Infected*Gender + Infected*Age, data=d)
summary(ols)

#' Fixed effects model (controls for individual-level differences)

fe <- lm(Weight ~ Infected*Gender + Infected*Age + as.factor(ID), data=d)
summary(fe)

# Note: This fixed-effects models has 1032 fewer degrees of freedom

#' Random effects model using lme4 package (controls for WITHIN block differences)

library(lme4)
re <- lmer(Weight ~ Infected*Gender + Infected*Age + (1 | ID), data=d, REML=FALSE)
summary(re)
ranef(re)
coef(re)

library(stargazer)
options(max.print=10000)
stargazer(ols, fe, re, type="text", single.row=TRUE)


#' Fixed and random effects using plm package 
#' Note: plm uses GLS estimation, while lme4 uses MLE estimation.
#'       plm can handle fixed effects or random effects but not both at the same time.
#'       plm is used only for panel models, where level 1 is time.

library(plm)
pooled <- plm(Weight ~ Infected*Gender + Infected*Age, data=d, index="ID", model="pooling")
fixed  <- plm(Weight ~ Infected*Gender + Infected*Age, data=d, index="ID", model="within")
random <- plm(Weight ~ Infected*Gender + Infected*Age, data=d, index="ID", model="random")

stargazer(pooled, fixed, random, type="text", single.row=TRUE)

summary(fixed)
summary(fixef(fixed))
summary(random)
ranef(random)

plmtest(pooled)                                # H0: No multi-level effects in data
pFtest(fixed, pooled)                          # H0: No difference between fixed and pooled models
phtest (fixed, random)                         # H0: NO difference between fixed and random models



