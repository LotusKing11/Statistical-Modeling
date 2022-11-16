#' Multi-Level Analysis of Oats Yield
#' Three varieties of oats grown in 6 different blocks with different fertilizer (nitrogen) settings
#' Question: How do nitrogen content in fertilizer impact yields of different varieties of oats?
#' Note: This is an example of a 3x6x4 factorial experimental design

setwd("C:/Users/abhatt/Desktop/SDM/Data")
setwd("/Users/ab/Desktop/SDM/Data")
d <- read.csv("OatsYield.csv")
str(d)
summary(d)
View(d)
d$block = factor(d$block)
d$variety = factor(d$variety)

#' Data visualizations

hist(d$yield)                                   # Misleading histogram: has different varieties
library(lattice)
histogram(~yield, data=d)
densityplot(~yield, data=d)
densityplot(~yield | variety, data=d)
densityplot(~yield | block, data=d)
densityplot(~yield | variety*block, data=d)

bwplot(yield ~ variety, data=d)
bwplot(yield ~ block, data=d)
bwplot(yield ~ block | variety, data=d)

xyplot(yield ~ nitrogen | variety, data=d)
xyplot(yield ~ nitrogen | block, data=d)
xyplot(yield ~ nitrogen | variety*block, data=d)

#' OLS model (pooled: does not control for block-level differences)

d$variety <- relevel(d$variety, ref="Victory")
ols <- lm(yield ~ nitrogen*variety, data=d)
summary(ols)
confint(ols)

#' Questions: What inference do you make from the OLS analysis?
#'            What is the slope of nitogen*varietyVictory in the OLS model?

#' Fixed effects model (controls for BETWEEN block differences)

fe <- lm(yield ~ nitrogen*variety + block, data=d)
summary(fe)
confint(fe)

#' Random effects model (controls for WITHIN block differences) using lme4 package (LMM estimation)
#' LMM = linear mixed model: an extension of linear models where the linear predictor 
#' contains random effects in addition to the usual fixed effects.
#' LME = linear mixed effects; mixed effects models contain both fixed and random effects.
#' Note: In lme4, random effects are specified within parentheses; fixed effects outside parenthesis.
#' Note: If REML (Restricted Maximum Likelihood) is set to FALSE, we use standard MLE.

library(lme4) 
re <- lmer(yield ~ nitrogen*variety + (1 | block), data=d, REML=FALSE)
summary(re)
confint(re)
AIC(re)
fixef(re)                                       # Magnitude of fixed effects
ranef(re)                                       # Magnitude of random effects
coef(re)                                        # Magnitude of total effects

library(stargazer)
stargazer(ols, fe, re, type="text", single.row=TRUE)
AIC(ols, fe, re)

