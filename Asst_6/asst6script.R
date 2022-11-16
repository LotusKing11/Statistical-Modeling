#asst6 script

library(survival)
library(survminer)
library(readxl)
library(stargazer)
df1 <- read_excel("LungCancer.xlsx")

factors <- c('Treatment', 'CellType', 'PriorChemotherapy')
df1[, factors] <- lapply(df1[, factors], factor)
View(head(df1)) 
str(df1)
attach(df1)
colSums(is.na(df1))  

y <- Surv(SurvivalDays, Status)
km1 <- survfit(y ~ 1, data = df1)      
summary(km1)
plot(km1, xlab="Time", ylab="Survival Probability")
ggsurvplot(fit = km1, xlab = "Time", ylab="Survival Probability")

#Non-Parametric
km2 <- survfit(y ~ Treatment, data = df1)      
summary(km2)
plot(km2, xlab="Time", ylab="Survival Probability")
ggsurvplot(fit = km2, xlab = "Time", ylab="Survival Probability")
summary(km2, times = 365)
summary(km2, times = 183)

cox <- coxph(y ~ Treatment + AgeInYears + KarnofskyScore + PriorChemotherapy + 
               CellType + MonthsFromDiagnosis)
summary(cox)

#Parametric
exp <- survreg(y ~ Treatment + AgeInYears + KarnofskyScore + PriorChemotherapy + 
                 CellType + MonthsFromDiagnosis, dist="exponential")
summary(exp)

loglogistic <- survreg(y ~ Treatment + AgeInYears + KarnofskyScore + PriorChemotherapy + 
                         CellType + MonthsFromDiagnosis, dist="loglogistic")
summary(loglogistic)

stargazer(cox, exp, loglogistic, out = "star_out_surv.txt", align = TRUE, 
          title = "Survival Model Results", single.row = T, type = "text")

expcoef <- (exp(exp$coefficients))
as.data.frame((expcoef))
expcoef$coef <- exp$coefficients
exp$coefficients

logcoef <- (exp(loglogistic$coefficients))
as.data.frame((logcoef))


