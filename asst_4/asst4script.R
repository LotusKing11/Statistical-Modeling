#asst4 script
library(Hmisc)
library(PerformanceAnalytics)
library(readxl)
library(stargazer)
library(car)
library(lmtest)
setwd("~/Documents/USF_new/SDM/Assignments/asst_4")
str(df1)
summary(df1$spend)
hist(df1$spend)
hist(log(df1$spend))
unique(df1$historysegment)
factors <- c('historysegment', 'zipcode', 'channel', 'campaign', 'mens', 'womens', 'newcustomer', 'phone', 'web')
df1[, factors] <- lapply(df1[, factors], factor)
colSums(is.na(df1)) 
library(dplyr)
df2 <- filter(df1, conversion >0)
str(df2)
sum(df1$conversion)

hist(df2$spend)
hist(log(df2$spend))
hist(df2$history)
df2$campaign <- relevel(df2$campaign, "No E-Mail")
attach(df2)

df2$web <- ifelse(df2$channel == "Web"| df2$channel == "Multichannel", 1, 0)
df2$phone <- ifelse(df2$channel == "Phone"| df2$channel == "Multichannel", 1, 0)

factors2 <- c('phone', 'web')
df2[, factors2] <- lapply(df2[, factors2], factor)

df2$round_spend <- round(df2$spend)
glm1 <- glm(df2$round_spend ~ 1, family = poisson (link = log), data = df2)
glm2 <- glm(df2$round_spend ~ recency + log(history) + zipcode + channel + campaign,
            family = poisson (link = log), data = df2)
glm3 <- glm(df2$round_spend ~ recency + log(history) + zipcode + channel + 
              campaign + mens*campaign + womens*campaign, family = poisson (link = log), data = df2)

nbinom1<- glm.nb(round_spend ~  log(history) + zipcode + channel + campaign + mens +womens,
                 data = df2)
nbinom2 <- glm.nb(round_spend ~ history + zipcode + campaign + newcustomer + phone +web +
                    campaign*newcustomer + campaign*history +campaign*phone +campaign*web +campaign*mens + campaign*womens,
                  data = df2)
nbinom3 <- glm.nb(round_spend ~ recency + history + zipcode + newcustomer + phone + web +
                    campaign + campaign*newcustomer + campaign*history +campaign*phone +campaign*web +campaign*mens + campaign*womens,
                  data = df2)


nbinom4 <- glm.nb(round_spend ~ log(history) + zipcode + campaign + newcustomer + phone +web +
                    campaign*newcustomer + campaign*log(history) +campaign*phone +campaign*web +campaign*mens + campaign*womens,
                  data = df2)
nbinom5 <- glm.nb(round_spend ~ recency + history + zipcode + newcustomer + phone + web +
                    campaign + campaign*newcustomer + campaign*log(history) +campaign*phone +campaign*web +campaign*mens + campaign*womens,
                  data = df2)


##nbinom3 <- glm.nb(round_spend ~ recency + log(history) + zipcode + channel + newcustomer +
                   #campaign + mens*campaign + womens*campaign, data = df2)
##nbinom4 <- glm.nb(round_spend ~ recency + log(history) + zipcode + channel + newcustomer +
                    #campaign + mens*campaign + womens*campaign + newcustomer*campaign,data = df2)
summary(nbinom1)
summary(nbinom2)
summary(nbinom3)
summary(nbinom4)

stargazer(nbinom1, nbinom2, nbinom3, out = "star_out_spend_nbinom.txt", align = TRUE, title = "NBinom Regression Results", single.row = T, type = "text")

stargazer(nbinom1, nbinom4, nbinom5, out = "star_out_spend_nbinom2.txt", align = TRUE, title = "NBinom Regression Results", single.row = T, type = "text")



stargazer(glm1, glm2, glm3, out = "star_out_spend.txt", align = TRUE, title = "Regression Results", single.row = T, type = "text")





