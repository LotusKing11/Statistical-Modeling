library(stargazer)
attach(CreditRating)
CreditRating$Gender <- as.factor(CreditRating$Gender)
names <- c('Student', 'Married', 'Ethnicity')
CreditRating[, names] <- lapply(CreditRating[,names], factor)
str(CreditRating)

model1 <- lm(Rating ~ Income + Age +Student+Education+Balance, data = CreditRating)
model2 <- lm(Rating ~ Income + Age +Student+Education+Balance+Married, data = CreditRating)
model3 <- lm(Rating ~ Income + Age +Student+Education+Balance+Married+Cards, data = CreditRating)
model4 <- lm(Rating ~ Income + Student+Married+Ethnicity+Gender, data = CreditRating)
model5 <- lm(Rating ~ Income + Student+ Balance + Ethnicity+Gender, data = CreditRating)
model6 <- lm(Rating ~ Income + Student+ Balance + Ethnicity+Gender + Student*Ethnicity+Student*Gender, data = CreditRating)
model7 <- lm(Rating ~ Income + Student+ Balance + Ethnicity+Gender + Balance*Ethnicity+Balance*Gender, data = CreditRating)

basemodel <- lm(Rating ~1, data = CreditRating)

stargazer(model2, model5, model7, out = "star_out_CreditRating.txt", align = TRUE, title = "Regression Results", single.row = T)



