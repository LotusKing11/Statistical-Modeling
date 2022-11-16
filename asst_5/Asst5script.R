#' ASST 5 SCRIPT
#' 
#' 

###Libraries
library(Hmisc)
library(PerformanceAnalytics)
library(readxl)
library(car)
library(stargazer)
library(corrplot)
library(lme4)
library(lattice)
library(lmtest)

BigMartSales <- read_excel("Documents/USF_new/SDM/Assignments/asst_5/BigMartSales.xlsx")
View(BigMartSales) 
str(BigMartSales)
attach(BigMartSales)
length(unique(Outlet_ID))


#Exploration - sales is target variable
hist(Item_Sales)
hist(log(Item_Sales))
BigMartSales$Item_Sales<- round(BigMartSales$Item_Sales, digits = 0)
View(head(BigMartSales))
df1<-BigMartSales
'# df1$ItemSalesdiv1000 <- df1$Item_Sales/1000
'#df1$Item_MRPdiv100 <- df1$Item_MRP/100
df1$Outlet_Year <- as.numeric(df1$Outlet_Year)
df1$Store_Age <- (2022-df1$Outlet_Year)
View(head(df1))
densityplot(~Item_Sales|Outlet_Type)
densityplot(~Item_Sales|City_Type)
densityplot(~Item_Sales|Outlet_ID)

factors <- c('Item_Fat_Content', 'Item_Type', 'Outlet_ID','Outlet_Year', 'Outlet_Size', 'City_Type', 'Outlet_Type', 'Store_Age') ##factorize multiple variables
df1[, factors] <- lapply(df1[, factors], factor)
str(df1)
# Checking for missing values
colSums(is.na(df1))  #' many missing values in item_weight and outlet_size

#MODELS

#Random Effects
library(lme4) 
re1 <- glmer(ItemSalesdiv1000 ~ Item_Type+ City_Type + Item_MRPdiv100 + Item_Fat_Content + 
             Item_Visibility + Outlet_Year + Outlet_Type +(1 | Outlet_ID), data=df1, family=poisson(link=log))
summary(re1)
confint(re1)
AIC(re1)
fixef(re1)                                       # Magnitude of fixed effects
ranef(re1)                                       # Magnitude of random effects
coef(re1)  
#model summary not getting created - gets hung

re2 <- lmer(log(Item_Sales) ~ Item_Type+ City_Type + log(Item_MRP) + Item_Fat_Content + 
                  Item_Visibility + Outlet_Type +(1 | Outlet_ID), data=df1) #**works

re3 <- lmer(log(Item_Sales) ~ Item_Type+ log(Item_MRP) + Item_Fat_Content + 
              Item_Visibility + Store_Age + Outlet_ID + Outlet_Type +(1 | City_Type ), data=df1)

re4 <- lmer(log(Item_Sales) ~  log(Item_MRP) + Item_Fat_Content + 
              Item_Visibility + Store_Age + Outlet_ID + Outlet_Type +(1 | City_Type ), data=df1) 
summary(re2)

summary(re3)
summary(re4)

confint(re2)
AIC(re2)
fixef(re2)                                       # Magnitude of fixed effects
ranef(re2)                                       # Magnitude of random effects
coef(re2)  

##FixedEffects
fe1 <- lm(log(Item_Sales)~ Item_Type+ City_Type + Item_MRP + Item_Fat_Content + 
            Item_Visibility + Store_Age + Outlet_Type +Outlet_ID, data=df1)

summary(fe1)


fe2 <- glm(Item_Sales ~ Item_Type+ City_Type + Item_MRP + Item_Fat_Content + 
               Item_Visibility + Store_Age + Outlet_Type +Outlet_ID, data=df1, family=poisson(link=log))
summary(fe2)

fe3 <- glm(Item_Sales ~ Item_Visibility + Item_Fat_Content + Item_Type + Item_MRP
          + City_Type + Outlet_Type + Outlet_ID, family=poisson (link=log), data=df1)
summary(fe3)

fe4 <- glm(Item_Sales ~ Item_Visibility + Item_Fat_Content + Item_MRP
           + City_Type + Outlet_Type + Outlet_ID, family=poisson (link=log), data=df1)
summary(fe4)

#Assumption testing
### Normality
par(mfrow=c(2,2))
plot(df1$Item_Sales, re2$fitted.values,
     pch=19,main="Actuals v. Fitted")
abline(0,1,col="red",lwd=3)
qqnorm(re2$residuals,pch=19,
       main="Model Normality Plot")
qqline(re2$residuals,lwd=3,col="red")
hist(re2$residuals,col="red",
     main=" Model Residuals Histogram",
     freq=FALSE)
curve(dnorm(x,mean(re2$residuals),
            sd(re2$residuals)),
      from=min(re2$residuals),
      to=max(re2$residuals),lwd=3,
      add=TRUE)
plot(re2$fitted.values,rstandard(re2),
     pch=19,main="Versus Fit Plot")
abline(0,0,col="red",lwd=3)
par(mfrow=c(1,1))
moments::skewness(re2$residuals)
moments::kurtosis((re2$residuals))
bartlett.test(list(re2$res, re2$fit)) ##homoskedasticity

dwtest(resid(re2))##Durbin Watson test for Independence
vif(re2)##Vairance inflation test for multicollinearity

#stargazer of best models
stargazer(fe2, re2, re3, out = "star_out_mlm.txt", 
                    align = TRUE, title = "MLM Regression Results", single.row = T, type = "text")



