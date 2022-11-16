#' Assignment A6: Mixed-level models
#' Data: BigMartSales.xlsx
#' Multi-level data: Upper level: Outlets (Outlet_ID, Outlet_Type) 
#'                   Lower level: Items (Item_ID, Item_MRP, Item_Desc)

# Read & preprocess data

setwd("C:/Users/abhatt/Desktop/SDM/Data")
setwd("/Users/ab/Desktop/SDM/Data")
library("readxl")
d <- read_excel("BigMartSales.xlsx", sheet="Data")
str(d)

col <- c("Item_ID", "Item_Fat_Content", "Item_Type", "Outlet_ID", "Outlet_Size", 
         "City_Type", "Outlet_Type")
d[col] <- lapply(d[col], factor)
str(d)
View(d)

d$Outlet_Type <- relevel(d$Outlet_Type, ref="Grocery Store")     # Smallest store
d$City_Type <- relevel(d$City_Type, ref="Tier 1")                # Largest city

colSums(is.na(d))                                                # Missing values
#' Note: 2410 missing values under Outlet_Size, 1463 missing values for Item-Weight.
#'       Outlet_Size is a proxy for Outlet_ID, we will drop Outlet_Size from our analysis.
#'       But since item price is a function of item weight, we have to keep weight in our models.

#' Data Exploration

hist(d$Item_Sales)
hist(log(d$Item_Sales))

library(lattice)
histogram(~Item_Sales | Outlet_Type, data=d)
densityplot(~Item_Sales | Outlet_Type, data=d)
bwplot(Item_Sales ~ Outlet_Type, data=d)
bwplot(Item_Sales ~ City_Type, data=d)

table(d$Outlet_Type)
table(d$City_Type)
table(d$City_Type, d$Outlet_Type)

df <- d[, c(2, 4, 6, 12)]
str(df)
library(PerformanceAnalytics)
chart.Correlation(df)                                 # Note: Only item-level variables


#' Variable selection for Mixed Level Analysis

#' Dependent variable: Item_Sales

#' Lower (item) level variables that affect item sales:
#'   Item_ID:          Unit of analysis (will not be included in model)
#'   Item_Visibility:  More visible products should sell more
#'   Item_Type:        Certain item types such as dairy or vegetables may sell more
#'   Item_MRP:         Pricier items may sell less

#' Upper (store) level Variables that affect item sales
#'   Outlet_ID:        Unit of analysis for upper-level; needed for Question 3
#'   Outlet_Type:      For Question 1: how sales vary by outlet type
#'   City_Type:        For Question 2: how sales vary by city type
#'   Outlet_Age:       New variable: 2013 - Outlet_Year, since older outlets may have 
#'                     more established clientale and may sell more than new ones. 

d$Outlet_Age = 2013 - d$Outlet_Year


#' Regression Models

ols <- lm(log(Item_Sales) ~ Item_Visibility + Item_Type + Item_MRP + Outlet_Type +
             City_Type + Outlet_Age, data=d)       # Note: May have correlated errors

fe  <- lm(log(Item_Sales) ~ Item_Visibility + Item_Type + Item_MRP + Outlet_Type + 
            City_Type + Outlet_Age + Outlet_ID, data=d)  

library(lme4)             
re <- lmer(log(Item_Sales) ~ Item_Visibility + Item_Type + Item_MRP + Outlet_Type + 
             City_Type + Outlet_Age + (1 | Outlet_ID), data=d, REML=FALSE)   

library(stargazer)
stargazer(ols, fe, re, type="text", single.row=TRUE)

ranef(re)
AIC(ols, fe, re)
BIC(ols, fe, re)                                      #' RE model has the best fit


#' Test of assumptions
#' Note: RE model is a GLM model; DW test does not work for multi-level class

library(car)
vif(re)

#' install.packages("ivpanel")
library(ivpanel)
hausman(fe, re)                                       # Hausman test: FE is better




