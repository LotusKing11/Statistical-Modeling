##ASST 8 Script
library(readxl)
library(xts)
library(lattice)
library(PerformanceAnalytics)#corr plot
library(MASS) ##nbinom models
library(stargazer)


#Import all three sheets
stores <- read_excel("~/Documents/USF_new/SDM/Assignments/Asst_8/SnackChain.xlsx", 
                     sheet = "stores")
products <- read_excel("~/Documents/USF_new/SDM/Assignments/Asst_8/SnackChain.xlsx",
                       sheet = "products")
transactions <- read_excel("~/Documents/USF_new/SDM/Assignments/Asst_8/SnackChain.xlsx",
                           sheet = "transactions")
View(head(transactions))
View(head(products))
View(head(stores))

str(transactions)
str(products)
str(stores)

## Merge all three datasets into one

df1 = merge(x=products,y=transactions,by="UPC",all=TRUE)
df2 = merge(x=df1,y=stores,by.x = "STORE_NUM", by.y = "STORE_ID",all=TRUE)
View(head(df2))
str(df2)
colSums(is.na(df2))

### Remove Oral Hygiene Transactions
df2 <- df2[df2$CATEGORY != "ORAL HYGIENE PRODUCTS", ]

### Remove Parking column because we wont use it and it has 300K+ missing values
df2 <- df2[ , -which(names(df2) %in% c("PARKING"))]
str(df2)

### Remove NAs
colSums(is.na(df2))
df2 <- df2[complete.cases(df2), ]

### Make appropriate variables into factors
attach(df2)
factors <- c('CATEGORY', 'SUB_CATEGORY', 'DESCRIPTION', 'MANUFACTURER', 'MSA', 'SEGMENT', 'CITY', 'STATE', 'PRODUCT_SIZE')
df2[, factors] <- lapply(df2[, factors], factor)
str(df2)

### FEATURE ENGINEERING
df2$DISCOUNT_AMOUNT <- df2$BASE_PRICE - df2$PRICE #creating column that tells us the price difference amount
df2$SPEND <- round(df2$SPEND)

##DESCRIPTIVE ANALYTICS

####histogram of targets
hist(df2$SPEND)
hist(df2$UNITS)
hist(df2$HHS) #all are exponentially distributed, would be better to round to 
              #nearest integer and then use poisson --> adding this to FEATURE ENGINEERING
densityplot(~UNITS | MSA, data=df2)

###Correlation matrix
corrvars <- c('VISITS', 'PRICE', 'BASE_PRICE', 'SIZE', 'AVG_WEEKLY_BASKETS', 'DISCOUNT_AMOUNT')
set.seed(100)
df4 <- df2[sample(nrow(df2), 10000), ]
df4 <- subset(df4, select = corrvars)
chart.Correlation(df4)

##Q1 
'#What are the effects of product display, being featured on in-store circular, 
and temporary price reduction on product sales (spend), unit sales, and number of household purchasers?
'#

Q1spend_nbinom <- glm.nb(SPEND ~ SEGMENT + SIZE + MANUFACTURER + CATEGORY + 
                + PRICE + DISPLAY +  DISCOUNT_AMOUNT+FEATURE + TPR_ONLY + MSA + AVG_WEEKLY_BASKETS + PRODUCT_SIZE,
                data=df2)
summary(Q1spend_nbinom)

Q1unit_nbinom <- glm.nb(UNITS ~ SEGMENT + SIZE + MANUFACTURER + CATEGORY + 
                          + PRICE + DISPLAY + DISCOUNT_AMOUNT+ FEATURE + TPR_ONLY + MSA + AVG_WEEKLY_BASKETS + PRODUCT_SIZE,
                        data=df2)
summary(Q1unit_nbinom)

Q1hhs_nbinom <- glm.nb(HHS ~ SEGMENT + SIZE + MANUFACTURER + CATEGORY + 
                          + PRICE +  DISCOUNT_AMOUNT+ DISPLAY + FEATURE + TPR_ONLY + MSA + AVG_WEEKLY_BASKETS + PRODUCT_SIZE,
                        data=df2)
summary(Q1hhs_nbinom)

stargazer(Q1spend_nbinom, Q1unit_nbinom, Q1hhs_nbinom, out = "star_out_Q1.txt", align = TRUE, 
          title = "Promotion on Targets Model Results", single.row = T, type = "text")


##Q2 How do the effects of display, feature, and TPR on SPEND vary by product 
'#categories (cold cereals, frozen pizza, bag snacks) and store segments 
'#(mainstream, upscale, value)?

Q2promo_spend <-glm.nb(SPEND ~ SEGMENT + SIZE + MANUFACTURER + CATEGORY + 
                        + PRICE + DISPLAY + FEATURE + TPR_ONLY + MSA + 
                        AVG_WEEKLY_BASKETS +  DISCOUNT_AMOUNT+ 
                        DISPLAY*CATEGORY+ DISPLAY*SEGMENT
                      +FEATURE*CATEGORY+ FEATURE*SEGMENT
                      + TPR_ONLY*CATEGORY+ TPR_ONLY*SEGMENT,
                      data=df2)
summary(Q2promo_spend)

df3<-df2
df3$UPC <-as.factor(df3$UPC)
length(unique(df3$UPC))
attach(df3)


Q3Price_elas <-glm.nb(UNITS ~ SEGMENT + CATEGORY + 
                       + PRICE + DISPLAY + FEATURE + TPR_ONLY + MSA + 
                       AVG_WEEKLY_BASKETS + DISCOUNT_AMOUNT + PRICE*UPC, data=df3)
summary(Q3Price_elas)

#Q4 Maximize Unit Sales and Spend

Q4Spend_PriceElas <-glm.nb(SPEND ~ SEGMENT + CATEGORY + 
                        + PRICE + DISPLAY + FEATURE + TPR_ONLY + MSA + 
                        AVG_WEEKLY_BASKETS + DISCOUNT_AMOUNT + PRICE*UPC, data=df3)
summary(Q4Spend_PriceElas)
