##Asst 7 telco

library(readxl)
library(ROCR) 
library(MLmetrics)
setwd("~/Documents/USF_new/SDM/Assignments/Asst_7")
TelcoChurn <- read_excel("~/Documents/USF_new/SDM/Assignments/Asst_7/TelcoChurn.xlsx")
View(head(TelcoChurn))

df <- TelcoChurn
colSums(is.na(df))
str(df)
df <- df[complete.cases(df), ]  
attach(df)
unique(df$PhoneService)
unique(df$InternetService)

##creating binary columns 3 cases for analysis
df$phoneonly <- ifelse(df$PhoneService=='Yes' & df$InternetService=='No', 1, 0)
df$internetonly <- ifelse(df$PhoneService=='No' & df$InternetService != 'No', 1, 0)
df$both <- ifelse(df$phoneonly=='0' & df$internetonly == '0', 1, 0)
df$binary_churn <- ifelse(df$Churn == 'Yes', 1, 0)
View(head(df))
str(df)

table(df$Churn)
table(df$Churn, df$phoneonly)
table(df$Churn, df$internetonly)
table(df$Churn, df$both)

#factorizing appropriate variables
factors <- c(2:5, 7:18)
df[, factors] <- lapply(df[, factors], factor)
df$Contract <- ordered(df$Contract)
View(head(df))
str(df)

## Creating data sets for the three different cases

internet_data <- df[df$internetonly==1, ]
View(head(internet_data))
str(internet_data)

phone_data <- df[df$phoneonly ==1, ]
View(head(phone_data))
str(phone_data)

both_data <- df[df$both ==1, ]
View(head(both_data))
str(both_data)

##creating train and test sets for the 3 cases
set.seed(1024)

trainIndex <- sample(1:nrow(phone_data), size=round(0.75*nrow(phone_data)), 
                     replace=FALSE)
trainphone <- phone_data[trainIndex,]
testphone  <- phone_data[-trainIndex,]
dim(trainphone); dim(testphone)
str(testphone)

trainIndex1 <- sample(1:nrow(internet_data), size=round(0.75*nrow(internet_data)), replace=FALSE)
traininternet <- internet_data[trainIndex1,]
testinternet  <- internet_data[-trainIndex1,]
dim(traininternet); dim(testinternet)

trainIndex2 <- sample(1:nrow(both_data), size=round(0.75*nrow(both_data)), replace=FALSE)
trainboth <- both_data[trainIndex2,]
testboth  <- both_data[-trainIndex2,]
dim(trainboth); dim(testboth)

## The 3 models + recall, precision, F1-score, and AUC

### PHONE ONLY
logitphone  <- glm(binary_churn ~ gender + SeniorCitizen + Dependents + tenure
                      + Contract + PaperlessBilling +MonthlyCharges
                     +TotalCharges , family=binomial (link="logit"), data=trainphone)

testphone_x <- testphone[ , c(1:24)]
predlogitphone_decimal <-predict(logitphone, newdata=testphone_x, type="response")
View(head(predlogitphone_decimal))
predlogitphone_binary <- ifelse(predlogitphone_decimal>0.3, 1, 0)

table(testphone$binary_churn, predlogitphone_binary)        # Confusion matrix
class_error_phone <- mean(predlogitphone_binary!= testphone$binary_churn) # Classification error
print(paste("Accuracy = ", 1- class_error_phone))        # Accuraty rate

####from rocr package
pr_phone <- prediction(predlogitphone_binary, testphone$binary_churn)
prf_phone <- performance(pr_phone, measure="tpr", x.measure="fpr")
plot(prf_phone, main = "ROC Curve - Phone Only")                                                 # ROC plot: TPR vs FPR

auc_phone <- performance(pr_phone, measure="auc")
auc_phone <- auc_phone@y.values[[1]]
auc_phone                                                       # Area Under the Curve

library(MLmetrics)
recall_phone = Recall(testphone$binary_churn, predlogitphone_binary, positive = NULL)
print(paste('Recall =' ,recall_phone))
precision_phone = Precision(testphone$binary_churn, predlogitphone_binary, positive = NULL)
print(paste('Precision =' ,precision_phone))
f1_phone = F1_Score(predlogitphone_binary,testphone$binary_churn)
print(paste('F1 Score =' ,f1_phone))
auc_phone = AUC(predlogitphone_binary,testphone$binary_churn)
print(paste('AUC value =' , auc_phone))


### INTERNET ONLY
logitinternet <- glm(binary_churn ~ gender + SeniorCitizen + Dependents + tenure  
                     + Contract + PaperlessBilling +MonthlyCharges
                   +TotalCharges + OnlineBackup + OnlineSecurity + TechSupport, family=binomial (link="logit"), data=traininternet)

testinternet_x <- testinternet[ , c(1:24)]
predlogitinternet_decimal <-predict(logitinternet, newdata=testinternet_x, type="response")
View(head(predlogitinternet_decimal))
predlogitinternet_binary <- ifelse(predlogitinternet_decimal>0.5, 1, 0)

table(testinternet$binary_churn, predlogitinternet_binary)        # Confusion matrix
class_error_internet <- mean(predlogitinternet_binary!= testinternet$binary_churn) # Classification error
print(paste("Accuracy = ", 1- class_error_internet))        # Accuraty rate


pr_internet <- prediction(predlogitinternet_binary, testinternet$binary_churn)
prf_internet <- performance(pr_internet, measure="tpr", x.measure="fpr")
plot(prf_internet, main = "ROC Curve - Internet Only")                                                 # ROC plot: TPR vs FPR

auc_internet <- performance(pr_internet, measure="auc")
auc_internet <- auc_internet@y.values[[1]]
auc_internet                                                       # Area Under the Curve


recall_internet = Recall(testinternet$binary_churn, predlogitinternet_binary, positive = NULL)
print(paste('Recall =' ,recall_internet))
precision_internet = Precision(testinternet$binary_churn, predlogitinternet_binary, positive = NULL)
print(paste('Precision =' ,precision_internet))
f1_internet = F1_Score(predlogitinternet_binary,testinternet$binary_churn)
print(paste('F1 Score =' ,f1_internet))
auc_internet = AUC(predlogitinternet_binary,testinternet$binary_churn)
print(paste('AUC value =' , auc_internet))

###BOTH

logitboth <- glm(binary_churn ~ gender + SeniorCitizen + Dependents + tenure  
                 + Contract + PaperlessBilling +MonthlyCharges
                 +TotalCharges + OnlineBackup + OnlineSecurity + TechSupport , family=binomial (link="logit"), data=trainboth)

testboth_x <- testboth[ , c(1:24)]
predlogitboth_decimal <-predict(logitboth, newdata=testboth_x, type="response")
View(head(predlogitboth_decimal))
predlogitboth_binary <- ifelse(predlogitboth_decimal>0.4, 1, 0)

table(testboth$binary_churn, predlogitboth_binary)        # Confusion matrix
class_error_both <- mean(predlogitboth_binary!= testboth$binary_churn) # Classification error
print(paste("Accuracy = ", 1- class_error_both))        # Accuraty rate


pr_both <- prediction(predlogitboth_binary, testboth$binary_churn)
prf_both <- performance(pr_both, measure="tpr", x.measure="fpr")
plot(prf_both, main = "ROC Curve - Both")                                                 # ROC plot: TPR vs FPR

auc_both <- performance(pr_both, measure="auc")
auc_both <- auc_both@y.values[[1]]
auc_both                                                       # Area Under the Curve


recall_both = Recall(testboth$binary_churn, predlogitboth_binary, positive = NULL)
print(paste('Recall =' ,recall_both))
precision_both = Precision(testboth$binary_churn, predlogitboth_binary, positive = NULL)
print(paste('Precision =' ,precision_both))
f1_both = F1_Score(predlogitboth_binary,testboth$binary_churn)
print(paste('F1 Score =' ,f1_both))
auc_both = AUC(predlogitboth_binary,testboth$binary_churn)
print(paste('AUC value =' , auc_both))



### Starazer of models
stargazer(logitphone, logitinternet, logitboth, out = "star_out_classification.txt", align = TRUE, 
          title = "Classification Model Results", single.row = T, type = "text")



