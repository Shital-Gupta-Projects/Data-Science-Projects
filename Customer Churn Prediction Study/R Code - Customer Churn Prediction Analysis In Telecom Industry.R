#############################GROUP ASSIGNMENT FOR PREDICTIVE MODELING############################
#############################SUBMITTED BY: GROUP 10 - SHITAL GUPTA###############################
###############R PROJECT: CUSTOMER CHURN PREDICTION ANALYSIS IN TELECOM INDUSTRY#################
#################################################################################################
#########################Start of the Analysis################################
#########################Start of the Code####################################
####Step 1: - Setting the Current Working Directory####
getwd()
setwd("C:/Users/shital/Documents/R/RProgramming")
dir()
####Step 2 :- Reading and viewing the data from the dataset####
CustServData <- read.csv(file.choose())
CustomerServiceData <- CustServData
View(CustomerServiceData) #To view the dataset#
####Step 3 :-Start of Exploratory Data Analysis####
###Step 3.1: - Data Interpretation###
##Step 3.1.1:- Data Definition##
library(psych) #For describe function#
library(dplyr)
library(plotrix)
library(DescTools)
library(PerformanceAnalytics)
library(ggplot2)
library(tidyr)
library(purrr)
library(DataExplorer)
library(corrplot)
library(GGally)
library(pastecs)
library(descr)
library(RColorBrewer)
library(gridExtra)
library(ltm)
library(explore)
library(naniar)
library(caret)
dim(CustomerServiceData) #Checking the dimensions for dataset variables#
head(CustomerServiceData) #Reading the data#
##Step 3.1.2:- Data Summary##
introduce(CustomerServiceData)
plot_intro(CustomerServiceData)
str(CustomerServiceData) #Checking the data structure#
#Fixing the data types for the variables#
CustomerServiceData$Churn <- as.factor(CustomerServiceData$Churn)
CustomerServiceData$ContractRenewal <- as.factor(CustomerServiceData$ContractRenewal)
CustomerServiceData$DataPlan <- as.factor(CustomerServiceData$DataPlan)
#Rechecking data structure to confirm on data type conversion#
str(CustomerServiceData)
#Five-Number Summary for Data#
summary(CustomerServiceData)
#Descriptive Statistics for Data#
describe(select_if(CustomerServiceData, is.numeric))
#Additional descriptive Statistics for Data#
options(scipen = 100)
options(digits = 2)
stat.desc(select_if(CustomerServiceData, is.numeric), basic = F)
#NearZeroVarianceCheck - Only Data usage shows zero variance#
nearZeroVar(CustomerServiceData, saveMetrics = TRUE) 
###Step 3.2: - Data Processing###
##Step 3.2.1: - Imbalanced Data Check##
par(mfrow = c(2,2))
#Plotting a 3D Pie Chart to check for Imbalance#
pie3D((prop.table(table(CustomerServiceData$Churn))), main = "Customer Churn Favorability", labels = c("Customer Did Not Cancel Service", "Customer Cancelled Service"), cex.main = 1.2, labelcex = 1.2,explode=0.1)
pie3D((prop.table(table(CustomerServiceData$ContractRenewal))), main = "Customer Contract Renewal Favorability", labels = c("Customer Did Not Renew Contract", "Customer Renewed Contract"), cex.main = 1.2, labelcex = 1.2, explode=0.1)
pie3D((prop.table(table(CustomerServiceData$DataPlan))), main = "Customer Data Plan Favorability", labels = c("Customer Does Not Have Data Plan", "Customer Has Data Plan"), cex.main = 1.2, labelcex = 1.2, explode=0.1)
##Step 3.2.2: - Missing Value Analysis##
#Checking for missing values#
any(is.na(CustomerServiceData))
sum(is.na(CustomerServiceData))
colSums(is.na(CustomerServiceData))
dim(unique(CustomerServiceData))[1] #No Duplicate Records#
colSums(CustomerServiceData[,c(2,5:11)]<0) #No Negative Records in continuous variables#
##Step 3.2.3: - Outlier Treatment##
#Printing boxplots for all the numeric variables#
boxplot(CustomerServiceData$AccountWeeks, col = "Red", main = "Boxplot for Account Weeks Variable")
boxplot(CustomerServiceData$DataUsage, col = "Green", main = "Boxplot for Data Usage Variable")
boxplot(CustomerServiceData$CustServCalls,col = "Pink", main = "Boxplot for Cust Serv Calls Variable")
boxplot(CustomerServiceData$DayMins, col = "Blue", main = "Boxplot for Day Mins Variable")
boxplot(CustomerServiceData$DayCalls, col = "Light Blue", main = "Boxplot for Day Calls Variable")
boxplot(CustomerServiceData$MonthlyCharge,col = "Light Green", main = "Boxplot for Monthly Charge Variable")
boxplot(CustomerServiceData$OverageFee,col = "Purple", main = "Boxplot for Overage Fee Variable")
boxplot(CustomerServiceData$RoamMins, main = "Boxplot for Roaming Mins Variable")
boxplot(select_if(CustomerServiceData, is.numeric)) #To view the boxplot for all variables in one plot#
#To display the outliers present in each variable#
boxplot(CustomerServiceData$AccountWeeks)$out
boxplot(CustomerServiceData$DataUsage)$out
boxplot(CustomerServiceData$CustServCalls)$out
boxplot(CustomerServiceData$DayMins)$out
boxplot(CustomerServiceData$DayCalls)$out
boxplot(CustomerServiceData$MonthlyCharge)$out
boxplot(CustomerServiceData$OverageFee)$out
boxplot(CustomerServiceData$RoamMins)$out
#To treat the outliers in the new data frame CustServDataUpd#
CustServDataUpd <- CustomerServiceData
#CustServDataUpd will be used only during model creation for comparisons#
CustServDataUpd$AccountWeeks <- Winsorize(CustServDataUpd$AccountWeeks)
CustServDataUpd$DataUsage <- Winsorize(CustServDataUpd$DataUsage)
CustServDataUpd$CustServCalls <- Winsorize(CustServDataUpd$CustServCalls)
CustServDataUpd$DayMins <- Winsorize(CustServDataUpd$DayMins)
CustServDataUpd$DayCalls <- Winsorize(CustServDataUpd$DayCalls)
CustServDataUpd$MonthlyCharge<- Winsorize(CustServDataUpd$MonthlyCharge)
CustServDataUpd$OverageFee <- Winsorize(CustServDataUpd$OverageFee)
CustServDataUpd$RoamMins <- Winsorize(CustServDataUpd$RoamMins)
#To Check if the outliers were removed from CustServDataUpd#
boxplot(CustServDataUpd$AccountWeeks)$out
boxplot(CustServDataUpd$DataUsage)$out
boxplot(CustServDataUpd$CustServCalls)$out
boxplot(CustServDataUpd$DayMins)$out
boxplot(CustServDataUpd$DayCalls)$out
boxplot(CustServDataUpd$MonthlyCharge)$out
boxplot(CustServDataUpd$OverageFee)$out
boxplot(CustServDataUpd$RoamMins)$out
##Step 3.2.4: - Analyzing the dependent variable and factors of interest##
#The other interpretations used in the report are derived from excel#
#We will only check for skewness for the variables here#
#For Dependent Variable#
skewness(as.numeric(CustomerServiceData$Churn))
SkewPlot <- CustomerServiceData %>% ggplot(aes(x = (as.numeric(CustomerServiceData$Churn)))) + stat_density(geom = "line", alpha = 1, colour = "cornflowerblue") + labs(x = "Customer Churn Probability - Churn", y = "Density")
Plot_Shaded_Area <- ggplot_build(SkewPlot)$data[[1]] %>% filter(x < mean(as.numeric(CustomerServiceData$Churn)))
ShadedSkewPlot <-  SkewPlot + geom_area(data = Plot_Shaded_Area, aes(x = x, y = y), fill="pink", alpha = 0.5)+ ggtitle("Density Plot Illustrating Skewness For Churn Variable")
print(ShadedSkewPlot)
#For Independent Variables#
skewness(select_if(CustomerServiceData, is.numeric))
#Density Plots for continuous variables#
CustomerServiceData %>% keep(is.numeric) %>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_density()
##Step 3.2.5: - Multicollinearity Analysis##
cor(select_if(CustomerServiceData, is.numeric)) #For only true numerical variables#
#We will be taking the data frame CustServData with all variables converted into numerical type for ease of analysis#
CustServData$Churn <- as.numeric(CustServData$Churn)
CustServData$Churn <-  ifelse(CustServData$Churn == 1, 0,1)
round(cor(CustServData),2) #To find out correlation coefficient values for the variables#
#To plot the correlation for the data variables#
corrplot(cor(select_if(CustomerServiceData, is.numeric)), method = "number",type = "lower", tl.cex = .9)
corrplot(cor(CustServData), method = "number",type = "lower", tl.cex = .9)
ggpairs(CustomerServiceData)
ggpairs(CustServData) #Data with all variables as numeric#
#Individual Correlation Test for both continuous and categorical variables#
#We will use Point Biserial Correlation test for continuous variables#
biserial.cor(CustomerServiceData$AccountWeeks, CustomerServiceData$Churn)
biserial.cor(CustomerServiceData$DataUsage, CustomerServiceData$Churn)
biserial.cor(CustomerServiceData$CustServCalls, CustomerServiceData$Churn)
biserial.cor(CustomerServiceData$DayMins, CustomerServiceData$Churn)
biserial.cor(CustomerServiceData$DayCalls, CustomerServiceData$Churn)
biserial.cor(CustomerServiceData$MonthlyCharge, CustomerServiceData$Churn)
biserial.cor(CustomerServiceData$OverageFee, CustomerServiceData$Churn)
biserial.cor(CustomerServiceData$RoamMins, CustomerServiceData$Churn)
## We will use Chi Square Test For categorical variables#
chisq.test(CustomerServiceData$Churn, CustomerServiceData$ContractRenewal)
as.matrix((prop.table(table(CustomerServiceData$ContractRenewal)))*100)
chisq.test(CustomerServiceData$Churn, CustomerServiceData$DataPlan)
as.matrix((prop.table(table(CustomerServiceData$DataPlan)))*100)
#Step 3.3: - Data Visualization###
##Step 3.3.1: - Univariate Analysis##
##For Continuous Numerical Variables##
par(mfrow = c(3,3), font.main = 2, font.axis = 2, font.lab = 2) #Convert our plotting space in 12 different frames#
NumData <- select_if(CustomerServiceData, is.numeric)
NumDataColName <- colnames(NumData)
for (ColNumber in (1:8)) {
  h = max(NumData[,ColNumber])+1
  l = min(NumData[,ColNumber])-1
  n = NumDataColName[ColNumber]
  hist (NumData[,ColNumber], breaks = seq(l,h,((h-l)/6)), include.lowest=T, right=T, col= brewer.pal(6,"Blues") , border=1, main = NULL , xlab= n, ylab=NULL, cex.lab= 1.5, cex.axis=1.2, cex.main=1)
}
CustomerServiceData %>% keep(is.numeric) %>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_histogram(fill = "blue")
dev.off()
#Bar charts for Categorical Variables - Class Favorability Analysis#
p1 <- ggplot(CustomerServiceData, aes(x = Churn, fill = Churn)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 2) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p2 <- ggplot(CustomerServiceData, aes(x = ContractRenewal, fill = ContractRenewal)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 2) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p3 <- ggplot(CustomerServiceData, aes(x = DataPlan, fill = DataPlan)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 2) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
grid.arrange(p1,p2,p3, nrow = 1, ncol = 3)
dev.off()
#Kernel Density Plots for Variables#
par(mfrow = c(3,3), font.main = 2, font.axis = 2, font.lab = 2)
plot(density(NumData$AccountWeeks), main = NA)
plot(density(NumData$DataUsage), main = NA)
plot(density(NumData$CustServCalls), main = NA)
plot(density(NumData$DayMins), main = NA)
plot(density(NumData$DayCalls), main = NA)
plot(density(NumData$MonthlyCharge), main = NA)
plot(density(NumData$OverageFee), main = NA)
plot(density(NumData$RoamMins), main = NA)
dev.off()
##Step 3.3.2:- Bivariate Analysis##
#Data Summary by our Dependent Variable Churn#
by(CustomerServiceData,CustomerServiceData$Churn,FUN = summary)
#CrossTab - Additional Data#
crosstab(CustomerServiceData$Churn, CustomerServiceData$ContractRenewal)
crosstab(CustomerServiceData$Churn, CustomerServiceData$DataPlan)
##Plotting the Categorical Variables##
#With Counts for each class#
p4 <- ggplot( data = CustomerServiceData, aes(x = ContractRenewal, fill = Churn)) + geom_bar(position ="dodge") + geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))+ scale_x_discrete(labels = c("0" = "Did Not Renew Contract", "1" = "Renewed Contract"))+ scale_fill_discrete(name = "Customer Churn Status", labels = c("Stayed", "Cancelled")) +ggtitle("ContractRenewal vs Churn")
p5 <-ggplot( data = CustomerServiceData, aes(x = DataPlan, fill = Churn)) + geom_bar(position ="dodge") + geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))+ scale_x_discrete(labels = c("0" = "Did Not Have Active Data Plan", "1" = "Had Active Data Plan"))+ scale_fill_discrete(name = "Customer Churn Status", labels = c("Stayed", "Cancelled")) +ggtitle("DataPlan vs Churn")
#With Percentages for each class#
p6 <- ggplot( data = CustomerServiceData, aes(x = DataPlan, fill = Churn)) + geom_bar(position ="fill") + geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.30))+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))+ scale_x_discrete(labels = c("0" = "Did Not Renew Contract", "1" = "Renewed Contract"))+ scale_fill_discrete(name = "Customer Churn Status", labels = c("Stayed", "Cancelled")) +ggtitle("ContractRenewal vs Churn")
p7 <- ggplot( data = CustomerServiceData, aes(x = DataPlan, fill = Churn)) + geom_bar(position ="fill") + geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.30))+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))+ scale_x_discrete(labels = c("0" = "Did Not Have Active Data Plan", "1" = "Had Active Data Plan"))+ scale_fill_discrete(name = "Customer Churn Status", labels = c("Stayed", "Cancelled")) +ggtitle("DataPlan vs Churn")
grid.arrange(p4,p5,p6,p7, nrow = 2, ncol = 2)
#Density Plot Analysis for the numerical variables#
CustomerServiceData[,c(1:11)] %>% explore_all(target = Churn)
#Histogram with Legends#
ggplot(CustomerServiceData, aes(x = AccountWeeks,fill = Churn)) + geom_histogram()
ggplot(CustomerServiceData, aes(x = DataUsage,fill = Churn)) + geom_histogram()
ggplot(CustomerServiceData, aes(x = CustServCalls,fill = Churn)) + geom_histogram()
ggplot(CustomerServiceData, aes(x = DayMins,fill = Churn)) + geom_histogram()
ggplot(CustomerServiceData, aes(x = DayCalls,fill = Churn)) + geom_histogram()
ggplot(CustomerServiceData, aes(x = MonthlyCharge,fill = Churn)) + geom_histogram()
ggplot(CustomerServiceData, aes(x = OverageFee,fill = Churn)) + geom_histogram()
ggplot(CustomerServiceData, aes(x = RoamMins,fill = Churn)) + geom_histogram()
##Plotting the boxplots for the Numerical Variables##
p8 <- ggplot(data=CustomerServiceData, mapping=aes(x=Churn, y=AccountWeeks,fill = Churn)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p9 <- ggplot(data=CustomerServiceData, mapping=aes(x=Churn, y=DataUsage,fill = Churn)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p10 <- ggplot(data=CustomerServiceData, mapping=aes(x=Churn, y=CustServCalls,fill = Churn)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p11 <- ggplot(data=CustomerServiceData, mapping=aes(x=Churn, y=DayMins,fill = Churn)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p12 <- ggplot(data=CustomerServiceData, mapping=aes(x=Churn, y=DayCalls,fill = Churn)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p13 <- ggplot(data=CustomerServiceData, mapping=aes(x=Churn, y=MonthlyCharge,fill = Churn)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p14 <- ggplot(data=CustomerServiceData, mapping=aes(x=Churn, y=OverageFee,fill = Churn)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p15 <- ggplot(data=CustomerServiceData, mapping=aes(x=Churn, y=RoamMins,fill = Churn)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
grid.arrange(p8,p9,p10,p11,p12,p13,p14,p15,nrow = 3, ncol = 3)
####Step 4 :- Predictive Modeling####
library(caTools)
library(car)
library(mctest)
###Step 4.1 : - Data Partitioning###
set.seed(333)
#Checking the observations count with respect to Churn Variable#
dim(CustomerServiceData)
table(CustomerServiceData$Churn)
prop.table(table(CustomerServiceData$Churn))
#Splitting the data into 70:30 Train:Test datasets#
DataSample1 <- sample.split(CustomerServiceData$Churn, SplitRatio = 0.7)
DataSample2 <- sample.split(CustServDataUpd$Churn, SplitRatio = 0.7)
#Splitting the sample into Train and Test dataset#
Train1 <- subset(CustomerServiceData, DataSample1==T)
Test1 <- subset(CustomerServiceData, DataSample1==F)
Train2 <- subset(CustServDataUpd, DataSample2==T)
Test2 <- subset(CustServDataUpd, DataSample2==F)
#Checking/Verifying the proportion uniformity in the train & test sets#
#Data Proportion Calculation for verification#
(70*3333)/100 # = 2333#
(30*3333)/100 # = 1000#
#Dimension Check#
dim(Train1)
dim(Test1)
dim(Train2)
dim(Test2)
#Proportion Check for Dependent Variable#
prop.table(table(Train1$Churn))
prop.table(table(Test1$Churn))
prop.table(table(Train2$Churn))
prop.table(table(Test2$Churn))
###Step 4.2: - Logistic Regression Model 1 with all variables and multicollinearity#
D1M1Tr <- glm(Churn~., family = binomial, Train1)
D1M1Te <- glm(Churn~., family = binomial, Test1)
D2M1Tr <- glm(Churn~., family = binomial, Train2)
D2M1Te <- glm(Churn~., family = binomial, Test2)
summary(D1M1Tr)
summary(D1M1Te)
summary(D2M1Tr)
summary(D2M1Te)
#Checking for multicollinearity#
vif(D1M1Tr)
vif(D1M1Te)
vif(D2M1Tr)
vif(D2M1Te)
#Additional commands to detect multicollinearity#
LMModel <- lm(Churn~., CustServData)
summary(LMModel)
omcdiag(LMModel)
imcdiag(LMModel)
#For Plotting the VIF and Eigen Value Plots#
mc.plot(LMModel)
#Comparing the VIF values for LM model with & without the correlated variables#
vif(lm(Churn~. , CustServData))
vif(lm(Churn~.-MonthlyCharge,CustServData)) #With only MonthlyCharge Removed next highest VIF was for DataUsage#
vif(lm(Churn~.-MonthlyCharge-DataUsage,CustServData)) #With both correlated variables Removed#
###Step 4.3: - Logistic Regression Model 2 without Multicollinearity###
#Creating the model without correlated variables
D1M2Tr <- glm(Churn~.-MonthlyCharge - DataUsage , family = binomial, Train1)
D1M2Te <- glm(Churn~.-MonthlyCharge - DataUsage , family = binomial, Test1)
D2M2Tr <- glm(Churn~.-MonthlyCharge - DataUsage , family = binomial, Train2)
D2M2Te <- glm(Churn~.-MonthlyCharge - DataUsage , family = binomial, Test2)
summary(D1M2Tr)
summary(D1M2Te)
summary(D2M2Tr)
summary(D2M2Te)
#Verifying if multicollinearity was correctly treated#
vif(D1M2Tr)
vif(D1M2Te)
vif(D2M2Tr)
vif(D2M2Te)
###Step 4.3: - Logistic Regression Model 3 without multicollinearity and insignificant variables###
D1M3Tr <- glm(Churn~.-MonthlyCharge - DataUsage-AccountWeeks -DayCalls , family = binomial, Train1)
D1M3Te <- glm(Churn~.-MonthlyCharge - DataUsage-AccountWeeks -DayCalls , family = binomial, Test1)
D2M3Tr <- glm(Churn~.-MonthlyCharge - DataUsage-AccountWeeks -DayCalls , family = binomial, Train2)
D2M3Te <- glm(Churn~.-MonthlyCharge - DataUsage-AccountWeeks -DayCalls , family = binomial, Test2)
summary(D1M3Tr)
summary(D1M3Te)
summary(D2M3Tr)
summary(D2M3Te)
####Step 5 :- Model Performance Measures Analysis####
library(lmtest)
library(pscl)
library(MLmetrics)
library(generalhoslem)
library(InformationValue)
#Log Likelihood Test#
lrtest(D1M1Tr)
lrtest(D1M3Tr)
lrtest(D2M1Tr)
lrtest(D2M3Tr)
lrtest(D1M1Te)
lrtest(D1M3Te)
lrtest(D2M1Te)
lrtest(D2M3Te)
#MCFadden Rsquare Test#
pR2(D1M1Tr)
pR2(D1M3Tr)
pR2(D1M1Te)
pR2(D1M3Te)
pR2(D2M1Tr)
pR2(D2M3Tr)
pR2(D2M1Te)
pR2(D2M3Te)
#Deviance Table Analysis#
anova(D1M1Tr)
anova(D1M1Te)
anova(D1M3Tr)
anova(D1M3Te)
anova(D2M1Tr)
anova(D2M1Te)
anova(D2M3Tr)
anova(D2M3Te)
#Hosmer-Lemeshow Test#
logitgof(Train1$Churn, fitted(D1M1Tr))
logitgof(Train1$Churn, fitted(D1M3Tr))
logitgof(Test1$Churn, fitted(D1M1Te))
logitgof(Test1$Churn, fitted(D1M3Te))
logitgof(Train2$Churn, fitted(D2M1Tr))
logitgof(Train2$Churn, fitted(D2M3Tr))
logitgof(Test2$Churn, fitted(D2M1Te))
logitgof(Test2$Churn, fitted(D2M3Te))
#Prediction-Confusion Matrix#
#Predictions will be done based on two models i.e. M1 & M3 only#
#For Train Dataset#
#Dataset 1#
D1M1Prob <- predict(D1M1Tr, type="response", newdata = Train1)
D1M1Pred <- ifelse(D1M1Prob > 0.5, 1,0)
D1M3Prob <- predict(D1M3Tr, type="response", newdata = Train1)
D1M3Pred <- ifelse(D1M3Prob > 0.5, 1,0)
#Dataset 2#
D2M1Prob <- predict(D2M1Tr, type="response", newdata = Train2)
D2M1Pred <- ifelse(D2M1Prob > 0.5, 1,0)
D2M3Prob <- predict(D2M3Tr, type="response", newdata = Train2)
D2M3Pred <- ifelse(D2M3Prob > 0.5, 1,0)
#For Test Dataset#
#Dataset 1#
D1M1Prob2 <- predict(D1M1Tr, type="response", newdata = Test1)
D1M1Pred2 <- ifelse(D1M1Prob2 > 0.5, 1,0)
D1M3Prob2 <- predict(D1M3Tr, type="response", newdata = Test1)
D1M3Pred2 <- ifelse(D1M3Prob2 > 0.5, 1,0)
#Dataset 2#
D2M1Prob2 <- predict(D2M1Tr, type="response", newdata = Test2)
D2M1Pred2 <- ifelse(D2M1Prob2 > 0.5, 1,0)
D2M3Prob2 <- predict(D2M3Tr, type="response", newdata = Test2)
D2M3Pred2 <- ifelse(D2M3Prob2 > 0.5, 1,0)
#Confusion Matrix#
#For Train Dataset#
caret::confusionMatrix(Train1$Churn, as.factor(D1M1Pred))
caret::confusionMatrix(Train1$Churn, as.factor(D1M3Pred))
caret::confusionMatrix(Train2$Churn, as.factor(D2M1Pred))
caret::confusionMatrix(Train2$Churn, as.factor(D2M3Pred))
#For Test Dataset#
caret::confusionMatrix(Test1$Churn, as.factor(D1M1Pred2))
caret::confusionMatrix(Test1$Churn, as.factor(D1M3Pred2))
caret::confusionMatrix(Test2$Churn, as.factor(D2M1Pred2))
caret::confusionMatrix(Test2$Churn, as.factor(D2M3Pred2))
#F1 Score#
#For Train Dataset#
F1_Score(Train1$Churn, as.factor(D1M1Pred))
F1_Score(Train1$Churn, as.factor(D1M3Pred))
F1_Score(Train2$Churn, as.factor(D2M1Pred))
F1_Score(Train2$Churn, as.factor(D2M3Pred))
#For Test Dataset#
F1_Score(Test1$Churn, as.factor(D1M1Pred2))
F1_Score(Test1$Churn, as.factor(D1M3Pred2))
F1_Score(Test2$Churn, as.factor(D2M1Pred2))
F1_Score(Test2$Churn, as.factor(D2M3Pred2))
#Classification Error Rate#
#For Train Dataset#
mean(D1M1Pred!= Train1$Churn)
mean(D1M3Pred!= Train1$Churn)
mean(D2M1Pred!= Train2$Churn)
mean(D2M3Pred!= Train2$Churn)
#For Test Dataset#
mean(D1M1Pred2!= Test1$Churn)
mean(D1M3Pred2!= Test1$Churn)
mean(D2M1Pred2!= Test2$Churn)
mean(D2M3Pred2!= Test2$Churn)
#AUC- ROC Curve#
library(ROCR)
#For Train Dataset#
par(mfrow = c(2,2))
plot(performance(prediction(D1M1Prob,Train1$Churn), "tpr", "fpr"), col = "red", main = "ROC Curve for Train data - Model 1 - Dataset 1")
abline(0, 1, lty = 8, col = "blue")
plot(performance(prediction(D1M3Prob,Train1$Churn), "tpr", "fpr"), col = "red", main = "ROC Curve for Train data - Model 3 - Dataset 1")
abline(0, 1, lty = 8, col = "blue")
plot(performance(prediction(D2M1Prob,Train2$Churn), "tpr", "fpr"), col = "red", main = "ROC Curve for Train data - Model 1 - Dataset 2")
abline(0, 1, lty = 8, col = "blue")
plot(performance(prediction(D2M3Prob,Train2$Churn), "tpr", "fpr"), col = "red", main = "ROC Curve for Train data - Model 3 - Dataset 2")
abline(0, 1, lty = 8, col = "blue")
#For Test Dataset#
plot(performance(prediction(D1M1Prob2,Test1$Churn), "tpr", "fpr"), col = "red", main = "ROC Curve for Test data - Model 1 - Dataset 1")
abline(0, 1, lty = 8, col = "blue")
plot(performance(prediction(D1M3Prob2,Test1$Churn), "tpr", "fpr"), col = "red", main = "ROC Curve for Test data - Model 3 - Dataset 1")
abline(0, 1, lty = 8, col = "blue")
plot(performance(prediction(D2M1Prob2,Test2$Churn), "tpr", "fpr"), col = "red", main = "ROC Curve for Test data - Model 1 - Dataset 2")
abline(0, 1, lty = 8, col = "blue")
plot(performance(prediction(D2M3Prob2,Test2$Churn), "tpr", "fpr"), col = "red", main = "ROC Curve for Test data - Model 3 - Dataset 2")
abline(0, 1, lty = 8, col = "blue")
#AUC Statistic#
#For Train Dataset#
slot(performance(prediction(D1M1Prob,Train1$Churn), "auc"), "y.values")
slot(performance(prediction(D1M3Prob,Train1$Churn), "auc"), "y.values")
slot(performance(prediction(D2M1Prob,Train2$Churn), "auc"), "y.values")
slot(performance(prediction(D2M3Prob,Train2$Churn), "auc"), "y.values")
#For Test Dataset#
slot(performance(prediction(D1M1Prob2,Test1$Churn), "auc"), "y.values")
slot(performance(prediction(D1M3Prob2,Test1$Churn), "auc"), "y.values")
slot(performance(prediction(D2M1Prob2,Test2$Churn), "auc"), "y.values")
slot(performance(prediction(D2M3Prob2,Test2$Churn), "auc"), "y.values")
#Gini Index#
Gini(D1M1Tr$fitted.values,as.numeric(as.character(Train1$Churn)))
Gini(D1M1Te$fitted.values,as.numeric(Test1$Churn))
Gini(D1M3Tr$fitted.values,as.numeric(Train1$Churn))
Gini(D1M3Te$fitted.values,as.numeric(Test1$Churn))
Gini(D2M1Tr$fitted.values,as.numeric(Train2$Churn))
Gini(D2M1Te$fitted.values,as.numeric(Test2$Churn))
Gini(D2M3Tr$fitted.values,as.numeric(Train2$Churn))
Gini(D2M3Te$fitted.values,as.numeric(Test2$Churn))
#Kolmogorov-Smirnov Test#
max(attr((performance(prediction(D1M1Prob,Train1$Churn), "tpr", "fpr")), "y.values")[[1]] - (attr((performance(prediction(D1M1Prob,Train1$Churn), "tpr", "fpr")), "x.values")[[1]]))
max(attr((performance(prediction(D1M3Prob,Train1$Churn), "tpr", "fpr")), "y.values")[[1]] - (attr((performance(prediction(D1M1Prob,Train1$Churn), "tpr", "fpr")), "x.values")[[1]]))
max(attr((performance(prediction(D2M1Prob,Train2$Churn), "tpr", "fpr")), "y.values")[[1]] - (attr((performance(prediction(D1M1Prob,Train1$Churn), "tpr", "fpr")), "x.values")[[1]]))
max(attr((performance(prediction(D2M3Prob,Train2$Churn), "tpr", "fpr")), "y.values")[[1]] - (attr((performance(prediction(D1M1Prob,Train1$Churn), "tpr", "fpr")), "x.values")[[1]]))
max(attr((performance(prediction(D1M1Prob2,Test1$Churn), "tpr", "fpr")), "y.values")[[1]] - (attr((performance(prediction(D1M1Prob,Train1$Churn), "tpr", "fpr")), "x.values")[[1]]))
max(attr((performance(prediction(D1M3Prob2,Test1$Churn), "tpr", "fpr")), "y.values")[[1]] - (attr((performance(prediction(D1M1Prob,Train1$Churn), "tpr", "fpr")), "x.values")[[1]]))
max(attr((performance(prediction(D2M1Prob2,Test2$Churn), "tpr", "fpr")), "y.values")[[1]] - (attr((performance(prediction(D1M1Prob,Train1$Churn), "tpr", "fpr")), "x.values")[[1]]))
max(attr((performance(prediction(D2M3Prob2,Test2$Churn), "tpr", "fpr")), "y.values")[[1]] - (attr((performance(prediction(D1M1Prob,Train1$Churn), "tpr", "fpr")), "x.values")[[1]]))
#Customer Churn Odds Explanatory Power - Ratio/Probabilty#
#For Odd Ratio#
exp(coef(D1M1Tr))
exp(coef(D1M1Te))
exp(coef(D2M1Tr))
exp(coef(D2M1Te))
exp(coef(D1M3Tr))
exp(coef(D1M3Te))
exp(coef(D2M3Tr))
exp(coef(D2M3Te))
#For Odd Probability#
exp(coef(D1M1Tr))/(1+exp(coef(D1M1Tr)))
exp(coef(D1M1Te))/(1+exp(coef(D1M1Te)))
exp(coef(D2M1Tr))/(1+exp(coef(D2M1Tr)))
exp(coef(D2M1Te))/(1+exp(coef(D2M1Te)))
exp(coef(D1M3Tr))/(1+exp(coef(D1M3Tr)))
exp(coef(D1M3Te))/(1+exp(coef(D1M3Te)))
exp(coef(D2M3Tr))/(1+exp(coef(D2M3Tr)))
exp(coef(D2M3Te))/(1+exp(coef(D2M3Te)))
#########################End of the Code####################################
#########################End of the Analysis################################
