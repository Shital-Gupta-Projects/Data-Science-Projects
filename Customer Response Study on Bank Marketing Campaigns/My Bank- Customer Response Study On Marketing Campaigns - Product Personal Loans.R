###################################INDIVIDUAL ASSIGNMENT FOR DATA MINING####################################
########################################SUBMITTED BY :- SHITAL GUPTA########################################
####R PROJECT: CUSTOMER RESPONSE STUDY FOR BANK MARKETING STRATEGIES USING PERSONAL LOANS CAMPAIGNS DATA####

############################################################################################################

#########################################START OF THE ANALYSIS##############################################
###########################################START OF THE CODE################################################

####Step 1 :- Setting the Current Working Directory####
getwd()
setwd("C:/Users/shital/Documents/R/RProgramming")
####Step 2 :- Reading and viewing the data from the dataset file####
CustomerData <- read.csv("PersonalLoanCampaign.csv", header = TRUE)
head(CustomerData)
####Step 3 :-Start of Exploratory Data Analysis ####
####Step 3.1:- Interpreting the data ####
####Step 3.1.1 :- Checking the dimensions of the dataset####
dim(CustomerData)
####Step 3.1.2 :-Summarizing the data and doing a structure check####
str(CustomerData)
summary(CustomerData)
####Step 3.1.3 :- Performing a descriptive analysis on the data####
library(psych)
describe(CustomerData)
####Step 3.2 :- Processing the data####
####Step 3.2.1 :- Data Cleanup and Imbalanced Data Check####
####Removing the unwanted variables from the dataset####
CustomerData_Updated <- CustomerData[,-c(1,7,11,40)]
####Checking if the data is balanced or not####
library(plotrix)
pie3D((prop.table(table(CustomerData$TARGET))), main = "Customer Campaign Response Favorability", labels = c("Negative Response", "Positive Response"), cex.main = 2, labelcex = 1.3)
pie3D((prop.table(table(CustomerData$GENDER))), main = "Gender Favorability", labels = c("F", "M", "O"), cex.main = 2, labelcex = 1.3)
pie3D((prop.table(table(CustomerData$OCCUPATION))), main = "Occupation Favorability", labels = c("Prof", "Sal", "Self-Emp", "Senp"), cex.main = 2, labelcex = 1.3)
pie3D((prop.table(table(CustomerData$AGE_BKT))), main = "Age Bracket Favorability", labels = c("<25", ">50", "26-30", "31-35","36-40","41-45","46-50"), cex.main = 2, labelcex = 1.3)
pie3D((prop.table(table(CustomerData$ACC_TYPE))), main = "Account Type Favorability", labels = c("CA", "SA"), cex.main = 2, labelcex = 1.3)
pie3D((prop.table(table(CustomerData$FLG_HAS_CC))), main = " Credit Card Favorability", labels = c("Do not have CC", "Has CC"), cex.main = 2, labelcex = 1.3)
pie3D((prop.table(table(CustomerData$FLG_HAS_OLD_LOAN))), main = "Previous Loan History", labels = c("No Old Loans", "Has Old Loans"), cex.main = 2, labelcex = 1.3)
####Data Structure Correction####
####Converting the data types for variables incorrectly setup####
CustomerData_Updated$TARGET <- as.factor(CustomerData_Updated$TARGET)
CustomerData_Updated$FLG_HAS_CC <- as.factor(CustomerData_Updated$FLG_HAS_CC)
CustomerData_Updated$FLG_HAS_ANY_CHGS <- as.factor(CustomerData_Updated$FLG_HAS_ANY_CHGS)
CustomerData_Updated$FLG_HAS_NOMINEE <- as.factor(CustomerData_Updated$FLG_HAS_NOMINEE)
CustomerData_Updated$FLG_HAS_OLD_LOAN <- as.factor(CustomerData_Updated$FLG_HAS_OLD_LOAN)
str(CustomerData_Updated)
####Step 3.2.2 :- Analyzing the dependent variables and factors of interest####
####Skewness Check####
library(PerformanceAnalytics)
DVNumeric <- as.numeric(CustomerData$TARGET)
DVSkewnessCheck <- skewness(DVNumeric)
DVSkewnessCheck
library(ggplot2)
library(dplyr)
Skewness_plot <- CustomerData_Updated %>% ggplot(aes(x = DVNumeric)) + stat_density(geom = "line", alpha = 1, colour = "cornflowerblue")
Skewness_plot
Plot_Shaded_Area <- ggplot_build(Skewness_plot)$data[[1]] %>% filter(x < Mean)
Skewness_plot_shaded <-  Skewness_plot + geom_area(data = Plot_Shaded_Area, aes(x = x, y = y), fill="pink", alpha = 0.5)+ ggtitle("Density Plot Illustrating Skewness For Customer Response")
print(Skewness_plot_shaded+ labs( x = "Customer Campaign Response - Target", y = "Density"))
####Step 3.2.3 :- Missing Value Treatment####
colSums(is.na(CustomerData_Updated))
####Negative Value Treatment####
colSums((CustomerData_Updated)<0)
####Step 3.2.4 :- Outlier Treatment####
####Boxplots for Numerical Variables####
library(RColorBrewer)
####Age and Holding Period####
par(font.main = 4, font.axis = 2)
boxplot(CustomerData_Updated[,c(2,7)], las = 1, col = c("Red", "Light Blue"), names = c("Age", "HoldingPeriod"), main = "Boxplot for Customer's Age & Holding Period", cex.main = 1, cex.axis = 1)
box(lwd = 3)
####Customer Account Balance####
par(font.main = 4, font.axis = 2)
boxplot(CustomerData_Updated[,4],horizontal = TRUE, col = c("Red"), names = c("Account Balance"), main = "Boxplot for Customer's Account Balance", cex.main = 1, cex.axis = 1)
box(lwd = 3)
####SCR and Relation Length in Months####
par(font.main = 4, font.axis = 2)
boxplot(CustomerData_Updated[,c(6,9)], las = 1, col = c("Light Green", "Light Yellow"), names = c("SCR", "Relationship Length in Months") , main = "Boxplot for Customer's SCR & Relationship Length", cex.main = 1, cex.axis = 1)
box(lwd = 3)
####Total Number of Transactions####
par(font.main = 4, font.axis = 2)
boxplot(CustomerData_Updated[,c(10,11,12)], horizontal = TRUE, las = 0, col = c("Light Blue","Light Yellow"), names = c("Credit Txns", "Debit Txns", "Total Txns") , main = "Boxplot for Customer's Transactional Data", cex.main = 1, cex.axis = 1)
box(lwd = 3)
####Total Number of Debit Transactions####
par(font.main = 4, font.axis = 2)
boxplot(CustomerData_Updated[,c(13,14,15,16,17)], las = 0, col = brewer.pal(5,"Greens"), names = c("Branch Withdrawal", "ATM", "Net", "Mobile Banking", "Cheque" ) , main = "Boxplot for Customer's Debit - Total Number Count", cex.main = 1, cex.axis = 1)
box(lwd = 3)
####Total Amount of Debit Transactions####
par(font.main = 4, font.axis = 2)
boxplot(CustomerData_Updated[,c(19,23)],horizontal = TRUE, las = 0, col = c("Green","Cornflower Blue" ) ,names = c("ATM", "Mobile Banking"), main = "Boxplot for Customer's Debit Transactions - Amount Debited", cex.main = 1, cex.axis = 1)
box(lwd = 3)
boxplot(CustomerData_Updated[,c(20,21,22,24)],horizontal = TRUE, las = 0, col = brewer.pal(4,"Greens"), names = c("Branch Withdrawal", "Cheque", "Net", "Total") , main = "Boxplot for Customer's Debit Transactions - Amount Debited", cex.main = 1, cex.axis = 1)
box(lwd = 3)
####Charges or Deductions####
par(font.main = 4, font.axis = 2)
boxplot(CustomerData_Updated[,c(26,27,28,29)], las = 0, col = brewer.pal(4,"Blues"), names = c("Other Bank Txns", "Min Bal", "InwardChequeBounce", "OutwardChequeBounce") , main = "Boxplot for Customer's Charges & Deductions", cex.main = 1, cex.axis = 0.8)
box(lwd = 3)
####Average Amount Per Transactions####
par(font.main = 4, font.axis = 2)
boxplot(CustomerData_Updated[,c(30,34)],horizontal = TRUE, las = 0, col = c("Green","Cornflower Blue" ) ,names = c("ATM", "Mobile Banking"), main = "Boxplot for Customer's Debit Transactions - Average Amount Debited", cex.main = 1, cex.axis = 1)
box(lwd = 3)
boxplot(CustomerData_Updated[,c(31,32,33)],horizontal = TRUE, las = 0, col = brewer.pal(3,"Greens"), names = c("Branch Withdrawal", "Cheque", "Net") , main = "Boxplot for Customer's Debit Transactions - Average Amount Debited", cex.main = 1, cex.axis = 1)
box(lwd = 3)
####Step 3.3 :- Visualizing the data####
####Step 3.3.1:- Univariate Analysis through Histogram and Bar Charts####
####For Continuous Numerical Variables####
Data_Hist <- CustomerData_Updated[,c(2,4,6,7,9:17,19:24,26:34)]
ColNamesHist <- colnames(Data_Hist)
par(mfrow = c(3,4), font.main = 2, font.axis = 2, font.lab = 2) #Convert our plotting space in 12 different frames#
for (ColNumber in (1:12)) {
  h = max(Data_Hist[,ColNumber])+1
  l = min(Data_Hist[,ColNumber])-1
  n = ColNamesHist[ColNumber]
  hist (Data_Hist[,ColNumber], breaks = seq(l,h,((h-l)/6)), include.lowest=T, right=T, col= brewer.pal(6,"Blues") , border=1, main = NULL , xlab= n, ylab=NULL, cex.lab= 1.5, cex.axis=1.2, cex.main=1)
}
for (ColNumber in (13:24)) {
  h = max(Data_Hist[,ColNumber])+1
  l = min(Data_Hist[,ColNumber])-1
  n = ColNamesHist[ColNumber]
  hist (Data_Hist[,ColNumber], breaks = seq(l,h,((h-l)/6)), include.lowest=T, right=T, col= brewer.pal(6,"Blues") , border=1, main = NULL , xlab= n, ylab=NULL, cex.lab= 1.5, cex.axis=1.2, cex.main=1)
}
for (ColNumber in (25:28)) {
  h = max(Data_Hist[,ColNumber])+1
  l = min(Data_Hist[,ColNumber])-1
  n = ColNamesHist[ColNumber]
  hist (Data_Hist[,ColNumber], breaks = seq(l,h,((h-l)/6)), include.lowest=T, right=T, col= brewer.pal(6,"Blues") , border=1, main = NULL , xlab= n, ylab=NULL, cex.lab= 1.5, cex.axis=1.2, cex.main=1)
}
####Density Plot for all continuous numerical variables####
library(tidyr)
library(purrr)
CustomerData_Updated %>%
  keep(is.numeric) %>%                    # Keep only numeric columns
  gather() %>%                            # Convert to key-value pairs
  ggplot(aes(value)) +                    # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density()                         # as density
####For Categorical Variables####
library(gridExtra)
p1 <- ggplot(CustomerData_Updated, aes(x = TARGET, fill = TARGET)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 3) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p2 <- ggplot(CustomerData_Updated, aes(x = GENDER, fill = GENDER)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 3) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p3 <- ggplot(CustomerData_Updated, aes(x = OCCUPATION, fill = OCCUPATION)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 3) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p4 <- ggplot(CustomerData_Updated, aes(x = ACC_TYPE, fill = ACC_TYPE)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 3) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p5 <- ggplot(CustomerData_Updated, aes(x = FLG_HAS_CC, fill = FLG_HAS_CC)) + geom_bar()+ geom_text(stat = "count", aes(label = ..count..), vjust = 3) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p6 <- ggplot(CustomerData_Updated, aes(x = FLG_HAS_ANY_CHGS, fill = FLG_HAS_ANY_CHGS)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 3) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p7 <- ggplot(CustomerData_Updated, aes(x = FLG_HAS_NOMINEE, fill = FLG_HAS_NOMINEE)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 3) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p8 <- ggplot(CustomerData_Updated, aes(x = FLG_HAS_OLD_LOAN, fill = FLG_HAS_OLD_LOAN)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 3) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 2)
####Step 3.3.2:- Bi-variate Analysis####
####Chi Square Test for correlation analysis between Target & Categorical IVS and their Visualizations####
attach(CustomerData_Updated)
chisq.test(TARGET, GENDER)
as.matrix((prop.table(table(GENDER)))*100)
ggplot( data = CustomerData_Updated, aes(x = GENDER, y = as.numeric(TARGET), fill = TARGET)) + geom_bar(position ="fill", stat = "identity") + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))
chisq.test(TARGET, OCCUPATION)
as.matrix((prop.table(table(OCCUPATION)))*100)
ggplot( data = CustomerData_Updated, aes(x = OCCUPATION, y = as.numeric(TARGET), fill = TARGET)) + geom_bar(position ="fill", stat = "identity") + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))
chisq.test(TARGET, ACC_TYPE)
as.matrix((prop.table(table(ACC_TYPE)))*100)
ggplot( data = CustomerData_Updated, aes(x = ACC_TYPE, y = as.numeric(TARGET), fill = TARGET)) + geom_bar(position ="fill", stat = "identity") + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))
chisq.test(TARGET, FLG_HAS_CC)
as.matrix((prop.table(table(FLG_HAS_CC)))*100)
ggplot( data = CustomerData_Updated, aes(x = FLG_HAS_CC, y = as.numeric(TARGET), fill = TARGET)) + geom_bar(position ="fill", stat = "identity") + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))
chisq.test(TARGET, FLG_HAS_ANY_CHGS)
as.matrix((prop.table(table(FLG_HAS_ANY_CHGS)))*100)
ggplot( data = CustomerData_Updated, aes(x = FLG_HAS_ANY_CHGS, y = as.numeric(TARGET), fill = TARGET)) + geom_bar(position ="fill", stat = "identity") + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))
chisq.test(TARGET, FLG_HAS_NOMINEE)
as.matrix((prop.table(table(FLG_HAS_NOMINEE)))*100)
ggplot( data = CustomerData_Updated, aes(x = FLG_HAS_NOMINEE, y = as.numeric(TARGET), fill = TARGET)) + geom_bar(position ="fill", stat = "identity") + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))
chisq.test(TARGET, FLG_HAS_OLD_LOAN)
as.matrix((prop.table(table(FLG_HAS_OLD_LOAN)))*100)
ggplot( data = CustomerData_Updated, aes(x = FLG_HAS_OLD_LOAN, y = as.numeric(TARGET), fill = TARGET)) + geom_bar(position ="fill", stat = "identity") + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))
chisq.test(TARGET, CustomerData$AGE_BKT)
as.matrix((prop.table(table(CustomerData$AGE_BKT)))*100)
ggplot( data = CustomerData, aes(x = AGE_BKT, y = as.numeric(as.factor(TARGET)), fill = as.factor(TARGET))) + geom_bar(position ="fill", stat = "identity") + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10)) + labs(fill = "TARGET")+scale_fill_manual(values = c("deepskyblue4","cornflowerblue"))
####Binary Logistic Regression for correlation analysis between Target & Continuous IVs####
library(MASS)
CorData <- CustomerData_Updated[,c(1,2,4,6,7,9:17,19:24,26:34)]
LRModel <- glm(TARGET~.,family = binomial(link = logit), data = CorData )
summary(LRModel)
####To check for highly significant variables####
LRModelUpdated <- stepAIC(LRModel)
summary(LRModelUpdated)
####Using Boxplots to analyze Target and other continuous numerical independent variables####
p9 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=AGE,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p10 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=BALANCE,fill = TARGET)) + geom_boxplot() + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p11 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=SCR,fill = TARGET)) + geom_boxplot() + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p12 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=HOLDING_PERIOD,fill = TARGET)) + geom_boxplot() + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p13 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=LEN_OF_RLTN_IN_MNTH,fill = TARGET)) + geom_boxplot() + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p14 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=NO_OF_L_CR_TXNS,fill = TARGET)) + geom_boxplot() + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p15 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=NO_OF_L_DR_TXNS,fill = TARGET)) + geom_boxplot() + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p16 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=TOT_NO_OF_L_TXNS,fill = TARGET)) + geom_boxplot() + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p17 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=NO_OF_BR_CSH_WDL_DR_TXNS,fill = TARGET)) + geom_boxplot() + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p18 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=NO_OF_ATM_DR_TXNS,fill = TARGET)) + geom_boxplot() + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p19 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=NO_OF_NET_DR_TXNS,fill = TARGET)) + geom_boxplot() + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p20 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=NO_OF_MOB_DR_TXNS,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
grid.arrange(p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20, nrow = 3)
p21 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=NO_OF_CHQ_DR_TXNS,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p22 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=AMT_ATM_DR,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p23 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=AMT_BR_CSH_WDL_DR,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p24 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=AMT_CHQ_DR,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p25 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=AMT_NET_DR,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p26 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=AMT_MOB_DR,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p27 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=AMT_L_DR,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p28 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=AMT_OTH_BK_ATM_USG_CHGS,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p29 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=AMT_MIN_BAL_NMC_CHGS,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p30 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=NO_OF_IW_CHQ_BNC_TXNS,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p31 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=NO_OF_OW_CHQ_BNC_TXNS,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
grid.arrange(p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31, nrow = 3)
p32 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=AVG_AMT_PER_ATM_TXN,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p33 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=AVG_AMT_PER_CSH_WDL_TXN,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p34 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=AVG_AMT_PER_CHQ_TXN,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p35 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=AVG_AMT_PER_NET_TXN,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p36 <- ggplot(data=CustomerData_Updated, mapping=aes(x=TARGET, y=AVG_AMT_PER_MOB_TXN,fill = TARGET)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
grid.arrange(p32,p33,p34,p35,p36, nrow = 2)
####Step 4:- Cluster Analysis####
#####Step 4.1:- Scaling the data####
library(cluster)
ClusteringData <- CustomerData_Updated[,-1]
gower_dist <- daisy(ClusteringData, metric = "gower")
gower_mat <- as.matrix(gower_dist)
ClusteringData[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]
ClusteringData[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]
####Step 4.2:- Running the K-Medoids Clustering Technique using the PAM Algorithm####
Sil_width <- c(NA)
Pam_fit <- pam(gower_dist, diss = TRUE, k = 2)
Sil_width[2] <- Pam_fit$silinfo$avg.width
Pam_fit2 <- pam(gower_dist, diss = TRUE, k = 3)
Sil_width[3] <- Pam_fit2$silinfo$avg.width
Pam_fit3 <- pam(gower_dist, diss = TRUE, k = 4)
Sil_width[4] <- Pam_fit3$silinfo$avg.width
Pam_fit4 <- pam(gower_dist, diss = TRUE, k = 5)
Sil_width[5] <- Pam_fit4$silinfo$avg.width
Pam_fit5 <- pam(gower_dist, diss = TRUE, k = 6)
Sil_width[6] <- Pam_fit5$silinfo$avg.width
####Step 4.3:- Plotting the different cluster models based on silhouette width for comparison####
plot(1:6, Sil_width, xlab = "Number of clusters", ylab = "Silhouette Width")
lines(1:6, Sil_width)
####Step 4.4:- Summarizing the ideal Cluster Model i.e. 5 Cluster Model for Cluster Profiling####
Pam_Results <- ClusteringData %>% mutate(cluster = Pam_fit4$clustering) %>% group_by(cluster) %>% do(PAMSummary = summary(.))
Pam_Results$PAMSummary
####Step 4.5:- Visualizing the Cluster Model####
library(Rtsne)
ClusterVisual_Obj <- Rtsne(gower_dist, is_distance = TRUE)
ClusterVisual_Data <- ClusterVisual_Obj$Y %>% data.frame() %>% setNames(c("X", "Y")) %>% mutate(cluster = factor(Pam_fit4$clustering))
####Clusters Representation####
ggplot(aes(x = X, y = Y, color = cluster), data = ClusterVisual_Data) + geom_point() + stat_ellipse(aes(fill = cluster),geom="polygon", level=0.95, alpha=0.05)
####Step 4.6:- Dendrogram Visualization####
####For complete dataset####
Aggl.clust.c <- hclust(gower_dist, method = "complete")
Dendrogram <- as.dendrogram(Aggl.clust.c)
plot(Dendrogram, main = "Agglomerative Hierarchical Clustering - Complete Linkages")
####For Sample Dataset of 100 Observations####
SampleData <- CustomerData_Updated[sample(nrow(CustomerData_Updated), 100),]
gower_dist2 <- daisy(SampleData, metric = "gower")
Sample.Clustering <- hclust(gower_dist2)
plot(Sample.Clustering)
####Step 5:- CART Model Analysis####
####Step 5.1:- Partitioning the dataset into train and test dataset####
library(caTools)
set.seed(2000)
Split <- sample(2, nrow(CustomerData_Updated), prob = c(0.7,0.3),replace=T)
TrainingData <-  CustomerData_Updated[Split==1,]
TestingData <-  CustomerData_Updated[Split==2,]
####Checking for the imbalance in both the datasets created####
prop.table(table(TrainingData$TARGET))
prop.table(table(TestingData$TARGET))
####Step 5.2:- Balancing the dataset via ROSE Algorithm####
library(ROSE)
table(TrainingData$TARGET)
OSTRD_OUComb <- ovun.sample(TARGET~., data = TrainingData, method = "both", N = 20000)$data
OSTRD_O <- ovun.sample(TARGET~., data = TrainingData, method = "over", N = 24544)$data
table(OSTRD_OUComb$TARGET)
table(OSTRD_O$TARGET)
####Step 5.3:- Creating the CART model####
library(rattle)
library(rpart.plot)
library(rpart)
library(caret)
####Unbalanced Dataset####
r.ctrl <- rpart.control(minsplit = 200, minbucket = 20,cp = 0, xval = 10)
CARTModel1 <- rpart(formula = TARGET~., data = TrainingData , method = "class",control = r.ctrl)
fancyRpartPlot(CARTModel1)
printcp(CARTModel1)
plotcp(CARTModel1)
PrunedCARTModel<- prune(CARTModel1, cp= 0.0017 ,"CP") 
printcp(PrunedCARTModel)
fancyRpartPlot(PrunedCARTModel, uniform = TRUE, main = "Final Decision Tree",palettes = c("Blues", "Reds"))
####Balanced Dataset- Oversampling Model and Combined Over-Under Sampling Model####
CARTModel2 <- rpart(formula = TARGET~., data = OSTRD_O , method = "class",control = r.ctrl) ##Oversampling Model##
CARTModel3 <- rpart(formula = TARGET~., data = OSTRD_OUComb , method = "class",control = r.ctrl) ##Over-Under Sampling Model##
#Model Statistic Calculations:- This will not be shown in the report#
printcp(CARTModel2)
plotcp(CARTModel2)
printcp(CARTModel3)
plotcp(CARTModel3)
PrunedCARTModel2<- prune(CARTModel2, cp= 0.0017 ,"CP")
PrunedCARTModel3<- prune(CARTModel3, cp= 0.0017 ,"CP")
printcp(PrunedCARTModel2)
printcp(PrunedCARTModel3)
#This will not be shown in the report#
fancyRpartPlot(PrunedCARTModel2, uniform = TRUE, main = "Final Decision Tree - CART Model 2",palettes = c("Oranges", "Greens"))
fancyRpartPlot(PrunedCARTModel3, uniform = TRUE, main = "Final Decision Tree- CART Model 3",palettes = c("Blues", "Reds"))
####Step 5.4:- Model Interpretation - Performance Measure Metrics####
##Common Function To be Used for plotting Confusion Matrix##
Draw_confusion_matrix <- function(cm) {
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}
##Model1 - Unbalanced Dataset CART Model##
#Creating and Plotting the Confusion Matrix#
Predictions_Model1 <- predict(PrunedCARTModel, TestingData, type = "class")
CM <- confusionMatrix(Predictions_Model1, TestingData$TARGET)
CM
F1Score = 2*((0.881*0.989)/(0.881+0.989))
F1Score
ClassificationError <- (694 + 56) / (5160 + 70 + 694 + 56)
ClassificationError
Draw_confusion_matrix(CM)
##KS Statistic and ROC Curve##
library(ROCR)
library(ineq)
KSPredict = predict(PrunedCARTModel, TestingData, type = "prob")
KSPredictValue <- prediction(KSPredict[,2],TestingData$TARGET)
KSPerfValue <- performance(KSPredictValue,"tpr", "fpr")
plot(KSPerfValue, col = "green", lwd = 1.5) # ROC Curve#
KSStat <- max(attr(KSPerfValue, "y.values")[[1]] - (attr(KSPerfValue, "x.values")[[1]]))
KSStat
##AUC Curve Statistic##
AUCPerfValue <- performance(KSPredictValue,"auc")
AUCPerfValue <- as.numeric(AUCPerfValue@y.values)
AUCPerfValue
##GINI Coefficient##
GiniTD <- TrainingData
GiniTD$PredictScore = predict(PrunedCARTModel, GiniTD, type = "prob")
GiniTD$PredictClass = predict(PrunedCARTModel, GiniTD, type = "class")
GINICoeff <- ineq(GiniTD$PredictScore[,2],TestingData$TARGET)
with(GiniTD, table(TARGET, PredictClass))
GINICoeff
##Model2 - Balanced Dataset CART Model##
#Creating and Plotting the Confusion Matrix#
OSTED_OUComb <- ovun.sample(TARGET~., data = TestingData, method = "both", N = 20000)$data #Optional Command#
Predictions_Model2 <- predict(PrunedCARTModel3, OSTED_OUComb, type = "class")
CM2 <- confusionMatrix(Predictions_Model2, OSTED_OUComb$TARGET)
CM2
F1Score2 = 2*((0.706*0.704)/(0.706+0.704))
F1Score2
ClassificationError <- (2927 + 2960) / (7048 + 7065 + 2927 + 2960)
ClassificationError
Draw_confusion_matrix(CM2)
##KS Statistic and ROC Curve##
library(ROCR)
library(ineq)
KSPredictM2 = predict(PrunedCARTModel3, OSTED_OUComb, type = "prob")
KSPredictValueM2 <- prediction(KSPredictM2[,2],OSTED_OUComb$TARGET)
KSPerfValueM2 <- performance(KSPredictValueM2,"tpr", "fpr")
plot(KSPerfValueM2, col = "green", lwd = 1.5) # ROC Curve#
KSStatM2 <- max(attr(KSPerfValueM2, "y.values")[[1]] - (attr(KSPerfValueM2, "x.values")[[1]]))
KSStatM2
##AUC Curve Statistic##
AUCPerfValueM2 <- performance(KSPredictValueM2,"auc")
AUCPerfValueM2 <- as.numeric(AUCPerfValueM2@y.values)
AUCPerfValueM2
##GINI Coefficient##
GiniTDM2 <- OSTED_OUComb
GiniTDM2$PredictScore = predict(PrunedCARTModel3, GiniTDM2, type = "prob")
GiniTDM2$PredictClass = predict(PrunedCARTModel3, GiniTDM2, type = "class")
GINICoeffM2 <- ineq(GiniTDM2$PredictScore[,2],OSTED_OUComb$TARGET)
with(GiniTDM2, table(TARGET, PredictClass))
GINICoeffM2

#########################################END OF THE ANALYSIS################################################
###########################################END OF THE CODE##################################################

############################################################################################################
