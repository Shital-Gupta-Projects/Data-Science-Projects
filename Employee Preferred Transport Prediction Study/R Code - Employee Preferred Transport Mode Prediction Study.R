##############################ASSIGNMENT FOR MACHINE LEARNING#################################
#############################SUBMITTED BY: SHITAL GUPTA#######################################
###############R PROJECT: EMPLOYEE PREFERRED TRANSPORT MODE PREDICTION ANALYSIS###############
##############################################################################################
#########################Start of the Analysis################################
#########################Start of the Code####################################
#----------------------------------------------------------------------#
####Step 1: - Setting the Current Working Directory####
#----------------------------------------------------------------------#
getwd()
setwd("C:/Users/shital/Documents/R/RProgramming")
dir()
#----------------------------------------------------------------------#
####Step 2 :- Reading and viewing the data from the dataset####
#----------------------------------------------------------------------#
EmpData <- read.csv(file.choose())
EmployeeData <- EmpData
View(EmployeeData) #To view the dataset#
#----------------------------------------------------------------------#
####Step 3 :-Start of Exploratory Data Analysis####
#----------------------------------------------------------------------#
#*****************************************************************#
###Step 3.1: - Data Interpretation###
#*****************************************************************#
##Step 3.1.1:- Data Definition##
library(psych) #For describe function#
library(DataExplorer)
library(dplyr)
library(pastecs)
library(caret)
library(plotrix)
library(mice)
library(RColorBrewer)
library(DescTools)
library(ggplot2)
library(tidyr)
library(purrr)
library(PerformanceAnalytics)
library(gridExtra)
library(descr)
library(explore)
library(corrplot)
library(GGally)
library(ltm)
library(nnet)
library(usdm)
dim(EmployeeData) #Checking the dimensions for dataset variables#
head(EmployeeData) #Reading the data#
##Step 3.1.2:- Data Summary##
plot_intro(EmployeeData)
introduce(EmployeeData)
str(EmployeeData) #Checking the data structure#
#Fixing the data types for the variables#
EmployeeData$Engineer <- as.factor(EmployeeData$Engineer)
EmployeeData$MBA <- as.factor(EmployeeData$MBA)
EmployeeData$license <- as.factor(EmployeeData$license)
#Rechecking data structure to confirm on data type conversion#
str(EmployeeData)
#Five-Number Summary for Data#
summary(EmployeeData)
#Descriptive Statistics for Data#
describe(select_if(EmployeeData, is.numeric))
#Additional descriptive Statistics for Data#
options(scipen = 100)
options(digits = 2)
stat.desc(select_if(EmployeeData, is.numeric), basic = F)
#NearZeroVarianceCheck - Only Data usage shows zero variance#
nearZeroVar(EmployeeData, saveMetrics = TRUE)
#*****************************************************************#
###Step 3.2: - Data Processing###
#*****************************************************************#
##Step 3.2.1: - Imbalanced Data Check##
par(mfrow = c(2,3))
#Plotting a 3D Pie Chart to check for Imbalance#
pie3D((prop.table(table(EmployeeData$Gender))), main = "Employee Gender Favorability", labels = c("Female", "Male"), cex.main = 1.2, labelcex = 0.9,explode=0.1)
pie3D((prop.table(table(EmployeeData$Engineer))), main = "Employee Education - Engineer Favorability", labels = c("Not an Engineer", "Is an Engineer"), cex.main = 1.2, labelcex = 0.9, explode=0.1)
pie3D((prop.table(table(EmployeeData$MBA))), main = "Employee Education - MBA Favorability", labels = c("Not an MBA", "Is an MBA"), cex.main = 1.2, labelcex = 0.9, explode=0.1)
pie3D((prop.table(table(EmployeeData$license))), main = "Employee Driving License Favorability", labels = c("Does Not Have Driving License", "Has Driving License"), cex.main = 1.2, labelcex = 0.9, explode=0.1)
pie3D((prop.table(table(EmployeeData$Transport))), main = "TransportMode Favorability", labels = c("2Wheeler", "Car", "Public Transport"), cex.main = 1.2, labelcex = 0.9, explode=0.1)
dev.off()
##Step 3.2.2: - Missing Value Analysis##
#Checking for missing values#
any(is.na(EmployeeData))
sum(is.na(EmployeeData))
colSums(is.na(EmployeeData))
EDNAmiceimput <- mice(data = EmployeeData, method = "pmm",maxit = 50, seed = 500) 
EDNAmiceimput$imp$MBA
EmployeeDataUpd <- mice::complete(EDNAmiceimput,2) #Will be used during modelling process#
#Rechecking if the missing value has been updated or not#
colSums(is.na(EmployeeDataUpd))
dim(unique(EmployeeData))[1] #No Duplicate Records#
colSums(EmployeeData[,c(1,5:7)]<0) #No Negative Records in continuous variables#
##Step 3.2.3: - Outlier Treatment##
#Printing boxplots for all the numeric variables#
boxplot(select_if(EmployeeData, is.numeric), col = brewer.pal(4, "Reds"), main = "Boxplot for Numeric Variables")
#To display the outliers present in each variable#
boxplot(EmployeeData$Age, plot = FALSE)$out
boxplot(EmployeeData$Work.Exp, plot = FALSE)$out
boxplot(EmployeeData$Salary, plot = FALSE)$out
boxplot(EmployeeData$Distance, plot = FALSE)$out
#Using winsorization for treatment of outliers- maxval was calculated per upper outlier fence#
EDOT <- EmployeeDataUpd #To be used during modelling process#
EDOT$Age <- Winsorize(EDOT$Age, maxval = 37.5)
EDOT$Work.Exp <- Winsorize(EDOT$Work.Exp, maxval = 15.5)
EDOT$Salary <- Winsorize(EDOT$Salary, maxval = 24.6)
EDOT$Distance <- Winsorize(EDOT$Distance, maxval = 20.4)
#Confirming the if the treatment for outliers was correctly done#
boxplot(EDOT$Age, plot = FALSE)$out
boxplot(EDOT$Work.Exp, plot = FALSE)$out
boxplot(EDOT$Salary, plot = FALSE)$out
boxplot(EDOT$Distance, plot = FALSE)$out
##Step 3.2.4: - Analyzing the dependent variable and factors of interest##
#The other interpretations used in the report are derived from excel#
#We will only check for skewness for the variables here#
#For Independent Variables#
skewness(select_if(EmployeeData, is.numeric))
#Density Plots for continuous variables#
EmployeeData %>% keep(is.numeric) %>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_density()
#*****************************************************************#
###Step 3.3: - Data Visualization###
#*****************************************************************#
##Step 3.3.1: - Univariate Analysis##
##For Continuous Numerical Variables##
par(mfrow = c(2,2), font.main = 2, font.axis = 2, font.lab = 2) #Convert our plotting space in 12 different frames#
NumData <- select_if(EmployeeData, is.numeric)
NumDataColName <- colnames(NumData)
for (ColNumber in (1:4)) {
  h = max(NumData[,ColNumber])+1
  l = min(NumData[,ColNumber])-1
  n = NumDataColName[ColNumber]
  hist (NumData[,ColNumber], breaks = seq(l,h,((h-l)/6)), include.lowest=T, right=T, col= brewer.pal(6,"Blues") , border=1, main = NULL , xlab= n, ylab=NULL, cex.lab= 1.5, cex.axis=1.2, cex.main=1)
}
dev.off()
#Bar charts for Categorical Variables - Class Favorability Analysis#
p1 <- ggplot(EmployeeData, aes(x = Gender, fill = Gender)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 2) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p2 <- ggplot(EmployeeData, aes(x = Engineer, fill = Engineer)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 2) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p3 <- ggplot(EmployeeData, aes(x = MBA, fill = MBA)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 2) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p4 <- ggplot(EmployeeData, aes(x = license, fill = license)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 2) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p5 <- ggplot(EmployeeData, aes(x = Transport, fill = Transport)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 2) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
grid.arrange(p1,p2,p3,p4,p5, nrow = 2, ncol = 3)
dev.off()
#Kernel Density Plots for Variables#
par(mfrow = c(2,2), font.main = 2, font.axis = 2, font.lab = 2)
plot(density(NumData$Age), main = NA)
plot(density(NumData$Work.Exp), main = NA)
plot(density(NumData$Salary), main = NA)
plot(density(NumData$Distance), main = NA)
dev.off()
##Step 3.3.2:- Bivariate Analysis##
#Data Summary by our Dependent Variable Transport#
by(EmployeeData,EmployeeData$Transport,FUN = summary)
#Bar Plots for Categorical Variables with Transport Classes as Legends#
p6 <- ggplot( data = EmployeeData, aes(x = Gender, fill = Transport)) + geom_bar(position ="dodge") + geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))+ scale_x_discrete(labels = c("Male" = "Male", "Female" = "Female"))+ scale_fill_discrete(name = "TransportMode", labels = c("2Wheeler", "Car","Public Transport")) +ggtitle("Gender vs Transport")
p7 <- ggplot( data = EmployeeData, aes(x = Engineer, fill = Transport)) + geom_bar(position ="dodge") + geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))+ scale_x_discrete(labels = c("0" = "Not an Engineer", "1" = "Is an Engineer"))+ scale_fill_discrete(name = "TransportMode", labels = c("2Wheeler", "Car","Public Transport")) +ggtitle("Engineer vs Transport")
p8 <- ggplot( data = EmployeeData, aes(x = MBA, fill = Transport)) + geom_bar(position ="dodge") + geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))+ scale_x_discrete(labels = c("0" = "Not a MBA", "1" = "Is An MBA"))+ scale_fill_discrete(name = "TransportMode", labels = c("2Wheeler", "Car","Public Transport")) +ggtitle("MBA vs Transport")
p9 <- ggplot( data = EmployeeData, aes(x = license, fill = Transport)) + geom_bar(position ="dodge") + geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))+ scale_x_discrete(labels = c("0" = "No Driving License", "1" = "Has Driving License"))+ scale_fill_discrete(name = "TransportMode", labels = c("2Wheeler", "Car","Public Transport")) +ggtitle("License vs Transport")
grid.arrange(p6,p7,p8,p9, nrow = 2, ncol = 2)
#With Percentages for each class#
p10 <- ggplot( data = EmployeeData, aes(x = Gender, fill = Transport)) + geom_bar(position ="fill") + geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.30))+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))+ scale_x_discrete(labels = c("Male" = "Male", "Female" = "Female"))+ scale_fill_discrete(name = "TransportMode", labels = c("2Wheeler", "Car","Public Transport")) +ggtitle("Gender vs Transport")
p11 <- ggplot( data = EmployeeData, aes(x = Engineer, fill = Transport)) + geom_bar(position ="fill") + geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.30))+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))+ scale_x_discrete(labels = c("0" = "Not an Engineer", "1" = "Is an Engineer"))+ scale_fill_discrete(name = "TransportMode", labels = c("2Wheeler", "Car","Public Transport")) +ggtitle("Engineer vs Transport")
p12 <- ggplot( data = EmployeeData, aes(x = MBA, fill = Transport)) + geom_bar(position ="fill", na.rm = T) + geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.30))+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))+ scale_x_discrete(labels = c("0" = "Not a MBA", "1" = "Is An MBA"))+ scale_fill_discrete(name = "TransportMode", labels = c("2Wheeler", "Car","Public Transport")) +ggtitle("MBA vs Transport")
p13 <- ggplot( data = EmployeeData, aes(x = license, fill = Transport)) + geom_bar(position ="fill") + geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_fill(vjust=0.30))+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))+ scale_x_discrete(labels = c("0" = "No Driving License", "1" = "Have A Driving License"))+ scale_fill_discrete(name = "TransportMode", labels = c("2Wheeler", "Car","Public Transport")) +ggtitle("License vs Transport")
grid.arrange(p10,p11,p12,p13, nrow = 2, ncol = 2)
dev.off()
#CrossTab - Additional Data#
descr::crosstab(EmployeeDataUpd$Transport, EmployeeDataUpd$Gender)
descr::crosstab(EmployeeDataUpd$Transport, EmployeeDataUpd$Engineer)
descr::crosstab(EmployeeDataUpd$Transport, EmployeeDataUpd$MBA)
descr::crosstab(EmployeeDataUpd$Transport, EmployeeDataUpd$license)
#Density Plot Analysis for the numerical variables#
EmployeeData %>% explore_all(target = Transport)
#Histogram with Legends- Additional Data#
ggplot(EmployeeData, aes(x = Age,fill = Transport)) + geom_histogram()
ggplot(EmployeeData, aes(x = Work.Exp,fill = Transport)) + geom_histogram()
ggplot(EmployeeData, aes(x = Salary,fill = Transport)) + geom_histogram()
ggplot(EmployeeData, aes(x = Distance,fill = Transport)) + geom_histogram()
##Plotting the boxplots for the Numerical Variables##
p14 <- ggplot(data=EmployeeData, mapping=aes(x=Transport, y=Age,fill = Transport)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p15 <- ggplot(data=EmployeeData, mapping=aes(x=Transport, y=Work.Exp,fill = Transport)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p16 <- ggplot(data=EmployeeData, mapping=aes(x=Transport, y=Salary,fill = Transport)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p17 <- ggplot(data=EmployeeData, mapping=aes(x=Transport, y=Distance,fill = Transport)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
grid.arrange(p14,p15,p16,p17,nrow = 2, ncol = 2)
#*****************************************************************#
###Step 3.4: - Multicollinearity Analysis###
#*****************************************************************#
##Step 3.4.1: - To find out correlation coefficient values for the variables##
CorData <- select_if(EmpData, is.numeric)
round(cor(CorData, use="complete.obs"),2)
##Step 3.4.2: -To plot the correlation for the data variables##
corrplot(round(cor(CorData, use="complete.obs"),2), method = "number",type = "lower", tl.cex = .9)
corrplot(round(cor(CorData, use="complete.obs"),2), method = "pie",type = "lower", tl.cex = .9)
ggpairs(EmpData)
##Step 3.4.3: -Individual test for variables to understand statistical significance##
#Creating the variable CarPrefTrans for next level of analysis#
EmployeeDataUpd$CarPrefTrans <- as.factor(ifelse(EmployeeDataUpd$Transport=="Car",1,0))
#Chi-Square Test for Categorical Variables#
chisq.test(EmployeeDataUpd$CarPrefTrans, EmployeeData$Gender)
chisq.test(EmployeeDataUpd$CarPrefTrans, EmployeeData$Engineer)
chisq.test(EmployeeDataUpd$CarPrefTrans, EmployeeData$MBA)
chisq.test(EmployeeDataUpd$CarPrefTrans, EmployeeData$license)
#Point-Biserial Correlation Test for Continuous Variables#
biserial.cor(EmployeeDataUpd$Age, EmployeeDataUpd$CarPrefTrans)
biserial.cor(EmployeeDataUpd$Work.Exp, EmployeeDataUpd$CarPrefTrans)
biserial.cor(EmployeeDataUpd$Salary, EmployeeDataUpd$CarPrefTrans)
biserial.cor(EmployeeDataUpd$Distance, EmployeeDataUpd$CarPrefTrans)
#Variation-Inflation Factor Analysis#
vifcor(CorData)
##Step 3.4.4: - Removing Work.Exp from the data for multicollinearity treatment##
EmpModellingData <- EmployeeDataUpd[,-5]
#----------------------------------------------------------------------#
####Step 4:- Data Preparation - Partitioning and Balancing####
#----------------------------------------------------------------------#
library(caTools)
library(DMwR)
#*****************************************************************#
###Step 4.1: - Data Partitioning###
#*****************************************************************#
set.seed(333)
#Removing the Transport variable since we we will use CarPrefTrans#
EmpModellingData <- EmpModellingData[,-8]
#Checking the observations count with respect to CarPrefTrans Variable#
dim(EmpModellingData)
table(EmpModellingData$CarPrefTrans)
prop.table(table(EmpModellingData$CarPrefTrans))
#Splitting the data into 70:30 Train:Test datasets#
DataSample <- sample.split(EmpModellingData$CarPrefTrans, SplitRatio = 0.7)
#Splitting the sample into Train and Test dataset#
Train <- subset(EmpModellingData, DataSample==T)
Test <- subset(EmpModellingData, DataSample==F)
#Verifying the proportion uniformity in the train & test sets#
#Dimension Check#
dim(Train) 
dim(Test)
#Proportion Check#
prop.table(table(Train$CarPrefTrans))
prop.table(table(Test$CarPrefTrans))
#*****************************************************************#
###Step 4.2: - Data Balancing###
#*****************************************************************#
TrainBal <- SMOTE(CarPrefTrans~., Train,perc.over = 250 ,perc.under = 150 )
prop.table(table(TrainBal$CarPrefTrans))
#----------------------------------------------------------------------#
####Step 5:- Machine Learning - Predictive Modelling####
#----------------------------------------------------------------------#
library(mlr)
library(pscl)
library(MLmetrics)
library(generalhoslem)
library(ROCR)
library(InformationValue)
library(class)
library(ipred)
library(rpart)
library(gbm)
library(xgboost)
#*******************************************************************************************************#
###Step 5.1: - Making Classification Tasks for the Train and Test datasets for the modelling process###
#*******************************************************************************************************#
TrainTask=mlr::makeClassifTask(data = TrainBal, target = "CarPrefTrans", positive = "1")
TestTask=mlr::makeClassifTask(data= Test, target = "CarPrefTrans", positive = "1")
TrainTask
TestTask
#****************************************************************************#
###Step 5.2: - Logistic Regression Model - Analysis & Interpretation###
#****************************************************************************#
#Model Creation and Summary#
LogRegLearner <- mlr::makeLearner("classif.logreg", predict.type = "response")
LogRegModel <- train(LogRegLearner, TrainTask)
summary(LogRegModel$learner.model)
#Variable Importance Plot for the Model#
LRVI <- caret::varImp(LogRegModel$learner.model)
ggplot(LRVI, aes(x=reorder(rownames(LRVI),Overall), y=Overall)) + geom_point( color="blue", size=4, alpha=0.6) + geom_segment( aes(x=rownames(LRVI), xend=rownames(LRVI), y=0, yend=Overall), color='skyblue') + xlab('Variable')+ylab('Overall Importance')+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none") +coord_flip() 
#Predicting the response variable through trained model#
LogRegPredict <- predict(LogRegModel, TestTask)
#Model Performance Analysis#
#Goodness of Fit Test#
pR2(LogRegModel$learner.model) #McFadden R Square Test#
logitgof(TrainBal$CarPrefTrans, fitted(LogRegModel$learner.model)) #Hosmer-Lemeshow Test#
#Performance Statistics#
mlr::performance(LogRegPredict) #Mean Misclassification Error#
calculateROCMeasures(LogRegPredict) #ROC Measures#
caret::confusionMatrix(Test$CarPrefTrans,LogRegPredict$data$response) #Confusion Matrix#
F1_Score(Test$CarPrefTrans,LogRegPredict$data$response) #F1 Score#
#AUC Statistic - ROC Curve#
LRPredROCR <- prediction(predict(LogRegModel$learner.model,Test, type = "response"),Test$CarPrefTrans)
LRPerfROCR <- performance(LRPredROCR , "tpr" , "fpr")
plot(LRPerfROCR,colorize = TRUE) #ROC Plot#
lines(x = c(0,1),y=c(0,1))
LRAUC <- performance(LRPredROCR, measure = "auc")
LRAUC@y.values[[1]] #AUC Statistic#
#KS Statistic and Plot#
LRKS <- max(attr(LRPerfROCR, "y.values")[[1]] - attr(LRPerfROCR, "x.values")[[1]])
LRKS #KS Statistic#
ks_plot(as.numeric(Test$CarPrefTrans), as.numeric(LogRegPredict$data$response))
LRGini <- 2*(LRAUC@y.values[[1]]) - 1
LRGini #Gini Coefficient#
#****************************************************************************#
###Step 5.3: - K-Nearest Neighbors (KNN) Model - Analysis & Interpretation###
#****************************************************************************#
#We will be using a generic KNN model here for better interpretation of the results#
#Normalizing the features for the model#
Normalize <- function(x){(x-min(x))/(max(x)-min(x))}
NormTrain <- as.data.frame(lapply(TrainBal[,c(1,5,6)], Normalize))
NormTrain <- cbind(NormTrain, TrainBal[,c(2,3,4,7,8)])
NormTest <- as.data.frame(lapply(Test[,c(1,5,6)], Normalize))
NormTest <- cbind(NormTest, Test[,c(2,3,4,7,8)])
#Model Creation and Summary#
TrControl <- trainControl(method = "cv")
KNNModel <- caret::train(CarPrefTrans~.,method = "knn",tuneGrid   = expand.grid(k = 2:20), trControl  = TrControl,metric = "Accuracy",preProcess = c("center","scale"),data = NormTrain)
KNNModel #Model Summary#
#Variable Importance Plot for the Model#
caret::varImp(KNNModel)
plot(caret::varImp(KNNModel))
#Predicting the response variable through trained model#
KNNPredict <- predict(KNNModel, NormTest)
#Model Performance Analysis#
#Performance Statistics#
mean(KNNPredict!= NormTest$CarPrefTrans) #Misclassification Error Rate#
caret::confusionMatrix(NormTest$CarPrefTrans,KNNPredict) #Confusion Matrix#
F1_Score(NormTest$CarPrefTrans,KNNPredict) #F1 Score#
#AUC Statistic - ROC Curve#
KNNProb <- predict(KNNModel,NormTest, type = "prob")
KNNPredROCR <- prediction(KNNProb$`1`, NormTest$CarPrefTrans)
KNNPerfROCR <- ROCR::performance(KNNPredROCR, "tpr" , "fpr")
plot(KNNPerfROCR,colorize = TRUE) #ROC Plot#
lines(x = c(0,1),y=c(0,1))
KNNAUC <- ROCR::performance(KNNPredROCR, measure = "auc")
KNNAUC@y.values[[1]] #AUC Statistic#
#KS Statistic and Plot#
KNNKS <- max(attr(KNNPerfROCR, "y.values")[[1]] - attr(KNNPerfROCR, "x.values")[[1]])
KNNKS #KS Statistic#
ks_plot(as.numeric(NormTest$CarPrefTrans), as.numeric(KNNPredict))
KNNGini <- 2*(KNNAUC@y.values[[1]]) - 1
KNNGini #Gini Coefficient#
#****************************************************************************#
###Step 5.4: - Naive Bayes Model - Analysis & Interpretation###
#****************************************************************************#
NBLearner <- mlr::makeLearner("classif.naiveBayes", predict.type = "response")
NBModel <- mlr::train(NBLearner, TrainTask)
getLearnerModel(NBModel)
#Predicting the response variable through trained model#
NBPredict <- predict(NBModel, TestTask)
#Model Performance Analysis#
calculateROCMeasures(NBPredict) #ROC Measures#
caret::confusionMatrix(Test$CarPrefTrans,NBPredict$data$response) #Confusion Matrix#
mlr::performance(NBPredict) #Mean Misclassification Error#
F1_Score(Test$CarPrefTrans,NBPredict$data$response) #F1 Score#
#AUC Statistic - ROC Curve#
NBProbROCR <- as.data.frame(predict(NBModel$learner.model,Test, type = "raw"))
NBPredROCR <- prediction(NBProbROCR$`1` ,Test$CarPrefTrans)
NBPerfROCR <- ROCR::performance(NBPredROCR , "tpr" , "fpr")
plot(NBPerfROCR,colorize = TRUE) #ROC Plot#
lines(x = c(0,1),y=c(0,1))
NBAUC <- ROCR::performance(NBPredROCR, measure = "auc")
NBAUC@y.values[[1]] #AUC Statistic#
#KS Statistic and Plot#
NBKS <- max(attr(NBPerfROCR, "y.values")[[1]] - attr(NBPerfROCR, "x.values")[[1]])
NBKS #KS Statistic#
ks_plot(as.numeric(Test$CarPrefTrans), as.numeric(NBPredict$data$response))
NBGini <- 2*(NBAUC@y.values[[1]]) - 1
NBGini #Gini Coefficient#
#**********************************************************************************#
#Predictive Modelling for Unbalanced Train Dataset- For performance comparison only#
#**********************************************************************************#
#This is not a part of the report but just done here for additional analysis#
#THIS IS FOR UNBALANCED TRAIN DATASET MODELS#
#Creating the models and making the predictions#
TrainTaskUnb <- mlr::makeClassifTask(data = Train, target = "CarPrefTrans", positive = "1")
NormTrainUnb <- as.data.frame(lapply(TrainBal[,c(1,5,6)], Normalize))
NormTrainUnb <- cbind(NormTrainUnb, TrainBal[,c(2,3,4,7,8)])
UTLRM1Pred <- predict((train(LogRegLearner,TrainTaskUnb)), TestTask)
UTKNNM2Pred <- predict((caret::train(CarPrefTrans~.,method = "knn",tuneGrid   = expand.grid(k = 2:20), trControl  = TrControl,metric = "Accuracy",preProcess = c("center","scale"),data = NormTrainUnb)), NormTest)
UTNBM3Pred <- predict((train(NBLearner,TrainTaskUnb)), TestTask)
#Model Performance Analysis#
#Creating a confusion matrix to record the results#
UTCM <- data.frame(matrix(ncol=4,nrow=9, dimnames=list(NULL, c("Measures","Logistic Regression", "KNN", "Naive Bayes"))))
UTCM$Measures <- c("Accuracy", "Balanced Accuracy", "Sensitivity", "Specificity", "Classification Error Rate", "F1 Score", "AUC Stat","KS", "GINI")
#Logistic Regression Model#
caret::confusionMatrix(Test$CarPrefTrans,UTLRM1Pred$data$response) #Confusion Matrix#
UTCM[1,2] <- 0.932
UTCM[2,2] <- 0.833
UTCM[3,2] <- 1.000
UTCM[4,2] <- 0.667
UTCM[5,2] <- mlr::performance(UTLRM1Pred) #Mean Misclassification Error#
UTCM[6,2] <- F1_Score(Test$CarPrefTrans,UTLRM1Pred$data$response) #F1 Score#
UTCM[7,2] <-(performance((prediction(predict((train(LogRegLearner,TrainTaskUnb))$learner.model,Test, type = "response"),Test$CarPrefTrans)), measure = "auc"))@y.values[[1]] #AUC Statistic#
UTCM[8,2] <-max(attr(performance((prediction(predict((train(LogRegLearner,TrainTaskUnb))$learner.model,Test, type = "response"),Test$CarPrefTrans)) , "tpr" , "fpr"), "y.values")[[1]] - attr(performance((prediction(predict((train(LogRegLearner,TrainTaskUnb))$learner.model,Test, type = "response"),Test$CarPrefTrans)) , "tpr" , "fpr"), "x.values")[[1]]) #KS Statistic#
UTCM[9,2] <- 2*((performance((prediction(predict((train(LogRegLearner,TrainTaskUnb))$learner.model,Test, type = "response"),Test$CarPrefTrans)), measure = "auc"))@y.values[[1]]) - 1 #Gini Coefficient#
#KNN Model#
caret::confusionMatrix(NormTest$CarPrefTrans,UTKNNM2Pred) #Confusion Matrix#
UTCM[1,3] <- 0.872
UTCM[2,3] <- 0.757
UTCM[3,3] <- 1.000
UTCM[4,3] <- 0.514
UTCM[5,3] <- mean(UTKNNM2Pred!= NormTest$CarPrefTrans) #Misclassification Error Rate#
UTCM[6,3] <- F1_Score(NormTest$CarPrefTrans,UTKNNM2Pred) #F1 Score#
UTCM[7,3] <- (ROCR::performance((prediction((predict((caret::train(CarPrefTrans~.,method = "knn",tuneGrid   = expand.grid(k = 2:20), trControl  = TrControl,metric = "Accuracy",preProcess = c("center","scale"),data = NormTrainUnb)),NormTest, type = "prob"))$`1`, NormTest$CarPrefTrans)), measure = "auc"))@y.values[[1]] #AUC Statistic#
UTCM[8,3] <- max(attr((ROCR::performance((prediction((predict((caret::train(CarPrefTrans~.,method = "knn",tuneGrid   = expand.grid(k = 2:20), trControl  = TrControl,metric = "Accuracy",preProcess = c("center","scale"),data = NormTrainUnb)),NormTest, type = "prob"))$`1`, NormTest$CarPrefTrans)), "tpr" , "fpr")), "y.values")[[1]] - attr((ROCR::performance((prediction((predict((caret::train(CarPrefTrans~.,method = "knn",tuneGrid   = expand.grid(k = 2:20), trControl  = TrControl,metric = "Accuracy",preProcess = c("center","scale"),data = NormTrainUnb)),NormTest, type = "prob"))$`1`, NormTest$CarPrefTrans)), "tpr" , "fpr")), "x.values")[[1]]) #KS Statistic#
UTCM[9,3] <- 2*((ROCR::performance((prediction((predict((caret::train(CarPrefTrans~.,method = "knn",tuneGrid   = expand.grid(k = 2:20), trControl  = TrControl,metric = "Accuracy",preProcess = c("center","scale"),data = NormTrainUnb)),NormTest, type = "prob"))$`1`, NormTest$CarPrefTrans)), measure = "auc"))@y.values[[1]]) - 1 #Gini Coefficient#
#Naive Bayes Model#
caret::confusionMatrix(Test$CarPrefTrans,UTNBM3Pred$data$response) #Confusion Matrix#
UTCM[1,4] <- 0.925
UTCM[2,4] <- 0.822
UTCM[3,4] <- 0.991
UTCM[4,4] <- 0.654
UTCM[5,4] <- mlr::performance(UTNBM3Pred) #Mean Misclassification Error#
UTCM[6,4] <- F1_Score(Test$CarPrefTrans,UTNBM3Pred$data$response) #F1 Score#
UTCM[7,4] <- ROCR::performance((prediction((as.data.frame(predict((train(NBLearner,TrainTaskUnb))$learner.model,Test, type = "raw")))$`1` ,Test$CarPrefTrans)), measure = "auc")@y.values[[1]] #AUC Statistic#
UTCM[8,4] <- max(attr((ROCR::performance((prediction((as.data.frame(predict((train(NBLearner,TrainTaskUnb))$learner.model,Test, type = "raw")))$`1` ,Test$CarPrefTrans)) , "tpr" , "fpr")), "y.values")[[1]] - attr((ROCR::performance((prediction((as.data.frame(predict((train(NBLearner,TrainTaskUnb))$learner.model,Test, type = "raw")))$`1` ,Test$CarPrefTrans)) , "tpr" , "fpr")), "x.values")[[1]]) #KS Statistic#
UTCM[9,4] <- 2*(ROCR::performance((prediction((as.data.frame(predict((train(NBLearner,TrainTaskUnb))$learner.model,Test, type = "raw")))$`1` ,Test$CarPrefTrans)), measure = "auc")@y.values[[1]]) - 1
#Viewing the comparison of performance for all models#
UTCM
#Conclusion: Logistic Regression performs best for unbalanced Train in comparison to other models#
#********************************************************************************#
###Step 5.5: - Confusion Matrix Interpretation and Model Performance Validation###
#********************************************************************************#
#THIS IS FOR BALANCED TRAIN DATASET MODELS#
#Adding the performance measures to the matrix for comparison of each model#
#Creating the model performance matrix to store the performance measures for comparison later on#
ModelPerformanceAnalysis <- data.frame(matrix(ncol=4,nrow=9, dimnames=list(NULL, c("Measures","Logistic Regression", "KNN", "Naive Bayes"))))
ModelPerformanceAnalysis$Measures <- c("Accuracy", "Balanced Accuracy", "Sensitivity", "Specificity", "Classification Error Rate", "F1 Score", "AUC Stat","KS", "GINI")
ModelPerformanceAnalysis$Logistic.Regression <- c(0.932,0.833,1.000,0.667,0.068,0.96,0.99,0.94,0.98)
ModelPerformanceAnalysis$KNN <- c(0.857,0.738,0.990,0.486,0.140,0.91,0.95,0.87,0.91)
ModelPerformanceAnalysis$Naive.Bayes <- c(0.917,0.810,1.000,0.621,0.083,0.95,0.99,0.92,0.98)
#Comparing the models for performance#
ModelPerformanceAnalysis
#Conclusion: Logistic Regression came out to be the best model amongst all#
#****************************************************************************#
###Step 5.6: - Bagging & Boosting Ensemble Method Analysis###
#****************************************************************************#
##Bagging Method Analysis##
Bagging <- bagging(CarPrefTrans~., data = Train,control=rpart.control(maxdepth=5, minsplit=4))
BaggingPred <- predict(Bagging,Test)
#Model Performance Analysis#
#Performance Statistics#
mean(BaggingPred!= Test$CarPrefTrans) #Misclassification Error Rate#
caret::confusionMatrix(Test$CarPrefTrans,BaggingPred) #Confusion Matrix#
F1_Score(Test$CarPrefTrans,BaggingPred) #F1 Score#
#AUC Statistic - ROC Curve#
BaggingProb <- as.data.frame(predict(Bagging,Test, type = "prob"))
BaggingPredROCR <- prediction(BaggingProb$`1` , Test$CarPrefTrans)
BaggingPerfROCR <- ROCR::performance(BaggingPredROCR, "tpr" , "fpr")
plot(BaggingPerfROCR,colorize = TRUE) #ROC Plot#
lines(x = c(0,1),y=c(0,1))
BaggingAUC <- ROCR::performance(BaggingPredROCR, measure = "auc")
BaggingAUC@y.values[[1]] #AUC Statistic#
#KS Statistic and Plot#
BaggingKS <- max(attr(BaggingPerfROCR, "y.values")[[1]] - attr(BaggingPerfROCR, "x.values")[[1]])
BaggingKS #KS Statistic#
ks_plot(as.numeric(Test$CarPrefTrans), as.numeric(BaggingPred))
BaggingGini <- 2*(BaggingAUC@y.values[[1]]) - 1
BaggingGini #Gini Coefficient#
##Boosting Method Analysis##
#Converting all the variable to numeric for model compatibility#
BoostingTrain <- data.frame(lapply(Train, as.numeric))
BoostingTest <- data.frame(lapply(Test, as.numeric))
BoostingTrain$CarPrefTrans <- ifelse(BoostingTrain$CarPrefTrans==1,0,1)
BoostingTest$CarPrefTrans <- ifelse(BoostingTest$CarPrefTrans==1,0,1)
str(BoostingTrain)
str(BoostingTest)
#Model 1 - Gradient Boosting#
GBMBoosting = gbm::gbm(CarPrefTrans~.,distribution = "bernoulli" ,data = BoostingTrain, n.trees = 5000,interaction.depth = 4,shrinkage = 0.01)
summary(GBMBoosting) #Variable Importance Analysis#
GBMBoostingPred <- predict(GBMBoosting,BoostingTest,n.trees=5000, type = "response")
GBMPred <-ifelse(GBMBoostingPred>0.5,1,0)
GBMPred <- factor(GBMPred, levels = c(0,1))
table(GBMPred,BoostingTest$CarPrefTrans)
#Model Performance Analysis#
#Performance Statistics#
BoostTestComp <- factor(BoostingTest$CarPrefTrans, levels = c(0,1)) 
mean(GBMPred!= BoostTestComp) #Misclassification Error Rate#
caret::confusionMatrix(GBMPred,BoostTestComp) #Confusion Matrix#
F1_Score(BoostTestComp,GBMPred) #F1 Score#
#AUC Statistic - ROC Curve#
GBMBoostingProb <- predict(GBMBoosting,BoostingTest, type = "response")
GBMBoostingPredROCR <- prediction(GBMBoostingProb , BoostingTest$CarPrefTrans)
GBMBoostingPerfROCR <- ROCR::performance(GBMBoostingPredROCR , "tpr" , "fpr")
plot(GBMBoostingPerfROCR,colorize = TRUE) #ROC Plot#
lines(x = c(0,1),y=c(0,1))
GBMBoostingAUC <- ROCR::performance(GBMBoostingPredROCR, measure = "auc")
GBMBoostingAUC@y.values[[1]] #AUC Statistic#
#KS Statistic and Plot#
GBMBoostingKS <- max(attr(GBMBoostingPerfROCR, "y.values")[[1]] - attr(GBMBoostingPerfROCR, "x.values")[[1]])
GBMBoostingKS #KS Statistic#
ks_plot(BoostingTest$CarPrefTrans, as.numeric(GBMPred))
GBMBoostingGini <- 2*(GBMBoostingAUC@y.values[[1]]) - 1
GBMBoostingGini #Gini Coefficient#
#Model 2 - Extreme Gradient Boosting#
XGBTrainFeatures <- as.matrix(BoostingTrain[,-8])
XGBTrainLabels <- as.matrix(BoostingTrain[,8])
XGBTestFeatures <- as.matrix(BoostingTest[,-8])
XGBBoostingModel <-xgboost(data = XGBTrainFeatures,label = XGBTrainLabels, nrounds = 100, eta = 0.1,max_depth = 5) 
XGBBoostingPred <- predict(XGBBoostingModel,XGBTestFeatures)
xgb.plot.importance(xgb.importance(model = XGBBoostingModel)) #Variable Importance Analysis#
#Converting the prediction probabilities for model performance comparison#
XGBPred<- as.numeric(XGBBoostingPred>0.5)
XGBPred
#Model Performance Analysis#
#Performance Statistics#
mean(factor(XGBPred)!= BoostTestComp) #Misclassification Error Rate#
caret::confusionMatrix(BoostTestComp,factor(XGBPred)) #Confusion Matrix#
F1_Score(BoostTestComp,factor(XGBPred)) #F1 Score#
#AUC Statistic - ROC Curve#
XGBBoostingProb <- as.data.frame(predict(XGBBoostingModel ,XGBTestFeatures, type = "prob"))
XGBBoostingPredROCR <- prediction(XGBBoostingProb, data.frame(BoostTestComp))
XGBBoostingPerfROCR <- ROCR::performance(XGBBoostingPredROCR, "tpr" , "fpr")
plot(XGBBoostingPerfROCR,colorize = TRUE) #ROC Plot#
lines(x = c(0,1),y=c(0,1))
XGBBoostingAUC <- ROCR::performance(XGBBoostingPredROCR, measure = "auc")
XGBBoostingAUC@y.values[[1]] #AUC Statistic#
#KS Statistic and Plot#
XGBBoostingKS <- max(attr(XGBBoostingPerfROCR, "y.values")[[1]] - attr(XGBBoostingPerfROCR, "x.values")[[1]])
XGBBoostingKS #KS Statistic#
ks_plot(as.numeric(BoostTestComp), XGBPred)
XGBBoostingGini <- 2*(XGBBoostingAUC@y.values[[1]]) - 1
XGBBoostingGini #Gini Coefficient#
#Creating the final model performance matrix with the Bagging/Boosting models for analysis and comparison#
ModelPerformanceAnalysis$Bagging <- c(0.940,0.846,1.000,0.692,0.06,0.96,0.99,0.93,0.98)
ModelPerformanceAnalysis$GradientBoosting <- c(0.962,0.978,0.957,1.000,0.038,0.98,1.00,0.98,1.00)
ModelPerformanceAnalysis$ExtremeGradientBoosting <- c(0.947,0.865,0.991,0.739,0.053,0.97,0.99,0.95,0.98)
ModelPerformanceAnalysis
#Conclusion: Gradient Boosting Model has performed the best amongst all models, Logistic Regression#
#            model can be considered if no changes are preferred for data variables.               #

#########################End of the Code####################################
#########################End of the Analysis################################

##############################################################################################
