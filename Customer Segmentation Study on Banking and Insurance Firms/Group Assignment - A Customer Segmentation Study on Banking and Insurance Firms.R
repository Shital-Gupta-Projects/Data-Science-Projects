#######################ASSIGNMENT FOR DATA MINING###########################
######################SUBMITTED BY :- SHITAL GUPTA##########################
####R PROJECT: CUSTOMER SEGMENTATION STUDY FOR BANKING & INSURANCE FIRMS####
############################################################################

########################START OF THE ANALYSIS###############################
##########################START OF THE CODE#################################

####Step 1 :- Setting the Current Working Directory####
getwd()
setwd("C:/Users/shital/Documents/R/RProgramming")
####Step 2 :- Reading and viewing the data from both the datasets####
Banking <- read.csv("bank_marketing_part1_Data.csv", header = TRUE)
head(Banking)
Insurance <- read.csv("insurance_part2_data.csv", header = TRUE)
head(Insurance)
####Step 3 :-Start of Exploratory Data Analysis####
####Step 3.1:- Interpreting the data####
####Step 3.1.1 :- Checking the dimensions of the datasets####
dim(Banking)
dim(Insurance)
####Step 3.1.2 :-Summarizing, Structure Check and Descriptive Statistics for the data####
str(Banking)
str(Insurance)
summary(Banking)
summary(Insurance)
library(psych)
describe(Banking)
describe(Insurance)
####Step 3.2:- Processing the data####
####Step 3.2.1 :- Checking for imbalanced data in our variables####
library(plotrix)
par(mfrow = c(2,3))
pie3D((prop.table(table(Insurance$Claimed))), main = "Claim Status Favorability", labels = c("Negative Response", "Positive Response"), cex.main = 1.3, labelcex = 1)
pie3D((prop.table(table(Insurance$Agency_Code))), main = "Agency Favorability", labels = c("C2B", "CWT", "EPX", "JZI"), cex.main = 1.3, labelcex = 1)
pie3D((prop.table(table(Insurance$Type))), main = "Insurance Type Favorability", labels = c("Airlines", "Travel Agency"), cex.main = 1.3, labelcex = 1)
pie3D((prop.table(table(Insurance$Channel))), main = "Distribution Channel Favorability", labels = c("Online", "Offline"), cex.main = 1.3, labelcex = 1)
pie3D((prop.table(table(Insurance$Product.Name))), main = "Product Favorability", labels = c("Bronze", "Cancellation", "Customized", "Gold", "Silver"), cex.main = 1.3, labelcex = 1)
pie3D((prop.table(table(Insurance$Destination))), main = "Destination Favorability", labels = c("Americas", "Asia", "Europe"), cex.main = 1.3, labelcex = 1)
####Step 3.2.2 :- Analyzing the Response Variable and Factors of Interest ####
##Skewness Check##
library(PerformanceAnalytics)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
BankingSkewnessCheck <- skewness(Banking)
BankingSkewnessCheck
InsuranceNumerical <- Insurance[,-c(2,3,4,6,9,10)]
InsuranceSkewnessCheck <- skewness(InsuranceNumerical)
InsuranceSkewnessCheck
##Density Plot for Skewness Check##
Banking %>% keep(is.numeric) %>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_density()
Insurance %>% keep(is.numeric) %>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_density()
####Step 3.2.3:- Missing Value Treatment####
colSums(is.na(Banking))
colSums(is.na(Insurance))
##Negative Value Treatment##
colSums((Banking)<0)
colSums((Insurance)<0)
####Step 3.2.4:- Outlier Treatment####
##Boxplots for variables##
library(RColorBrewer)
boxplot(Banking$spending,horizontal = TRUE, col = c("Red"), names = c("Spending"), main = "Boxplot for Spending Variable", cex.main = 1)
boxplot(Banking$advance_payments,horizontal = TRUE, col = c("Green"), names = c("Advance Payments"), main = "Boxplot for Advance Payments Variable", cex.main = 1)
boxplot(Banking$probability_of_full_payment,horizontal = TRUE, col = c("Blue"), names = c("Full Payment Probability"), main = "Boxplot for Full Payment Probability Variable", cex.main = 1)
boxplot(Banking$current_balance,horizontal = TRUE, col = c("Orange"), names = c("Account Balance"), main = "Boxplot for Account Balance Variable", cex.main = 1)
boxplot(Banking$credit_limit,horizontal = TRUE, col = c("cornflower blue"), names = c("Credit Limit"), main = "Boxplot for Credit Limit Variable", cex.main = 1)
boxplot(Banking$min_payment_amt,horizontal = TRUE, col = c("Pink"), names = c("Minimum Payment"), main = "Boxplot for Minimum Payment Variable", cex.main = 1)
boxplot(Banking$max_spent_in_single_shopping,horizontal = TRUE, col = c("Purple"), names = c("Maximum Spent in One Off Shopping"), main = "Boxplot for Maximum Spent OneOff Shopping Variable", cex.main = 1)
boxplot(InsuranceNumerical$Age,horizontal = TRUE, col = c("Tan"), names = c("Age"), main = "Boxplot for Age variable", cex.main = 1)
boxplot(InsuranceNumerical$Commision,horizontal = TRUE, col = c("Grey"), names = c("Commision"), main = "Boxplot for Commision variable", cex.main = 1)
boxplot(InsuranceNumerical$Duration,horizontal = TRUE, col = c("Light Blue"), names = c("Duration"), main = "Boxplot for Duration variable", cex.main = 1)
boxplot(InsuranceNumerical$Sales,horizontal = TRUE, col = c("Red"), names = c("Sales"), main = "Boxplot for Sales variable", cex.main = 1)
##For Banking Dataset##
list("OutlierValues")
OutlierValues <- Banking
for (Values in c(1:7)) {
  
  Box_Plot <- boxplot(Banking[,Values],plot = F)$out
  OutlierValues[,Values] <- NA
  if (length(Box_Plot)>0) {
    OutlierValues[(1:length(Box_Plot)),Values] <- Box_Plot 
  }
}
OutlierValues <- OutlierValues[(1:4),]
OutlierValues[,c(3,6)]
##For Insurance Dataset##
boxplot(InsuranceNumerical$Age,plot = FALSE)$out
boxplot(InsuranceNumerical$Duration, plot = FALSE)$out
boxplot(InsuranceNumerical$Sales, plot = FALSE)$out
boxplot(InsuranceNumerical$Commision, plot = FALSE)$out
####Step 3.3:- Visualizing the data####
####Step 3.3.1:- Univariate Analysis####
##For Continuous Numerical Variables##
BankCol <- colnames(Banking)
par(mfrow = c(3,3), font.main = 2, font.axis = 2, font.lab = 2) #Convert our plotting space in 12 different frames#
for (ColNumber in (1:7)) {
  h = max(Banking[,ColNumber])+1
  l = min(Banking[,ColNumber])-1
  n = BankCol[ColNumber]
  hist (Banking[,ColNumber], breaks = seq(l,h,((h-l)/6)), include.lowest=T, right=T, col= brewer.pal(6,"Blues") , border=1, main = NULL , xlab= n, ylab=NULL, cex.lab= 1.5, cex.axis=1.2, cex.main=1)
}
InsurCol <- colnames(InsuranceNumerical)
par(mfrow = c(2,2), font.main = 2, font.axis = 2, font.lab = 2)
for (ColNumber in (1:4)) {
  h = max(InsuranceNumerical[,ColNumber])+1
  l = min(InsuranceNumerical[,ColNumber])-1
  n = InsurCol[ColNumber]
  hist (InsuranceNumerical[,ColNumber], breaks = seq(l,h,((h-l)/6)), include.lowest=T, right=T, col= brewer.pal(6,"Blues") , border=1, main = NULL , xlab= n, ylab=NULL, cex.lab= 1.5, cex.axis=1.2, cex.main=1)
}
##For Categorical Variables##
library(gridExtra)
p1 <- ggplot(Insurance, aes(x = Claimed, fill = Claimed)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 2) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p2 <- ggplot(Insurance, aes(x = Agency_Code, fill = Agency_Code)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 2) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p3 <- ggplot(Insurance, aes(x = Type, fill = Type)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 2) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p4 <- ggplot(Insurance, aes(x = Channel, fill = Channel)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 1.5) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p5 <- ggplot(Insurance, aes(x = Product.Name, fill = Product.Name)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 1.9) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")+ scale_x_discrete(labels = c("Bronze", "Cancellation","Custom","Gold", "Silver"))
p6 <- ggplot(Insurance, aes(x = Destination, fill = Destination)) + geom_bar() + geom_text(stat = "count", aes(label = ..count..), vjust = 1.6) + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
grid.arrange(p1,p2,p3,p4,p5,p6, nrow = 2)
####Step 3.3.2:- Bivariate Analysis####
##For Banking Dataset##
library(corrplot)
BCor <- cor(Banking)
round(BCor,2)
corrplot(round(BCor,2),method = "number", tl.cex = .9)
corrplot(BCor,method = "pie", tl.col = "Black", tl.cex = .8)
plot(Banking) 
##For Insurance Dataset##
##Chi Square Test For Categorical Variables for Correlation Analysis##
chisq.test(Insurance$Claimed, Insurance$Agency_Code)
as.matrix((prop.table(table(Insurance$Agency_Code)))*100)
chisq.test(Insurance$Claimed, Insurance$Type)
as.matrix((prop.table(table(Insurance$Type)))*100)
chisq.test(Insurance$Claimed, Insurance$Channel)
as.matrix((prop.table(table(Insurance$Channel)))*100)
chisq.test(Insurance$Claimed, Insurance$Product.Name)
as.matrix((prop.table(table(Insurance$Product.Name)))*100)
chisq.test(Insurance$Claimed, Insurance$Destination)
as.matrix((prop.table(table(Insurance$Destination)))*100)
##Plotting the Categorical Variables##
p7 <- ggplot( data = Insurance, aes(x = Agency_Code, y = as.numeric(Claimed), fill = Claimed)) + geom_bar(position ="fill", stat = "identity") + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))
p8 <-ggplot( data = Insurance, aes(x = Type, y = as.numeric(Claimed), fill = Claimed)) + geom_bar(position ="fill", stat = "identity") + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))
p9 <-ggplot( data = Insurance, aes(x = Channel, y = as.numeric(Claimed), fill = Claimed)) + geom_bar(position ="fill", stat = "identity") + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))
p10 <-ggplot( data = Insurance, aes(x = Product.Name, y = as.numeric(Claimed), fill = Claimed)) + geom_bar(position ="fill", stat = "identity") + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))+ scale_x_discrete(labels = c("Bronze", "Cancellation","Custom","Gold", "Silver"))
p11 <-ggplot( data = Insurance, aes(x = Destination, y = as.numeric(Claimed), fill = Claimed)) + geom_bar(position ="fill", stat = "identity") + theme(axis.title.x = element_text(face="bold"),axis.title.y = element_blank(), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10))
grid.arrange(p7,p8,p9,p10,p11, nrow = 2)
##Binary Logistic Regression for Numerical Variables Correlation Analysis##
library(MASS)
LRModel <- glm(Claimed~Age+Commision+Duration+Sales,family = binomial(link = logit), data = Insurance)
summary(LRModel)
##Plotting the Numerical Variables##
p12 <- ggplot(data=Insurance, mapping=aes(x=Claimed, y=Age,fill = Claimed)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p13 <- ggplot(data=Insurance, mapping=aes(x=Claimed, y=Duration,fill = Claimed)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p14 <- ggplot(data=Insurance, mapping=aes(x=Claimed, y=Sales,fill = Claimed)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
p15 <- ggplot(data=Insurance, mapping=aes(x=Claimed, y=Commision,fill = Claimed)) + geom_boxplot()+ theme(axis.title.x = element_text(face="bold"),axis.title.y = element_text(face="bold"), axis.text.x = element_text(face="bold", size = 10),axis.text.y = element_text(face="bold", size = 10), legend.position = "none")
grid.arrange(p12,p13,p14,p15,nrow = 2)
####Step 4 :- Customer Segmentation####
####Step 4.1 :- Scaling the Data####
bank_data_scaled <- scale(Banking)
print(bank_data_scaled)
##Checking the scaled data##
apply(bank_data_scaled, 2, mean)
apply(bank_data_scaled, 2, sd)
##Computing the distance matrix##
distMatrix <- dist(bank_data_scaled, method = "euclidean")
####Start of Cluster Analysis####
####Step 4.2 :- Creating the Hierchical Clustering Model####
cluster <- hclust(distMatrix, method = "average")
cluster$height
plot(cluster)
rect.hclust(cluster, k=3, border = "red")
Banking$cluster <- cutree(cluster, k = 3 )
HCCustomerProfile <- aggregate(Banking,list(Banking$cluster), FUN = "mean")
print(HCCustomerProfile)
####Step 4.3 :- Creating the K-means Clustering Model####
##Setting the seed value##
set.seed(100)
library(cluster)
cluster2 <- kmeans(bank_data_scaled, nstart = 10, centers = 2)
clusplot(bank_data_scaled, cluster2$cluster, color = T,shade = T,labels = 2, lines = 1)
##Method 1:Finding the optimum number of clusters##
totWss = rep(0:16)
for (k in 1:16) {
  set.seed(100)
  clust <- kmeans(bank_data_scaled, nstart = 16, centers = k)
  totWss[k] <- clust$tot.withinss
}
plot(totWss, type = "b")
print(totWss)
##Method 2: Using Nbcluster to identify the number of clusters##
library(NbClust)
nc <- NbClust(Banking[,-8], min.nc = 2, max.nc = 10, method = "kmeans")
##Clustering with 3 Clusters - Optimum Model##
cluster3 <- kmeans(bank_data_scaled, nstart = 10, centers = 3)
##Plotting the Optimum Cluster Model##
clusplot(bank_data_scaled, cluster3$cluster, color = T,shade = T,labels = 2, lines = 1)
Banking$cluster2 <- cluster3$cluster
####Step 4.4 :- Cluster Profiling for the data####
FinalCustomerProfile <- aggregate(Banking[,-8],list(Banking$cluster2), FUN = "mean")
print(FinalCustomerProfile)
###End of Cluster Analysis###
####Step 5 :- Predictive Analysis####
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(data.table)
library(ineq)
library(neuralnet)
##Setting the seed value##
set.seed(2225)
####Step 5.1 :- Partitioning our datasets into train and test dataset####
Sample <- sample(2, nrow(Insurance), prob = c(0.7,0.3),replace=T)
Train <-  Insurance[Sample==1,]
Test<-  Insurance[Sample==2,]
##Checking if the proportion of data is correct w.r.t. Response Variable##
prop.table((table(Train$Claimed)))
prop.table((table(Test$Claimed)))
####Step 5.2 :- Creating the balanced partitioned datasets####
library(ROSE)
##Checking the count of Majority Class favored customers in both the sets##
table(Train$Claimed)
table(Test$Claimed)
#N is twice the response variable majority class count in the set#
Train_Bal <- ovun.sample(Claimed~., data = Train, method = "over", N = 2888)$data
Test_Bal <- ovun.sample(Claimed~., data = Test, method = "over", N = 1264)$data
####Step 5.3:- Classification and Regression Trees Model####
##To Specify the control parameters of the cart r.ctrl is assigned##
r.ctrl <- rpart.control(minsplit = 200,minbucket = 20,cp=0,xval = 10)
##To create the CART model using the above control parameters##
##We use "method = class" as our DV is categorial, if it was continuous we would have used "method = anova"##
CMUnbalanced <- rpart(formula = Claimed~.,data = Train,control = r.ctrl,method = "class")
CMBalanced <- rpart(formula = Claimed~.,data = Train_Bal,control = r.ctrl,method = "class")
##Let us now find from the model the rule which classifies the person as delinquint##
fancyRpartPlot(CMUnbalanced)
fancyRpartPlot(CMBalanced)
##Pruning the trees to show only the significant variables##
PCMUNB <- prune(CMUnbalanced, cp= 0.0017 ,"CP")
PCMB <- prune(CMBalanced, cp= 0.0017 ,"CP")
fancyRpartPlot(PCMUNB,  uniform = TRUE, main = "Final Tree - Unbalanced Dataset", palettes = c("Blues", "Oranges"))
fancyRpartPlot(PCMB,  uniform = TRUE, main = "Final Tree - Balanced Dataset", palettes = c("Blues", "Oranges"))
#Unbalanced Data Customer Delinquency Rules: (1.Agency_code should be CWT,EPX,JZI)(2.Sales less than 84)(3.Product.Name = Bronze Plan and Cancellation Plan)#
#Balanced Data Customer Delinquency Rules: (1.Agency_code should be CWT,EPX,JZI)(2.Product.Name = Bronze Plan and Cancellation Plan)#
####Step 5.4:- Random Forest Model####
library(mlbench)
library(randomForest)
library(caret)
##Setting the seed value##
set.seed(224)
##Number of IVS are 9 here so probable value of mtry = 3##
##Lets take ntree = 500##
##Using the tuneRF function value to find the correct value of mtry##
mtry <- tuneRF(Train[-4],Train$Claimed,ntreeTry = 500,stepFactor = 1.5,improve = 0.01,trace = T,plot = T)
mtry <- tuneRF(Train_Bal[-4],Train_Bal$Claimed,ntreeTry = 500,stepFactor = 1.5,improve = 0.01,trace = T,plot = T)
##As the OOB error is lowest at mtry = 2 hence that is our value##
##To create the RF model based on mtry value##
RFUnbalanced <- randomForest(Claimed~.,data=Train,mtry = 2,importance=T,ntree=500)
RFBalanced <- randomForest(Claimed~.,data=Train_Bal,mtry = 6,importance=T,ntree=500)
print(RFUnbalanced)
print(RFBalanced)
##Plotting the RF tree to verify the significant variables##
varImpPlot(RFUnbalanced, main = "Random Forest Model - Unbalanced Dataset")
varImpPlot(RFBalanced, main = "Random Forest Model - Balanced Dataset")
####Step 5.5:- Artifical Neural Network Model####
library(factoextra)
##Normalizing the numerical variables for the dataset##
ANNData <- Insurance
ANNData$Age <- (ANNData$Age-min(ANNData$Age))/(max(ANNData$Age)-min(ANNData$Age))
ANNData$Commision <- (ANNData$Commision-min(ANNData$Commision))/(max(ANNData$Commision)-min(ANNData$Commision))
ANNData$Duration <- (ANNData$Duration-min(ANNData$Duration))/(max(ANNData$Duration)-min(ANNData$Duration))
ANNData$Sales <- (ANNData$Sales-min(ANNData$Sales))/(max(ANNData$Sales)-min(ANNData$Sales))
#Checking the structure of the data again#
str(ANNData)
##Converting factor data into dummy variables##
Type_Dum <- model.matrix(~Type - 1,data = ANNData)
Agency_Code_Dum <- model.matrix(~Agency_Code - 1,data = ANNData)
Channel_Dum <- model.matrix(~Channel - 1,data = ANNData)
Product_Name_Dum <- model.matrix(~Product.Name - 1,data = ANNData)
Destination_Dum <- model.matrix(~Destination - 1,data = ANNData)
ANNDataUpd <- data.frame(ANNData[c(-2,-3,-6,-9,-10)],Type_Dum,Agency_Code_Dum,Channel_Dum,Product_Name_Dum,Destination_Dum)
#Checking the structure of the data again#
str(ANNDataUpd)
##Partitioning the dataset into training and testing sets##
set.seed(222)
ANN_Sample <- sample(2,nrow(ANNDataUpd),prob=c(0.7,0.3),replace=T)
ANN_Train <- ANNDataUpd[ANN_Sample==1,]
ANN_Test <- ANNDataUpd[ANN_Sample==2,]
table(ANN_Train$Claimed)
table(ANN_Test$Claimed)
ANN_Train_Balanced <- ovun.sample(Claimed~., data = ANN_Train, method = "over", N = 2874)$data
ANN_Test_Balanced <- ovun.sample(Claimed~., data = ANN_Test, method = "over", N = 1278)$data
##Creating the Artificial Neural Network Model##
ANNModelUnbalanced <- neuralnet(Claimed~., data = ANN_Train,hidden = 1,err.fct = "ce",linear.output = F)
ANNModelBalanced <- neuralnet(Claimed~., data = ANN_Train_Balanced,hidden = 1,err.fct = "ce",linear.output = F)
plot(ANNModelUnbalanced)
plot(ANNModelBalanced)
##Considering only the numerical IVS for the model##
AMUN <- neuralnet(Claimed~Age+Commision+Duration+Sales, data = ANN_Train,hidden = 1,err.fct = "ce",linear.output = F)
plot(AMUN)
AMBN <- neuralnet(Claimed~Age+Commision+Duration+Sales, data = ANN_Train_Balanced)
plot(AMBN)
####Step 5.6:- Models Performance Analysis####
####Step 5.6.1 :- For CART Model##
##Calculating the confusion matrix for the Train Dataset##
Train$CMUnbPred <- predict(CMUnbalanced,Train,type = "class")
Train_Bal$CMBalPred <- predict(CMBalanced,Train_Bal,type = "class")
confusionMatrix(table(Train$CMUnbPred, Train$Claimed))
confusionMatrix(table(Train_Bal$CMBalPred, Train_Bal$Claimed))
#Calculating the confusion matrix for the Test Dataset#
Test$CMUnbPred <- predict(CMUnbalanced,Test,type = "class")
Test_Bal$CMBalPred <- predict(CMBalanced,Test_Bal,type = "class")
confusionMatrix(table(Test$CMUnbPred, Test$Claimed))
confusionMatrix(table(Test_Bal$CMBalPred, Test_Bal$Claimed))
##Step 5.6.2 :-For RF Model##
##Calculating the confusion matrix for the Train Dataset##
Train$RFUnbPred <- predict(RFUnbalanced,newdata = Train[,-11],type="class")
Train_Bal$RFBalPred <- predict(RFBalanced,newdata = Train_Bal[,-11],type="class")
confusionMatrix(table(Train$RFUnbPred,Train$Claimed))
confusionMatrix(table(Train_Bal$RFBalPred,Train_Bal$Claimed))
##Calculating the confusion matrix for the Test Dataset##
Test$RFUnbPred <- predict(RFUnbalanced,newdata = Test[,-11],type="class")
Test_Bal$RFBalPred <- predict(RFBalanced,newdata = Test_Bal[,-11],type="class")
confusionMatrix(table(Test$RFUnbPred,Test$Claimed))
confusionMatrix(table(Test_Bal$RFBalPred,Test_Bal$Claimed))
##Step 5.6.2 :-For ANN Model##
##Calculating the confusion matrix for the Train Dataset##
ANN_Train$ANNUPred <- ((compute(ANNModelUnbalanced,ANN_Train[-2]))$net.result[,2])
ANN_Train_Balanced$ANNBPred <- ((compute(ANNModelBalanced,ANN_Train_Balanced[-2]))$net.result[,2])
ANN_Train$ANNUPred <- ifelse(ANN_Train$ANNUPred>0.5,"Yes","No")
ANN_Train_Balanced$ANNBPred <- ifelse(ANN_Train_Balanced$ANNBPred>0.5,"Yes","No")
confusionMatrix(table(ANN_Train$Claimed, ANN_Train$ANNUPred))
confusionMatrix(table(ANN_Train_Balanced$ANNBPred,ANN_Train_Balanced$Claimed))
##Calculating the confusion matrix for the Test Dataset##
ANN_Test$ANNUPred <- ((compute(ANNModelUnbalanced,ANN_Test[-2]))$net.result[,2])
ANN_Test_Balanced$ANNBPred <- ((compute(ANNModelBalanced,ANN_Test_Balanced[-2]))$net.result[,2])
ANN_Test$ANNUPred <- ifelse(ANN_Test$ANNUPred>0.5,"Yes","No")
ANN_Test_Balanced$ANNBPred <- ifelse(ANN_Test_Balanced$ANNBPred>0.5,"Yes","No")
confusionMatrix(table(ANN_Test$Claimed, ANN_Test$ANNUPred))
confusionMatrix(table(ANN_Test_Balanced$ANNBPred,ANN_Test_Balanced$Claimed))

##########################END OF THE CODE#################################
########################END OF THE ANALYSIS###############################

##########################################################################
