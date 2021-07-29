####Group Assignment for Advance Statistics - Group 10####
####SUBMITTED BY :- KUNAL MALIK/SHITAL GUPTA/SHAURYA RAJOUR####
####PRODUCT EXPERIENCE STUDY FOR HAIR PRODUCTS####
####Data Analysis through R Programming####
################################################
####Start Of The Code####
####Step 1 :- Setting the Current Working Directory####
getwd()
setwd("C:/Users/shital/Documents/R/RProgramming")
####Step 2 :- Reading and viewing the data from the dataset file####
HairProductDetails <- read.csv("Factor-Hair-Revised.csv", header = TRUE)
head(HairProductDetails)
####Step 3 :-Start of Exploratory Data Analysis ####
####Step 3.1:- Interpreting the data ####
####Step 3.1.1 :- Checking the dimensions of the dataset####
dim(HairProductDetails)
####Step 3.1.2 :-Summarizing the data and doing a structure check####
str(HairProductDetails)
summary(HairProductDetails)
library(psych)
describe(HairProductDetails)
####Step 3.1.3 :-Changing the variable names excluding ID column and storing data in a new variable ####
NewColNames <- c("Product Quality", "E-Commerce", "Technical Support" ,"Complaint Resolution" ,"Advertising" , "Product Line" , "Salesforce Image", "Competitive Pricing" , "Warranty & Claims" , "Order & Billing" , "Delivery Speed" , "Customer Satisfaction")
HPD_Upd <- HairProductDetails[,-1]
colnames(HPD_Upd) <- NewColNames
####Step 3.2 Processing the data####
####Step 3.2.1 :- To filter hair products based on highest/lowest customer satisfaction score####
library(dplyr)
filter(HairProductDetails, HairProductDetails$Satisfaction == max(HairProductDetails$Satisfaction))
filter(HairProductDetails, HairProductDetails$Satisfaction == min(HairProductDetails$Satisfaction))
####Step 3.2.2 :- Checking & Visualizing Skewness for variable customer satisfaction####
library(PerformanceAnalytics)
DVSkewnessCheck <- skewness(HPD_Upd$`Customer Satisfaction`)
DVSkewnessCheck
library(ggplot2)
Median <- median(HPD_Upd$'Customer Satisfaction')
Mean <- mean(HPD_Upd$'Customer Satisfaction')
Skewness_plot <- HPD_Upd %>% ggplot(aes(x = HPD_Upd$'Customer Satisfaction')) + stat_density(geom = "line", alpha = 1, colour = "cornflowerblue")
Skewness_plot
Plot_Shaded_Area <- ggplot_build(Skewness_plot)$data[[1]] %>% filter(x < Mean)
Skewness_plot_shaded <-  Skewness_plot + geom_area(data = Plot_Shaded_Area, aes(x = x, y = y), fill="pink", alpha = 0.5)
Skewness_plot_shaded
Median_Line_Info <- ggplot_build(Skewness_plot)$data[[1]] %>% filter(x <= Median)
Skewness_plot_ShadedWithLabels <- Skewness_plot_shaded + geom_segment(aes(x = 6.9, y = 0.1, xend = 5.0, yend = 0.1),arrow = arrow(length = unit(1.0, "cm")), size = 0.05) + annotate(geom = "text", x = 6, y = 0.1, label = "Customer Satisfaction <= Mean", fontface = "plain", alpha = .8, vjust =  -1) + geom_segment(data = Plot_Shaded_Area, aes(x = Mean, y = 0, xend = Mean, yend = density), color = "red", linetype = "dotted") + annotate(geom = "text", x = Mean, y = 0.13, label = "Mean", color = "red", fontface = "plain", angle = 90, alpha = .8, vjust =  -1.75) + geom_segment(data = Median_Line_Info, aes(x = Median, y = 0, xend = Median, yend = density), color = "black", linetype = "dotted") + annotate(geom = "text", x = Median, y = 0.13, label = "Median", fontface = "plain", angle = 90, alpha = .8, vjust =  1.75) + ggtitle("Density Plot Illustrating Skewness For Customer Satisfaction")
print(Skewness_plot_ShadedWithLabels + labs( x = "Customer Satisfaction", y = "Density"))
####Step 3.2.3 :- Missing Value Treatment#### 
sum(is.na(HPD_Upd))
####Step 3.2.4 :- Outlier Treatment####
####Boxplot for Dependent Variable####
par(font.axis = 4, font.main = 4, font.lab = 4, pin = c(5,3))
boxplot(HPD_Upd$`Customer Satisfaction`, horizontal = T, xlab = "Customer Satisfaction", ylim=c(2,11), col = c("cornflowerblue"), main = "Boxplot for Customer Satisfaction", cex.main = 1.0)
text(x = round(fivenum(HPD_Upd$`Customer Satisfaction`), digits = 1), labels = round(fivenum(HPD_Upd$`Customer Satisfaction`), digits = 1), y = 1.25, font = 2)
box(lwd = 3)
####Boxplot for Independent Variables For Comparisons####
par(font.axis = 3, font.main = 4)
boxplot(HPD_Upd[,-12], las = 0, col = c("Green", "Red", "Pink","Blue", "Orange","Purple","Grey"), names = NewColNames[-12], main = "Boxplot for all Product Variables", cex.main = 0.9, cex.axis = 0.65)
box(lwd = 3)
####Boxplot for Outlier Variables For Comparisons####
library(RColorBrewer)
par(font.axis = 2, font.main = 4)
boxplot(HPD_Upd$`E-Commerce`,HPD_Upd$`Salesforce Image`,HPD_Upd$`Order & Billing`,HPD_Upd$`Delivery Speed`, las = 0, col = brewer.pal(4,"Reds"), names = c(NewColNames[2],NewColNames[7],NewColNames[10],NewColNames[11]), main = "Boxplot for Variables with Outliers", cex.main = 0.9, cex.axis = 0.8)
box(lwd = 3)
####Extracting the Outliers####
list("OutlierValues")
OutlierValues <- HPD_Upd[(1:12),]
for (Values in c(1:12)) {
  
  Box_Plot <- boxplot(HPD_Upd[,Values],plot = F)$out
  OutlierValues[,Values] <- NA
  if (length(Box_Plot)>0) {
    OutlierValues[(1:length(Box_Plot)),Values] <- Box_Plot 
  }
}
OutlierValues <- OutlierValues[(1:6),]
OutlierValues[,c(2,7,10,11)]
####Step 3.3 :- Visualizing the Data####
####Step 3.3.1 :- Univariate Analysis through Histogram####
par(mfrow = c(3,4), font.main = 2, font.axis = 2, font.lab = 2) #Convert our plotting space in 12 different frames#
for (ColNumber in (1:12)) {
h = round(max(HPD_Upd[,ColNumber]),1)+1
l = round(min(HPD_Upd[,ColNumber]),1)-1
n = NewColNames[ColNumber]
hist (HPD_Upd[,ColNumber], breaks = seq(l,h,((h-l)/6)), labels = T, include.lowest=T, right=T, col= brewer.pal(6,"Blues") , border=1, main = NULL , xlab= n, ylab=NULL, cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1, xlim = c(0,12), ylim = c(0,70))
}
par(font.axis = 2, font.lab = 2)
hist(HPD_Upd$`Customer Satisfaction`, labels = T, col = brewer.pal(6, "Reds"), main = "Variation for Customer Satisfaction", xlab = "Customer Satisfaction", cex.main = 1.0 )
####Step 3.3.2 :- Bi-Variate Analysis through Scatter Plot####
par(mfrow = c(4,3), font.main = 2, font.axis = 2, font.lab = 2)
for (ColPos in c(1:11)){
plot(HPD_Upd[,ColPos],HPD_Upd$`Customer Satisfaction`, xlab = NewColNames[ColPos], ylab = "Customer Satisfaction", col = "Blue",cex.lab=1, cex.axis=1, cex = 1.5, xlim = c(0,10),ylim = c(0,10), pch = 3)
abline(lm(formula = HPD_Upd$`Customer Satisfaction` ~ HPD_Upd[,ColPos]),col = "Red")
}
####Starting here we will perform analysis based on whole data and just the independent variables####
####Step 4 :- MultiCollinearity Analysis####
####Step 4.1 :- Correlation Analysis####
####Step 4.1.1 :- Correlation Matrix & Plot####
library(corrplot)
CorMtrx <- cor(HairProductDetails[,-1])
round(CorMtrx,2)
corrplot(round(cor(HPD_Upd),3),method = "number", tl.cex = .9)
corrplot(cor(HPD_Upd),method = "pie", tl.col = "Black", tl.cex = .8)
####Step 4.1.2 :- Verifying the existence of multicollinearity####
library(ppcor)
pcor <- pcor(HairProductDetails[,-1], method = "pearson")
round(pcor$estimate,3)
round(pcor$p.value ,3)
round(pcor$statistic ,3)
pcor$n
pcor$gp
pcor$method
library(car)
VIFModel <- lm(`Customer Satisfaction` ~., data = HPD_Upd)
VIFCheck <- vif(VIFModel)
VIFCheck
####Step 4.1.2 :- Plotting the multicollinearity####
plot(HPD_Upd)
library(GGally)
####Only for better output fit- All variables excluding ID without modified colnames####
HPD_Upd2 <- HairProductDetails[,-1] 
####Multicollinearity Plot####
MulticollinearityPlot <- ggpairs(HPD_Upd2, columnLabels = c("PD","EC","TS","CR","A","PL","SFI","CP","WC","OB","DS","CS"))
MulticollinearityPlot
####Step 5 :- Regression Analysis####
####Step 5.1 :-Simple Linear Regression with Each Variable####
PQModel <-  lm(formula = HPD_Upd$`Customer Satisfaction` ~ HPD_Upd$`Product Quality`, data = HPD_Upd)
ECModel <-  lm(formula = HPD_Upd$`Customer Satisfaction` ~ HPD_Upd$`E-Commerce`, data = HPD_Upd)
TSModel <-  lm(formula = HPD_Upd$`Customer Satisfaction` ~ HPD_Upd$`Technical Support`, data = HPD_Upd)
CRModel <-  lm(formula = HPD_Upd$`Customer Satisfaction` ~ HPD_Upd$`Complaint Resolution`, data = HPD_Upd)
AModel <-  lm(formula = HPD_Upd$`Customer Satisfaction` ~ HPD_Upd$`Advertising`, data = HPD_Upd)
PLModel <-  lm(formula = HPD_Upd$`Customer Satisfaction` ~ HPD_Upd$`Product Line`, data = HPD_Upd)
SFIModel <-  lm(formula = HPD_Upd$`Customer Satisfaction` ~ HPD_Upd$`Salesforce Image`, data = HPD_Upd)
CPModel <-  lm(formula = HPD_Upd$`Customer Satisfaction` ~ HPD_Upd$`Competitive Pricing`, data = HPD_Upd)
WCModel <-  lm(formula = HPD_Upd$`Customer Satisfaction` ~ HPD_Upd$`Warranty & Claims`, data = HPD_Upd)
OBModel <-  lm(formula = HPD_Upd$`Customer Satisfaction` ~ HPD_Upd$`Order & Billing`, data = HPD_Upd)
DSModel <-  lm(formula = HPD_Upd$`Customer Satisfaction` ~ HPD_Upd$`Delivery Speed`, data = HPD_Upd)
summary(PQModel)
summary(ECModel)
summary(TSModel)
summary(CRModel)
summary(AModel)
summary(PLModel)
summary(SFIModel)
summary(CPModel)
summary(WCModel)
summary(OBModel)
summary(DSModel)
####Step 5.2 :-Multiple Linear Regression for all Variables####
summary(VIFModel)
par(mfrow = c(2,2), font.axis = 2, font.lab = 2, font.main = 2, font.sub = 2)
plot(VIFModel, cex.main = .8, cex.sub = .8,cex.lab = .9, cex.axis = .9)
####Step 6 :- PCA/ Factor Analysis####
####Step 6.1 :- Data Adequacy Check###
####Step 6.1.2 :- Cortest Bartlett of sphericity####
CB <- cortest.bartlett(CorMtrx, 100) ####For Whole dataset- Independent & Dependent Variable####
CB
round(CB$p.value, 3)
CorMtrx2 <- cor(HairProductDetails[, c(-1,-13)])####For Independent Variables Only####
CB2 <- cortest.bartlett(CorMtrx2, 100)
CB2
round(CB2$p.value, 3)
####Step 6.1.2 :- KMO Rule for Sampling Adequacy####
library(factoextra)
KMO(CorMtrx)
KMO(CorMtrx2)
####Step 6.2 :- Principal Component Analysis####
####Step 6.2.1 :- Generating the covariance matrix####
cov(HPD_Upd)####For Whole dataset- IV & DV####
cov(HPD_Upd2[,-12])####For IV Only####
####Step 6.2.2:- Running the Principal Component Analysis ###
PCA = princomp(HPD_Upd, cor=TRUE, scores = TRUE)####For Whole dataset- IV & DV####
####To check for Cumulative Percentage of Variance (80%) Rule####
summary(PCA)
Load <- loadings(PCA)
Load
PCA2 = princomp(HPD_Upd[,-12], cor=TRUE, scores = TRUE)####For IV Only####
summary(PCA2)
Load2 <- loadings(PCA2)
Load2
####Step 6.2.3 :- Eigen Values & Eigen Vector Generation####
#### To check for Kaiser Guttman Normalization rule####
Eigen_Vector <- eigen(CorMtrx)####For Whole dataset- IV & DV####
Eigen_Values <- Eigen_Vector$values
Eigen_Vector
Eigen_Values
Eigen_Vector2 <- eigen(CorMtrx2)####For IV Only####
Eigen_Values2 <- Eigen_Vector2$values
Eigen_Vector2
Eigen_Values2
####Plotting the Eigen Values to check for Bend Elbow Rule####
####Default Plot####
fviz_eig(PCA)
fviz_eig(PCA2)
####Eigen Values Plot####
fviz_eig(PCA, choice = "eigenvalue", addlabels=TRUE)
fviz_eig(PCA2, choice = "eigenvalue", addlabels=TRUE)
####Step 7 :- Model Optimality Validation####
####Step 7.1 :- Factor Optimality Check####
####Step 7.1.1 :- Factanal Analysis With & Without Rotation####
####For Whole dataset- IV & DV####
FactorCheckWithoutRotation2 <- factanal(HPD_Upd, factors = 4, scores = "regression", rotation = "none")
FactorCheckWithoutRotation2
FactorCheckWithRotation2 <- factanal(HPD_Upd, factors = 4, scores = "regression", rotation = "varimax")
FactorCheckWithRotation2
#### For IV Only####
FactorCheckWithoutRotation <- factanal(HPD_Upd[,-12], factors = 4, scores = "regression", rotation = "none")
FactorCheckWithoutRotation
FactorCheckWithRotation <- factanal(HPD_Upd[,-12], factors = 4, scores = "regression", rotation = "varimax")
FactorCheckWithRotation
####Optimality Check####
####For Whole dataset- IV & DV####
factanal(HPD_Upd, factors = 5, scores = "regression") ###Best Fit####
factanal(HPD_Upd, factors = 6, scores = "regression")
factanal(HPD_Upd, factors = 7, scores = "regression")
####For IV Only####
factanal(HPD_Upd[,-12], factors = 5, scores = "regression") ####Best Fit Statistically####
factanal(HPD_Upd[,-12], factors = 6, scores = "regression")
####Step 7.1.2 :-Fa Method Statistics####
####For Whole dataset- IV & DV####
WDFAM <- fa(r = HPD_Upd, nfactors = 4, rotate = "varimax", fm ="pa")
WDFAM2 <- fa(r = HPD_Upd, nfactors = 5, rotate = "varimax", fm ="pa")
####Business Problem - With & Without Rotation - 4 factor model####
FAMR <-  fa(r = HPD_Upd[,-12], nfactors = 4, rotate ="none", fm ="pa")
FAMWR <- fa(r = HPD_Upd[,-12], nfactors = 4, rotate ="varimax", fm ="pa")
FAMR
FAMWR
####Business Problem -With & Without Rotation - 5 factor model####
FAMR2 <-  fa(r = HPD_Upd[,-12], nfactors = 5, rotate ="none", fm ="pa")
FAMWR2 <- fa(r = HPD_Upd[,-12], nfactors = 5, rotate ="varimax", fm ="pa")
FAMR2
FAMWR2
####Fa Method Visualization####
####For Whole dataset- IV & DV####
fa.diagram(WDFAM)
fa.diagram(WDFAM2)
####For Business Problem####
fa.diagram(FAMR)
fa.diagram(FAMWR)
fa.diagram(FAMR2)
fa.diagram(FAMWR2)
####Fa Method Factor Plot####
par(font.lab = 2, font.axis = 2, font.sub = 2)
plot(WDFAM2$loadings, col = "Blue", main = "Factor Plot", pch = 16,cex.axis = .7, cex.lab = .65)
text(WDFAM2$loadings,labels = names(HPD_Upd), cex = .69)
plot(FAMWR$loadings, col = "Blue", main = "Factor Plot", pch = 16,cex.axis = .7, cex.lab = .65)
text(FAMWR$loadings,labels = names(HPD_Upd[,-12]),  cex = .69)
####Step 7.2 :- Model Optimality Check####
####Step 7.2.1 :- Renaming the factors and storing them in a variable####
FactorsMatrixValues <- cbind(HPD_Upd[,12],FAMWR$scores)
head(FactorsMatrixValues)
colnames(FactorsMatrixValues) <- c("CustomerSatisfaction", "ProductDistributionExperience", "MarketingResponse","AfterSalesSupport","ValueForMoney")
head(FactorsMatrixValues)
####Step 7.2.2 :- Converting the matrix to data frame####
class(FactorsMatrixValues)
FactorsMatrixValues <- as.data.frame(FactorsMatrixValues)
####Step 7.2.3 :- Generating the Correlation Matrix####
corrplot(cor(FactorsMatrixValues), method = "pie", tl.col = "Black", tl.cex = .8)
####Step 7.2.4 :- Creating testing and training datasets to test the model####
library(caTools)
set.seed(100) ####Random Number Generation####
Splitter <- sample.split(FactorsMatrixValues$CustomerSatisfaction, SplitRatio = 0.8)
TrainDataset <-  subset(FactorsMatrixValues, Splitter==T)
TestDataset <-  subset(FactorsMatrixValues, Splitter==F)
####Checking the dimensions of Train and Test Dataset####
cat(" Train Dimention: ", dim(TrainDataset) ,"\n", "Test Dimention : ", dim(TestDataset))
####Step 7.2.5 :- Creating the linear model through train dataset####
FinalLinearModel = lm(CustomerSatisfaction~., data = TrainDataset)
summary(FinalLinearModel)
vif(FinalLinearModel)
####Step 7.2.6 :- Goodness of Fit Test for the model through test dataset####
Predictions = predict(FinalLinearModel, newdata = TestDataset)
Predictions
####Compute R-square from the test dataset####
SST = sum((TestDataset$CustomerSatisfaction - mean(TrainDataset$CustomerSatisfaction))^2)
SSE = sum((Predictions - TestDataset$CustomerSatisfaction)^2)
SSR = sum((Predictions - mean(TrainDataset$CustomerSatisfaction))^2)
R_Square_Test <- SSR/SST
cat(" SST :", SST, "\n", "SSE :", SSE, "\n", "SSR :", SSR, "\n", "R squared Test :" , R_Square_Test)
Adjusted_R_Square_Test <- 1 - (SSE/SST)
Adjusted_R_Square_Test
RMSE <- sqrt(R_Square_Test) #Root Mean Squared Error#
RMSE
################################################
####End of the Code####
####End of the Analysis####