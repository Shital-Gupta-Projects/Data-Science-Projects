###########################################################################################################################
########################### INDIVIDUAL ASSIGNMENT - CAFE TROPICAL SALES ANALYSIS ##########################################
################################### SUBMITTED BY :- SHITAL GUPTA###########################################################
############################### DATA ANALYSIS THROUGH R PROGRAMMING #######################################################
###########################################################################################################################
######################################## Start of the Analysis ############################################################
########################################## Start Of The Code ##############################################################
#------------------------------------------------------------------------------------------#
####              Step 1 :- Setting the Current Working Directory                       ####
#------------------------------------------------------------------------------------------#
getwd()
#-This should be as per your own local system R directory-#
setwd("C:/Users/shital/Documents/R/RProgramming") 
dir()
#------------------------------------------------------------------------------------------#
####          Step 2 :- Reading and viewing the data from both the dataset              ####
#------------------------------------------------------------------------------------------#
CSDA <- read.csv(file.choose())
CSDAO <- CSDA
View(CSDA)
#------------------------------------------------------------------------------------------#
####                    Step 3 :- Exploratory Data Analysis                             ####
#------------------------------------------------------------------------------------------#
library(DataExplorer)
library(psych)
library(pastecs)
library(caret)
library(anytime)
library(hms)

dim(CSDA)
head(CSDA,n=10) #Printing first 10 observations#
str(CSDA)
class(CSDA)
plot_intro(CSDA) #General Analysis for data structure and missing observations#
###Fixing the data types for the variables Date and Time###
CSDA_Mod <- CSDA
CSDA_Mod$Date=anydate(CSDA_Mod$Date)
CSDA_Mod$Time=as.character(CSDA_Mod$Time)
CSDA_Mod$Time=as_hms(CSDA_Mod$Time)
str(CSDA_Mod)
class(CSDA_Mod)
View(CSDA_Mod)
####Geting the Data Summary###
summary(CSDA_Mod)
###Getting Descriptive Statistics for the numerical variables#
options(scipen = 100)
options(digits = 2)
#All variables shows positive skewness & kurtosis#
psych::describe(select_if(CSDA_Mod, is.numeric))
#Highest standard error shows in Total and Highest Variance comes in Rate#
stat.desc(select_if(CSDA_Mod, is.numeric), basic = F)
#Discount shows near zero total variance#
nearZeroVar(select_if(CSDA_Mod, is.numeric), saveMetrics = TRUE) 
#-The above statistics are only for analysis, since our main focus is to study the data and give patterns   -#
#-and market basket analysis hence we need real data to understand the same so no modifications will be done-#
#-We will also be not doing outlier treatment since we need these raw sales values for understanding the    -#
#-dominating product items and categories.                                                                  -# 
#-REST OF THE ANALYSIS IS DONE IN EXCEL INSTEAD OF HERE IN R, WE WILL ONLY BE DOING APRIORI/ASSOCIATION RULES HERE-#
#------------------------------------------------------------------------------------------#
####    Step 4 :- Market Basket Analysis through Association Rules/Apriori Algorithm    ####
#------------------------------------------------------------------------------------------#
library(plyr)
library(arules)
library(arulesViz)
library(RColorBrewer)
##We will not be using time and numerical variables for analysis here for the algorithm hence removing the same##
CSDAFinal <- CSDA_Mod[,-c(4:9)]
str(CSDAFinal)
View(CSDAFinal)
##WE WILL BE USING THE TIME & NUMERICAL VARIABLES FOR OVERALL ANALYSIS IN THE FINAL PRESENTATION##
###Converting the data into transaction data for association rules analysis###
CSDATD <- CSDAFinal
##Triming the white spaces in the observations from both sides(left,right)##
CSDATD$Item.Desc <- trimws(CSDATD$Item.Desc, which = "both")
CSDATD$Category <- trimws(CSDATD$Category, which = "both")
##Merging the transactions together based on Bill Number and Date##
CSDATemp1 <- ddply(CSDATD,c("Bill.Number","Date"),function(data)paste(data$Item.Desc,collapse=","))
CSDATemp2 <- ddply(CSDATD,c("Bill.Number","Date"),function(data)paste(data$Category,collapse=","))
##Gathering the merged data together##
CSDAFA <- data.frame(CSDATemp1,"Category" = CSDATemp2$V1)
str(CSDAFA)
class(CSDAFA)
names(CSDAFA)[names(CSDAFA) == "V1"] <- "Items.Desc"
##Creating individual data frames for transaction data analysis##
ProdItemAnalysis <- CSDAFA[,3]
FoodCategAnalysis <- CSDAFA[,4]
##Verifying if the transaction data was correctly written##
write.csv(ProdItemAnalysis,"./PIA.csv", quote = FALSE, row.names = FALSE)
write.csv(FoodCategAnalysis,"./FCA.csv", quote = FALSE, row.names = FALSE)
##Converting the data into data type transactions for further analysis##
PIA <- read.transactions('./PIA.csv', format = 'basket', sep=',')
FCA <- read.transactions('./FCA.csv', format = 'basket', sep=',')
###Viewing and summarizing the converted transaction data for analysis###
summary(PIA)
summary(FCA)
View(PIA)
View(FCA)
###Analyzing the most frequent occuring product items and categories in the data###
##PRODUCT ITEMS##
itemFrequency(PIA, type = "relative")
itemFrequencyPlot(PIA,topN = 10,type = "relative",col=brewer.pal(8,'Pastel2'), main="Relative Item Frequency Plot")
itemFrequency(PIA, type = "absolute")
itemFrequencyPlot(PIA,topN = 10,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
##PRODUCT CATEGORY##
itemFrequency(FCA, type = "relative")
itemFrequencyPlot(FCA,topN = 10,type = "relative",col=brewer.pal(8,'Pastel2'), main="Relative Item Frequency Plot")
itemFrequency(FCA, type = "absolute")
itemFrequencyPlot(FCA,topN = 10,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
#-This analysis gives us an idea as to what we have to make our combos with and what product items we can use to make the combo meals-#
###Aggregating the data and creating the apriori rules###
##PRODUCT ITEMS##
PIAR1 = apriori(PIA, parameter=list(support=0.001, confidence=0.1))
PIAR2 = apriori(PIA, parameter=list(support=0.005, confidence=0.8, minlen = 3))
PIAR3 = apriori(PIA, parameter=list(support=0.005, confidence=0.8, maxlen = 4))
#-Upon Checking we saw no rules created for PIAR2 & PIAR3 hence we will be ignoring them for next part of analysis-#
##PRODUCT CATEGORY##
FCAR1 = apriori(FCA, parameter=list(support=0.001, confidence=0.1))
FCAR2 = apriori(FCA, parameter=list(support=0.005, confidence=0.8, minlen = 3))
FCAR3 = apriori(FCA, parameter=list(support=0.005, confidence=0.8, maxlen = 4))
###Writing the rules created earlier into csv files###
write.csv(as(PIAR1,"data.frame"),file = "./PIAR1.csv",row.names = FALSE)
write.csv(as(FCAR1,"data.frame"),file = "./FCAR1.csv",row.names = FALSE)
write.csv(as(FCAR2,"data.frame"),file = "./FCAR2.csv",row.names = FALSE)
write.csv(as(FCAR3,"data.frame"),file = "./FCAR3.csv",row.names = FALSE)
###Inspecting the first rule created for both PIA & FCA###
##Inspection##
inspect( subset( PIAR1, subset = rhs %pin% "Product H" ))
inspect(PIAR1)
inspect( subset( FCAR1, subset = rhs %pin% "Product H" )) #-FCAR2 & FCAR3 had only one rule hence we will not use them here-#
inspect(FCAR1)
##Top 20 Rules Created##
options(digits=2)
inspect(PIAR1[1:20])
view(inspect(PIAR1[1:20]))
inspect(FCAR1[1:20])
view(inspect(FCAR1[1:20]))
#-In the presentation we will be showing the most repeating combo category        -#
#-Since the category combo analysis will cover the product items that we propose  -#
#-to sell hence we will only show Product Category combos and estimated sales from-#
#-proposed combos generated through market basket analysis.                       -#
##Rules Summary##
summary(PIAR1)
summary(FCAR1)
##Removing unnecessary rules,Sorting Rules by Confidence and visualizing them##
#PRODUCT ITEMS#
PIARSorSub <- which(colSums(is.subset(PIAR1, PIAR1)) > 1)
PIARSorPru <- PIAR1[-PIARSorSub]
inspect(sort(PIARSorPru,by="confidence", decreasing=TRUE))
plot(PIARSorPru,method="graph")
plot(PIARSorPru, method = "graph", shading = NA)
#PRODUCT CATEGORY#
FCARSorSub <- which(colSums(is.subset(FCAR1, FCAR1)) > 1)
FCARSorPru <- FCAR1[-FCARSorSub]
inspect(sort(FCARSorPru,by="confidence", decreasing=TRUE))
plot(FCARSorPru,method="graph")
plot(FCARSorPru, method = "graph", shading = NA)
#------------------------------------------------------------------------------------------#
####                           Step 5 :- End of Study                                  #####
#------------------------------------------------------------------------------------------#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#-CONCLUSION: From the above analysis and rules created, you can clearly observe:         -#
#-FOOD+BEVERAGE, TOBACCO+FOOD and TOBACCO+BEVERAGE combos to be the most common/dominating-#
#-customer selections. We will be doing an estimated sales analysis in the final          -#
#-presentation based on the observations for overall analysis.The other created combos    -#
#-were only removed due to lesser sales from them when assessed individually, they can    -#
#-still be tested for but at the moment the given combinations are the most favorable     -#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
########################################### End Of The Code ###############################################################
#   Note:                                                                                                                 #
#   #### starts a new section, ### a new sub-section, ## a new sub-section topic, #- a new comment and # Random Headings  #
#      The starting symbol will match with a respective ending symbol to mark the ending of the topic.                    #
#      Each section will mention the packages to be used in the start for quick reference.                                #
#                                                                                                                         #
######################################## End of the Analysis ##############################################################
