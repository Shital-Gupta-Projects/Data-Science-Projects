######################## GROUP ASSIGNMENT FOR FINANCE AND RISK ANALYTICS #########################
############################# SUBMITTED BY: GROUP 10 - SHITAL GUPTA ##############################
############ R PROJECT: DATA ANALYSIS FOR BLUE CHIP STOCK PORTFOLIO OPTIMIZATION #################
##################################################################################################
#################################### Start of the Analysis #######################################
##################################### Start of the Code ##########################################

####Step 1: - Setting the Current Working Directory####
getwd()
setwd("C:/Users/shital/Documents/R/RProgramming")
dir()

####Step 2: - Capturing the stocks data from BSE/Yahoo and Viewing/Validating the data collected####
library(quantmod)
library(xts)
#-Capturing data from 22nd March 2019 to 22nd March 2021-#
start <- as.POSIXct("2019-03-22")
end <- as.POSIXct("2021-03-24")
getSymbols(Symbols = "HINDUNILVR.BO",src = "yahoo", from = start, to = end)
getSymbols(Symbols = "RELIANCE.BO",src = "yahoo", from = start, to = end)
getSymbols(Symbols = "TCS.BO",src = "yahoo", from = start, to = end)
getSymbols(Symbols = "HDFC.BO",src = "yahoo", from = start, to = end)
getSymbols(Symbols = "INFY.BO",src = "yahoo", from = start, to = end)
getSymbols(Symbols = "BHARTIARTL.BO",src = "yahoo", from = start, to = end)
getSymbols(Symbols = "ITC.BO",src = "yahoo", from = start, to = end)
getSymbols(Symbols = "ASIANPAINT.BO",src = "yahoo", from = start, to = end)
getSymbols(Symbols = "NESTLEIND.BO",src = "yahoo", from = start, to = end)
getSymbols(Symbols = "HCLTECH.BO",src = "yahoo", from = start, to = end)
#-We will only be studying closing prices for the stocks hence combining the same in a dataframe-#
#-from the captured data.                                                                        -#
BCShareCPData <- as.xts(data.frame(HINDUNILVR.BO$HINDUNILVR.BO.Close,RELIANCE.BO$RELIANCE.BO.Close,
                                 TCS.BO$TCS.BO.Close ,HDFC.BO$HDFC.BO.Close, 
                                 INFY.BO$INFY.BO.Close,BHARTIARTL.BO$BHARTIARTL.BO.Close, 
                                 ITC.BO$ITC.BO.Close, ASIANPAINT.BO$ASIANPAINT.BO.Close,
                                 NESTLEIND.BO$NESTLEIND.BO.Close, HCLTECH.BO$HCLTECH.BO.Close))
#-Changing the column names to stock company abbreviations, given in the report-#
colnames(BCShareCPData) <- c("HUL","RIL","TCS","HDFC","INFOSYS","BAL","ITCL","APL","NIL","HCL")
#-Verifying if the data frame was created correctly-#
class(BCShareCPData) #eXtensible Time Series Object#
ts_info(BCShareCPData)
#-Viewing the data gathered for final validation-#
View(BCShareCPData)
#-Storing the original data in a seperate variable for reference-#
BCShareCPDataO <- BCShareCPData

####Step 3 :- Exploratory Data Analysis####

library(TSstudio)
library(psych)
library(forecast)
library(descr)
library(pastecs)
library(ggplot2)
library(GGally)
library(caret)

###Step 3.1 :- Data Interpretation###
#-Understanding the structure of the data-#
str(BCShareCPData) 
ts_info(BCShareCPData)
#-Viewing the first and last six lines of the data to understand data type-#
head(BCShareCPData)
tail(BCShareCPData) #Numerical values are provided in the data#

###Step 3.2 :- Data Processing###

##Step 3.2.1 :- MISSING VALUE ANALYSIS##
#-Checking if we have an missing values in the data-#
any(is.na(BCShareCPData))
colSums(is.na(BCShareCPData))
#-As seen above, two rows i.e.,for dates 27th Oct 2019 and 14th Nov 2020 showed missing-#
#-values for all stocks.Hence, manually checked for the stocks on BSE site and filled  -#
#-in the values missing from the data.                                                 -#
BCShareCPData[144,] <- c(2139.5,1434.1,2116.6,2105.05,648.95,372.65,250.5,1793.95,14808.3,1130.55)
BCShareCPData[408,] <- c(2188.6,2001.8,2670.7,2311.85,1133.45,481.4,188.65,2184.5,16842.1,830.75)
#-Verifying if the missing values have been treated correctly or not-#
any(is.na(BCShareCPData))
colSums(is.na(BCShareCPData))
#-No Missing values found now hence no further treatment is required-#

##Step 3.2.2 :- OUTLIER ANALYSIS##
#-Checking the outliers for different stocks used-#
tsoutliers(BCShareCPData$HUL)$index
tsoutliers(BCShareCPData$RIL)$index
tsoutliers(BCShareCPData$TCS)$index
tsoutliers(BCShareCPData$HDFC)$index
tsoutliers(BCShareCPData$INFOSYS)$index
tsoutliers(BCShareCPData$BAL)$index
tsoutliers(BCShareCPData$ITCL)$index
tsoutliers(BCShareCPData$APL)$index
tsoutliers(BCShareCPData$NIL)$index
tsoutliers(BCShareCPData$HCL)$index
#-No outlier treatment will be done since the values shown represent the performance -#
#-of a stock on a given day and imputing the same would produce inaccuracy during the-#
#-portfolio optimization stage.                                                      -#

###Step 3.3 :- Data Summary###

##Step 3.3.1 :- FIVE-NUMBER SUMMARY##
summary(BCShareCPData)

##Step 3.3.2 :- DESCRIPTIVE STATISTICS##

#For Closing Price Data#
psych::describe(BCShareCPData)
options(scipen = 100)
options(digits = 2)
stat.desc(BCShareCPData, basic = F)
nearZeroVar(BCShareCPData, saveMetrics = TRUE)

#For Closing Price Rate Of Change (ROC) Data#
#-Creating the Closing Prices ROC dataset for the stocks for visualization-#
BCPCEDTSData <- data.frame(BCShareCPData)
#-Assigning Default values-#
BCPCEDTSData$HUL.PC <- 0
BCPCEDTSData$RIL.PC <- 0
BCPCEDTSData$TCS.PC <- 0
BCPCEDTSData$HDFC.PC <- 0
BCPCEDTSData$INFOSYS.PC <- 0
BCPCEDTSData$BAL.PC <- 0
BCPCEDTSData$ITCL.PC <- 0
BCPCEDTSData$APL.PC <- 0
BCPCEDTSData$NIL.PC <- 0
BCPCEDTSData$HCL.PC <- 0
#-Calculating the ROC values for each stock-#
for(i in 2:494)
{
  BCPCEDTSData$HUL.PC[i] <- BCPCEDTSData$HUL[i]/BCPCEDTSData$HUL[i-1]-1
  BCPCEDTSData$RIL.PC[i] <- BCPCEDTSData$RIL [i]/BCPCEDTSData$RIL [i-1]-1
  BCPCEDTSData$TCS.PC[i] <- BCPCEDTSData$TCS [i]/BCPCEDTSData$TCS [i-1]-1
  BCPCEDTSData$HDFC.PC[i] <- BCPCEDTSData$HDFC [i]/BCPCEDTSData$HDFC [i-1]-1
  BCPCEDTSData$INFOSYS.PC[i] <- BCPCEDTSData$INFOSYS [i]/BCPCEDTSData$INFOSYS [i-1]-1
  BCPCEDTSData$BAL.PC[i] <- BCPCEDTSData$BAL [i]/BCPCEDTSData$BAL [i-1]-1
  BCPCEDTSData$ITCL.PC[i] <- BCPCEDTSData$ITCL [i]/BCPCEDTSData$ITCL [i-1]-1
  BCPCEDTSData$APL.PC[i] <- BCPCEDTSData$APL [i]/BCPCEDTSData$APL [i-1]-1
  BCPCEDTSData$NIL.PC[i] <- BCPCEDTSData$NIL [i]/BCPCEDTSData$NIL [i-1]-1
  BCPCEDTSData$HCL.PC[i] <- BCPCEDTSData$HCL [i]/BCPCEDTSData$HCL [i-1]-1
}
#-Removing the closing prices variables as we will not be using them for visual analysis-#
BCPCED <- BCPCEDTSData[,-c(1:10)]
Date <- index(BCShareCPData)
BCPCEDTS <- xts(BCPCED, order.by = Date )
#-Providing a summary for the ROC Data for the blue chip stocks-#
summary(BCPCEDTS) #Five-Number Summary#
#-Descriptive Statistics for the ROC Data for the blue chip stocks-#
psych::describe(BCPCEDTS) 
stat.desc(BCPCEDTS, basic = F)

###Step 3.4 :- Data Visualization###

##Step 3.4.1 :- FINDING THE DATA PERIODICITY##
findfrequency(BCPCEDTS)
periodicity(BCPCEDTS)
#-This shows the data to have a daily periodicity-#

##Step 3.4.2 :- NORMAL PLOT##
#-Comparing the performance variations for all stocks together-#
TSstudio::ts_plot(BCShareCPData, title = "Normal Plot: Closing Prices Variation for Blue-Chip Stocks Mar 2019 - Mar 2021")
TSstudio::ts_plot(BCPCEDTS, title = "Normal Plot: Closing Prices ROC Variation for Blue-Chip Stocks Mar 2019 - Mar 2021")
#-Plotting the ROC Performance Variations for stocks individually for clarity-#
TSstudio::ts_plot(BCPCEDTS$HUL.PC, title = "Normal Plot: Performance Variation for Hindustan Unilever Limited Mar 2019 - Mar 2021", color = "Blue" )
TSstudio::ts_plot(BCPCEDTS$RIL.PC, title = "Normal Plot: Performance Variation for Reliance Industries Limited Mar 2019 - Mar 2021", color = "Blue" )
TSstudio::ts_plot(BCPCEDTS$TCS.PC, title = "Normal Plot: Performance Variation for Tata Consultancy Services Mar 2019 - Mar 2021", color = "Blue" )
TSstudio::ts_plot(BCPCEDTS$HDFC.PC, title = "Normal Plot: Performance Variation for Housing Development Finance Corporation  Mar 2019 - Mar 2021", color = "Blue" )
TSstudio::ts_plot(BCPCEDTS$INFOSYS.PC, title = "Normal Plot: Performance Variation for Infosys Limited Mar 2019 - Mar 2021", color = "Blue" )
TSstudio::ts_plot(BCPCEDTS$BAL.PC, title = "Normal Plot: Performance Variation for Bharti Airtel Limited Mar 2019 - Mar 2021", color = "Blue" )
TSstudio::ts_plot(BCPCEDTS$ITCL.PC, title = "Normal Plot: Performance Variation for ITC Limited Mar 2019 - Mar 2021", color = "Blue" )
TSstudio::ts_plot(BCPCEDTS$APL.PC, title = "Normal Plot: Performance Variation for Asian Paints Limited Mar 2019 - Mar 2021", color = "Blue" )
TSstudio::ts_plot(BCPCEDTS$NIL.PC, title = "Normal Plot: Performance Variation for Nestle India Limited Mar 2019 - Mar 2021", color = "Blue" )
TSstudio::ts_plot(BCPCEDTS$HCL.PC, title = "Normal Plot: Performance Variation for HCL Technologies Limited Mar 2019 - Mar 2021", color = "Blue" )

##Step 3.4.3 :- SUBSERIES/SUBSEASONAL PLOT##
ggsubseriesplot(ts(BCPCEDTS$HUL.PC, frequency = 7)) + ggtitle ("Hindustan Unilever Limited Mar 2019 - Mar 2021") + theme(plot.background = element_rect(fill = "Black"), panel.background = element_rect(fill = "Light Blue"),axis.text = element_text(colour = "White"), axis.title.x = element_blank(), axis.title.y = element_blank(), title = element_text(colour = "White") )
ggsubseriesplot(ts(BCPCEDTS$RIL.PC, frequency = 7)) + ggtitle ("Reliance Industries Limited Mar 2019 - Mar 2021") + theme(plot.background = element_rect(fill = "Black"), panel.background = element_rect(fill = "Light Blue"),axis.text = element_text(colour = "White"), axis.title.x = element_blank(), axis.title.y = element_blank(), title = element_text(colour = "White"))
ggsubseriesplot(ts(BCPCEDTS$TCS.PC, frequency = 7)) + ggtitle ("Tata Consultancy Services Mar 2019 - Mar 2021") + theme(plot.background = element_rect(fill = "Black"), panel.background = element_rect(fill = "Light Blue"),axis.text = element_text(colour = "White"), axis.title.x = element_blank(), axis.title.y = element_blank(), title = element_text(colour = "White") )
ggsubseriesplot(ts(BCPCEDTS$HDFC.PC, frequency = 7)) + ggtitle ("Housing Development Finance Corporation Mar 2019 - Mar 2021") + theme(plot.background = element_rect(fill = "Black"), panel.background = element_rect(fill = "Light Blue"),axis.text = element_text(colour = "White"), axis.title.x = element_blank(), axis.title.y = element_blank(), title = element_text(colour = "White") )
ggsubseriesplot(ts(BCPCEDTS$INFOSYS.PC, frequency = 7)) + ggtitle ("Infosys Limited Mar 2019 - Mar 2021") + theme(plot.background = element_rect(fill = "Black"), panel.background = element_rect(fill = "Light Blue"),axis.text = element_text(colour = "White"), axis.title.x = element_blank(), axis.title.y = element_blank(), title = element_text(colour = "White") )
ggsubseriesplot(ts(BCPCEDTS$BAL.PC, frequency = 7)) + ggtitle ("Bharti Airtel Limited Mar 2019 - Mar 2021") + theme(plot.background = element_rect(fill = "Black"), panel.background = element_rect(fill = "Light Blue"),axis.text = element_text(colour = "White"), axis.title.x = element_blank(), axis.title.y = element_blank(), title = element_text(colour = "White") )
ggsubseriesplot(ts(BCPCEDTS$ITCL.PC, frequency = 7)) + ggtitle ("ITC Limited Mar 2019 - Mar 2021") + theme(plot.background = element_rect(fill = "Black"), panel.background = element_rect(fill = "Light Blue"),axis.text = element_text(colour = "White"), axis.title.x = element_blank(), axis.title.y = element_blank(), title = element_text(colour = "White") )
ggsubseriesplot(ts(BCPCEDTS$APL.PC, frequency = 7)) + ggtitle ("Asian Paints Limited Mar 2019 - Mar 2021") + theme(plot.background = element_rect(fill = "Black"), panel.background = element_rect(fill = "Light Blue"),axis.text = element_text(colour = "White"), axis.title.x = element_blank(), axis.title.y = element_blank(), title = element_text(colour = "White") )
ggsubseriesplot(ts(BCPCEDTS$NIL.PC, frequency = 7)) + ggtitle ("Nestle India Limited Mar 2019 - Mar 2021") + theme(plot.background = element_rect(fill = "Black"), panel.background = element_rect(fill = "Light Blue"),axis.text = element_text(colour = "White"), axis.title.x = element_blank(), axis.title.y = element_blank(), title = element_text(colour = "White") )
ggsubseriesplot(ts(BCPCEDTS$HCL.PC, frequency = 7)) + ggtitle ("HCL Technologies Limited Mar 2019 - Mar 2021") + theme(plot.background = element_rect(fill = "Black"), panel.background = element_rect(fill = "Light Blue"),axis.text = element_text(colour = "White"), axis.title.x = element_blank(), axis.title.y = element_blank(), title = element_text(colour = "White") )

##Step 3.4.4 :- SEASONAL PLOT##
ts_seasonal(ts(BCPCEDTS$HUL.PC, frequency = 12), title = "Seasonal Plot: Performance Variation for Hindustan Unilever Limited Mar 2019 - Mar 2021", type = "cycle")
ts_seasonal(ts(BCPCEDTS$RIL.PC, frequency = 12), title = "Seasonal Plot: Performance Variation for Reliance Industries Limited Mar 2019 - Mar 2021", type = "cycle")
ts_seasonal(ts(BCPCEDTS$TCS.PC, frequency = 12), title = "Seasonal Plot: Performance Variation for Tata Consultancy Services Mar 2019 - Mar 2021", type = "cycle")
ts_seasonal(ts(BCPCEDTS$HDFC.PC, frequency = 12), title = "Seasonal Plot: Performance Variation for Housing Development Finance Corporation  Mar 2019 - Mar 2021", type = "cycle")
ts_seasonal(ts(BCPCEDTS$INFOSYS.PC, frequency = 12), title = "Seasonal Plot: Performance Variation for Infosys Limited Mar 2019 - Mar 2021", type = "cycle")
ts_seasonal(ts(BCPCEDTS$BAL.PC, frequency = 12), title = "Seasonal Plot: Performance Variation for Bharti Airtel Limited Mar 2019 - Mar 2021", type = "cycle")
ts_seasonal(ts(BCPCEDTS$ITCL.PC, frequency = 12), title = "Seasonal Plot: Performance Variation for ITC Limited Mar 2019 - Mar 2021", type = "cycle")
ts_seasonal(ts(BCPCEDTS$APL.PC, frequency = 12), title = "Seasonal Plot: Performance Variation for Asian Paints Limited Mar 2019 - Mar 2021", type = "cycle")
ts_seasonal(ts(BCPCEDTS$NIL.PC, frequency = 12), title = "Seasonal Plot: Performance Variation for Nestle India Limited Mar 2019 - Mar 2021", type = "cycle")
ts_seasonal(ts(BCPCEDTS$HCL.PC, frequency = 12), title = "Seasonal Plot: Performance Variation for HCL Technologies Limited Mar 2019 - Mar 2021", type = "cycle")

##Step 3.4.5 :- ROLLING WINDOW PLOT##
ts_ma(BCShareCPData$HUL , n_left = 6, n_right = 5, n = NULL)$plot
ts_ma(BCShareCPData$RIL, n_left = 6, n_right = 5, n = NULL)$plot
ts_ma(BCShareCPData$TCS, n_left = 6, n_right = 5, n = NULL)$plot
ts_ma(BCShareCPData$HDFC, n_left = 6, n_right = 5, n = NULL)$plot
ts_ma(BCShareCPData$INFOSYS, n_left = 6, n_right = 5, n = NULL)$plot
ts_ma(BCShareCPData$BAL, n_left = 6, n_right = 5, n = NULL)$plot
ts_ma(BCShareCPData$ITCL, n_left = 6, n_right = 5, n = NULL)$plot
ts_ma(BCShareCPData$APL, n_left = 6, n_right = 5, n = NULL)$plot
ts_ma(BCShareCPData$NIL, n_left = 6, n_right = 5, n = NULL)$plot
ts_ma(BCShareCPData$HCL, n_left = 6, n_right = 5, n = NULL)$plot

##Step 3.4.6 :- CORRELATION PLOT##
ggpairs(BCPCEDTS) ##BASED ON STOCK CLOSING PRICE ROC#

##Step 3.4.7 :- BAR CHARTS FOR GENERAL ANALYSIS##
barChart(HINDUNILVR.BO)
addSMA(n = 20, col = "blue")
addBBands(n=20)
barChart(RELIANCE.BO)
addSMA(n = 20, col = "blue")
addBBands(n=20)
barChart(HDFC.BO)
addSMA(n = 20, col = "blue")
addBBands(n=20)
barChart(INFY.BO)
addSMA(n = 20, col = "blue")
addBBands(n=20)
barChart(BHARTIARTL.BO)
addSMA(n = 20, col = "blue")
addBBands(n=20)
barChart(ITC.BO)
addSMA(n = 20, col = "blue")
addBBands(n=20)
barChart(ASIANPAINT.BO)
addSMA(n = 20, col = "blue")
addBBands(n=20)
barChart(NESTLEIND.BO)
addSMA(n = 20, col = "blue")
addBBands(n=20)
barChart(HCLTECH.BO)
addSMA(n = 20, col = "blue")
addBBands(n=20)

####Step 4 :- Portfolio Optimization####

library(PerformanceAnalytics)
library(dplyr)
library(fPortfolio)
library(caTools)
library(timeSeries)
library(Quandl)

###Step 4.1 :- Downloading the benchmark data for portfolio BETA calculation###

BenchmarkCP <- getSymbols(Symbols = "^BSESN", src = "yahoo", from=start, to = end, periodicity = "daily", auto.assign=FALSE)[,4]
#-Manually synchronized Prices for the index with Dates With No Prices-#
BenchmarkCP[144,] <- 39250.2
BenchmarkCP[189,] <- 41306.02
BenchmarkCP[408,] <- 43637.98
BenchmarkCP[440,] <- 47868.98
colSums(is.na(BenchmarkCP))
colnames(BenchmarkCP) <- "S&P BSE Sensex"
Benchmark_Ret <- BenchmarkCP %>% lapply(function(x) monthlyReturn(x)) 
#Fetching the Risk Free Data#
RF_Data = Quandl("USTREASURY/YIELD",start_date=start)
RF <- data.frame(RF_Data[,c(1,4)])
RF <- RF[-c(1:6),]
RF <- xts(RF[,-1], order.by = RF$Date)
#Calculating monthly returns for the Risk Free Data#
RFRet <- RF %>% lapply(function(x) monthlyReturn(x))

###Step 4.2 :- Selecting the stocks for Portfolio Analysis###

#-We will be selecting two groups of five stocks to be used for our portfolios here-#
#-We will be using the ROC values for closing prices for analysis here -#

##CREATING THE DATA FRAMES FOR ANALYSIS##
Stock.Perf.Analysis <- data.frame("PerfAttr","HUL","RIL","TCS","HDFC","INFOSYS","BAL","ITCL","APL","NIL","HCL")
colnames(Stock.Perf.Analysis) <- c("PerfAttr","HUL","RIL","TCS","HDFC","INFOSYS","BAL","ITCL","APL","NIL","HCL")
Stock.Perf.Analysis[,1] <- "Expected Return"
Stock.Perf.Analysis[2,1] <-"Standard Deviation"
str(Stock.Perf.Analysis)
Stock.Perf.Analysis$PerfAttr <- as.factor(Stock.Perf.Analysis$PerfAttr )
for(i in 2:11)
{
  Stock.Perf.Analysis[,i] <- as.numeric(Stock.Perf.Analysis[,i] )
}
str(Stock.Perf.Analysis)
Stock_Variance_Analysis <- data.frame("Stock","VaR", "cVaR","Sharpe")
colnames(Stock_Variance_Analysis) <- c("Stock","VaR", "cVaR","Sharpe")
ColName <- colnames(BCShareCPData)
str(Stock_Variance_Analysis)
Stock_Variance_Analysis$VaR <- as.numeric(Stock_Variance_Analysis$VaR)
Stock_Variance_Analysis$cVaR <- as.numeric(Stock_Variance_Analysis$cVaR)
Stock_Variance_Analysis$Sharpe <- as.numeric(Stock_Variance_Analysis$Sharpe)
str(Stock_Variance_Analysis)

##ASSIGNING THE EXPECTED RETURN VALUES AND STANDARD DEVIATION VALUES FOR STOCK PERFORMANCE ANALYSIS##
#Expected Return#
Stock.Perf.Analysis[1,2] <- mean(BCPCEDTS$HUL.PC)
Stock.Perf.Analysis[1,3] <- mean(BCPCEDTS$RIL.PC)
Stock.Perf.Analysis[1,4] <- mean(BCPCEDTS$TCS.PC)
Stock.Perf.Analysis[1,5] <- mean(BCPCEDTS$HDFC.PC)
Stock.Perf.Analysis[1,6] <- mean(BCPCEDTS$INFOSYS.PC)
Stock.Perf.Analysis[1,7] <- mean(BCPCEDTS$BAL.PC)
Stock.Perf.Analysis[1,8] <- mean(BCPCEDTS$ITCL.PC)
Stock.Perf.Analysis[1,9] <- mean(BCPCEDTS$APL.PC)
Stock.Perf.Analysis[1,10] <- mean(BCPCEDTS$NIL.PC)
Stock.Perf.Analysis[1,11] <- mean(BCPCEDTS$HCL.PC)
#Standard Deviation#
Stock.Perf.Analysis[2,2] <- StdDev(BCPCEDTS$HUL.PC)
Stock.Perf.Analysis[2,3] <- StdDev(BCPCEDTS$RIL.PC)
Stock.Perf.Analysis[2,4] <- StdDev(BCPCEDTS$TCS.PC)
Stock.Perf.Analysis[2,5] <- StdDev(BCPCEDTS$HDFC.PC)
Stock.Perf.Analysis[2,6] <- StdDev(BCPCEDTS$INFOSYS.PC)
Stock.Perf.Analysis[2,7] <- StdDev(BCPCEDTS$BAL.PC)
Stock.Perf.Analysis[2,8] <- StdDev(BCPCEDTS$ITCL.PC)
Stock.Perf.Analysis[2,9] <- StdDev(BCPCEDTS$APL.PC)
Stock.Perf.Analysis[2,10] <- StdDev(BCPCEDTS$NIL.PC)
Stock.Perf.Analysis[2,11] <- StdDev(BCPCEDTS$HCL.PC)
#Transposing the data frame for better visualization#
Stock.Perf.Analysis_Final <- as.data.frame(t(Stock.Perf.Analysis))
Stock.Perf.Analysis_Final <- Stock.Perf.Analysis_Final[-1,]
colnames(Stock.Perf.Analysis_Final) <- c("Expected Return","Standard Deviation")
#-Arranging the funds based on maximum returns and minimum risk (minimum variance)-#
#Maximum Expected Return#
Stock.Perf.Analysis_Final %>% arrange(desc(Stock.Perf.Analysis_Final$`Expected Return`))
#Minimum Risk#
Stock.Perf.Analysis_Final %>% arrange(Stock.Perf.Analysis_Final$`Standard Deviation`)

##ASSIGNING THE VAR, CVAR & SHARPE VALUES FOR STOCK VARIANCE/RISK ANALYSIS##
#-Risk free rate will be considered 0 for analysis-#
for (i in 1:10)
{
  Stock_Variance_Analysis[i,2] <- VaR(BCPCEDTS[,i], p=0.95,method = "historical")
  Stock_Variance_Analysis[i,3] <- CVaR(BCPCEDTS[,i], p=0.95,method = "historical")
  Stock_Variance_Analysis[i,4] <- SharpeRatio(BCPCEDTS[,i], Rf = 0 ,FUN = "StdDev")
}
Stock_Variance_Analysis$Stock <- c("HUL","RIL","TCS","HDFC","INFOSYS","BAL","ITCL","APL","NIL","HCL")
View(Stock_Variance_Analysis) #Viewing the data frame created#
#-Arranging the funds based on minimum VaR, CVaR values and maximum Sharpe values-#
Stock_Variance_Analysis %>% arrange(desc(Stock_Variance_Analysis$VaR ))
Stock_Variance_Analysis %>% arrange(desc(Stock_Variance_Analysis$cVaR))
Stock_Variance_Analysis %>% arrange(Stock_Variance_Analysis$Sharpe)

##CREATING THE FINAL TWO GROUPS FOR PORTFOLIO ANALYSIS BASED ON THE ANALYSIS DONE ABOVE##
Max.Ret.Stock.Grp_CP <- BCShareCPData[,-c(1,3:4,7,9)]
Max.Ret.Stock.Grp_ROC <- BCPCEDTS[,-c(1,3:4,7,9)]
colnames(Max.Ret.Stock.Grp_ROC ) <- c("RIL","INFY","BAL","APL","HCL")
Min.Risk.Stock.Grp_CP <- BCShareCPData[,-c(2,4:6,10)]
Min.Risk.Stock.Grp_ROC <- BCPCEDTS[,-c(2,4:6,10)]
colnames(Min.Risk.Stock.Grp_ROC ) <- c("HUL","TCS","ITCL","APL","NIL")
#-Viewing the groups formed for Portfolio analysis-#
Max.Ret.Stock.Grp_CP
Min.Risk.Stock.Grp_CP
Max.Ret.Stock.Grp_ROC
Min.Risk.Stock.Grp_ROC
#-Now that we have decided on our two stocks groups to use, we will be accordingly moving ahead with-#
#-portfolio creation phase.                                                                         -#

###Step 4.3 :- Generating Monthly Returns Data for the stocks based on their ROC in closing prices###

##GENERATING MONTHLY RETURNS FOR THE STOCK GROUPS CREATED##
MRSGMonthRetData <- Max.Ret.Stock.Grp_CP %>% lapply(function(x) monthlyReturn(x))
MRskSGMonthRetData <- Min.Risk.Stock.Grp_CP  %>% lapply(function(x) monthlyReturn(x))
#-Taking the ROC data in another variable to maintain the original as is-#
MERSG <- Max.Ret.Stock.Grp_ROC
MRSKSG <- Min.Risk.Stock.Grp_ROC
#-Merging the Monthly Returns with the ROC data to get the final objects that we will use for analysis-#
MERSG <-  do.call(merge, MRSGMonthRetData)
colnames(MERSG) <- colnames(Max.Ret.Stock.Grp_ROC)
MRSKSG <-  do.call(merge, MRskSGMonthRetData)
colnames(MRSKSG) <- colnames(Min.Risk.Stock.Grp_ROC)
#-Verifying if there are any null values in the returns object created-#
colSums(is.na(MERSG))
colSums(is.na(MRSKSG))
#-No null values found hence no treatment required, we can now move to the next step-#

##GENERATING MONTHLY RETURNS FOR ALL FUNDS FOR ADDITIONAL ANALYSIS##
ASCP <- BCShareCPData
ASROCO <- BCPCEDTS
ASROC <- ASROCO
ASMonthRetData <- ASCP %>% lapply(function(x) monthlyReturn(x))
ASROC <- do.call(merge, ASMonthRetData)
colnames(ASROC ) <- colnames(ASCP)
colSums(is.na(ASROC))

###Step 4.4 :- Generating & Plotting the Efficient Frontier Portfolio Objects for the stocks###

##CALCULATING COVARIANCE & CORRELATION FOR THE MONTHLY RETURNS OF THE STOCKS##
round(cor(MERSG),3)
round(cov(MERSG),3)
round(cor(MRSKSG),3)
round(cov(MRSKSG),3)
round(cor(ASROC),3)
round(cov(ASROC),3)
#-Lower covariance values indicates performances of funds do not have similar patterns-#
#-which is good since we want to create a diversified portfolio and do not have stocks-#
#-for whom if the performance goes down for 1 the other's performance also falls.     -#
#-The analysis above further validates the stock groups we created and move ahead     -#
#-accordingly with the portfolio creation.                                            -#

##CREATING THE EFFICIENT FRONTIER PORTFOLIO OBJECTS FOR ANALYSIS##
MERSGPortfolio <- portfolioFrontier(as.timeSeries(MERSG), constraints = "LongOnly")
MRSKSGPortfolio <- portfolioFrontier(as.timeSeries(MRSKSG), constraints = "LongOnly")
ASPortfolio <- portfolioFrontier(as.timeSeries(ASROC), constraints = "LongOnly")
plot(MERSGPortfolio,1)
plot(MRSKSGPortfolio,1)
plot(ASPortfolio,1)

###Step 4.5 :- Creating an Equal Weights Portfolio###
#-Both the stock groups will be analyzed here and stocks will have equal weightage or exposure in the portfolio-#

##PERFORMANCE ANALYSIS FOR THE STOCK GROUPS CREATED##
#-Plotting the efficient frontier with equal weights portfolio for the groups-#
plot(MERSGPortfolio,c(1,5))
plot(MRSKSGPortfolio,c(1,5))
#-Assigning the exposure weightage per stock for the portfolio-#
StockExposureWeights <- c(0.20,0.20,0.20,0.20,0.20)
#-Calculating the VaR,CVaR,mean,standard deviation,Sharpe, Beta for the portfolio-#
#-Calculating VaR and CVaR-#
#VaR#
VaR(MERSG,p=0.95,weights=StockExposureWeights,portfolio_method = "component", method = "historical")
VaR(MRSKSG,p=0.95,weights=StockExposureWeights,portfolio_method = "component", method = "historical")
#CVaR#
CVaR(MERSG,p=0.95,weights=StockExposureWeights,portfolio_method = "component", method = "historical")
CVaR(MRSKSG,p=0.95,weights=StockExposureWeights,portfolio_method = "component", method = "historical")
#-Calculating the portfolio based expected returns(Value should be multiplied by 100 to get % equivalent)-#
mean(Return.portfolio(MERSG,weights=StockExposureWeights))
mean(Return.portfolio(MRSKSG,weights=StockExposureWeights))
#-Calculating the portfolio based standard deviation-#
stdev(Return.portfolio(MERSG,weights=StockExposureWeights))
stdev(Return.portfolio(MRSKSG,weights=StockExposureWeights))
#-Calculating the Sharpe & Beta for the portfolio-#
#Sharpe#
#Assuming Risk Free Rate to be 0 for Sharpe#
MERSGSharpeEW <- (mean(Return.portfolio(MERSG,weights=StockExposureWeights)))/(stdev(Return.portfolio(MERSG,weights=StockExposureWeights)))
MERSGSharpeEW
MRSKSGSharpeEW <- (mean(Return.portfolio(MRSKSG,weights=StockExposureWeights)))/(stdev(Return.portfolio(MRSKSG,weights=StockExposureWeights)))
MRSKSGSharpeEW
#Beta#
MERSGRetEW <- (Return.portfolio(MERSG,weights=StockExposureWeights))
MERSGRetEW
MRSKSGRetEW <- (Return.portfolio(MRSKSG,weights=StockExposureWeights))
MRSKSGRetEW
MERSGBetaEW <- CAPM.beta( data.frame(MERSGRetEW),data.frame(Benchmark_Ret),Rf = data.frame(RFRet))
MRSKSGBetaEW <- CAPM.beta( data.frame(MRSKSGRetEW),data.frame(Benchmark_Ret),Rf = data.frame(RFRet))
MERSGBetaEW
MRSKSGBetaEW
#-Analyzing Target Risk & Return/ Stock Weightage for the portfolio without equal weights-#
#-Calculating and Plotting weights for the portfolio-#
#Here we calculate allocations for each stock for each data point on the efficient frontier#
MERSGFrontierWeights <- getWeights(MERSGPortfolio)
MRSKSGFrontierWeights <- getWeights(MRSKSGPortfolio)
MERSGFrontierWeights
MRSKSGFrontierWeights
#-Plotting the stock weights for the portfolio-#
pie(StockExposureWeights, col=cm.colors(ncol(MERSGFrontierWeights)+2))
pie(StockExposureWeights, col=cm.colors(ncol(MRSKSGFrontierWeights)+2))
ggplot(data= data.frame(StockExposureWeights), aes(x=colnames(MERSGFrontierWeights), y=StockExposureWeights , fill=colnames(MERSGFrontierWeights))) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",StockExposureWeights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")
ggplot(data= data.frame(StockExposureWeights), aes(x=colnames(MRSKSGFrontierWeights), y=StockExposureWeights , fill=colnames(MRSKSGFrontierWeights))) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",StockExposureWeights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")
#-Since our allocation of stock weights has equal proportions hence you will see no difference between the stocks on the plot-#

##PERFORMANCE ANALYSIS FOR ALL STOCKS##
#-Plotting the efficient frontier with the Equal Weights portfolio-#
plot(ASPortfolio,c(1,5))
ASExposureWeights <- c(0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10)
#-Calculating the VaR,CVaR,mean,standard deviation,Sharpe, Beta for the portfolio-#
VaR(ASROC,p=0.95,weights=ASExposureWeights,portfolio_method = "component", method = "historical")
CVaR(ASROC,p=0.95,weights=ASExposureWeights,portfolio_method = "component", method = "historical")
mean(Return.portfolio(ASROC,weights=ASExposureWeights))
stdev(Return.portfolio(ASROC,weights=ASExposureWeights))
ASSharpeEW <- (mean(Return.portfolio(ASROC,weights = ASExposureWeights)))/(stdev(Return.portfolio(ASROC,weights=ASExposureWeights)))
ASSharpeEW
ASRetEW <- (Return.portfolio(ASROC,weights=ASExposureWeights))
ASRetEW
ASBetaEW <- CAPM.beta( data.frame(ASRetEW),data.frame(Benchmark_Ret),Rf = data.frame(RFRet))
ASBetaEW
#-Analyzing Target Risk & Return/ Stock Weightage for the portfolio without equal weights-#
ASWeights <- getWeights(ASPortfolio)
ASWeights
ASRR <- frontierPoints(ASPortfolio) #Target Risk-Return Analysis#
ASAP <- data.frame(targetRisk=ASRR[, "targetRisk"] * sqrt(252), targetReturn=ASRR[,"targetReturn"] * 252)
plot((ASAP[,"targetReturn"]-0) / ASAP[,"targetRisk"], xlab="Point on Efficient Frontier", ylab="Sharpe Ratio")
pie(ASExposureWeights, col=cm.colors(ncol(ASWeights)+2))
#-We are not showing the weights plot here since all funds have an equal weightage and hence it is not relevant here -#
#-Portfolio 49 shows to be the one with maximum return and Portfolio 30-35 shows the minimum target risk with highest-#
#-return along with minimum risk in Portfolio 35.                                                                    -#


###Step 4.6 :- Creating a Maximum Return Portfolio###
#
##PERFORMANCE ANALYSIS FOR THE STOCK GROUP CREATED##
#-Here we will only be analyzing group with the stocks that had the maximum expected return (MERSG)-#
#-Analyzing the best portfolio with maximum returns that can be used for analysis-#
plot(MERSGPortfolio,c(1,4))
print(MERSGPortfolio,c(1,4))
#-Analyzing Target Risk & Return/ Stock Weightage for the portfolio-#
MERSGRR <- frontierPoints(MERSGPortfolio) #Target Risk-Return Analysis#
MERSGRR
MERSGAP <- data.frame(targetRisk=MERSGRR[, "targetRisk"] * sqrt(252), targetReturn=MERSGRR[,"targetReturn"] * 252)
plot((MERSGAP[,"targetReturn"]-0) / MERSGAP[,"targetRisk"], xlab="Point on Efficient Frontier", ylab="Sharpe Ratio")
#-Viewing the stock weights based on the portfolio with the highest targetReturn observed above-#
MERSGFrontierWeights

#-As shown above Portfolio 50 seems to show the highest return of 0.031 with weights-#
#-                RIL         INFY        BAL        APL          HCL               -#
#-            0.000000000 0.000000000 0.000000092 0.000000000 0.999999908           -#
MERSGWeights <- MERSGFrontierWeights[50,]
MERSGRR[50,]
ggplot(data= data.frame(MERSGWeights), aes(x=colnames(MERSGFrontierWeights), y=MERSGWeights , fill=colnames(MERSGFrontierWeights))) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",MERSGWeights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")

#-As seen above the maximum return portfolio generated is inclined solely towards investing in HCL stock-#
#-Values for other stocks was also given but it was very low hence has to be ignored                    -#
#-You can check for other portfolios with 5 stock coverage and a near 30% exposure                      -#
#-The stock exposure can be adjusted to other stocks once you narrow down on the portfolio              -#
#-One such example is given below:                                                                      -#

MERSGRR[20,]
MERSGWeightsUpd <- MERSGFrontierWeights[20,]
#-As shown, Portfolio 20 is our final portfolio with ideal stocks balance and highest return of 0.026 -#
#- The stock weights for the portfolio are as follows:                                                -#
#-                RIL         INFY        BAL        APL          HCL                                 -#
#-             0.0038        0.1842      0.4031     0.3462       0.0626                               -#
ggplot(data= data.frame(MERSGWeightsUpd ), aes(x=colnames(MERSGFrontierWeights), y=MERSGWeightsUpd  , fill=colnames(MERSGFrontierWeights))) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",MERSGWeightsUpd *100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")
#-Calculating the VaR,CVaR,mean,standard deviation,Sharpe, Beta for the portfolio#
VaR(MERSG,p=0.95,weights= MERSGWeightsUpd , portfolio_method = "component", method = "historical")
CVaR(MERSG,p=0.95,weights = MERSGWeightsUpd , portfolio_method = "component", method = "historical")
mean(Return.portfolio(MERSG, weights = MERSGWeightsUpd ))
stdev(Return.portfolio(MERSG, weights = MERSGWeightsUpd))
MERSGSharpe <- (mean(Return.portfolio(MERSG, weights = MERSGWeightsUpd)))/(stdev(Return.portfolio(MERSG, weights = MERSGWeightsUpd)))
MERSGSharpe
MERSGRet <- (Return.portfolio(MERSG, weights = MERSGWeightsUpd))
MERSGBeta <- CAPM.beta( data.frame(MERSGRet),data.frame(Benchmark_Ret),Rf = data.frame(RFRet))
MERSGBeta

##PERFORMANCE ANALYSIS FOR ALL STOCKS##
#-Plotting the efficient frontier with the portfolio-#
plot(ASPortfolio,c(1,4))
print(ASPortfolio,c(1,4))
#-Analyzing Target Risk & Return/ Stock Weightage for the portfolio-#
ASRR #Target Risk-Return Analysis#
plot((ASAP[,"targetReturn"]-0) / ASAP[,"targetRisk"], xlab="Point on Efficient Frontier", ylab="Sharpe Ratio")
ASWeights <- getWeights(ASPortfolio)
ASWeights
ASMRWeights <- ASWeights[49,] #Portfolio with one stock usage#
pie(ASMRWeights, col=cm.colors(ncol(ASWeights)+2))
ggplot(data= data.frame(ASMRWeights), aes(x=colnames(ASWeights), y=ASMRWeights , fill=colnames(ASWeights))) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",ASMRWeights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")
#-As shown above even with all funds the maximum return portfolio generated is inclined solely towards            -#
#-investing in HCL stock. There are other portfolios where other funds are involved but as shown the              -#
#-target Return reduces if they are introduced hence we have to consider them keeping this in mind.               -#
#-If not the last portfolio then Portfolio 41 can be used with five stocks usage and near to 30% exposure         -#
#-,the difference in extra weights can be shifted to the other stocks next in line,maximum returns can            -#
#-be optimized by try and test since we are allocating the weights priority wise. At the moment the portfolio     -#
#-gives a maximum return of 0.0243 with stocks Infosys, Bharti Airtel, Asian Paints, Nestle India & HCL.          -#

ASMRWeightsAdd <- ASWeights[41,] #Portfolio with five stock usage and 30% exposure - check last comment for clarity#
ASWeights[41,]
ASRR[41,]
#-As shown, Portfolio 41 is our final portfolio with ideal stocks balance and highest return of 0.024 -#
#- The stock weights for the portfolio are as follows:                                                -#
#-        HUL     RIL     TCS     HDFC  INFOSYS   BAL     ITCL    APL     NIL     HCL                 -#
#-       0.000   0.000   0.000   0.000   0.253   0.248   0.000   0.043   0.369   0.086                -#
ggplot(data= data.frame(ASMRWeightsAdd), aes(x=colnames(ASWeights), y=ASMRWeightsAdd , fill=colnames(ASWeights))) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",ASMRWeightsAdd*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")
#-Calculating the VaR,CVaR,mean,standard deviation,Sharpe, Beta for the portfolio-#
VaR(ASROC,p=0.95,weights = ASMRWeightsAdd , portfolio_method = "component", method = "historical")
CVaR(ASROC,p=0.95,weights = ASMRWeightsAdd ,portfolio_method = "component", method = "historical")
mean(Return.portfolio(ASROC,weights = ASMRWeightsAdd ))
stdev(Return.portfolio(ASROC,weights = ASMRWeightsAdd ))
ASSharpe <- (mean(Return.portfolio(ASROC,weights = ASMRWeightsAdd )))/(stdev(Return.portfolio(ASROC,weights = ASMRWeightsAdd )))
ASSharpe
ASRet <- (Return.portfolio(ASROC,weights = ASMRWeightsAdd ))
ASRet
ASBeta <- CAPM.beta( data.frame(ASRet),data.frame(Benchmark_Ret),Rf = data.frame(RFRet))
ASBeta


###Step 4.7 :- Creating a Minimum Risk Portfolio###

##PERFORMANCE ANALYSIS FOR THE STOCK GROUP CREATED##
#-Here we will only be analyzing group with the stocks that had the minimum risk (MRSKSG)-#
#-Plotting the efficient frontier with the minimum variance portfolio-#
MRSKSGMVP <- minvariancePortfolio(as.timeSeries(MRSKSG), spec=portfolioSpec(), constraints="LongOnly")
MRSKSGMVP
plot(MRSKSGPortfolio,c(1,2)) #Stock Portfolio that had minimum standard deviation#
#-Analyzing Target Risk & Return/ Stock Weightage for the portfolio-#
MRSKSGRR <- frontierPoints(MRSKSGMVP) #Target Risk-Return Analysis#
MRSKSGRR
MRSKSGAP <- data.frame(targetRisk=MRSKSGRR[, "targetRisk"] * sqrt(252), targetReturn=MRSKSGRR[,"targetReturn"] * 252)
plot((MRSKSGAP[,"targetReturn"]-0) / MRSKSGAP[,"targetRisk"], xlab="Point on Efficient Frontier", ylab="Sharpe Ratio")
MVPWeights <- getWeights(MRSKSGMVP)
MVPWeights
#-As shown above Portfolio with a minimum risk of 0.036 was generated with stocks weights-#
#-                HUL         TCS        ITCL        APL          NIL                    -#
#-                0.15        0.22       0.19        0.00         0.44                   -#
#-APL since is currently at 0.00 weightage hence to cut off the additional weight of 0.14-#
#-considering the 30% exposure,we can add the same to APL accordingly.                   -#
pie(MVPWeights, col=cm.colors(ncol(MRSKSGFrontierWeights)+2))
ggplot(data=data.frame(MVPWeights), aes(x=colnames(MRSKSGFrontierWeights), y=MVPWeights, fill=colnames(MRSKSGFrontierWeights))) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",MVPWeights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Minimum Variance Portfolio Optimal Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")
#-Calculating the VaR,CVaR,mean,standard deviation,Sharpe, Beta for the portfolio#
VaR(MRSKSG,p=0.95,weights = MVPWeights,portfolio_method = "component", method = "historical")
CVaR(MRSKSG,p=0.95,weights = MVPWeights,portfolio_method = "component", method = "historical")
mean(Return.portfolio(MRSKSG,weights = MVPWeights))
stdev(Return.portfolio(MRSKSG,weights = MVPWeights))
MRSKSGSharpe <- (mean(Return.portfolio(MRSKSG,weights = MVPWeights)))/(stdev(Return.portfolio(MRSKSG,weights = MVPWeights)))
MRSKSGSharpe
MRSKSGRet <- (Return.portfolio(MRSKSG,weights = MVPWeights))
MRSKSGBeta <- CAPM.beta( data.frame(MRSKSGRet),data.frame(Benchmark_Ret),Rf = data.frame(RFRet))
MRSKSGBeta

##PERFORMANCE ANALYSIS FOR ALL STOCKS##
#-Plotting the efficient frontier with the Tangency portfolio-#
ASMVP <- minvariancePortfolio(as.timeSeries(ASROC), spec=portfolioSpec(), constraints="LongOnly")
ASMVP
plot(ASPortfolio,c(1,2))
#-Analyzing Target Risk & Return/ Stock Weightage for the portfolio-#
ASMVPRR <- frontierPoints(ASMVP)
ASMVPRR
ASMVPAP <- data.frame(targetRisk=ASMVPRR[, "targetRisk"] * sqrt(252), targetReturn=ASMVPRR[,"targetReturn"] * 252)
plot((ASMVPAP[,"targetReturn"]-0) / ASMVPAP[,"targetRisk"], xlab="Point on Efficient Frontier", ylab="Sharpe Ratio")
ASMVPWeights <- getWeights(ASMVP)
ASMVPWeights
#-As shown above Portfolio with a minimum risk of 0.034 was generated with stocks weights-#
#-      HUL     RIL     TCS    HDFC   INFOSYS   BAL    ITCL     APL     NIL     HCL      -#
#-    0.178    0.000   0.157   0.000   0.087   0.119   0.098   0.000   0.361   0.000     -#
#-As observed the higher weights are given to the stocks we decided to keep for our minimum-#
#-risk group hence this further validates our selection of them.                           -#
#-Keeping the 30% exposure per share in mind, we can relocate the additional 0.06 and 0.08 -#
#-weight to any of the stocks below the 30% weightage and also per weightage priority      -#
pie(ASMVPWeights, col=cm.colors(ncol(ASWeights)+2))
ggplot(data=data.frame(ASMVPWeights), aes(x=colnames(ASWeights), y=ASMVPWeights, fill=colnames(ASWeights))) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",ASMVPWeights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Minimum Variance Portfolio Optimal Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")
#-Calculating the VaR,CVaR,mean,standard deviation,Sharpe, Beta for the portfolio-#
VaR(ASROC,p=0.95,weights = ASMVPWeights,portfolio_method = "component", method = "historical")
CVaR(ASROC,p=0.95,weights = ASMVPWeights,portfolio_method = "component", method = "historical")
mean(Return.portfolio(ASROC,weights = ASMVPWeights))
stdev(Return.portfolio(ASROC,weights = ASMVPWeights))
ASMVPSharpe <- (mean(Return.portfolio(ASROC,weights = ASMVPWeights)))/(stdev(Return.portfolio(ASROC,weights = ASMVPWeights)))
ASMVPSharpe
ASMVPRet <- (Return.portfolio(ASROC,weights = ASMVPWeights))
ASMVPBeta <- CAPM.beta( data.frame(ASMVPRet),data.frame(Benchmark_Ret),Rf = data.frame(RFRet))
ASMVPBeta


###Step 4.8 :- Creating a Tangency Portfolio with Best Sharpe Ratio###

##PERFORMANCE ANALYSIS FOR ALL STOCKS##
#-Plotting the efficient frontier with the Tangency portfolio-#
ASTP <- tangencyPortfolio(as.timeSeries(ASROC), spec=portfolioSpec(), constraints="LongOnly")
ASTP
plot(ASPortfolio,c(1,3))
#-Analyzing Target Risk & Return/ Stock Weightage for the portfolio-#
ASTPRR <- frontierPoints(ASTP) #Target Risk-Return Analysis#
ASTPRR
ASTPAP <- data.frame(targetRisk=ASTPRR[, "targetRisk"] * sqrt(252), targetReturn=ASTPRR[,"targetReturn"] * 252)
plot((ASTPAP[,"targetReturn"]-0) / ASTPAP[,"targetRisk"], xlab="Point on Efficient Frontier", ylab="Sharpe Ratio")
ASTPWeights <- getWeights(ASTP)
ASTPWeights
pie(ASTPWeights, col=cm.colors(ncol(ASWeights)+2))
#-As shown above Portfolio with a minimum risk of 0.034 was generated with stocks weights-#
#-       HUL     RIL     TCS    HDFC   INFOSYS   BAL    ITCL     APL     NIL     HCL     -#
#-      0.097   0.000   0.139   0.000   0.176   0.177   0.000   0.000   0.411   0.000    -#
#-Considering the five stocks 30% exposure rule, we can relocate the 0.111 additional weight from NIL-#
#-to the next in line stock i.e. BAL and fulfill the condition accordingly.                          -#
ggplot(data=data.frame(ASTPWeights), aes(x=colnames(ASWeights), y=ASTPWeights, fill=colnames(ASWeights))) +
  geom_bar(stat="identity", position=position_dodge(),colour="black") +
  geom_text(aes(label=sprintf("%.02f %%",ASTPWeights*100)),
            position=position_dodge(width=0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Minimum Variance Portfolio Optimal Weights")+ theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Assets", y = "Weight (%)")
#-Calculating the VaR,CVaR,mean,standard deviation,Sharpe, Beta for the portfolio-#
VaR(ASROC,p=0.95,weights =ASTPWeights,  portfolio_method = "component", method = "historical")
CVaR(ASROC,p=0.95,weights =ASTPWeights,portfolio_method = "component", method = "historical")
mean(Return.portfolio(ASROC,weights =ASTPWeights))
stdev(Return.portfolio(ASROC,weights =ASTPWeights))
ASTPSharpe <- (mean(Return.portfolio(ASROC,weights = ASTPWeights)))/(stdev(Return.portfolio(ASROC,weights = ASTPWeights)))
ASTPSharpe
ASTPRet <- (Return.portfolio(ASROC,weights = ASTPWeights))
ASTPBeta <- CAPM.beta( data.frame(ASTPRet),data.frame(Benchmark_Ret),Rf = data.frame(RFRet))
ASTPBeta
##################################### End of the Analysis ########################################
###################################### End of the Code ###########################################