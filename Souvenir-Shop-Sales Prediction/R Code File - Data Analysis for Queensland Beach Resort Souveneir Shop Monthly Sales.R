######################GROUP ASSIGNMENT FOR TIME SERIES FORECASTING#################################
##############################SUBMITTED BY: GROUP 12 - SHITAL GUPTA################################
######## R PROJECT: DATA ANALYSIS FOR QUEENSLAND BEACH RESORT SOUVENIR SHOP MONTHLY SALES #########
###################################################################################################
#########################Start of the Analysis################################
#########################Start of the Code####################################
####Step 1: - Setting the Current Working Directory####
getwd()
setwd("C:/Users/shital/Documents/R/RProgramming")
dir()
####Step 2: - Reading and viewing the data from the dataset file####
#Reading the data file in a variable as a list#
QBRSSSalesData <- read.csv(file.choose())
#Unlisting the variable and converting data type to a numeric vector to get proper structure#
QBRSSSalesData <- as.numeric(unlist(QBRSSSalesData))
#Converting the vector variable into a time series object#
SalesData <- ts(QBRSSSalesData, start = c(1987,1), end = c(1993,12), frequency = 12)
str(SalesData) #To verify the structure of the time series data#
class(SalesData) #Checking if the variable storing the data has the correct class#
print(SalesData) #To print all the values present in the data#
####Step 3 :- Exploratory Data Analysis - Understanding the dataset####
library(psych) #For describe function#
library(TSstudio)
library(forecast)
library(ggplot2)
##Step 3.1 :- Data Interpretation - Data Definition and Summarization##
#Step 3.1.1 :- Data Definition#
head(SalesData, 100) #To check if the time series is Univariate or Multivariate#
ts_info(SalesData) #To get information on data#
#Step 3.1.2 :- Data Summary#
summary(SalesData) #Descriptive Statistics for the time series data#
describe(SalesData)
##Step 3.2 :- Data Processing - Missing Values and Outliers Analysis##
#Step 3.2.1 :- Missing Values Analysis#
SalesData
any(is.na(SalesData)) #No Missing Values found#
#Step 3.2.2 :- Outliers Analysis#
tsoutliers(SalesData) #To check for the outliers
SDCleaned <- tsclean(SalesData) #Storing Cleaned Data in a variable#
SDCleaned #To show the outliers replacement#
##Step 3.3 :- Data Visualization - Time Series Data Plots##
#Step 3.3.1 :- Normal Plot#
par(font.main = 4, font.axis = 2, font.lab = 2)
plot(SalesData, main = "Normal Plot: Monthly Sales Queensland Beach Resort Souvenir Shop 1987 - 1993", xlab = "Year", ylab = "Sales (in thousands AU$)", col = "Blue", lwd = 2, pch = 19)
abline(reg=lm(SalesData~time(SalesData)))
#Interactive version - Normal Plot#
ts_plot(SalesData, title = "Normal Plot: Monthly Sales Queensland Beach Resort Souvenir Shop 1987 - 1993" , Xtitle = "Year" , Ytitle = "Sales (in thousands AU$)", color = "Green", slider = TRUE )
#Step 3.3.2 :- Subseries/Subseasonal Plot#
ggsubseriesplot(SalesData, times = time(SalesData), phase = cycle (SalesData)) + ylab("Sales (in thousands AU$)") + ggtitle ("Sub Series Plot: Monthly Sales Queensland Beach Resort Souvenir Shop 1987 - 1993") + theme(plot.background = element_rect(fill = "Black"), panel.background = element_rect(fill = "Light Blue"),axis.text = element_text(colour = "White"), axis.title.x = element_text(colour = "White"), axis.title.y = element_text(colour = "White"), title = element_text(colour = "White") )
#Step 3.3.3 :- Time Series Decomposition Plot#
#There preference is a multiplicative model here but we will show both#
ts_decompose(SalesData , type = c("multiplicative"))
ts_decompose(SalesData , type = c("additive"))
TSDS <- decompose(SalesData)
TSDS
#Step 3.3.4 :- Seasonal Plot#
ts_seasonal(SalesData, title = "Normal Seasonal Plot: Monthly Sales Queensland Beach Resort Souvenir Shop 1987 - 1993", type = "normal")
ts_seasonal(SalesData, title = "Cycle - Seasonal Plot: Monthly Sales Queensland Beach Resort Souvenir Shop 1987 - 1993", type = "cycle")
ts_seasonal(SalesData, title = "Box Plot - Seasonal Plot: Monthly Sales Queensland Beach Resort Souvenir Shop 1987 - 1993", type = "box")
ggseasonplot(SalesData, polar = TRUE)+ ylab("Sales (in thousands AU$)") + ggtitle("Polar Seasonal Plot: Monthly Sales Queensland Beach Resort Souvenir Shop 1987 - 1993 - View 1")+ theme(plot.title = element_text(hjust = 0.5))
ts_polar(SalesData, title = "Polar Seasonal Plot: Monthly Sales Queensland Beach Resort Souvenir Shop 1987 - 1993 - View 2")
#Step 3.3.5 :- Rolling Window Plot#
MA <- ts_ma(SalesData, n_left = 6, n_right = 5, n = NULL)
MA$plot
#Step 3.3.6 :- Lag Plot#
gglagplot(SalesData) + ylab("Sales (in thousands AU$)") + ggtitle("Lag Plot: Monthly Sales Queensland Beach Resort Souvenir Shop 1987 - 1993") + theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "White"), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))
#Step 3.3.7 :- Autocorrelation Plot#
#ACF & PACF Plot#
ts_cor(SalesData, lag.max = 24)
plot(sin(SalesData))
ts_cor(sin(SalesData), lag.max = 24) #Checking for differenced series ACF/PACF Plots#
#Original Data and Autocorrelation Plot Combined Representation#
tsdisplay(SalesData, main = "Correlation Plot: Monthly Sales Queensland Beach Resort Souvenir Shop 1987 - 1993")
####Step 4 :- Forecasting Model Assumptions Validation####
#Now that we have completed EDA for our time series let us now validate for forecasting model assumptions#
##Step 4.1 :- Data Periodicity Validation##
library(xts)
library(GeneCycle)
library(tseries)
#Step 4.1.1 :- Forecast & xts functions indicating a monthly & yearly periodicity for the time series#
findfrequency(SalesData)
periodicity(SalesData)
periodicity(to.yearly(as.xts(SalesData)))
#Step 4.1.2 :- Discrete Fourier Transform Method#
N = length(SalesData)
I = abs(fft(SalesData)/sqrt(N))^2
P = (4/N) * I  #Scaled Periodogram#
f = (0:floor(N/2))/N
plot(f, I[1:((N/2) + 1)], type = "o", xlab = "Frequency", ylab = "", main = "Raw Periodogram without Trend Removal")
spec.pgram(SalesData, detrend = FALSE, ci=0, col = "blue", main = "Without (blue) and with (red) trend removal")
spec.pgram(SalesData, detrend = TRUE, ci = 0, col = "red", add = TRUE)
PeriodogramStats <- periodogram(SalesData)
DataPeriodogram <- data.frame(Frequency = PeriodogramStats$freq, Spec = PeriodogramStats$spec)
Power <- DataPeriodogram[order(-PeriodogramStats$spec),]
TopPowerFreq <- head(Power,2) #Getting the top 2 Frequencies#
Time = 1/TopPowerFreq$Frequency #Conversion of Frequency to Time Periods#
TopPowerFreq #Getting the power frequencies for the time series#
Time #Checking the converted frequency or time period to validate periodicity#
#Removing the trend from the time series data and then rechecking the frequencies again#
DecomDataUpd <- SalesData - ((stl(SalesData, s.window = "periodic"))$time.series[,"trend"])
plot(DecomDataUpd)
DetrendPower <- (data.frame(Freq = (periodogram(DecomDataUpd))$freq, Spec = (periodogram(DecomDataUpd))$spec))[order(-((periodogram(DecomDataUpd))$spec)),]
DTP <- head(DetrendPower,2)
DTP <- 1/DTP$Freq
DTP
#Step 4.1.3 :- Auto Correlation Statistics and Plot for periodicity Analysis#
ts_cor(SalesData, type = "acf")
ggAcf(SalesData, plot = FALSE)
#The power spectral densities and time periods concludes that our time series has annual and monthly periodicity#
##Step 4.2 :- Data Partitioning##
#Train dataset will start from year 1987 and end on last month of 1992#
Train <- window(SalesData, start = c(1987,1), end = c(1992,12), frequency = 12)
#Test dataset will start from year 1993 till the end stamp of data# 
Test <- window(SalesData, start = c(1993,1), frequency = 12)
##Step 4.3 :- Data Stationarity Validation##
#Visual Observations#
ts_seasonal(SalesData, type = "box") #Seasonal Plot- Box Plot Type#
qqnorm(SalesData,main = "Normal Probability Plot") #Normal Probability Plot#
qqline(SalesData) #Normal Probability Plot with a QQ (Quartiles) Line#
hist(SalesData, col = "Green", ylab = "Sales(in thousands)", main = "Histogram") #Histogram Plot#
plot(SalesData, col = "Dark Blue", ylab = "Sales(in thousands)", main = "Run Sequence Plot") #Run Sequence Plot#
lag.plot(SalesData,main = "Lag Plot", col = "Red") #Lag Plot#
ts_heatmap(SalesData) #Heat Map Plot#
#Statistical Observations#
#Variation comparison between Train and Test dataset#
summary(Train) #For summary comparison#
summary(Test)
mean(Train) #For mean comparison#
mean(Test)
var(Train) #For variance comparison#
var(Test)
#Stationarity Test for SalesData#
#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Stationarity Test#
kpss.test(SalesData)
#Augmented Dickey Fuller (ADF) Stationarity Test#
adf.test(SalesData)
#Showcasing time series differencing concept for stationarity#
kpss.test(diff(log(SalesData)))
adf.test(diff(log(SalesData)))
#For showing the stationarity of the data after differencing#
plot(diff(log(SalesData)),col = "maroon", main = "Differenced Time Series Plot - Monthly Sales Queensland Beach Resort Souvenir Shop 1987 - 1993")
#Step 4.4 :- Data Deseasonalization Validation#
#Detrending the series#
ts_seasonal(SalesData - decompose(SalesData)$trend, type = "all", title = "Time series seasonal plot without Trend component")
ts_surface(SalesData - decompose(SalesData)$trend)
#Decomposition statistics through STL method#
#Log is used since we are using a multiplicative model#
DS <- stl(log(SalesData), s.window = "periodic")
plot(DS, main = "STL Model - Multiplicative Approach")
DS #To check for statistics#
#Seasonal Adjustment for the STL decomposed time series from original data#
AdjTS <- exp(seasadj(DS)) #Exp is to back transform the values into original form#
#Comparing the transformed seasonally adjusted time series from original time series#
autoplot(cbind(OriginalTimeSeries = SalesData, SeasonallyAdjusted = AdjTS), main = "Comparison between Original and Seasonally Adjusted Time Series") + xlab("Year") + ylab("Sales(in thousands)")
####Step 5 :- Time Series Forecasting####
library(lmtest)
##Step 5.1 :- Preparing the Train time series data for modelling process##
#Step 5.1.1 :- Checking the partitioned datasets to ensure they are correctly divided#
ts_info(Train)
ts_info(Test)
par(mfrow = c(1,2))
plot(Train)
plot(Test)
#Step 5.1.2 :- Transforming the train series to four different types for modelling study#
#Original Train Series-Logged#
T1 <- log(Train) 
#Original Train Series-Logged-Outliers Cleaned#
T2 <-  log(tsclean(Train, replace.missing = TRUE))
#Original Train Series-Logged-Seasonally Adjusted#
T3 <-  log(exp(seasadj(stl(log(Train), s.window = "periodic"))))
#Original Train Series-Logged-Seasonally Adjusted-Outliers Cleaned#
T4 <-  log(exp(seasadj(stl(log(tsclean(Train, replace.missing = TRUE)), s.window = "periodic"))))
#Plotting the transformed train series to check for trend component#
par(mfrow = c(4,1))
plot(T1)
plot(T2)
plot(T3)
plot(T4)
#As we can see all the series has trend components while T3/T4 does not have seasonal components#
#Step 5.1.3 :- Verifying stationarity for all processed Train time series by statistics and plots# 
#Before Differencing#
adf.test(T1) #p-value = 0.108#
adf.test(T2) #p-value = 0.056#
adf.test(T3) #p-value = 0.671#
adf.test(T4) #p-value = 0.298#
#After Differencing#
adf.test(diff(T1)) #p-value = 0.01#
adf.test(diff(T2)) #p-value = 0.01#
adf.test(diff(T3)) #p-value = 0.01#
adf.test(diff(T4)) #p-value = 0.01#
#Plotting all the time series to visualize stationarity#
par(mfrow = c(4,1))
plot(diff(T1))
plot(diff(T2))
plot(diff(T3))
plot(diff(T4))
#For Original Time Series#
adf.test(SalesData) #Before differencing p-value = 0.542#
adf.test(diff(SalesData)) #After differencing p-value = 0.026#
plot(diff(SalesData))
#Hence, from above plots and statistics we can now confirm that all series have stationarity#
##Step 5.2 :- Creating the Holts-Winter Model##
#T3 & T4 will be used for Holts Winter Model while T1 & T2 will be used for Holts Winter Seasonal Model#
#Model 5 with original Time Series will only be shown here in code#
#Step 5.2.1 :- Creating the seasonal Winter-Holts model#
#Model with Level,Trend & Seasonality Smoothing#
HW1 <- HoltWinters(T1)
HW2 <- HoltWinters(T2)
HW1
HW2
par(mfrow = c(1,2))
plot(HW1, main = "Holts-Winter Filtering - T1 Series")
plot(HW2, main = "Holts-Winter Filtering - T2 Series")
#Step 5.2.2 :- Creating the general Winter-Holts model#
#Model with Trend and Level Smoothing#
HW3 <- HoltWinters(T3,gamma = FALSE)
HW4 <- HoltWinters(T4,gamma = FALSE)
HW3
HW4
par(mfrow = c(1,2))
plot(HW3, main = "Holts-Winter Filtering - T3 Series")
plot(HW4, main = "Holts-Winter Filtering - T4 Series")
#Step 5.2.3 :- Holts Winter Model with Original time series#
HW5 <- HoltWinters(log(SalesData))
HW5
plot(HW5, main = "Holts-Winter Filtering - Original Time Series")
#Step 5.2.4 :- Model Forecasting#
#Forecasting monthly sales for next 6 year (6x12 = 72 Months)#
#Extra 1 year is because we are also predicting test data as well#
PHW1 <- forecast(HW1, h = 72)
PHW2 <- forecast(HW2, h = 72)
PHW3 <- forecast(HW3, h = 72)
PHW4 <- forecast(HW4, h = 72)
PHW5 <- forecast(HW5, h = 60) #Since this is original time series with complete timeline#
#Plotting the model forecast for all time series#
par(mfrow = c(3,2))
plot(PHW1, main = "HW Model 1 Forecast For 72 periods")
plot(PHW2, main = "HW Model 2 Forecast For 72 periods")
plot(PHW3, main = "HW Model 3 Forecast For 72 periods")
plot(PHW4, main = "HW Model 4 Forecast For 72 periods")
plot(PHW5, main = "HW Model 5 Forecast For 60 periods")
#Plotting a subset forecast (Only 1994-98) with the Original time series#
par(mfrow = c(2,2))
seqplot.ts(SalesData, 2.718^(window(PHW1$mean, start = c(1994,1), frequency = 12)), main = "Original Data (1987-93) and Forecast Data (1994-98) Plot - Model 1", coly = "blue", typey = "l")
seqplot.ts(SalesData, 2.718^(window(PHW2$mean, start = c(1994,1), frequency = 12)), main = "Original Data (1987-93) and Forecast Data (1994-98) Plot - Model 2", coly = "blue", typey = "l")
seqplot.ts(SalesData, 2.718^(window(PHW3$mean, start = c(1994,1), frequency = 12)), main = "Original Data (1987-93) and Forecast Data (1994-98) Plot - Model 3", coly = "blue", typey = "l")
seqplot.ts(SalesData, 2.718^(window(PHW4$mean, start = c(1994,1), frequency = 12)), main = "Original Data (1987-93) and Forecast Data (1994-98) Plot - Model 4", coly = "blue", typey = "l")
seqplot.ts(SalesData, 2.718^(window(PHW5$mean, start = c(1994,1), end = c(1998,12), frequency = 12)), main = "Original Data (1987-93) and Forecast Data (1994-98) Plot - Model 5", coly = "blue", typey = "l")
##Step 5.3 :- Creating the ARIMA Model##
#Step 5.3.1 :- Getting an idea on the P,D,Q order for the models#
#From differencing process earlier we know D = 1#
#Checking the autocorrelation plots#
ts_cor(diff(T1))#Suggested Order(2,1,2)#
ts_cor(diff(T2))#Suggested Order(2,1,2)#
ts_cor(diff(T3))#Suggested Order(1,1,2)#
ts_cor(diff(T4))#Suggested Order(1,1,2)#
ts_cor(diff(log(SalesData))) #Suggested Order(2,1,2)#
#Step 5.3.2 :- Making an Auto-ARIMA Model#
auto.arima(T1, d = 1)
auto.arima(T2, d = 1)
auto.arima(T3, d = 1)
auto.arima(T4, d = 1)
auto.arima(log(SalesData), d = 1)
par(mfrow = c(3,2))
plot(forecast(auto.arima(T1, d = 1), h = 72), main = "AA Model - T1 Forecast For 72 periods") 
plot(forecast(auto.arima(T2, d = 1), h = 72), main = "AA Model - T2 Forecast For 72 periods")
plot(forecast(auto.arima(T3, d = 1), h = 72), main = "AA Model - T3 Forecast For 72 periods")
plot(forecast(auto.arima(T4, d = 1), h = 72), main = "AA Model - T4 Forecast For 72 periods")
plot(forecast(auto.arima(log(SalesData), d = 1), h = 60), main = "AA Model - Original Time Series Forecast For 60 periods")
#Checking the accuracy for the Auto ARIMA models#
forecast::accuracy(forecast(auto.arima(T1, d = 1), h = 12), log(Test))
forecast::accuracy(forecast(auto.arima(T2, d = 1), h = 12),log(Test))
forecast::accuracy(forecast(auto.arima(T3, d = 1), h = 12),log(Test))
forecast::accuracy(forecast(auto.arima(T4, d = 1), h = 12),log(Test))
forecast::accuracy(forecast(auto.arima(log(SalesData), d = 1)))
#Step 5.3.3 :- Making an Manual-ARIMA Model#
MA1 <- arima(T1, order = c(3,1,12), seasonal = list(order=c(0,1,2)))
MA1
MA2 <- arima(T2, order = c(3,1,14), seasonal = list(order=c(2,1,2)))
MA2
MA3 <- arima(T3, order = c(5,1,1))
MA3
MA4 <- arima(T4, order = c(5,1,4))
MA4
MA5 <- arima(log(SalesData), order = c(2,1,2), seasonal = list( order=c(2,1,2)))
MA5
#Step 5.3.4 :- Model Forecasting#
#Forecasting monthly sales for next 6 year (6x12 = 72 Months)#
#Extra 1 year is because we are also predicting test data as well#
PMA1 <- forecast(MA1, h= 72)
PMA2 <- forecast(MA2, h= 72)
PMA3 <- forecast(MA3, h= 72)
PMA4 <- forecast(MA4, h= 72)
PMA5 <- forecast(MA5, h= 60)
#Plotting the model forecast for all time series#
par(mfrow = c(3,2))
plot(PMA1, main = "MA Model 1 Forecast For 72 periods")
plot(PMA2, main = "MA Model 2 Forecast For 72 periods") 
plot(PMA3, main = "MA Model 3 Forecast For 72 periods") 
plot(PMA4, main = "MA Model 4 Forecast For 72 periods") 
plot(PMA5, main = "MA Model 5 Forecast For 60 periods")
#Plotting a subset forecast (Only 1994-98) with the Original time series#
par(mfrow = c(3,2))
seqplot.ts(SalesData, 2.718^(window(PHW1$mean, start = c(1994,1), frequency = 12)), main = "Original Data (1987-93) and Forecast Data (1994-98) Plot - Model 1", coly = "blue", typey = "l")
seqplot.ts(SalesData, 2.718^(window(PHW2$mean, start = c(1994,1), frequency = 12)), main = "Original Data (1987-93) and Forecast Data (1994-98) Plot - Model 2", coly = "blue", typey = "l")
seqplot.ts(SalesData, 2.718^(window(PHW3$mean, start = c(1994,1), frequency = 12)), main = "Original Data (1987-93) and Forecast Data (1994-98) Plot - Model 3", coly = "blue", typey = "l")
seqplot.ts(SalesData, 2.718^(window(PHW4$mean, start = c(1994,1), frequency = 12)), main = "Original Data (1987-93) and Forecast Data (1994-98) Plot - Model 4", coly = "blue", typey = "l")
seqplot.ts(SalesData, 2.718^(window(PHW5$mean, start = c(1994,1), end = c(1998,12), frequency = 12)), main = "Original Data (1987-93) and Forecast Data (1994-98) Plot - Model 5", coly = "blue", typey = "l")
##Step 5.4 :- Model Accuracy Comparison##
#Step 5.4.1 :- Comparing Predicted Values for both models ##
#Original Data#
SalesData
#Holt-Winter Model Predictions#
window(2.718^PHW1$mean, start = c(1994,1)) #T1-MODEL 1#
window(2.718^PHW2$mean, start = c(1994,1)) #T2-MODEL 2#
window(2.718^PHW3$mean, start = c(1994,1)) #T3-MODEL 3#
window(2.718^PHW4$mean, start = c(1994,1)) #T4-MODEL 4#
2.718^PHW5$mean  #ORIGINAL TIME SERIES MODEL#
#ARIMA Model Predictions#
window(2.718^PMA1$mean, start = c(1994,1)) #T1-MODEL 1#
window(2.718^PMA2$mean, start = c(1994,1)) #T2-MODEL 2#
window(2.718^PMA3$mean, start = c(1994,1)) #T3-MODEL 3#
window(2.718^PMA4$mean, start = c(1994,1)) #T4-MODEL 4#
2.718^PMA5$mean #ORIGINAL TIME SERIES MODEL#
#Step 5.4.2 :- Tests for the residuals#
#Statistical Analysis#
#Box-Ljung Test for Residual Correlation#
#Holts-Winter Model#
Box.test(PHW1$residuals,type = c("Ljung-Box"), fitdf = 0)
Box.test(PHW2$residuals,type = c("Ljung-Box"), fitdf = 0)
Box.test(PHW3$residuals,type = c("Ljung-Box"), fitdf = 0)
Box.test(PHW4$residuals,type = c("Ljung-Box"), fitdf = 0)
Box.test(PHW5$residuals,type = c("Ljung-Box"), fitdf = 0)
#ARIMA Model#
Box.test(PMA1$residuals,type = c("Ljung-Box"), fitdf = 0)
Box.test(PMA2$residuals,type = c("Ljung-Box"), fitdf = 0)
Box.test(PMA3$residuals,type = c("Ljung-Box"), fitdf = 0)
Box.test(PMA4$residuals,type = c("Ljung-Box"), fitdf = 0)
Box.test(PMA5$residuals,type = c("Ljung-Box"), fitdf = 0)
#Shapiro Test for Residual Normality#
#Holts-Winter Model#
shapiro.test(PHW1$residuals)
shapiro.test(PHW2$residuals)
shapiro.test(PHW3$residuals)
shapiro.test(PHW4$residuals)
shapiro.test(PHW5$residuals)
#ARIMA Model#
shapiro.test(PMA1$residuals)
shapiro.test(PMA2$residuals)
shapiro.test(PMA3$residuals)
shapiro.test(PMA4$residuals)
shapiro.test(PMA5$residuals)
#Mean Check for Residuals#
#Holts-Winter Model#
mean(PHW1$residuals, na.rm = TRUE)
mean(PHW2$residuals, na.rm = TRUE)
mean(PHW3$residuals, na.rm = TRUE)
mean(PHW4$residuals, na.rm = TRUE)
mean(PHW5$residuals, na.rm = TRUE)
#ARIMA Model#
mean(PMA1$residuals, na.rm = TRUE)
mean(PMA2$residuals, na.rm = TRUE)
mean(PMA3$residuals, na.rm = TRUE)
mean(PMA4$residuals, na.rm = TRUE)
mean(PMA5$residuals, na.rm = TRUE)
#Breusch-Pagan Constant Variance Test for Residuals#
#Holts-Winter Model#
bptest(residuals(PHW1)~ c(1:length(residuals(PHW1))))
bptest(residuals(PHW2)~ c(1:length(residuals(PHW2))))
bptest(residuals(PHW3)~ c(1:length(residuals(PHW3))))
bptest(residuals(PHW4)~ c(1:length(residuals(PHW4))))
bptest(residuals(PHW5)~ c(1:length(residuals(PHW5))))
#ARIMA Model#
bptest(residuals(PMA1)~ c(1:length(residuals(PMA1))))
bptest(residuals(PMA2)~ c(1:length(residuals(PMA2))))
bptest(residuals(PMA3)~ c(1:length(residuals(PMA3))))
bptest(residuals(PMA4)~ c(1:length(residuals(PMA4))))
bptest(residuals(PMA5)~ c(1:length(residuals(PMA5))))
#Visual Analysis for the residuals#
#Holts-Winter Model#
checkresiduals(PHW1)
checkresiduals(PHW2)
checkresiduals(PHW3)
checkresiduals(PHW4)
checkresiduals(PHW5)
#ARIMA Model#
checkresiduals(PMA1)
checkresiduals(PMA2)
checkresiduals(PMA3)
checkresiduals(PMA4)
checkresiduals(PMA5)
#Step 5.4.3 :- Comparison on Model Accuracy#
#Holts-Winter Model#
forecast::accuracy(PHW1, log(Test))
forecast::accuracy(PHW2, log(Test))
forecast::accuracy(PHW3, log(Test))
forecast::accuracy(PHW4, log(Test))
forecast::accuracy(PHW5)
#ARIMA Model#
forecast::accuracy(PMA1, log(Test))
forecast::accuracy(PMA2, log(Test))
forecast::accuracy(PMA3, log(Test))
forecast::accuracy(PMA4, log(Test))
forecast::accuracy(PMA5)
#Hence to conclude, Model 1 - Time Series 1 - ARIMA was the best amongst all models and therefore can be used for analysis#
#########################End of the Analysis#################################
#########################End of the Code#####################################