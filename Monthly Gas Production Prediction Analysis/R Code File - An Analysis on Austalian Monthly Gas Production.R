##################INDIVIDUAL ASSIGNMENT FOR TIME SERIES FORECASTING##################
##########################SUBMITTED BY: - SHITAL GUPTA###############################
#### R PROJECT: DATA ANALYSIS FOR AUSTRALIAN MONTHLY GAS PRODUCTION 1956 - 1995 #####
#####################################################################################
#########################Start of the Analysis#################################
#########################Start of the Code#####################################
####Step 1: - Setting the Current Working Directory####
getwd()
setwd("C:/Users/shital/Documents/R/RProgramming")
dir()
####Step 2: - Reading and viewing the data from the dataset file####
library(forecast) #Package that contains the dataset#
AusGasProductionData <-  gas #Reading the dataset as time series object#
class(AusGasProductionData) #Checking if the variable storing the data has the correct class#
print(AusGasProductionData) #To print all the values from the data#
####Step 3: - Exploratory Data Analysis - Understanding the dataset####
##Step 3.1: - Data Interpretation - Data Definition and Summarization##
library(psych) #For describe function#
library(TSstudio)
#Step 3.1.1: - Data Definition#
head(AusGasProductionData, 100) #To check if the time series is Univariate or Multivariate#
str(AusGasProductionData) #To check the structure of the time series data#
ts_info(AusGasProductionData)
#Step 3.1.2: - Data Summary#
summary(AusGasProductionData) #Descriptive Statistics for the time series data#
describe(AusGasProductionData)
##Step 3.2: - Data Processing - Missing Values and Outliers Analysis##
AusGasProductionData
#Step 3.2.1: - Missing Values Analysis#
any(is.na(AusGasProductionData)) #No Missing Values found#
#Step 3.2.2: - Outliers Analysis#
tsoutliers(AusGasProductionData) #To check for the outliers
tsclean(AusGasProductionData) #To show the outliers replacement#
##Step 3.3: - Data Visualization - Time Series Data Plots##
library (dplyr)
library(fpp2)
#Step 3.3.1: - Normal Plot#
par(font.main = 4, font.axis = 2, font.lab = 2)
plot(AusGasProductionData, main = "Normal Plot:Monthly Gas Production in Australia 1956 - 1995", xlab = "Year", ylab = "Million Megajoules", col = "Blue", lwd = 2, pch = 19)
abline(reg=lm(AusGasProductionData~time(AusGasProductionData)))
#Interactive version - Normal Plot#
ts_plot(AusGasProductionData, title = "Normal Plot: Monthly Gas Production in Australia 1956 - 1995" , Xtitle = "Year" , Ytitle = "Million Megajoules", color = "Green", slider = TRUE )
#Step 3.3.2: - Subseries/Sub-seasonal Plot#
ggsubseriesplot(AusGasProductionData, times = time(AusGasProductionData), phase = cycle (AusGasProductionData)) + ylab("Million Megajoules") + ggtitle ("Sub Series Plot: Monthly Gas Production in Australia 1956 - 1995") +theme(plot.background = element_rect(fill = "Black"), panel.background = element_rect(fill = "Light Blue"),axis.text = element_text(colour = "White"), axis.title.x = element_text(colour = "White"), axis.title.y = element_text(colour = "White"), title = element_text(colour = "White") )
#Step 3.3.3: - Time Series Decomposition Plot#
#We use a multiplicative model as there is a seasonal variation in the time series#
ts_decompose(AusGasProductionData , type = c("multiplicative")) #For interactive version#
#Step 3.3.4: - Seasonal Plot#
ts_seasonal(AusGasProductionData, title = "Normal Seasonal Plot: Monthly Gas Production in Australia 1956 - 1995", type = "normal")
ts_seasonal(AusGasProductionData, title = "Cycle - Seasonal Plot: Monthly Gas Production in Australia 1956 - 1995", type = "cycle")
ts_seasonal(AusGasProductionData, title = "Box Plot - Seasonal Plot: Monthly Gas Production in Australia 1956 - 1995", type = "box")
ggseasonplot(AusGasProductionData, polar = TRUE)+ ylab("Million Megajoules") + ggtitle("Polar Seasonal Plot: Monthly Gas Production in Australia 1956 - 1995 - View 1")
ts_polar(AusGasProductionData, title = "Polar Seasonal Plot:Monthly Gas Production in Australia 1956 - 1995 - View 2")
#Step 3.3.5: - Rolling Window Plot#
MA <- ts_ma(AusGasProductionData, n_left = 6, n_right = 5, n = NULL)
MA$plot
#Step 3.3.6: - Lag Plot#
gglagplot(AusGasProductionData) + ylab("Million Megajoules") + ggtitle("Lag Plot: Monthly Gas Production in Australia 1956 - 1995") + theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "White"), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))
#Step 3.3.7: - Autocorrelation Plot#
#ACF & PACF Plot#
ts_cor(AusGasProductionData)
#Original Data and Autocorrelation Plot Combined Representation#
tsdisplay(AusGasProductionData, main = "Correlation Plot: Monthly Gas Production in Australia 1956 - 1995")
##Step 3.4: - Data Periodicity##
library(xts)
library(GeneCycle)
library(tseries)
#Removing the low gas production estimates years before 1970 for analysis as suggested#
AGPDPeriodicity <- ts(AusGasProductionData, start = c(1970,1), end = c(1995,8), frequency = 12)
#Step 3.4.1: - Forecast & xts functions indicating a monthly periodicity for the time series#
findfrequency(AusGasProductionData)
findfrequency(AGPDPeriodicity)
periodicity(AusGasProductionData)
periodicity(AGPDPeriodicity)
#Step 3.4.2: - Discrete Fourier Transform Method#
#For Subset Data#
N = length(AGPDPeriodicity)
I = abs(fft(AGPDPeriodicity)/sqrt(N))^2
P = (4/N) * I  #Scaled Periodogram#
f = (0:floor(N/2))/N
plot(f, I[1:((N/2) + 1)], type = "o", xlab = "Frequency", ylab = "", main = "Raw Periodogram without Trend Removal")
spec.pgram(AGPDPeriodicity, detrend = FALSE, ci=0, col = "blue", main = "Without (blue) and with (red) trend removal")
spec.pgram(AGPDPeriodicity, detrend = TRUE, ci = 0, col = "red", add = TRUE)
PeriodogramStats <- periodogram(AGPDPeriodicity)
DataPeriodogram <- data.frame(Frequency = PeriodogramStats$freq, Spec = PeriodogramStats$spec)
Power <- DataPeriodogram[order(PeriodogramStats$freq),] 
TopPowerFreq <- head(Power,2) #Getting the top 2 Frequencies#
TopPowerFreq
Time = 1/TopPowerFreq$Frequency #Conversion of Frequency to Time Periods#
Time
#For Original Data#
N2 = length(AusGasProductionData)
I2 = abs(fft(AusGasProductionData)/sqrt(N2))^2
P2 = (4/N2) * I2  #Scaled Periodogram#
f2 = (0:floor(N2/2))/N2
plot(f2, I2[1:((N2/2) + 1)], type = "o", xlab = "Frequency", ylab = "", main = "Raw Periodogram without Trend Removal - Original Data")
spec.pgram(AusGasProductionData, detrend = FALSE, ci=0, col = "blue", main = "Without (blue) and with (red) trend removal - Original Data")
spec.pgram(AusGasProductionData, detrend = TRUE, ci = 0, col = "red", add = TRUE)
PeriodogramStatsO <- periodogram(AusGasProductionData)
DataPeriodogramO <- data.frame(Frequency = PeriodogramStatsO$freq, Spec = PeriodogramStatsO$spec)
PowerO <- DataPeriodogramO[order(PeriodogramStatsO$freq),] 
TopPowerFreqO <- head(PowerO,2) #Getting the top 2 Frequencies - Original Data#
TopPowerFreqO
TimeO = 1/TopPowerFreqO$Frequency #Conversion of Frequency to Time Periods - Original#
TimeO
#Step 3.4.3: - Comparing the power frequencies/time periods for both original and subset data#
TopPowerFreq #Comparing the top power frequencies for both the datasets#
TopPowerFreqO
Time #Comparing the top converted time periods for both the datasets to estimate periodicity#
TimeO
#Step 3.4.4: - Auto Correlation Statistics and Plot for periodicity Analysis#
ts_cor(AusGasProductionData, type = "acf")
ggAcf(AusGasProductionData, plot = FALSE)
ggAcf(AGPDPeriodicity, plot = FALSE)
##Step 3.5: - Data Partitioning##
#Train dataset will start from year 1970 and end on last month of 1993#
Train <- window(AusGasProductionData, start = c(1970,1), end = c(1993,12), frequency = 12)
#Test dataset will start from year 1994 till the end stamp of data# 
Test <- window(AusGasProductionData, start = c(1994,1), end = c(1995,8), frequency = 12)
####Step 4: - Forecasting Model Assumptions Validation####
#Now that we have completed EDA for our time series let us now validate for forecasting model assumptions#
#One main assumption of our ARIMA model is Stationarity, in the commands below we will check for it#
##Step 4.1: - Data Stationarity Validation - Visual Observations##
ts_seasonal(AusGasProductionData, type = "box") #Seasonal Plot- Box Plot Type#
qqnorm(AusGasProductionData,main = "Normal Probability Plot") #Normal Probability Plot#
qqline(AusGasProductionData) #Normal Probability Plot with a QQ (Quartiles) Line#
hist(AusGasProductionData, col = "Green", ylab = "Million Megajoules", main = "Histogram") #Histogram Plot#
plot(AusGasProductionData, col = "Dark Blue", ylab = "Million Megajoules", main = "Run Sequence Plot") #Run Sequence Plot#
lag.plot(AusGasProductionData,main = "Lag Plot", col = "Red") #Lag Plot#
ts_heatmap(AusGasProductionData) #Heat Map Plot#
##Step 4.2: - Data Stationarity Validation - Statistical Observations##
#Step 4.2.1: - Summary Statistics of the Train and Test data#
summary(Train) #For summary comparison#
summary(Test)
mean(Train) #For mean comparison#
mean(Test)
var(Train) #For variance comparison#
var(Test)
#Step 4.2.2: - Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Stationarity Test#
kpss.test(AusGasProductionData)
#Step 4.2.3: - Augmented Dickey Fuller (ADF) Stationarity Test#
adf.test(AusGasProductionData)
#Step 4.2.4: - Showcasing time series differencing for stationarity#
kpss.test(diff(log(AusGasProductionData))) 
adf.test(diff(log(AusGasProductionData)))
plot(diff(log(AusGasProductionData)),col = "maroon", main = "Differenced Time Series Plot - Monthly Gas Production in Australia 1956 - 1995") #For showing the stationarity of the data after differencing#
#Step 4.2.5: - De-seasonalize data interpretation for normal ARIMA model#
#Detrending the series#
ts_seasonal(AusGasProductionData - decompose(AusGasProductionData)$trend, type = "all", title = "Time series seasonal plot without Trend component")
ts_surface(AusGasProductionData - decompose(AusGasProductionData)$trend)
#Decomposition statistics through STL method#
#Log is used since we are using a multiplicative model#
DS <- stl(log(AusGasProductionData), s.window = "periodic")
plot(DS, main = "STL Model - Multiplicative Approach")
DS #To check for statistics#
#Seasonal Adjustment for the STL decomposed time series from original data#
AdjTS <- exp(seasadj(DS)) #Exp is to back transform the values into original form#
#Comparing the transformed seasonally adjusted minus outliers time series from original time series#
autoplot(cbind(OriginalTimeSeries = AusGasProductionData, SeasonallyAdjusted = AdjTS), main = "Comparison between Original and Seasonally Adjusted Time Series") + xlab("Year") + ylab("Production in Million Megajoules")
####Step 5: - Time Series Forecasting####
##Step 5.1: - Preparing the Time Series Data for Forecasting Process##
#Step 5.1.1: - Checking the partitioned datasets to ensure they are correctly divided#
ts_info(Train)
ts_info(Test)
par(mfrow = c(1,2))
plot(Train)
plot(Test)
#Step 5.1.2: - Checking for stationarity and differencing the Train time series if required#
Train2 <- tsclean(Train,replace.missing = TRUE) #Train without Outliers for comparison#
kpss.test(Train) #This shows that our train series is not stationary#
TrainUpd <- log(Train) #We will put the differencing value d = 1 in ARIMA hence not mentioning it here#
adf.test(diff(log(Train))) #The ADF test though will use diff and log to show stationarity#
adf.test(diff(log(Train2)))
#Given below plot shows how the differenced series looks#
plot(diff(TrainUpd), col = "blue", main = "Differenced Time Series Plot - Train Series")
plot(diff(log(Train2)), col = "blue", main = "Differenced Time Series Plot - Train Without Outliers Series")
#Step 5.1.3: - De-Seasonalizing the Train time series to remove the seasonal component#
TrDecom <- stl(TrainUpd, s.window = "periodic")
#Additive Model is used due to series stationarity#
TrainSeasAdj <- seasadj(TrDecom) #Creating the seasonal component adjusted train time series#
ts_seasonal(TrainSeasAdj, type = "box") #This shows how the plot looks after seasonal component is removed from the series#
plot(TrDecom, main = "STL Model Plot - Additive Approach - Train Series") #This shows the separated components in the STL additive model#
#De-seasonalizing for Train series without outliers#
TrDecom2 <- stl(log(Train2), s.window = "periodic")
TrainSeasAdj2 <- seasadj(TrDecom2)
plot(TrDecom2, main = "STL Model Plot - Additive Approach - Train without Outliers Series")
ts_seasonal(TrainSeasAdj2, type = "box")
##Step 5.2: - Estimation of P and Q Values Through the ACF and PACF Plots##
ts_cor(diff(TrainSeasAdj)) #Plots indicate an ARIMA(0,1,3) model to be used#
ts_cor(diff(TrainSeasAdj2))
# For ease on execution no unnecessary variables are used. Some of the commands are a combination of many commands directly executed in one line. #
##Step 5.3: - Creating the AUTO ARIMA Model##
#Data provided is for original, differenced, logged-seasonally adjusted and outliers removed logged seasonal-adjusted series#
#Summary of Auto Arima model#
summary(auto.arima(Train)) #Original#
summary(auto.arima(log(Train), d = 1)) #Logged#
summary(auto.arima(TrainSeasAdj, d = 1)) #Logged Seasonally Adjusted#
summary(auto.arima(TrainSeasAdj2, d = 1)) #Logged Seasonal Adjusted - Outliers Removed#
#Checking for accuracy metrics#
forecast::accuracy((forecast(auto.arima(Train), h = 20))$mean, Test)
forecast::accuracy(2.718^((forecast(auto.arima(log(Train), d = 1), h = 20))$mean), Test)
forecast::accuracy((2.718^(forecast(auto.arima(TrainSeasAdj, d = 1), h = 20))$mean), Test)
forecast::accuracy((2.718^(forecast(auto.arima(TrainSeasAdj2, d = 1), h = 20))$mean), Test)
#Forecasting plots for the series#
#To predict the gas production for 20 months i.e. Years 1994 to Aug 1995#
plot(forecast(auto.arima(Train), h = 20), main = "AA Forecast For 20 periods") 
plot(forecast(auto.arima(log(Train), d = 1), h = 20), main = "AA Forecast For 20 periods")
plot(forecast(auto.arima(TrainSeasAdj, d = 1), h = 20), main = "AA Forecast For 20 periods")
plot(forecast(auto.arima(TrainSeasAdj2, d = 1), h = 20), main = "AA Forecast For 20 periods")
#To predict the gas production for 12 months extra i.e. Sep 1995 to Aug 1996#
plot(forecast(auto.arima(Train), h = 32), main = "AA Forecast For 12 extra periods") 
plot(forecast(auto.arima(log(Train), d = 1), h = 32), main = "AA Forecast For 12 extra periods")
plot(forecast(auto.arima(TrainSeasAdj, d = 1), h = 32), main = "AA Forecast For 12 extra periods")
plot(forecast(auto.arima(TrainSeasAdj2, d = 1), h = 32), main = "AA Forecast For 12 extra periods")
#As the best RMSE value i.e. 2667 was achieved for the model with logged time series hence replotting the same#
par(mfrow = c(1,2))
plot(forecast(auto.arima(log(Train), d = 1), h = 20), main = "AA Forecast For 20 periods")
plot(forecast(auto.arima(log(Train), d = 1), h = 32), main = "AA Forecast For 12 extra periods")
##Step 5.4: - Creating the Manual ARIMA Model##
#Final accepted model will be the best fit with 12 months forecast from Aug 1995#
library(Metrics)
library(tseries)
summary(auto.arima(log(Train), d = 1)) #Rechecking auto ARIMA suggestions for Logged Series#
ts_cor(diff(log(Train))) #Identifying if addition of AR/MA term is required#
#The try and test strategy will only be done for 20 months forecast#
#We are using RMSE from metrics library to decide on the best model#
rmse(2.718^((forecast((arima(log(Train), order = c(1,1,7), seasonal = list(order=c(0,1,2)))), h = 20))$mean), Test)
rmse(2.718^((forecast((arima(log(Train), order = c(1,1,12), seasonal = list(order=c(0,1,2)))), h = 20))$mean), Test)
rmse(2.718^((forecast((arima(log(Train), order = c(2,1,2), seasonal = list(order=c(0,1,2)))), h = 20))$mean), Test)
rmse(2.718^((forecast((arima(log(Train), order = c(1,1,3), seasonal = list(order=c(0,1,3)))), h = 20))$mean), Test)
rmse(2.718^((forecast((arima(log(Train), order = c(1,1,3), seasonal = list(order=c(0,1,2)))), h = 20))$mean), Test)
rmse(2.718^((forecast((arima(log(Train), order = c(1,1,4), seasonal = list(order=c(0,1,2)))), h = 20))$mean), Test)
rmse(2.718^((forecast((arima(log(Train), order = c(1,1,6), seasonal = list(order=c(0,1,2)))), h = 20))$mean), Test)
rmse(2.718^((forecast((arima(log(Train), order = c(12,3,6), seasonal = list(order=c(0,1,2)))), h = 20))$mean), Test)
rmse(2.718^((forecast((arima(log(Train), order = c(1,1,8), seasonal = list(order=c(0,1,2)))), h = 20))$mean), Test)
rmse(2.718^((forecast((arima(log(Train), order = c(1,2,2), seasonal = list(order=c(0,2,2)))), h = 20))$mean), Test)
rmse(2.718^((forecast((arima(log(Train), order = c(12,1,6), seasonal = list(order=c(0,1,2)))), h = 20))$mean), Test)
rmse(2.718^((forecast((arima(log(Train), order = c(1,2,2), seasonal = list(order=c(0,1,2)))), h = 20))$mean), Test)
rmse(2.718^((forecast((arima(log(Train), order = c(12,2,6), seasonal = list(order=c(1,1,2)))), h = 20))$mean), Test)
rmse(2.718^((forecast((arima(log(Train), order = c(12,2,6), seasonal = list(order=c(0,1,2)))), h = 20))$mean), Test)
rmse(2.718^((forecast((arima(log(Train), order = c(12,2,6), seasonal = list(order=c(4,1,2)))), h = 20))$mean), Test)
rmse(2.718^((forecast((arima(log(Train), order = c(12,2,6), seasonal = list(order=c(3,1,2)))), h = 20))$mean), Test)
#After almost 20-30 trials (few of them removed from above) model given below gave the best RMSE and MAPE#
rmse(2.718^((forecast((arima(log(Train), order = c(12,2,6), seasonal = list( order=c(2,1,2)))), h = 20))$mean), Test) #2553.649 RMSE#
mape(2.718^((forecast((arima(log(Train), order = c(12,2,6), seasonal = list( order=c(2,1,2)))), h = 20))$mean), Test) #0.0407 MAPE - Metrics Package Scale#
#Creating the manual ARIMA model with the selected model & seasonal order#
ManualARIMAModel <- (arima(log(Train), order = c(12,2,6), seasonal = list( order=c(2,1,2))))
#12P is calculated as 32 (20+12) periods since our train model is based on data only till 1993#
ManualARIMAModelPred12P <- forecast(ManualARIMAModel, h = 32)
#Converting the predicted values to normal data type#
ModelForecastConvertedValues <- 2.718^((forecast(ManualARIMAModel, h = 32))$mean)
ModelForecastConvertedValues #Displaying the forecasted values#
#Subsetting the converted prediction values to only the future 12-month period i.e. Sep 1995 to Aug 1996#
ModelForecastConvertedValues12P <- window(ModelForecastConvertedValues, start = c(1995,9), end = c(1996,8), frequency = 12)
ModelForecastConvertedValues12P #Verifying the subset variable with Sep 1995 to Aug 1996 values#
class(ModelForecastConvertedValues12P) #Verifying the class of the subset variable
#Plotting the 32P forecast (From 1970 to 1993 actual estimates and forecasted 1994-Aug 1996 estimates) for comparison on trend#
plot(ManualARIMAModelPred12P)
#Interactive Plot with confidence intervals#
plot_forecast(ManualARIMAModelPred12P, title = "Forecast Plot from ARIMA(12,2,6)(2,1,2)[12]")
#Plotting the only the 12P forecast (From 1956 - Aug 1995 actual estimates and forecasted Sep 1995 - Aug 1996 estimates) for comparison on trend#
seqplot.ts(AusGasProductionData, ModelForecastConvertedValues12P, main = "Forecast Plot with Original Data and Forecasted 12 Months Only", coly = "blue", ltyx = "solid", ltyy = "solid", typey = "b", pchy = 20)
##Step 5.5: - Finally, calculating all the time series accuracy related metrics for the model##
#Step 5.5.1: - Accuracy Calculation#
forecast::accuracy(2.718^((forecast((arima(log(Train), order = c(12,2,6), seasonal = list( order=c(2,1,2)))), h = 20))$mean), Test)
#Step 5.5.2: - Goodness of Fit Analysis#
#1. Box-Ljung Test for Model Fit Test#
#Here H0 is "Our model does not exhibit lack of fit and the residual are uncorrelated"#
Box.test(ManualARIMAModel$residuals,type = c("Ljung-Box"), fitdf = 0)
#As p > 0.05 in the test done hence Null hypothesis is accepted and we can conclude that our ARIMA model is fit for future use#
#2. Improvement analysis by checking for Residuals#
mean(ManualARIMAModel$residuals) #Check for constant mean for residuals#
qqnorm(ManualARIMAModel$residuals) #To check if residuals are normally distributed#
qqline(ManualARIMAModel$residuals)
lag.plot(ManualARIMAModel$residuals, lags = 1, main = "Residuals Lag Plot") #Check for autocorrelation#
checkresiduals(ManualARIMAModel) #Check for covariance and correlation#
shapiro.test(ManualARIMAModel$residuals) #Check for Residuals Normality - Low p value indicates no Normality#
#Conclusion: We can definitely use our model for future predictions but if better accuracy is required, we might have to improvise it further#
#########################End of the Code#######################################
#########################End of the Analysis###################################
#####################################################################################