#ARIMA Price Analysis 2020
# See https://otexts.com/fpp2/ Hyndman's Forecasting textbook
rm(list=ls())

# Call libraries

library(tidyverse)
library (dbplyr)
library(forecast)
library(zoo)
library(tseries) # now we use this
library(readxl) # we have used this
library(writexl)
library(CADFtest)
library(FinTS)# for 'function ArchTest()'
##################
###Read in data###
##################
df <- read_excel("Annual SKJ and YFN Thai import prices.xlsx")

################
###Clean data###
################
dim(df)
head(df)
tail(df)
#Drop all other coluumns except YFT
#data.yft <- select(df,-c(2,4:8))
#data.yft = data.yft[-1,]
#colnames(data.yft) <- c("year","YFT")
#data.yft

###OR###
##call the YFT data
data.yft <- df[2:25,1:3]
colnames(data.yft) <- c("year","SKJ","YFT")
data.yft

#Check data
head(data.yft)
tail(data.yft)

#Convert to time series object
skj_ts <- ts(data.yft[,2],start=c(1997),end=c(2019),frequency=1)
skj_ts

yft_ts <- ts(data.yft[,3],start=c(1997),end=c(2019),frequency=1)
yft_ts


##Plot and Save
#windows(8,4)
#savePlot("figs\\YFT\\yft norminal price", type="jpg")
#png("figs\\SKJ\\skj norminal price.png") 
par(mfrow=c(1,2))
plot(skj_ts)

#png("figs\\YFT\\yft norminal price.png") 
plot(yft_ts)
dev.off()

########################
###Stationarity tests###
########################

adf_skj <- adf.test(skj_ts)
adf_skj # p-value = 0.7102 is more than 5% significance level, hence FAIL to reject H0- (Non Stationary)- Variable is Non Stationary 

adf_yft <- adf.test(yft_ts)
adf_yft # p-value = 0.5649 is more than 5% significance level, hence FAIL to reject H0- (Non Stationary)- Variable is Non Stationary 


##################
###Differencing###
##################
# Take the first difference
###SKJ###
d_skj <- diff(skj_ts, lag = 1)
adf_dskj <- adf.test(d_skj)
adf_dskj ###p-value = 0.03814 is LESS than 5% significance level, hence REJECT H0 -(Non Stationary)- So Variable is now stationary
plot(d_skj) 
dev.off()

###YFT###
d_yft <- diff(yft_ts, lag = 1)
adf_dyft <- adf.test(d_yft)
adf_dyft ###p-value = 0.04958 is very close to 5% significance level, hence REJECT H0 -(Non Stationary)- So Variable is now stationary
plot(d_yft)  

#Second differencing ONLT TO YFT- NO NEED
#d2_yft <- diff(d_yft, lag = 1)
#d2_yft
#adf_dyft2 <- adf.test(d2_yft)
#adf_dyft2 #p-value = 0.01102 is NOW less than 5% significance level, hence REJECT H0 -(Non Stationary)- so the variables are now Stationary 
#plot(d2_yft)  
#dev.off()


############################################################################
###ACF (Auto Correlation Function)& Pacf- Partial Autocrrelation Function###
###########################################################################

####SKT####

##Note: For a stationary time series, the ACF will drop to zero relatively quickly, while the ACF of non-stationary data decreases slowly. 
##Also, for non-stationary data, the value of r1 is oftern large and postive   
## ACF drop to zero relatively quickly indicate the variable is stationary 

## Testing for autocorrelation by ploting ACF
#windows(10,6)
##png("figs\\YFT\\ACF_yft.png")##If you wish to save the plot
#windows(10,6)
par(mfrow=c(1,2))
Acf(skj_ts) # Level data
#Acf(d_skj) # First order differencing 
Pacf(skj_ts)
#savePlot("figs\\skj acf & pacf", type="jpg")
dev.off()

# From both ACF and PACF plots, the data may follow an ARIMA(p,d,0)model: The reseasons as follows

#The data may follow an ARIMA(p,d,0) model if the ACF and PACF plots of the differenced data show the following patterns:

#the ACF is exponentially decaying or sinusoidal;
#there is a significant spike at lag p in the PACF, but none beyond lag p

# In the PACF, there are 2 significant spikes and no significant spikes thereafter. 
# In the ACF, there are 3 signigicant spikes outside the limits 
# ###Hence ACF suggest of p=2 i.e 2 spikes and d=1(No of differencing needed for stationarity) = ARIMA(2,1,0)

##NOTES##
###ARIMA notes: ARIMA(p,d,q)" model where 
#p is the number of autoregressive terms, 
#d is the number of nonseasonal differences needed for stationarity, 
#q is the number of lagged forecast errors in the prediction equation.

###OR###
###ARIMA(AR,I,MA) where 
#p= AR(Auto Regressive)- look at PACF
#d= I (Intergrative part of the model)
#q= MA Moving Average - look at ACF


#################
##FIT THE MODEL##
################
auto.arima(skj_ts)#Suggest Arima (0,1,0) might be appropriate 

fit_skj <- arima(skj_ts, order=c(2,1,0))

#################
###FORECASTING###
#################
#windows(10,6)
#png("figs\\YFT\\forecast_yft.png", width=600, height=400)##If you wish to save the plot
f_skj <- plot(forecast(fit_skj), xlab = "Year", ylab ="SKJ Nominal Price")
#grid(lty=1, col=gray(.8)) # add a grid
lines(skj_ts, col=4) 
#savePlot("figs\\Arima(2,1,0)", type="jpg")
dev.off()

###USE AIC Akaike's An Information Criterion to select the best model- the lower the AIC the more reliable is the model
AIC(fit_skj)

#Arima (1,1,0) AIC 316.4521
#Arima (1,1,1) AIC 314.66
#Arima (2,1,0) AIC 308.0778 ## Preferred Model with lower AIC
#Arima (2,1,1) AIC 309.9044
#Arima (2,1,2) AIC 311.6384
#Arima (2,1,3) AIC 312.1297
#Arima (3,1,0) AIC 309.8609
#Arima (3,1,1) AIC 311.8473
#Arima (4,1,0) AIC 311.8126
#Arima (5,1,0) AIC 313.3204
#Arima (6,1,0) AIC 314.7135

####YFT####
par(mfrow=c(1,2))
Acf(yft_ts) # Level data
#Acf(d_yft) # First order differencing 
#Acf(d2_yft) # Second order differencing 
Pacf(yft_ts)
dev.off()

# From both ACF and PACF plots, the data may follow an ARIMA(p,d,0)model: The reseasons as follows

#The data may follow an ARIMA(p,d,0) model if the ACF and PACF plots of the differenced data show the following patterns:
#the ACF is exponentially decaying or sinusoidal;
#there is a significant spike at lag p in the PACF, but none beyond lag p

# In the PACF, there is 1 significant spike and no significant spikes thereafter. 
# In the ACF, there are 2 signigicant spikes 
# ###Hence ACF suggest of p=1 i.e 1 spike and d=1(No of differencing needed for stationarity) = ARIMA(1,1,0)

#################
##FIT THE MODEL##
################

auto.arima(yft_ts)#Suggest Arima (0,1,0) might be appropriate 

#fit_skj <- arima(yft_ts, order=c(2,0,0))
fit_yft <- arima(yft_ts, order=c(2,1,2))
#################
###FORECASTING###
#################
#windows(10,6)
#png("figs\\YFT\\forecast_yft.png", width=600, height=400)##If you wish to save the plot
f_yft <-plot(forecast(fit_yft), xlab = "Year", ylab ="YFT Nominal Price")
#grid(lty=1, col=gray(.8)) # add a grid
lines(yft_ts, col=4) 
#savePlot("figs\\Arima(2,1,2)", type="jpg")
dev.off()

###USE AIC Akaike's An Information Criterion to select the best model- the lower the AIC the more reliable is the model
AIC(fit_yft)

#Arima (1,1,0) AIC 313.0208
#Arima (1,1,1) AIC 311.5722
#Arima (1,1,2) AIC 311.4796
#Arima (2,1,0) AIC 309.1947 ### Preffered model with lower AICs
#Arima (2,1,1) AIC 310.5486
#Arima (2,1,2) AIC 309.8037 #### preffered model with more variations of prices
#Arima (2,1,3) AIC 311.8013
#Arima (3,1,0) AIC 310.4148
#Arima (4,1,0) AIC 312.3749 
#Arima (5,1,0) AIC 313.5243



####################################
##PLOT BOTH SKJ & YFT FORECAST PLOTS
####################################
#windows(10,6)
par(mfrow=c(1,2))

f_skj <- plot(forecast(fit_skj), xlab = "Year", ylab ="SKJ Nominal Price")
#grid(lty=1, col=gray(.8)) # add a grid
lines(skj_ts, col=4) 

f_yft <-plot(forecast(fit_yft), xlab = "Year", ylab ="YFT Nominal Price")
#grid(lty=1, col=gray(.8)) # add a grid
lines(yft_ts, col=4) 

#savePlot("figs\\xNominal Tuna Price Forecast", type="png")
#dev.off()





