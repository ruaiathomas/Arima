#ARIMA Analysis 2020
# See https://otexts.com/fpp2/ Hyndman's Forecasting textbook
rm(list=ls())

# Call libraries

library(TSstudio)
library(ggplot2)
library(tidyverse)
library (dbplyr)
library(tidyr)
library(forecast)
library(zoo)
library(tseries) # now we use this
library(readxl) # we have used this
library(writexl)
##################
###Read in FROZEN data###
##################
df <- read_csv("SB frozen data.csv")

################
###Clean data###
################
dim(df)
head(df)
tail(df)
#####################################################
####################################################
####Run the forecasting for FROZEN Products#########
###################################################
#Convert to time series object with year ended 2021 for frozen products
Exfr_ts <- ts(df[,2],start=c(2012),frequency=12)
Exfr_ts

##Plotting
plot(Exfr_ts)
autoplot(Exfr_ts)
ts_plot(Exfr_ts)
dev.off()

#Plot and Save
#windows(8,4)
#savePlot("figs\\YFT\\yft norminal price", type="jpg")
#png("figs\\SKJ\\skj norminal price.png") 
#par(mfrow=c(1,2))


#png("figs\\YFT\\yft norminal price.png") 
#plot(yft_ts)

##call the pre Export data from 2012 only to 2019
pre_frozen <- df[01:96,2]
#colnames(pre_data) <- c("year","MGO")
pre_frozen
head(pre_frozen)
tail(pre_frozen)

#Convert to time series object with year ended 2019
prefr_ts <- ts(pre_frozen[,1],start=c(2012),frequency=12)
prefr_ts
ts_plot(prefr_ts)#Using TSstudio package
plot(prefr_ts)
dev.off()
#Plot 2020 frozen data
post_frozen <- df[97:120,2]
post_frozen
postfr_ts <- ts(post_frozen[,1],start=c(2020),frequency=12)
postfr_ts
ts_plot(postfr_ts)#Using TSstudio package
plot(postfr_ts)
dev.off()
########################
###Stationarity tests###
########################

adf_pre <- adf.test(prefr_ts)
adf_pre # p-value = 0.01 is less than 5% significance level, hence REJECT H0 -(Non Stationary)- So Variable is stationary

############################################################################
###ACF (Auto Correlation Function)& Pacf- Partial Autocrrelation Function###
###########################################################################

####MGO####

##Note: For a stationary time series, the ACF will drop to zero relatively quickly, while the ACF of non-stationary data decreases slowly. 
##Also, for non-stationary data, the value of r1 is often large and positive   
## ACF drop to zero relatively quickly indicate the variable is stationary 

## Testing for autocorrelation by plotting ACF
#windows(10,6)
#png("figs\\Fr_volumes acf & pacf.png", width=600, height=400)##If you wish to save the plot
par(mfrow=c(1,2))
Acf(prefr_ts) # ACF Level data
Pacf(prefr_ts) # PACF Level data
#savePlot("figs\\2018 skj acf & pacf", type="jpg")
dev.off()
#################################
##FINDING the APPROPRIATE MODEL##
#################################
# From both ACF and PACF plots, the significant spike at lag 2 suggests a non-seasonal MA(2)
# The significant spike in lag 12 suggest a seasonal MA(1) component
# This suggest ARIMA(0,0,2) (0,0,1)[12]

# If we had started with the PACF, PACF plot will be used to select the non seasonal part and the ACF to select the seasonal part of the model
#This suggest ARIMA(Arima(1,0,0) (0,0,2)
#################
##FIT THE MODEL##
################
auto.arima(prefr_ts)#Suggest Arima (0,1,1) (1,0,0) might be appropriate 

####arima(p,d,0) based on the ACF and PACF plots
fit_pre <- arima(prefr_ts, order=c(2,0,0),seasonal=c(0,0,2))

#################
###FORECASTING###
#################
#windows(10,6)
#png("figs\\Frozen Export Volume Arima(2,0,0)(0,0,2)[12].png", width=600, height=400)##If you wish to save the plot
#windows(8,6)
f_pre <- plot(forecast(fit_pre), xlab = "Year", ylab ="Frozen Export (Metric Tonnes)")
#grid(lty=1, col=gray(.8)) # add a grid
#lines(pref_ts, col=4) 
#savePlot("figs//Frozen Export volumes Arima(2,0,0)(0,0,2)[12]", type="jpg")
dev.off()

###USE AIC Akaike's An Information Criterion to select the best model- the lower the AIC the more reliable is the model
AIC(fit_pre)
#fitted(fit_pre)

#Arima (0,0,2) (0,0,1) AIC 1577.339
#Arima (0,0,2) (0,0,2) AIC 1576.384
#Arima (0,0,2) (0,0,3) AIC 1577.553
#Arima (1,0,0) (0,0,1) AIC 1581.666
#Arima (1,0,0) (0,0,2) AIC 1581.49
#Arima (2,0,0) (0,0,1) AIC 1575.967
#Arima (2,0,0) (0,0,2) AIC 1575.944 # Low AIC best model
#Arima (2,0,0) (0,0,3) AIC 1577.13

##Checking rediduals### 
##Note: for (2,0,0) (0,0,2)[12] ACF plots of the residuals shows that all autocorrelation are within the threshold limits, 
##, then the residuals are behaving like white noise 
checkresiduals(fit_pre)
dev.off()
###Ljung Box test for Autocorrelation of the residuals: H0: No Autocorrelation; H1: Autocrrelation 
##p-value is 0.9286 GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation

###Extract fitted values###
fitted(fit_pre)
f_pre

# export fitted values to excel
frozenf <-data.frame(f_pre)
                                      
write_xlsx(frozenf,"C:\\Users\\thomas.ruaia\\Documents\\FFA\\MFMR\\MFMR\\R\\frozenv Arima(2,0,0)(0,0,2)[12].xlsx")

#######################################
###Plot forecast vs Actual values###### 
#######################################

#Actual values
actual_ts <- ts(postfr_ts[,1],start=c(2020),frequency=12)
actual_ts
plot(actual_ts)# title = "Thai Volume Export 2020", Xtitle = "Time", Ytitle = "Metric tonne")#using TSStudio and add slider
dev.off()

# Extract mean values from Arima (2,0,0) (0,0,2) for plotting
f_fr <- read_excel("frozenv Arima(2,0,0)(0,0,2)[12].xlsx")
#Call for mean values only

pref_fr <- f_fr[01:24,1]
#colnames(pref_fr) <- c("year","Forecast values")
head(pref_fr)
tail(pref_fr)

#Convert forecast values to time series ended 2021
f_frts <- ts(pref_fr[,1],start=c(2020),frequency=12)
plot(f_frts)
dev.off()
#Combine the graphs
comb <- zoo(cbind(actual_ts, f_frts))

#Plot the combine graphs
#jpeg("figs\\Thai Frozen SB Export.jpeg")
ts_plot(comb, title = "Solomon Is Frozen Export Volume Vs Forecast Data", Xtitle = "Time", Ytitle = " Metric tonne") 
dev.off()


#####################################################
####################################################
####Run the forecasting for FROZEN EXPORT VALUES#########
###################################################
#Convert to time series object with year ended 2021 for frozen export values
vafr_ts <- ts(df[,3],start=c(2012),frequency=12)
vafr_ts

##Plotting
plot(vafr_ts)
ts_plot(vafr_ts)
autoplot(vafr_ts)
dev.off()

#Plot and Save
#png("figs\\YFT\\yft norminal price.png") 
#plot(yft_ts)

##call the pre Export data from 2012 only to 2019
pre_valfrozen <- df[01:96,3]
#colnames(pre_data) <- c("year","MGO")
pre_valfrozen
head(pre_valfrozen)
tail(pre_valfrozen)

#Convert to time series object with year ended 2019
prevfr_ts <- ts(pre_valfrozen[,1],start=c(2012),frequency=12)
prevfr_ts
ts_plot(prevfr_ts)#Using TSstudio package
plot(prevfr_ts)
dev.off()
#Plot 2020 frozen export values data
post_valfrozen <- df[97:120,3]
post_valfrozen
postvfr_ts <- ts(post_valfrozen[,1],start=c(2020),frequency=12)
postvfr_ts
ts_plot(postvfr_ts)#Using TSstudio package
plot(postvfr_ts)
dev.off()
########################
###Stationarity tests###
########################

adf_vpre <- adf.test(prevfr_ts)
adf_vpre # p-value = 0.01921 is less than 5% significance level, hence REJECT H0 -(Non Stationary)- So Variable is stationary

############################################################################
###ACF (Auto Correlation Function)& Pacf- Partial Autocrrelation Function###
###########################################################################

##Note: For a stationary time series, the ACF will drop to zero relatively quickly, while the ACF of non-stationary data decreases slowly. 
##Also, for non-stationary data, the value of r1 is often large and positive   
## ACF drop to zero relatively quickly indicate the variable is stationary 

## Testing for autocorrelation by plotting ACF

#png("figs\\Fr_values acf & pacf.png", width=600, height=400)##If you wish to save the plot
par(mfrow=c(1,2))
Acf(prevfr_ts) # ACF Level data
Pacf(prevfr_ts) # PACF Level data
dev.off()
#################################
##FINDING the APPROPRIATE MODEL##
#################################
# From both ACF and PACF plots, the significant spike at lag 2 suggests a non-seasonal MA(2)
# The significant spike in lag 12 in ACF suggest a seasonal MA(1) component
# This suggest ARIMA(0,0,2) (0,0,1)[12]

# If we had started with the PACF, PACF plot will be used to select the non seasonal part and the ACF to select the seasonal part of the model
#This suggest ARIMA(Arima(2,0,0) (0,0,2)
#################
##FIT THE MODEL##
################

# From both ACF and PACF plots, the data may follow an ARIMA(0,d,q)model: The reasons as follows

#The data may follow an ARIMA(p,d,0) model if the ACF and PACF plots of the differenced data show the following patterns:

#the PACF is exponentially decaying or sinusoidal
#there is a significant spike at lag p in the ACF, but none beyond lag p

# In the PACF, there is 1 significant spike that passes the threshold. 
# In the ACF, there are quickly decaying  and recovered later 

###Hence PACF suggest AR(1), ACF suggest of q=1 i.e 1 spike and d=0(No of differencing needed for stationarity) = ARIMA(1,0,1)

#####NONSEASONAL ARIMA MODEL ARIMA(p,d,q)
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
auto.arima(prevfr_ts)#Suggest Arima (0,1,1) might be appropriate 

####arima(p,d,0) based on the ACF and PACF plots
fit_vpre <- arima(prevfr_ts, order=c(2,0,0),seasonal=c(0,0,2))

#################
###FORECASTING###
#################
#windows(10,6)
#png("figs\\Fresh Export Values Arima(2,0,0)(0,02)[12].png", width=600, height=400)##If you wish to save the plot
f_vpre <- plot(forecast(fit_vpre), xlab = "Year", ylab ="Frozen Export Value (US$ million)")
dev.off()

###USE AIC Akaike's An Information Criterion to select the best model- the lower the AIC the more reliable is the model
AIC(fit_vpre)
#fitted(fit_pre)
#Arima (0,0,2) (0,0,1) AIC 366.0944
#Arima (0,0,2) (0,0,2) AIC 365.3139
#Arima (0,0,2) (0,0,3) AIC 367.258
#Arima (1,0,0) (0,0,1) AIC 367.993
#Arima (1,0,0) (0,0,2) AIC 367.2938
#Arima (2,0,0) (0,0,1) AIC 365.6812
#Arima (2,0,0) (0,0,2) AIC 365.2404 # low AIC- best model
#Arima (2,0,0) (0,0,3) AIC 367.2241

###Extract fitted values###
fitted(fit_vpre)
f_vpre


##Checking rediduals### 
##Note: for ARIMA (3,0,0) ACF plots of the residuals shows that all autocorrelation are within the threshold limits, 
##, then the residuals are behaving like white noise 
checkresiduals(fit_vpre)
dev.off()
###Ljung Box test for Autocorrelation of the residuals: H0: No Autocorrelation; H1: Autocrrelation 
##p-value for Arima (2,0,0) (0,0,2) is 0.8602 GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation

########################################
###Plot forecast vs Actual values###### 
#######################################

# export fitted values to excel
vfrozenf <-data.frame(f_vpre)
head(vfrozenf)
head(vfrozenf)
write_xlsx(vfrozenf,"C:\\Users\\thomas.ruaia\\Documents\\FFA\\MFMR\\MFMR\\R\\frvalues Arima(2,0,0)(0,0,2)[12].xlsx")

#######################################
###Plot forecast vs Actual values###### 
#######################################

#Actual values
actual_vfts <- ts(postvfr_ts[,1],start=c(2020),frequency=12)
actual_vfts
plot(actual_vfts)# title = "Thai Volume Export 2020", Xtitle = "Time", Ytitle = "Metric tonne")#using TSStudio and add slider
dev.off()

# Extract mean values from Arima (2,0,0) (0,0,2) for plotting
f_vfr <- read_excel("frvalues Arima(2,0,0)(0,0,2)[12].xlsx")
#Select only the mean values
pref_vfr <- f_vfr[01:24,1]
#colnames(pref_fr) <- c("year","Forecast values")
head(pref_vfr)
tail(pref_vfr)

#Convert forecast values to time series ended 2021
f_vfrts <- ts(pref_vfr[,1],start=c(2020),frequency=12)
f_vfrts
plot(f_vfrts)
dev.off()
#Combine the graphs
combv <- zoo(cbind(actual_vfts, f_vfrts))

#Plot the combine graphs
#jpeg("figs\\Thai Frozen SB Export.jpeg")
ts_plot(combv, title = "Solomon Is Frozen Export Values Vs Forecast Data", Xtitle = "Time", Ytitle = " Metric tonne") 
dev.off()
#####################################################
####################################################
####Run the forecasting for FRESH Products#########
###################################################
##################
###Read in FRESH data###
##################

dfre <- read_csv("SB fresh data.csv")

################
###Clean data###
################
dfre
dim(dfre)
head(dfre)
tail(dfre)

#Convert to time series object with year ended 2021 for frozen products
Exfre_ts <- ts(dfre[,2],start=c(2012),frequency=12)
Exfre_ts

##Plotting
#png("figs\\fresh export.png") 
plot(Exfre_ts)
autoplot(Exfre_ts)
ts_plot(Exfre_ts)
dev.off()

##call the pre Export data from 2012 only to 2019
pre_fresh <- dfre[01:96,2]
#colnames(pre_data) <- c("year","MGO")
pre_fresh
head(pre_fresh)
tail(pre_fresh)

#Convert to time series object with year ended 2019
prefre_ts <- ts(pre_fresh[,1],start=c(2012),frequency=12)
prefre_ts
ts_plot(prefre_ts)#Using TSstudio package
plot(prefre_ts)
dev.off()
#Plot 2020 fresh data
post_fresh <- dfre[97:120,2]
post_fresh
postfre_ts <- ts(post_fresh[,1],start=c(2020),frequency=12)
postfre_ts
ts_plot(postfre_ts)#Using TSstudio package
plot(postfre_ts)

########################
###Stationarity tests###
########################

adf_prefresh <- adf.test(prefre_ts)
adf_prefresh # p-value = 0.04678 is less than 5% significance level, hence DO NOT REJECT H0 -(Non Stationary)- So Variable is stationary
dev.off()

############################################################################
###ACF (Auto Correlation Function)& Pacf- Partial Autocrrelation Function###
###########################################################################

####MGO####

##Note: For a stationary time series, the ACF will drop to zero relatively quickly, while the ACF of non-stationary data decreases slowly. 
##Also, for non-stationary data, the value of r1 is often large and positive   
## ACF drop to zero relatively quickly indicate the variable is stationary 

## Testing for autocorrelation by plotting ACF

#png("figs\\fresh volume acf & pacf.png", width=600, height=400)##If you wish to save the plot
par(mfrow=c(1,2))
Acf(prefre_ts) # ACF Level data
Pacf(prefre_ts) # PACF Level data
dev.off()

#################################
##FINDING the APPROPRIATE MODEL##
#################################
# From both ACF and PACF plots, the significant spike at lag 1 suggests a non-seasonal MA(1)
# The significant spike in lag 12 suggest a seasonal MA(1) component
# This suggest ARIMA(0,0,1) (0,0,1)[12]

# If we had started with the PACF, PACF plot will be used to select the non seasonal part and the ACF to select the seasonal part of the model
#This suggest ARIMA(Arima(1,0,0) (0,0,1)


##Additional notes
# From both ACF and PACF plots, the data may follow an ARIMA(0,d,q)model: The reasons as follows

#The data may follow an ARIMA(p,d,0) model if the ACF and PACF plots of the differenced data show the following patterns:

#the PACF is exponentially decaying or sinusoidal
#there is a significant spike at lag p in the ACF, but none beyond lag p

# In the PACF, there is 1 significant spike that passes the threshold. 
# In the ACF, there are quickly decaying  and recovered later 

###Hence PACF suggest AR(1), ACF suggest of q=1 i.e 1 spike and d=0(No of differencing needed for stationarity) = ARIMA(1,0,1)

#####NONSEASONAL ARIMA MODEL ARIMA(p,d,q)
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
auto.arima(prefre_ts)#Suggest Arima (2,1,2) might be appropriate 

####arima(p,d,0) based on the ACF and PACF plots
fit_prefresh <- arima(prefre_ts, order=c(2,1,0))

#################
###FORECASTING###
#################
#windows(10,6)
#png("figs\\Fresh Export Arima(3,0,0).png", width=600, height=400)##If you wish to save the plot
#windows(8,6)
f_prefresh <- plot(forecast(fit_prefresh), xlab = "Year", ylab ="Fresh Export (Metric Tonnes)")
#grid(lty=1, col=gray(.8)) # add a grid
#lines(pref_ts, col=4) 
#savePlot("figs//Fresh Export Arima(3,0,0)", type="jpg")
#dev.off()

###USE AIC Akaike's An Information Criterion to select the best model- the lower the AIC the more reliable is the model
AIC(fit_prefresh)
#fitted(fit_pre)
#seasonal
#Arima (0,0,1) (0,0,1) AIC 803.7209
#Arima (0,0,1) (0,0,2) AIC 805.1395
#Arima (0,0,1) (0,0,3) AIC 804.9634
#Arima (1,0,0) (0,0,1) AIC 768.2488
#Arima (1,0,0) (0,0,2) AIC 769.5488
#Arima (2,0,0) (0,0,1) AIC 765.426
#Arima (2,0,0) (0,0,2) AIC 766.5726
#Arima (2,0,0) (0,0,3) AIC 768.4811

#Non seasonal
#Arima (1,0,1) AIC 763.819
#Arima (1,0,2) AIC 765.732
#Arima (1,0,3) AIC 758.3328
#Arima (2,0,0) AIC 767.4973
#Arima (2,0,1) AIC 765.8066
#Arima (2,0,2) AIC 760.6541
#Arima (2,0,3) AIC 759.772
#Arima (2,0,4) AIC 759.0948 
#Arima (2,0,5) AIC 760.3147
#Arima (3,0,0) AIC 757.9307
#Arima (3,0,1) AIC 758.2669
#Arima (3,0,2) AIC 756.0179
#Arima (3,0,3) AIC 757.2728
#Arima (3,0,4) AIC 752.3469# Low AIC with d=0 no differencing 
#Arima (3,0,5) AIC 753.6643
#Arima (3,0,6) AIC 753.8494
#Arima (3,0,7) AIC 755.8259
#Arima (2,1,0) AIC 748.7553 # Low AIC and mean values all positive
#Arima (2,1,1) AIC 749.4296
#Arima (2,1,2) AIC 747.9298 # Low AIC- preferred model but ACF pass threshold limits but onea mean value is negative
#Arima (2,1,3) AIC 749.8656
#Arima (3,1,0) AIC 748.3601
#Arima (3,1,1) AIC 749.9774
#Arima (3,1,2) AIC 749.8622
#Arima (3,1,3) AIC 751.8598
###Extract fitted values###
fitted(fit_prefresh)
f_prefresh


##Checking rediduals### 
##Note: for ARIMA (2,1,2) ACF plots of the residuals shows that all autocorrelation are within the threshold limits except one, 
##, then the residuals are behaving like white noise 
checkresiduals(fit_prefresh)
dev.off()
###Ljung Box test for Autocorrelation of the residuals: H0: No Autocorrelation; H1: Autocrrelation 
##Arima(3,0,4) p-value is 0.3618 is GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation and acf plot of the residuals are within the threshold limits
##Arima(2,1,0) p-value is 0.1713 is GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation but mean values is positive
##Arima(2,1,2) p-value is 0.267  is GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation but mean values is negative

#Export forecast values to excel
fref <-data.frame(f_prefresh)
head(fref)
tail(fref)
write_xlsx(fref,"C:\\Users\\thomas.ruaia\\Documents\\FFA\\MFMR\\MFMR\\R\\frev Arima(2,1,0).xlsx")
#write_xlsx(fref,"C:\\Users\\thomas.ruaia\\Documents\\FFA\\MFMR\\MFMR\\R\\frev Arima(3,0,4).xlsx")

########################################
###Plot forecast vs Actual values###### 
#######################################

#Actual values
actual_fresh <- ts(postfre_ts[,1],start=c(2020),frequency=12)
actual_fresh
plot(actual_fresh)# title = "Fresh Volume Export 2020", Xtitle = "Time", Ytitle = "Metric tonne")#using TSStudio and add slider

# Extract mean values from Arima (2,0,0) (0,0,2) for plotting
f_fresh <- read_excel("frev Arima(2,1,0).xlsx")
#Call for mean values only

pref_fresh <- f_fresh[01:24,1]
#colnames(pref_fr) <- c("year","Forecast values")
head(pref_fresh)
tail(pref_fresh)

#Convert forecast values to time series ended 2021
ffresh_ts <- ts(pref_fresh[,1],start=c(2020),frequency=12)
ffresh_ts
plot(ffresh_ts)
dev.off()
#Combine the graphs
comb_fresh <- zoo(cbind(actual_fresh, ffresh_ts))

#Plot the combine graphs
#jpeg("figs\\Thai Frozen SB Export.jpeg")
ts_plot(comb_fresh, title = "Solomon Is Fresh Export Volume Values Vs Forecast Data", Xtitle = "Time", Ytitle = " Metric tonne") 
dev.off()

#####################################################
####################################################
####Run the forecasting for FRESH EXPORT VALUES#########
###################################################
#Convert to time series object with year ended 2021 for frozen export values
vafre_ts <- ts(dfre[,3],start=c(2012),frequency=12)
vafre_ts

##Plotting
plot(vafre_ts)
autoplot(vafre_ts)
ts_plot(vafre_ts)
dev.off()

#Plot and Save
#png("figs\\YFT\\yft norminal price.png") 
#plot(yft_ts)

##call the pre Export data from 2012 only to 2019
pre_valfresh <- dfre[01:96,3]
#colnames(pre_data) <- c("year","MGO")
pre_valfresh
head(pre_valfresh)
tail(pre_valfresh)

#Convert to time series object with year ended 2019
prevfre_ts <- ts(pre_valfresh[,1],start=c(2012),frequency=12)
prevfre_ts
ts_plot(prevfre_ts)#Using TSstudio package
plot(prevfre_ts)
dev.off()
#Plot 2020 frozen data
post_valfresh <- dfre[97:120,3]
post_valfresh
postvfre_ts <- ts(post_valfresh[,1],start=c(2020),frequency=12)
postvfre_ts
ts_plot(postvfre_ts)#Using TSstudio package
plot(postvfre_ts)
dev.off()
########################
###Stationarity tests###
########################

adf_vpre <- adf.test(prevfre_ts)
adf_vpre # p-value = 0.0345 is less than 5% significance level, hence REJECT H0 -(Non Stationary)- So Variable is stationary

############################################################################
###ACF (Auto Correlation Function)& Pacf- Partial Autocrrelation Function###
###########################################################################
#png("figs\\fresh volume values acf & pacf.png", width=600, height=400)##If you wish to save the plot
par(mfrow=c(1,2))
Acf(prevfre_ts) # ACF Level data
Pacf(prevfre_ts) # PACF Level data
dev.off()

#################################
##FINDING the APPROPRIATE MODEL##
#################################
# From both ACF and PACF plots, the significant spike at lag 1 suggests a non-seasonal MA(1)
# The significant spike in lag 12 suggest a seasonal MA(1) component
# This suggest ARIMA(0,0,1) (0,0,1)[12]

# If we had started with the PACF, PACF plot will be used to select the non seasonal part and the ACF to select the seasonal part of the model
#This suggest Arima(1,0,0) (0,0,1)

###Additional notes

# From both ACF and PACF plots, the data may follow an ARIMA(0,d,q)model: The reasons as follows

#The data may follow an ARIMA(p,d,0) model if the ACF and PACF plots of the differenced data show the following patterns:

#the PACF is exponentially decaying or sinusoidal
#there is a significant spike at lag p in the ACF, but none beyond lag p

# In the PACF, there is 1 significant spike that passes the threshold. 
# In the ACF, there are quickly decaying  and recovered later 

###Hence PACF suggest AR(1), ACF suggest of q=1 i.e 1 spike and d=0(No of differencing needed for stationarity) = ARIMA(1,0,1)

#####NONSEASONAL ARIMA MODEL ARIMA(p,d,q)
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
auto.arima(prevfre_ts)#Suggest Arima (0,1,2) (0,0,1)[12] might be appropriate 

####arima(p,d,0) based on the ACF and PACF plots
fit_vpre <- arima(prevfre_ts, order=c(0,1,2), seasonal=c(0,0,1))

#################
###FORECASTING###
#################
#windows(10,6)
#png("figs\\Fresh Export Arima(3,0,0).png", width=600, height=400)##If you wish to save the plot
#windows(8,6)
f_vpre <- plot(forecast(fit_vpre), xlab = "Year", ylab ="Fresh Export Value (US$ million)")
#grid(lty=1, col=gray(.8)) # add a grid
#lines(pref_ts, col=4) 
#savePlot("figs//Fresh Export Arima(3,0,0)", type="jpg")
#dev.off()

###USE AIC Akaike's An Information Criterion to select the best model- the lower the AIC the more reliable is the model
AIC(fit_vpre)
#fitted(fit_pre)
#seasonal
#Arima (0,0,1) (0,0,1) AIC -108.058
#Arima (0,0,1) (0,0,2) AIC -106.1501
#Arima (0,0,1) (0,0,3) AIC -105.3067
#Arima (0,1,1) (0,0,1) AIC -132.8195
#Arima (0,1,2) (0,0,1) AIC -134.1419 Suggested by auto.arima and has the lowest AIC relative to other models#The mean values- some are negative      
#Arima (0,1,3) (0,0,1) AIC -132.4244
#Arima (1,0,0) (0,0,1) AIC -133.4264
#Arima (1,0,0) (0,0,2) AIC -131.5726
#Arima (2,0,0) (0,0,1) AIC -133.3407
#Arima (2,0,0) (0,0,2) AIC -131.717

###Extract fitted values###
fitted(fit_vpre)
f_vpre


##Checking rediduals### 
##Note: for ARIMA (3,0,0) ACF plots of the residuals shows that all autocorrelation are within the threshold limits, 
##, then the residuals are behaving like white noise 
checkresiduals(fit_vpre)
dev.off()
###Ljung Box test for Autocorrelation of the residuals: H0: No Autocorrelation; H1: Autocrrelation 
##p-value for Arima (0,1,2) (0,0,1) is 0.1491 isGREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation

#Export forecast values to excel
fresh_values <-data.frame(f_vpre)
head(f_vpre)
tail(f_vpre)
write_xlsx(fresh_values,"C:\\Users\\thomas.ruaia\\Documents\\FFA\\MFMR\\MFMR\\R\\fresh_values Arima(0,1,2)(0,0,l)[12].xlsx")

########################################
###Plot forecast vs Actual values###### 
#######################################

#Actual values
actual_fresh <- ts(postfre_ts[,1],start=c(2020),frequency=12)
actual_fresh
plot(actual_fresh)# title = "Fresh Volume Export 2020", Xtitle = "Time", Ytitle = "Metric tonne")#using TSStudio and add slider

# Extract mean values from Arima (2,0,0) (0,0,2) for plotting
f_fresh <- read_excel("frev Arima(2,1,0).xlsx")
#Call for mean values only

pref_fresh <- f_fresh[01:24,1]
#colnames(pref_fr) <- c("year","Forecast values")
head(pref_fresh)
tail(pref_fresh)

#Convert forecast values to time series ended 2021
ffresh_ts <- ts(pref_fresh[,1],start=c(2020),frequency=12)
ffresh_ts
plot(ffresh_ts)
dev.off()
#Combine the graphs
comb_fresh <- zoo(cbind(actual_fresh, ffresh_ts))

#Plot the combine graphs
#jpeg("figs\\Thai Frozen SB Export.jpeg")
ts_plot(comb_fresh, title = "Solomon Is Fresh Export Volume Values Vs Forecast Data", Xtitle = "Time", Ytitle = " Metric tonne") 
dev.off()

#####################################################
####################################################
####Run the forecasting for LOINS Products#########
###################################################
##################
###Read in LOINS data###
##################
dfl <- read_csv("SB loins data.csv")

################
###Clean data###
################
dim(dfl)
head(dfl)
tail(dfl)

#Convert to time series object with year ended 2021 for frozen products
loin_ts <- ts(dfl[,2],start=c(2012),frequency=12)
loin_ts

##Plotting
plot(loin_ts)
ts_plot(loin_ts)
autoplot(loin_ts)
dev.off()

#Plot and Save
#png("figs\\YFT\\yft norminal price.png") 
#plot(yft_ts)

##call the pre Export data from 2012 only to 2019
pre_loin <- dfl[01:96,2]
#colnames(pre_data) <- c("year","MGO")
pre_loin
head(pre_loin)
tail(pre_loin)

#Convert to time series object with year ended 2019
preloin_ts <- ts(pre_loin[,1],start=c(2012),frequency=12)
preloin_ts
ts_plot(preloin_ts)#Using TSstudio package
plot(preloin_ts)
dev.off()
#Plot 2020 and 2021 loin data
post_loin <- dfl[97:114,2]
post_loin
#Check data
head(post_loin)
tail(post_loin)
postloin_ts <- ts(post_loin[,1],start=c(2020),frequency=12)
postloin_ts
ts_plot(postloin_ts)#Using TSstudio package
plot(postloin_ts)
dev.off()

########################
###Stationarity tests###
########################

adf_preloin <- adf.test(preloin_ts)
adf_preloin # p-value = 0.01 is less than 5% significance level, hence DO NOT REJECT H0 -(Non Stationary)- So Variable is stationary

############################################################################
###ACF (Auto Correlation Function)& Pacf- Partial Autocrrelation Function###
###########################################################################

#png("figs\\loins volume acf & pacf.png", width=600, height=400)##If you wish to save the plot
par(mfrow=c(1,2))
Acf(preloin_ts) # ACF Level data
Pacf(preloin_ts) # PACF Level data
dev.off()

#################################
##FINDING the APPROPRIATE MODEL##
#################################
# From both ACF and PACF plots, the significant spike at lag 12 suggests a non-seasonal MA(12) 
# The significant spike in lag 12 also suggest a seasonal MA(1) component
# This suggest ARIMA(0,0,12) (0,0,1)[12]

# If we had started with the PACF, PACF plot will be used to select the non seasonal part and the ACF to select the seasonal part of the model
#This suggest Arima(7,0,0) (0,0,1)

###Additional notes

# From both ACF and PACF plots, the data may follow an ARIMA(0,d,q)model: The reasons as follows

#The data may follow an ARIMA(p,d,0) model if the ACF and PACF plots of the differenced data show the following patterns:

#the PACF is exponentially decaying or sinusoidal
#there is a significant spike at lag p in the ACF, but none beyond lag p

# In the PACF, there is 1 significant spike that passes the threshold. 
# In the ACF, there are quickly decaying  and recovered later 

###Hence PACF suggest AR(1), ACF suggest of q=1 i.e 1 spike and d=0(No of differencing needed for stationarity) = ARIMA(1,0,1)

#####NONSEASONAL ARIMA MODEL ARIMA(p,d,q)
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
auto.arima(preloin_ts)#Suggest Arima (0,0,0) (0,0,1)[12] might be appropriate 

.####arima(p,d,0) based on the ACF and PACF plots
fit_loin <- arima(preloin_ts, order=c(0,0,12), seasonal=c(0,0,1))

#################
###FORECASTING###
#################
#windows(10,6)
#png("figs\\Loins Export Arima(0,0,9)(0,0,1)[12].png", width=600, height=400)##If you wish to save the plot
f_loin <- plot(forecast(fit_loin), xlab = "Year", ylab ="Loins Export (Metric Tonnes)")
dev.off()

###USE AIC Akaike's An Information Criterion to select the best model- the lower the AIC the more reliable is the model
AIC(fit_loin)
#fitted(fit_pre)
#seasonal
#Arima (0,0,0) (0,0,1) AIC  1418.944
#Arima (0,0,1) (0,0,1) AIC 1419.182
#Arima (0,0,1) (0,0,2) AIC 1420.896
#Arima (0,0,2) (0,0,1) AIC 1421.182
#Arima (0,0,3) (0,0,1) AIC 1421.874
#Arima (0,0,4) (0,0,1) AIC 1416.568
#Arima (0,0,5) (0,0,1) AIC 1417.001
#Arima (0,0,6) (0,0,1) AIC 1417.109
#Arima (0,0,7) (0,0,1) AIC 1417.297
#Arima (0,0,8) (0,0,1) AIC 1417.343
#Arima (0,0,9) (0,0,1) AIC 1415.793# low AIC# 
#Arima (0,0,10) (0,0,1) AIC 1415.968 Low  AIC and higher mean values
#Arima (0,0,11) (0,0,1) AIC 1416.219
#Arima (0,0,12) (0,0,1) AIC 1417.415 based on the ACF and PACF plots
#Arima (7,0,0)(0,0,1) AIC 1418.763) based on the PACF plots


###Extract fitted values###
fitted(fit_loin)
f_loin


##Checking rediduals### 
##Note: for ARIMA (3,0,0) ACF plots of the residuals shows that all autocorrelation are within the threshold limits except one. 
##, then the residuals are behaving like white noise 
checkresiduals(fit_loin)
dev.off()
###Ljung Box test for Autocorrelation of the residuals: H0: No Autocorrelation; H1: Autocrrelation 
##p-value for Arima (0,0,12) (0,0,1)  is 0.4585 GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation

#Export forecast values to excel
loin_vol <-data.frame(f_loin)
head(loin_vol)
tail(loin_vol)
write_xlsx(loin_vol,"C:\\Users\\thomas.ruaia\\Documents\\FFA\\MFMR\\MFMR\\R\\loin_volumes Arima(0,0,12)(0,0,l)[12].xlsx")
########################################
###Plot forecast vs Actual values###### 
#######################################

#Actual values
actual_loin <- ts(postloin_ts[,1],start=c(2020),frequency=12)
actual_loin
plot(actual_fresh)# title = "Fresh Volume Export 2020", Xtitle = "Time", Ytitle = "Metric tonne")#using TSStudio and add slider

# Extract mean values from Arima (2,0,0) (0,0,2) for plotting
f_loinvol <- read_excel("loin_volumes Arima(0,0,9)(0,0,l)[12].xlsx")
#Call for mean values only

pref_loin <- f_loinvol[01:24,1]
#colnames(pref_fr) <- c("year","Forecast values")
head(pref_loin)
tail(pref_loin)

#Convert forecast values to time series ended 2021
ffloin_ts <- ts(pref_loin[,1],start=c(2020),frequency=12)
ffloin_ts
plot(ffloin_ts)
dev.off()
#Combine the graphs
lcomb_fresh <- zoo(cbind(actual_loin, ffloin_ts))

#Plot the combine graphs
#jpeg("figs\\Thai Frozen SB Export.jpeg")
ts_plot(lcomb_fresh, title = "Solomon Is Loin Export Volume Vs Forecast Data", Xtitle = "Time", Ytitle = " Metric tonne") 
dev.off()

#####################################################
####################################################
####Run the forecasting for LOINS EXPORT VALUES#########
###################################################
#Convert to time series object with year ended 2021 for frozen export values
valoin_ts <- ts(dfl[,3],start=c(2012),frequency=12)
valoin_ts

##Plotting
plot(valoin_ts)
ts_plot(valoin_ts)
autoplot(valoin_ts)
dev.off()

#png("figs\\YFT\\yft norminal price.png") 
#plot(yft_ts)

##call the pre Export data from 2012 only to 2019
pre_valoin <- dfl[01:96,3]
pre_valoin
head(pre_valoin)
tail(pre_valoin)

#Convert to time series object with year ended 2019
prevloin_ts <- ts(pre_valoin[,1],start=c(2012),frequency=12)
prevloin_ts
ts_plot(prevloin_ts)#Using TSstudio package
plot(prevloin_ts)
dev.off()
#Plot 2020 frozen data
post_valoin <- dfl[97:120,3]
post_valoin
postvloin_ts <- ts(post_valoin[,1],start=c(2020),frequency=12)
postvloin_ts
ts_plot(postvloin_ts)#Using TSstudio package
plot(postvloin_ts)
dev.off()
########################
###Stationarity tests###
########################

adf_vloin <- adf.test(prevloin_ts)
adf_vloin # p-value = 0.01 is less than 5% significance level, hence REJECT H0 -(Non Stationary)- So Variable is stationary

############################################################################
###ACF (Auto Correlation Function)& Pacf- Partial Autocrrelation Function###
###########################################################################

##Note: For a stationary time series, the ACF will drop to zero relatively quickly, while the ACF of non-stationary data decreases slowly. 
##Also, for non-stationary data, the value of r1 is often large and positive   
## ACF drop to zero relatively quickly indicate the variable is stationary 

## Testing for autocorrelation by plotting ACF
#png("figs\\loins values acf & pacf.png")##If you wish to save the plot
par(mfrow=c(1,2))
Acf(prevloin_ts) # ACF Level data
Pacf(prevloin_ts) # PACF Level data
#Acf(dpre) # ACF first order differencing 
#Pacf(dpre)# PACF of first order differencing 
#savePlot("figs\\2018 skj acf & pacf", type="jpg")
dev.off()

#################################
##FINDING the APPROPRIATE MODEL##
#################################
# From both ACF and PACF plots, the significant spike at lag 12 suggests a non-seasonal MA(12) 
# The significant spike in lag 12 also suggest a seasonal MA(1) component
# This suggest ARIMA(0,0,12) (0,0,1)[12]

# If we had started with the PACF, PACF plot will be used to select the non seasonal part and the ACF to select the seasonal part of the model
#This suggest Arima(8,0,0) (0,0,1) as there is a spike at lag 8

###Additional notes

# From both ACF and PACF plots, the data may follow an ARIMA(0,d,q)model: The reasons as follows

#The data may follow an ARIMA(p,d,0) model if the ACF and PACF plots of the differenced data show the following patterns:

#the PACF is exponentially decaying or sinusoidal
#there is a significant spike at lag p in the ACF, but none beyond lag p

# In the PACF, there is 1 significant spike that passes the threshold. 
# In the ACF, there are quickly decaying  and recovered later 

###Hence PACF suggest AR(1), ACF suggest of q=1 i.e 1 spike and d=0(No of differencing needed for stationarity) = ARIMA(1,0,1)

#####NONSEASONAL ARIMA MODEL ARIMA(p,d,q)
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
auto.arima(prevloin_ts)#Suggest Arima (1,0,0) (0,0,1)[12] might be appropriate 

####arima(p,d,0) based on the ACF and PACF plots
fit_loinv <- arima(prevloin_ts, order=c(0,0,12), seasonal=c(0,0,1))

#################
###FORECASTING###
#################
#windows(10,6)
#png("figs\\Loins Export Values Arima(0,0,9)(0,0,1)[12].png", width=600, height=400)##If you wish to save the plot
f_vloin <- plot(forecast(fit_loinv), xlab = "Year", ylab ="Loins Export Values (US$ million)")

###USE AIC Akaike's An Information Criterion to select the best model- the lower the AIC the more reliable is the model
AIC(fit_loinv)
dev.off()
#seasonal
#Arima (0,0,0) (0,0,1) AIC  460.3983
#Arima (0,0,1) (0,0,1) AIC 459.9197
#Arima (0,0,1) (0,0,2) AIC 461.4764
#Arima (0,0,2) (0,0,1) AIC 461.8576
#Arima (0,0,3) (0,0,1) AIC 463.2401
#Arima (0,0,4) (0,0,1) AIC 458.0591 low AIC but the not much fluctuation seen in 2021
#Arima (0,0,5) (0,0,1) AIC 459.4845
#Arima (0,0,6) (0,0,1) AIC 459.6984
#Arima (0,0,7) (0,0,1) AIC 459.5822
#Arima (0,0,8) (0,0,1) AIC 461.5617
#Arima (0,0,9) (0,0,1) AIC 458.695 low AIC and more fluctation is seen so this is preferred model
#Arima (0,0,10) (0,0,1) AIC 459.8076
#Arima (0,0,11) (0,0,1) AIC 459.819
#Arima (0,0,12) (0,0,1) AIC 459.3186.415 based on the ACF and PACF plots
#Arima (7,0,0)(0,0,1) AIC 461.7365 based on the PACF plots


###Extract fitted values###
fitted(fit_loinv)
f_vloin

##Checking rediduals### 
##Note: for ARIMA (0,0,9) (0,0,1) ACF plots of the residuals shows that all autocorrelation are within the threshold limits except one. 
##, then the residuals are behaving like white noise 
checkresiduals(fit_loinv)
dev.off()
###Ljung Box test for Autocorrelation of the residuals: H0: No Autocorrelation; H1: Autocrrelation 
##p-value for Arima (0,0,12) (0,0,1)  is 0.5668 GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation

#Export forecast values to excel
loinv <- data.frame(f_vloin)
head(loinv)
tail(loinv)
write_xlsx(loinv,"C:\\Users\\thomas.ruaia\\Documents\\FFA\\MFMR\\MFMR\\R\\loin_values Arima(0,0,12)(0,0,l)[12].xlsx")

########################################
###Plot forecast vs Actual values###### 
#######################################

#Actual values
actual_loinv <- ts(post_valoin[,1],start=c(2020),frequency=12)
actual_loinv
plot(actual_loinv)# title = "Fresh Volume Export 2020", Xtitle = "Time", Ytitle = "Metric tonne")#using TSStudio and add slider

# Extract mean values from Arima (2,0,0) (0,0,2) for plotting
f_loinval <- read_excel("loin_values Arima(0,0,9)(0,0,l)[12].xlsx")
#Call for mean values only
f_loinvalues <- f_loinval[01:24,1]
#colnames(pref_fr) <- c("year","Forecast values")
head(f_loinvalues)
tail(f_loinvalues)

#Convert forecast values to time series ended 2021
ffloin_ts <- ts(pref_loin[,1],start=c(2020),frequency=12)
ffloin_ts
plot(ffloin_ts)
dev.off()
#Combine the graphs
comb_loinv <- zoo(cbind(actual_loinv, ffloin_ts))

#Plot the combine graphs
#jpeg("figs\\Thai Frozen SB Export.jpeg")
ts_plot(lcomb_fresh, title = "Solomon Is Loin Export Values Vs Forecast Data", Xtitle = "Time", Ytitle = " Metric tonne") 
dev.off()

#####################################################
####################################################
####Run the forecasting for PREPARED Products#########
###################################################
##################
###Read in prepared data###
##################
dfp <- read_csv("SB prepared data.csv")

################
###Clean data###
################
dim(dfp)
head(dfp)
tail(dfp)

#Convert to time series object with year ended 2021 for frozen products
prep_ts <- ts(dfp[,2],start=c(2012),frequency=12)
prep_ts

##Plotting
plot(loin_ts)
ts_plot(loin_ts)
autoplot(loin_ts)
dev.off()

#Plot and Save
#png("figs\\YFT\\yft norminal price.png") 
#plot(yft_ts)

##call the pre Export data from 2012 only to 2019
pre_prep <- dfp[01:96,2]
#colnames(pre_data) <- c("year","MGO")
pre_prep
head(pre_prep)
tail(pre_prep)

#Convert to time series object with year ended 2019
preprep_ts <- ts(pre_prep[,1],start=c(2012),frequency=12)
preprep_ts
ts_plot(preprep_ts)#Using TSstudio package
plot(preprep_ts)
dev.off()
#Plot 2020 and 2021 loin data
post_prep <- dfp[97:114,2]
post_prep
#Check data
head(post_prep)
tail(post_prep)
postprep_ts <- ts(post_prep[,1],start=c(2020),frequency=12)
postprep_ts
ts_plot(postprep_ts)#Using TSstudio package
plot(postprep_ts)
dev.off()

########################
###Stationarity tests###
########################

adf_prep <- adf.test(preprep_ts)
adf_prep # p-value = 0.05475 is MORE than 5% significance level, hence Do NOT REJECT H0 -(Non Stationary)- So Variable is Non- stationary, so must take the first difference

##################
###Differencing###
##################
# Take the first difference
###SKJ###
d_prep <- diff(preprep_ts, lag = 1)
adf_dprep <- adf.test(d_prep)
adf_dprep ###p-value = 0.01 is still NOW LESS than 5% significance level, hence REJECT H0 -(Non Stationary)- So Variable is NOW stationary
plot(d_prep) 
dev.off()
############################################################################
###ACF (Auto Correlation Function)& Pacf- Partial Autocrrelation Function###
###########################################################################

#png("figs\\prep volume acf & pacf.png", width=600, height=400)##If you wish to save the plot
par(mfrow=c(1,2))
Acf(preprep_ts) # ACF Level data
Pacf(preprep_ts) # PACF Level data
dev.off()

#################################
##FINDING the APPROPRIATE MODEL##
#################################
# From both ACF and PACF plots, the significant spike at lag 2 suggests a non-seasonal MA(2) 
# The significant spike in lag 11 & 12 also suggest a seasonal MA(1) component
# This suggest ARIMA(0,1,2) (0,1,1)[12]

# If we had started with the PACF, PACF plot will be used to select the non seasonal part and the ACF to select the seasonal part of the model
#This suggest Arima(2,1,0) (0,1,1)


#################
##FIT THE MODEL##
################
auto.arima(preprep_ts)#Suggest Arima (1,0,2) (0,0,1)[12] might be appropriate 

.####arima(p,d,0) based on the ACF and PACF plots
fit_prep <- arima(preprep_ts, order=c(2,1,3))

#################
###FORECASTING###
#################
#windows(10,6)
#png("figs\\Loins Export Arima(0,0,9)(0,0,1)[12].png", width=600, height=400)##If you wish to save the plot
f_prep <- plot(forecast(fit_prep), xlab = "Year", ylab ="Prepared Export (Metric Tonnes)")
dev.off()

###USE AIC Akaike's An Information Criterion to select the best model- the lower the AIC the more reliable is the model
AIC(fit_prep)
#fitted(fit_pre)

# Non seasonal
#Arima(2,1,0) AIC 974.6537
#Arima(2,1,1) AIC 970.5391
#Arima(2,1,2) AIC 972.1397
#Arima(2,1,3) AIC 969.6474 Low AIC
#Arima(2,1,4) AIC 971.6275
#Arima(2,1,5) AIC 971.764
###Extract fitted values###
fitted(fit_prep)
f_prep


##Checking rediduals### 
##Note: for ARIMA (2,1,3) ACF plots of the residuals shows that all autocorrelation are within the threshold limits except one. 
##, then the residuals are behaving like white noise 
checkresiduals(fit_prep)
dev.off()
###Ljung Box test for Autocorrelation of the residuals: H0: No Autocorrelation; H1: Autocrrelation 
##p-value for Arima (2,1,3) is 0.2212 GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation

#Export forecast values to excel
prep_vol <-data.frame(f_prep)
head(prep_vol)
tail(prep_vol)
write_xlsx(prep_vol,"C:\\Users\\thomas.ruaia\\Documents\\FFA\\MFMR\\MFMR\\R\\prep_volumes Arima(2,1,3).xlsx")
########################################
###Plot forecast vs Actual values###### 
#######################################

#Actual values
actual_loin <- ts(postloin_ts[,1],start=c(2020),frequency=12)
actual_loin
plot(actual_fresh)# title = "Fresh Volume Export 2020", Xtitle = "Time", Ytitle = "Metric tonne")#using TSStudio and add slider

# Extract mean values from Arima (2,0,0) (0,0,2) for plotting
f_loinvol <- read_excel("loin_volumes Arima(0,0,9)(0,0,l)[12].xlsx")
#Call for mean values only

pref_loin <- f_loinvol[01:24,1]
#colnames(pref_fr) <- c("year","Forecast values")
head(pref_loin)
tail(pref_loin)

#Convert forecast values to time series ended 2021
ffloin_ts <- ts(pref_loin[,1],start=c(2020),frequency=12)
ffloin_ts
plot(ffloin_ts)
dev.off()
#Combine the graphs
lcomb_fresh <- zoo(cbind(actual_loin, ffloin_ts))

#Plot the combine graphs
#jpeg("figs\\Thai Frozen SB Export.jpeg")
ts_plot(lcomb_fresh, title = "Solomon Is Loin Export Volume Vs Forecast Data", Xtitle = "Time", Ytitle = " Metric tonne") 
dev.off()

#####################################################
####################################################
####Run the forecasting for LOINS EXPORT VALUES#########
###################################################
#Convert to time series object with year ended 2021 for frozen export values
valoin_ts <- ts(dfl[,3],start=c(2012),frequency=12)
valoin_ts

##Plotting
plot(valoin_ts)
ts_plot(valoin_ts)
autoplot(valoin_ts)
dev.off()

#png("figs\\YFT\\yft norminal price.png") 
#plot(yft_ts)

##call the pre Export data from 2012 only to 2019
pre_valoin <- dfl[01:96,3]
pre_valoin
head(pre_valoin)
tail(pre_valoin)

#Convert to time series object with year ended 2019
prevloin_ts <- ts(pre_valoin[,1],start=c(2012),frequency=12)
prevloin_ts
ts_plot(prevloin_ts)#Using TSstudio package
plot(prevloin_ts)
dev.off()
#Plot 2020 frozen data
post_valoin <- dfl[97:120,3]
post_valoin
postvloin_ts <- ts(post_valoin[,1],start=c(2020),frequency=12)
postvloin_ts
ts_plot(postvloin_ts)#Using TSstudio package
plot(postvloin_ts)
dev.off()
########################
###Stationarity tests###
########################

adf_vloin <- adf.test(prevloin_ts)
adf_vloin # p-value = 0.01 is less than 5% significance level, hence REJECT H0 -(Non Stationary)- So Variable is stationary

############################################################################
###ACF (Auto Correlation Function)& Pacf- Partial Autocrrelation Function###
###########################################################################

##Note: For a stationary time series, the ACF will drop to zero relatively quickly, while the ACF of non-stationary data decreases slowly. 
##Also, for non-stationary data, the value of r1 is often large and positive   
## ACF drop to zero relatively quickly indicate the variable is stationary 

## Testing for autocorrelation by plotting ACF
#png("figs\\loins values acf & pacf.png")##If you wish to save the plot
par(mfrow=c(1,2))
Acf(prevloin_ts) # ACF Level data
Pacf(prevloin_ts) # PACF Level data
#Acf(dpre) # ACF first order differencing 
#Pacf(dpre)# PACF of first order differencing 
#savePlot("figs\\2018 skj acf & pacf", type="jpg")
dev.off()

#################################
##FINDING the APPROPRIATE MODEL##
#################################
# From both ACF and PACF plots, the significant spike at lag 12 suggests a non-seasonal MA(12) 
# The significant spike in lag 12 also suggest a seasonal MA(1) component
# This suggest ARIMA(0,0,12) (0,0,1)[12]

# If we had started with the PACF, PACF plot will be used to select the non seasonal part and the ACF to select the seasonal part of the model
#This suggest Arima(8,0,0) (0,0,1) as there is a spike at lag 8

###Additional notes

# From both ACF and PACF plots, the data may follow an ARIMA(0,d,q)model: The reasons as follows

#The data may follow an ARIMA(p,d,0) model if the ACF and PACF plots of the differenced data show the following patterns:

#the PACF is exponentially decaying or sinusoidal
#there is a significant spike at lag p in the ACF, but none beyond lag p

# In the PACF, there is 1 significant spike that passes the threshold. 
# In the ACF, there are quickly decaying  and recovered later 

###Hence PACF suggest AR(1), ACF suggest of q=1 i.e 1 spike and d=0(No of differencing needed for stationarity) = ARIMA(1,0,1)

#####NONSEASONAL ARIMA MODEL ARIMA(p,d,q)
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
auto.arima(prevloin_ts)#Suggest Arima (1,0,0) (0,0,1)[12] might be appropriate 

####arima(p,d,0) based on the ACF and PACF plots
fit_loinv <- arima(prevloin_ts, order=c(0,0,9), seasonal=c(0,0,1))

#################
###FORECASTING###
#################
#windows(10,6)
#png("figs\\Loins Export Values Arima(0,0,9)(0,0,1)[12].png", width=600, height=400)##If you wish to save the plot
f_vloin <- plot(forecast(fit_loinv), xlab = "Year", ylab ="Loins Export Values (US$ million)")

###USE AIC Akaike's An Information Criterion to select the best model- the lower the AIC the more reliable is the model
AIC(fit_loinv)
dev.off()
#seasonal
#Arima (0,0,0) (0,0,1) AIC  460.3983
#Arima (0,0,1) (0,0,1) AIC 459.9197
#Arima (0,0,1) (0,0,2) AIC 461.4764
#Arima (0,0,2) (0,0,1) AIC 461.8576
#Arima (0,0,3) (0,0,1) AIC 463.2401
#Arima (0,0,4) (0,0,1) AIC 458.0591 low AIC but the not much fluctuation seen in 2021
#Arima (0,0,5) (0,0,1) AIC 459.4845
#Arima (0,0,6) (0,0,1) AIC 459.6984
#Arima (0,0,7) (0,0,1) AIC 459.5822
#Arima (0,0,8) (0,0,1) AIC 461.5617
#Arima (0,0,9) (0,0,1) AIC 458.695 low AIC and more fluctation is seen so this is preferred model
#Arima (0,0,10) (0,0,1) AIC 459.8076
#Arima (0,0,11) (0,0,1) AIC 459.819
#Arima (0,0,12) (0,0,1) AIC 459.3186.415 based on the ACF and PACF plots
#Arima (7,0,0)(0,0,1) AIC 461.7365 based on the PACF plots


###Extract fitted values###
fitted(fit_loinv)
f_vloin

##Checking rediduals### 
##Note: for ARIMA (0,0,9) (0,0,1) ACF plots of the residuals shows that all autocorrelation are within the threshold limits except one. 
##, then the residuals are behaving like white noise 
checkresiduals(fit_loinv)
dev.off()
###Ljung Box test for Autocorrelation of the residuals: H0: No Autocorrelation; H1: Autocrrelation 
##p-value for Arima (0,0,9) (0,0,1)  is 0.3785 GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation

#Export forecast values to excel
loinv <- data.frame(f_vloin)
head(loinv)
tail(loinv)
write_xlsx(loinv,"C:\\Users\\thomas.ruaia\\Documents\\FFA\\MFMR\\MFMR\\R\\loin_values Arima(0,0,9)(0,0,l)[12].xlsx")

########################################
###Plot forecast vs Actual values###### 
#######################################

#Actual values
actual_loinv <- ts(post_valoin[,1],start=c(2020),frequency=12)
actual_loinv
plot(actual_loinv)# title = "Fresh Volume Export 2020", Xtitle = "Time", Ytitle = "Metric tonne")#using TSStudio and add slider

# Extract mean values from Arima (2,0,0) (0,0,2) for plotting
f_loinval <- read_excel("loin_values Arima(0,0,9)(0,0,l)[12].xlsx")
#Call for mean values only
f_loinvalues <- f_loinval[01:24,1]
#colnames(pref_fr) <- c("year","Forecast values")
head(f_loinvalues)
tail(f_loinvalues)

#Convert forecast values to time series ended 2021
ffloin_ts <- ts(pref_loin[,1],start=c(2020),frequency=12)
ffloin_ts
plot(ffloin_ts)
dev.off()
#Combine the graphs
comb_loinv <- zoo(cbind(actual_loinv, ffloin_ts))

#Plot the combine graphs
#jpeg("figs\\Thai Frozen SB Export.jpeg")
ts_plot(lcomb_fresh, title = "Solomon Is Loin Export Values Vs Forecast Data", Xtitle = "Time", Ytitle = " Metric tonne") 

