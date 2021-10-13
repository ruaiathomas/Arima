#Intervention Analysis with Arima 
# See https://nicolarighetti.github.io/Time-Series-Analysis-With-R/intervention-analysis.html#intervention-analysis-with-arima
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
library(xts) # used for the intervention analysis
library(lmtest) # used to quantify the impact
library(TSA)

setwd("~/FFA/MFMR/MFMR/R")# Set Working Directory 
##################
###Read in FRESH data###
##################
df <- read_csv("SB fresh data.csv")

################
###Clean data###
################
dim(df)
head(df)
tail(df)

#Convert to time series object 
df.ts <- ts(df[,2],start=c(2012,1),frequency=12)
df.ts

# Plot data to visualize time series
plot.ts(df.ts, ylim=c(0, 100), col = "blue", xlab = "Month", ylab = "Fresh Export Volumes")
# Add vertical line indicating date of intervention (January 1, 2014)
abline(v=2020.25, col = "black", lty = "dashed", lwd=1)
dev.off()

# t-test- #A t-test is a type of inferential statistic used to determine if there is a significant difference between the means of two groups, which may be related in certain features.
# Define x and y

x <- df[1:98,2] #pre period (end Feb 2020)
y <- df[99:114,2]# post period (start March 2020 to June 2021)
#x = df["Time" <"01-Mar-20"]

t.test(x,y) # p-value = 2.754e-06 < 5%, so we reject the null hypothesis of no difference
# hence with 95% confidence level that the true difference in means is not equal to zero.
# there is a statistical difference between the two means

###Create a dummy variables representing the Intervention. 
# Create variable representing step change and view
step <- as.numeric(as.yearmon(time(df.ts)) >= "Mar 2020")
step
ts.plot(step)
dev.off()
#The above vectors is a dummy variable for the intervention. 
#It has value equal zero before the date of the intervention, and 1 after that. 


### Create a specific case by creating a variable representing a constant DECREASING change
### capturing an DECREASING effect of the intervention over time. 
### We create two vectors by using the rep and the seq function, and concatenate them by using the c function.

#The argument of the rep function are two integers x and times (rep(x, times)), 
#and the function creates a vectors that repeat ("rep") the x values the number of times specified by times. 
#We have 96 months before the intervention, and we assign them the value zero.

rep(0, 98)
rep(1,16)

# Model intervention

#intervention <- c(rep(0,96),rep(1,18)) #define intervention dummies
#intervention
# model identification

model <-auto.arima(df.ts)
model

### from text book

# Use seq function to create a vectors with increasing values. This part of the variable
# represent a gradual INCREASE after the intervention (16 months of data after the intervention)
# we create a sequence of values that INCREASES from 1 to 18 by 1

seq(from = 1, to = 16, by = 1)

# Create variable representing ramp (change in slope) and view
ramp <- c(rep(0, 98), seq(1, 16, 1))
ramp
ts.plot(ramp)
dev.off()
# Search for an appropriate ARIMA model for the data by using auto.arima function

# Use automated algorithm to identify parameters
model1 <- auto.arima(df.ts, xreg = cbind(step, ramp), stepwise=FALSE)

# Check residuals
checkresiduals(model1) #Suggest Arima(2,1,2)

###Ljung Box test for Autocorrelation of the residuals: H0: No Autocorrelation; H1: Autocrrelation 
##p-value for Arima (2,1,0) (0,1,1) is 0.1483 GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation

#check model
model1

###Use the information retrieved from the auto.arima function to fit the same Arima model to the data without including the intervention.
###(variables we created), using just the data up to the date of the intervention(March 2020)
# To do that, use window function in order to restrict the set of data 
# and set Feb 2020 as the end of our series.

model1 <- Arima(window(df.ts, end = c(2020,2)), order = c(2, 1, 2), # Arima (2,1,2) - from the auto arima function with transfer function
                seasonal = list(order = c(0, 0, 2), period = 12))
# check model- diagnostics
par(mfrow=c(1,2))
acf(model1$residuals)
pacf(model1$residuals)
checkresiduals(model1)##p value = 0.2487 GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation
dev.off()

#auto arima at level data
auto.arima(df.ts)#suggest Arima(2,1,2)

#Check AIC for model1
AIC(model1)

#select best AIC
#Arima (2,1,2) (0,0,1) AIC 763.3936 # low AIC but forecast point value are negative
#Arima (2,1,2) (0,0,2) AIC 763.5463 # only forecast point values is negative
#Arima (2,1,2) (0,0,3) AIC 765.5433 # two forecast point values are negative


### Forecast 16 months post-intervention and convert to time series object
fc <- forecast(model1, h = 16)
fc
# covert the average forecast (fc$mean) in a time series object
fc.ts <- ts(as.numeric(fc$mean), start=c(2020,3), frequency = 12)
fc.ts
# Combine the observed and the forecast data
fc.ts.2 <- ts.union(df.ts, fc.ts)
fc.ts.2

#By plotting the data, we can visualize the predicted values in the absence of the intervention (red dashed line) as well as 
#the observed values (blue line). It seems that the health policy considerably impacted the analyzed prescriptions.

# Plot
#Save plot
#png("figs\\Fresh export volumes_intervention.png", width=600, height=400)##If you wish to save the plot
plot.ts(fc.ts.2, plot.type = "single", 
        col=c('blue','red'), xlab="Month", ylab="Volumes (Mt)", 
        lty=c("solid", "dashed"), ylim=c(0,90))
title(main="Solomon Islands Fresh Export")
legend(x = "topright",
       legend =,c("Observed Data","Predicted values without intervention"), lty = c (1,2),
       lwd = c(1,1),col = c("blue","red"))
abline(v=2020.25, lty="dashed", col="black")
dev.off()


# Plot only 2020 period and onwards # need more work

#o_data <- df[97:114,2]
#o_data.ts <- ts(o_data, start=c(2020,1),frequency=12)
#plot.ts(o_data.ts)
#dev.off()

# Coming back to our initial Arima model including the intervention variables,
#calculating the confidence intervals and the signifcance of the coefficients by using 
# coeftest and confint function in the lmtest library, 
# we can quantify the impact of the policy 

model3
coeftest(model3)
confint(model3)


############################################################################
########################## FROZEN EXPORT PRODUCTS #########################
##########################################################################
 
##################
###Read in FROZEN data###
##################
dfrozen <- read_csv("SB frozen data.csv")#### Need to double check again

################
###Clean data###
################
dim(dfrozen)
head(dfrozen)
tail(dfrozen)

#Convert to time series object 
dfrozen.ts <- ts(dfrozen[,2],start=c(2012,1),frequency=12)
dfrozen.ts

# Plot data to visualize time series
plot.ts(dfrozen.ts, ylim=c(0, 4000), col = "blue", xlab = "Month", ylab = "Frozen export volumes")
# Add vertical line indicating date of intervention (March 1, 2020)
abline(v= 2020.25, col = "black", lty = "dashed", lwd=1)
dev.off()

# t-test- #A t-test is a type of inferential statistic used to determine if there is a significant difference between the means of two groups, which may be related in certain features.
# Define x and y

x <- dfrozen[1:98,2, header = TRUE] #pre period (end Feb 2020) - 98 monthly data
y <- dfrozen[99:114,2]# post period (start March 2020 to June 2021) -16 monthly data

t.test(x,y) # p-value = 0.02681 < 5%, so we reject the null hypothesis of no difference
# hence with 95% confidence level that the true difference in means is not equal to zero.
# there is a statistical difference between the two means


###Create a dummy variables representing the Intervention. 
# Create variable representing step change and view
step <- as.numeric(as.yearmon(time(df.ts)) >= "Mar 2020")
step
ts.plot(step)
dev.off()
#The above vectors is a dummy variable for the intervention. 
#It has value equal zero before the date of the intervention, and 1 after that. 


### Create a specific case by creating a variable representing a constant INCREASING change
### capturing an INCREASING effect of the intervention over time. 
### We create two vectors by using the rep and the seq function, and concatenate them by using the c function.

#The argument of the rep function are two integers x and times (rep(x, times)), 
#and the function creates a vectors that repeat ("rep") the x values the number of times specified by times. 
#We have 98 months before the intervention, and we assign them the value zero.

rep(1, 98)
rep(0,16)

#intervention <- c(rep(1,96), rep(0,18))
#intervention

### from text book

# Use seq function to create a vectors with increasing values. This part of the variable
# represent a gradual INCREASE after the intervention (16 months of data after the intervention)
# we create a sequence of values that INCREASES from 1 to 18 by 1

seq(from = 1, to = 16, by = 1)

# Create variable representing ramp (change in slope) and view
ramp <- c(rep(0, 98), seq(1, 16, 1))
ramp
ts.plot(ramp)
dev.off()
# Search for an appropriate ARIMA model for the data by using auto.arima function

# Use automated algorithm to identify parameters

# Use only Step as an intervention variable
model1 <- auto.arima(df.ts, xreg = step, stepwise=FALSE)

# check model- diagnostics
par(mfrow=c(1,2))
acf(model1$residuals)
pacf(model1$residuals)
checkresiduals(model1)
dev.off()

#auto arima at level data
#auto.arima(df.ts)#suggest Arima(0,1,1)

#checkresiduals(model1) # suggest ARIMA(0,1,1)
#p value = 0.1386 GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation
#dev.off()

# Use only Ramp as an intervention variable
#model2 <- auto.arima(df.ts, xreg = ramp, stepwise=FALSE)
#checkresiduals(model2) # suggest ARIMA(0,1,1)
#p value = 0.455 is GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation
#dev.off()

# Combine Step and Ramp together and define as an intervention variable
model2 <- auto.arima(dfrozen.ts, xreg = cbind(step, ramp), stepwise=FALSE)# suggest ARIMA(0,1,1)
checkresiduals(model2)#suggest Arima(0,1,1)
# Ljung Box test for Autocorrelation of the residuals: H0: No Autocorrelation; H1: Autocrrelation 
# p value = 0.1417 GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation

#check model
model2

###USe the information retrieved from the auto.arima function to fit the same Arima model to the data without including the intervention.
###(variables we created), using just the data up to the date of the intervention(March 2020)
# To do that, use window function in order to restrict the set of data 
# and set Feb 2020 as the end of our series.

model2 <- Arima(window(dfrozen.ts, end = c(2020,2)), order = c(0, 1, 1), # Arima (0,1,1) - from the auto arima function with transfer function
          seasonal = list(order = c(0, 0, 2), period = 12))
AIC(model2)

#select best AIC
#Arima (0,1,1) (0,0,1) AIC 1590.812
#Arima (0,1,1) (0,0,2) AIC 1589.893 # low AIC best model
#Arima (0,1,1) (0,0,3) AIC 1590.306


### Forecast 16 months post-intervention and convert to time series object
fc <- forecast(model2, h = 16)
fc
# covert the average forecast (fc$mean) in a time series object
fc.ts <- ts(as.numeric(fc$mean), start=c(2020,3), frequency = 12)
fc.ts
plot(fc.ts)
dev.off()
# Combine the observed and the forecast data
fc.ts.2 <- ts.union(dfrozen.ts, fc.ts)
fc.ts.2

#By plotting the data, we can visualize the predicted values in the absence of the intervention (red dashed line) as well as 
#the observed values (blue line). It seems that the health policy considerably impacted the analyzed prescriptions.

# Plot
# Find the max lim to set for the plot
max(dfrozen.ts)
max(fc.ts)
#png("figs\\Frozen Export_intervention.png", width=600, height=400)##If you wish to save the plot
plot.ts(fc.ts.2, plot.type = "single", 
        col=c('blue','red'), xlab="Month", ylab="Volumes (Mt)", 
        lty=c("solid", "dashed"), ylim=c(0,5000))
title(main="Solomon Islands Frozen Export")
legend(x = "topleft",
       legend =,c("Observed Data","Predicted values without intervention"), lty = c (1,2),
       lwd = c(1,1),col = c("blue","red"))
abline(v=2020.25, lty="dashed", col="black")
dev.off()

# Coming back to our initial Arima model including the intervention variables,
#calculating the confidence intervals and the signifcance of the coefficients by using 
# coeftest and confint function in the lmtest library, 
# we can quantify the impact of the policy 

model3
coeftest(model3)
confint(model3)



############################################################################
########################## LOINS EXPORT PRODUCTS #########################
##########################################################################

##################
###Read in LOIN data###
##################
df_loins <- read_csv("SB loins data.csv")#### Need to double check again

################
###Clean data###
################
dim(df_loins)
head(df_loins)
tail(df_loins)

#Convert to time series object 
df_loins.ts <- ts(df_loins[,2],start=c(2012,1),frequency=12)
df_loins.ts

# Plot data to visualize time series
plot.ts(df_loins.ts, ylim=c(0, 2500), col = "blue", xlab = "Month", ylab = "Loins export volumes")

# Add vertical line indicating date of intervention (March 1, 2020)
abline(v= 2020.25, col = "black", lty = "dashed", lwd=1)
dev.off()

# t-test- #A t-test is a type of inferential statistic used to determine if there is a significant difference between the means of two groups, which may be related in certain features.
# Define x and y

x <- df_loins[1:98,2, header = TRUE] #pre period (end Feb 2020) - 98 monthly data
y <- df_loins[99:114,2]# post period (start March 2020 to June 2021) -16 monthly data

t.test(x,y) # p-value = 0.1531 > 5%, so we DO NOT reject the null hypothesis of no difference
# hence with 95% confidence level that the true difference in means is equal to zero.
# there is NO statistical difference between the two means


# Search for an appropriate ARIMA model for the data by using auto.arima function
# Use automated algorithm to identify parameters
# Use only Step as an intervention variable
#model1 <- auto.arima(df_loins.ts, xreg = step, stepwise=FALSE)

# check model- diagnostics
#par(mfrow=c(1,2))
#acf(model1$residuals)
#pacf(model1$residuals)
#checkresiduals(model1)
#dev.off()

#auto arima at level data
#auto.arima(df.ts)

#checkresiduals(model1) #suggest Arima(1,0,0)(0,0,1)
#p value = 0.4318 GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation
#dev.off()

# Use only Ramp as an intervention variable
#model2 <- auto.arima(df_loins.ts, xreg = ramp, stepwise=FALSE)
#checkresiduals(model2) #suggest Arima(1,0,0)(0,0,1)
#p value = 0.4367 is GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation
#dev.off()

# Combine Step and Ramp together and define as an intervention variable
model3 <- auto.arima(df_loins.ts, xreg = cbind(step, ramp), stepwise=FALSE)# suggest ARIMA(0,1,1)
checkresiduals(model3)#suggest Arima(1,0,0)(0,0,1)
# Ljung Box test for Autocorrelation of the residuals: H0: No Autocorrelation; H1: Autocrrelation 
# p value = 0.1417 GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation

#check model
model3

# Check ACF and PACF
par(mfrow=c(1,2))
Acf(df_loins.ts) # ACF Level data
Pacf(df_loins.ts) # PACF Level data
dev.off()

###USe the information retrieved from the auto.arima function to fit the same Arima model to the data without including the intervention.
###(variables we created), using just the data up to the date of the intervention(March 2020)
# To do that, use window function in order to restrict the set of data 
# and set March 2020 as the end of our series.

model3 <- Arima(window(df_loins.ts, end = c(2020,2)), order = c(1, 0, 0), # Arima (1,0,0) - from the auto arima function with transfer function
                seasonal = list(order = c(0, 0, 1), period = 12))
AIC(model3)
#Arima (0,1,1) (0,0,1) AIC 1446.605


### Forecast 16 months post-intervention and convert to time series object
fc <- forecast(model3, h = 16)
fc
# covert the average forecast (fc$mean) in a time series object
fc.ts <- ts(as.numeric(fc$mean), start=c(2020,3), frequency = 12)
fc.ts
# Combine the observed and the forecast data
fc.ts.2 <- ts.union(df_loins.ts, fc.ts)
fc.ts.2

#By plotting the data, we can visualize the predicted values in the absence of the intervention (red dashed line) as well as 
#the observed values (blue line). It seems that the health policy considerably impacted the analyzed prescriptions.

# Plot
#png("figs\\Loins Export_intervention.png", width=600, height=400)##If you wish to save the plot
plot.ts(fc.ts.2, plot.type = "single", 
        col=c('blue','red'), xlab="Month", ylab="Volumes (Mt)", 
        lty=c("solid", "dashed"), ylim=c(0,3000))
title(main="Solomon Islands Loins Export")
legend(x = "topleft",
       legend =,c("Observed Data","Predicted values without intervention"), lty = c (1,2),
       lwd = c(1,1),col = c("blue","red"))
abline(v=2020.25, lty="dashed", col="black")
dev.off()

# Coming back to our initial Arima model including the intervention variables,
#calculating the confidence intervals and the signifcance of the coefficients by using 
# coeftest and confint function in the lmtest library, 
# we can quantify the impact of the policy 

model3
coeftest(model3)
confint(model3)


############################################################################
########################## Prepared EXPORT PRODUCTS #########################
##########################################################################

##################
###Read in PREPARED data###
##################
df_prep <- read_csv("SB prepared data.csv")#### Need to double check again

################
###Clean data###
################
dim(df_prep)
head(df_prep)
tail(df_prep)

#Convert to time series object 
df_prep.ts <- ts(df_prep[,2],start=c(2012,1),frequency=12)
df_prep.ts

# Plot data to visualize time series
plot.ts(df_prep.ts, ylim=c(0, 300), col = "blue", xlab = "Month", ylab = "Prepared export volumes")

# Add vertical line indicating date of intervention (March 1, 2020)
abline(v= 2020.25, col = "black", lty = "dashed", lwd=1)
dev.off()

# t-test- #A t-test is a type of inferential statistic used to determine if there is a significant difference between the means of two groups, which may be related in certain features.
# Define x and y

x <- df_loins[1:98,2, header = TRUE] #pre period (end Feb 2020) - 98 monthly data
y <- df_loins[99:114,2]# post period (start March 2020 to June 2021) -16 monthly data

t.test(x,y) # p-value = 2.713e-05 < 5%, so we reject the null hypothesis of no difference
# hence with 95% confidence level that the true difference in means is not equal to zero.
# there is a statistical difference between the two means

# Search for an appropriate ARIMA model for the data by using auto.arima function
# Use automated algorithm to identify parameters
# Use only Step as an intervention variable
#model1 <- auto.arima(df_loins.ts, xreg = step, stepwise=FALSE)

# check model- diagnostics
#par(mfrow=c(1,2))
#acf(model1$residuals)
#pacf(model1$residuals)
#checkresiduals(model1)
#dev.off()

#auto arima at level data
#auto.arima(df.ts)

#checkresiduals(model1) #suggest Arima(1,0,0)(0,0,1)
#p value = 0.4318 GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation
#dev.off()

# Use only Ramp as an intervention variable
#model2 <- auto.arima(df_loins.ts, xreg = ramp, stepwise=FALSE)
#checkresiduals(model2) #suggest Arima(1,0,0)(0,0,1)
#p value = 0.4367 is GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation
#dev.off()

# Combine Step and Ramp together and define as an intervention variable
model4 <- auto.arima(df_prep.ts, xreg = cbind(step, ramp), stepwise=FALSE)# suggest ARIMA(0,1,1)
checkresiduals(model4)#suggest Arima(1,0,2)(0,0,1)
# Ljung Box test for Autocorrelation of the residuals: H0: No Autocorrelation; H1: Autocrrelation 
# p value = 0.2316 GREATER than 5% we fail to reject the null hypothesis(H0), so no Autocorrelation
dev.off()
#check model
model4

# Check ACF and PACF
par(mfrow=c(1,2))
Acf(df_prep.ts) # ACF Level data
Pacf(df_prep.ts) # PACF Level data
dev.off()

###USe the information retrieved from the auto.arima function to fit the same Arima model to the data without including the intervention.
###(variables we created), using just the data up to the date of the intervention(March 2020)
# To do that, use window function in order to restrict the set of data 
# and set March 2020 as the end of our series.

model4 <- Arima(window(df_prep.ts, end = c(2020,2)), order = c(1, 0, 2), # Arima (1,0,2) - from the auto arima function with transfer function
                seasonal = list(order = c(0, 0, 1), period = 12))
AIC(model4)
#Arima (1,0,2) (0,0,1) AIC 991.7998

### Forecast 16 months post-intervention and convert to time series object
fc <- forecast(model4, h = 16)
fc
# covert the average forecast (fc$mean) in a time series object
fc.ts <- ts(as.numeric(fc$mean), start=c(2020,3), frequency = 12)
fc.ts
# Combine the observed and the forecast data
fc.ts.2 <- ts.union(df_prep.ts, fc.ts)
fc.ts.2

#By plotting the data, we can visualize the predicted values in the absence of the intervention (red dashed line) as well as 
#the observed values (blue line). It seems that the health policy considerably impacted the analyzed prescriptions.

# Plot
#png("figs\\Prep Export_intervention.png", width=600, height=400)##If you wish to save the plot
plot.ts(fc.ts.2, plot.type = "single", 
        col=c('blue','red'), xlab="Month", ylab="Volumes (Mt)", 
        lty=c("solid", "dashed"), ylim=c(0,300))
title(main="Solomon Islands Prepared Export")
legend(x = "topright",
       legend =,c("Observed Data","Predicted values without intervention"), lty = c (1,2),
       lwd = c(1,1),col = c("blue","red"))
abline(v=2020.25, lty="dashed", col="black")
#dev.off()

# Coming back to our initial Arima model including the intervention variables,
#calculating the confidence intervals and the signifcance of the coefficients by using 
# coeftest and confint function in the lmtest library, 
# we can quantify the impact of the policy 

model3
coeftest(model3)
confint(model3)




