#Set Working Directory
setwd("/Users/douglaswaters/Documents/UCLA Docs/Spring 2015/Economics 144/Codes/Week 9/Project 2")

#Load the Libraries
library(lattice)
library(foreign)
library(MASS)
library(car)
require(stats)
require(stats4)
library(KernSmooth)
library(fastICA)
library(cluster)
library(leaps)
library(mgcv)
library(rpart)
library(pan)
library(mgcv)
library(DAAG)
library(TTR)
library(tis)
require("datasets")
require(graphics)
library(forecast)
require(astsa)
library(xtable)
library(stats)
library(TSA)
library(timeSeries)
library(fUnitRoots)
library(fBasics)
library(tseries)
library(timsac)
library(TTR)
library(fpp)
library(strucchange)
library(MSBVAR)
library(vars)
library(lmtest)
library(dlnm)

#Read the data 
Project2 = read.table("Project2.txt", header = T)
attach(Project2)
CPI_ts<-ts(CPI_Chg,start=2005,freq=12)
Unemp_ts<-ts(Unemp,start=2005,freq=12)

#(A) Look at each ACF, PACF, and Cross-Correlation Function
tsdisplay(CPI_ts, main="Time Series of Consumer Price Index")
tsdisplay(Unemp_ts, main="Time Series of Unemployment Rate")
ccf(CPI_ts,Unemp_ts,ylab="Cross-Correlation Function", main = "CPI and Unemployment CCF")

#(B) Fit an ARIMA model with appropriate trend, seasonality, and cyclical components
fit1=Arima(CPI_ts, order = c(3,0,0))
fit2=Arima(Unemp_ts, order = c(2,0,1))
summary(fit1)
summary(fit2)
fit3 = auto.arima(CPI_ts)
fit4 = auto.arima(Unemp_ts)
summary(fit3)
summary(fit4)                   

#NOTE: We have included the data for the percent change in the CPI and the percent change 
# in the unemployment rate. However, the data exhibit weak stationarity, so we will proceed 
# as normal, by forecasting the regular data. 

#(C) Plot the Residuals vs. Fitted Values
plot(fitted(fit1), pch = 46, main = "Fitted Values of the CPI Model", ylab = "Fitted Values", xlab = "Time")
plot(resid(fit1), pch = 46, main = "Residuals of the CPI Model", ylab = "Residuals", xlab = "Time")
plot(fitted(fit1), resid(fit1), pch = 46, main = "Residuals vs. Fitted Values of the CPI Model", ylab = "Residuals", xlab = "Fitted Values")

plot(fitted(fit2), pch = 46, main = "Fitted Values of the Unemployment Model", ylab = "Fitted Values", xlab = "Time")
plot(resid(fit2), pch = 46, main = "Residuals of the Unemployment Model", ylab = "Residuals", xlab = "Time")
plot(fitted(fit2), resid(fit2), pch = 20, main = "Residuals vs. Fitted Values of the Unemployment Model", ylab = "Residuals", xlab = "Fitted Values")

plot(fitted(fit3), pch = 46, main = "Fitted Values of the Auto ARIMA CPI Model", ylab = "Fitted Values", xlab = "Time")
plot(resid(fit3), pch = 46, main = "Residuals of the Auto ARIMA CPI Model", ylab = "Residuals", xlab = "Time")
plot(fitted(fit1), resid(fit3), pch = 20, main = "Residuals vs. Fitted Values of the Auto ARIMA CPI Model", ylab = "Residuals", xlab = "Fitted Values")

plot(fitted(fit4), pch = 46, main = "Fitted Values of the Auto ARIMA Unemployment Model", ylab = "Fitted Values", xlab = "Time")
plot(resid(fit4), pch = 46, main = "Residuals of the Auto ARIMA Unemployment Model", ylab = "Residuals", xlab = "Time")
plot(fitted(fit2), resid(fit4), pch = 20, main = "Residuals vs. Fitted Values of the Auto ARIMA Unemployment Model", ylab = "Residuals", xlab = "Fitted Values")

#(D) Plot the ACF and PACF of the respective residuals
par(mfrow=c(2,1))
acf(residuals(fit1), main = "ACF of the Residuals for CPI Model")
pacf(residuals(fit1), main = "PACF of the Residuals for CPI Model")

par(mfrow=c(2,1))
acf(residuals(fit2), main = "ACF of the Residuals for Unemployment Model")
pacf(residuals(fit2), main = "PACF of the Residuals for Unemployment Model")

par(mfrow=c(2,1))
acf(residuals(fit3), main = "ACF of the Residuals for Auto ARIMA CPI Model")
pacf(residuals(fit3), main = "PACF of the Residuals for Auto ARIMA CPI Model")

par(mfrow=c(2,1))
acf(residuals(fit4), main = "ACF Residuals for Auto ARIMA Unemployment Model")
pacf(residuals(fit4), main = "PACF Residuals for Auto ARIMA Unemployment Model")

#(E) Plot the respective CUSUM
par(mfrow=c(2,1))
plot(efp(fit1$res~1, type = "Rec-CUSUM"), main="Recursive CUSUM for CPI Model")
plot(efp(fit2$res~1, type = "Rec-CUSUM"), main="Recursive CUSUM for Unemployment Model")
plot(efp(fit3$res~1, type = "Rec-CUSUM"), main="Recursive CUSUM for Auto ARIMA CPI Model")
plot(efp(fit4$res~1, type = "Rec-CUSUM"), main="Recursive CUSUM for Auto ARIMA Unemployment Model")

#(F) Plot the respective Recursive Residuals
par(mfrow=c(2,1))
plot(recresid(fit1$res~1), pch = 20, ylab = "Recursive Residuals", main = "Recursive Residuals for CPI Fit Model")
plot(recresid(fit2$res~1), pch = 20, ylab = "Recursive Residuals", main = "Recursive Residuals for Unemployment Rate Fit Model")
plot(recresid(fit3$res~1), pch = 20, ylab = "Recursive Residuals", main = "Recursive Residuals of Auto ARIMA CPI Model")
plot(recresid(fit4$res~1), pch = 20, ylab = "Recursive Residuals", main = "Recursive Residuals of Auto Arima Unemployment Model")

#(H) 12-steps ahead Forecast
par(mfrow=c(2,1))
plot(forecast(fit1, 12))
plot(forecast(fit2, 12))
plot(forecast(fit3, 12))
plot(forecast(fit4, 12))

#(I) Fit an appropriate VAR model
data=cbind(CPI_ts, Unemp_ts)
data_tot=data.frame(data)

var1 = VAR(data_tot, p=1)
var2 = VAR(data_tot, p=2)
var3 = VAR(data_tot, p=3)
var4 = VAR(data_tot, p=4)
var5 = VAR(data_tot, p=5)
var6 = VAR(data_tot, p=6)
var7 = VAR(data_tot, p=7)
var8 = VAR(data_tot, p=8)

AIC(var1, var2, var3, var4, var5, var6, var7, var8)
BIC(var1, var2, var3, var4, var5, var6, var7, var8)

# We choose VAR(3) 

#(J) Compute and plot the respective Impulse Response Functions
irf(var3)
plot(irf(var3))

#(K) Granger-Causality Test*
grangertest(CPI_ts ~ Unemp_ts, order = 3)
grangertest(Unemp_ts ~ CPI_ts, order = 3)
grangertest(CPI_ts ~ Unemp_ts, order = 20)
grangertest(Unemp_ts ~ CPI_ts, order = 20)

#(L) 12-steps ahead Forecast for VAR model 
var.predict = predict(object=var3, n.ahead=12)
plot(var.predict)

#Current Concerns:

#  *Granger-Causality test is creating problems. In the impulse response function we are discovering
#  that as unemployment increases the CPI is decreasing (gradually). This would imply that UR 
#  causes CPI to decrease. Need to determine how to get this Granger test to reflect this. 
#      Possible Solutions: 
#       1. Consider change in CPI (CPI_Chg) in the VAR model
#       2. If CPI is the data we want, continue tweeking the Granger test. 


