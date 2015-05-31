#Set Working Directory
setwd( "C:/Users/Ben/Desktop/144/R_Code/project2")

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


#(A) Look at each ACF, PACF, and Cross-Correlation Function
quartz()
tsdisplay(CPI)
quartz()
tsdisplay(Unemp)
quartz()
ccf(CPI,Unemp,ylab="Cross-Correlation Function", main = "CPI and Unemployment CCF")

#(B) Fit an ARIMA model with appropriate trend, seasonality, and cyclical components
fit1 = auto.arima(CPI)
fit2 = auto.arima(Unemp)
summary(fit1)
summary(fit2)

#NOTE: We have included the data for the percent change in the CPI and the percent change 
# in the unemployment rate. However, the data exhibit weak stationarity, so we will proceed 
# as normal, by forecasting the regular data. 

#(C) Plot the Residuals vs. Fitted Values
quartz()
plot(CPI, pch = 20, main = "Consumer Price Index")
lines(fit1$fitted.values, col = "red")

quartz()
plot(Unemp, pch = 20, main = "Unemployment Rate")
lines(fit2$fitted.values, col = "red")

#(D) Plot the ACF and PACF of the respective residuals
quartz()
par(mfrow=c(2,1))
acf(residuals(fit1), main = "ACF Plot of the Residuals for CPI Data")
pacf(residuals(fit1), main = "PACF Plot of the Residuals for CPI Data")

quartz()
par(mfrow=c(2,1))
acf(residuals(fit1), main = "ACF Plot of the Residuals for CPI Data")
pacf(residuals(fit1), main = "PACF Plot of the Residuals for CPI Data")

#(E) Plot the respective CUSUM
quartz()
par(mfrow=c(2,1))
plot(efp(fit1$res~1, type = "Rec-CUSUM"))
plot(efp(fit2$res~1, type = "Rec-CUSUM"))

#(F) Plot the respective Recursive Residuals
quartz()
par(mfrow=c(2,1))
plot(recresid(fit$res~1), pch = 20, ylab = "Recursive Residuals", main = "Recursive Residuals for CPI Fit Model")
plot(recresid(fit2$res~1), pch = 20, ylab = "Recursive Residuals", main = "Recursive Residuals for Unemployment Rate Fit Model")

#(H) 12-steps ahead Forecast
plot(forecast(fit, 12))
plot(forecast(fit2, 12))

#(I) Fit an appropriate VAR model
data=cbind(CPI, Unemp)
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
quartz()
plot(irf(var3))

#(K) Granger-Causality Test*
grangertest(CPI ~ Unemp, order = 21)
grangertest(Unemp ~ CPI, order = 21)

#(L) 12-steps ahead Forecast for VAR model 
var.predict = predict(object=var3, n.ahead=12)
quartz()
plot(var.predict)

#Current Concerns:

#  *Granger-Causality test is creating problems. In the impulse response function we are discovering
#  that as unemployment increases the CPI is decreasing (gradually). This would imply that UR 
#  causes CPI to decrease. Need to determine how to get this Granger test to reflect this. 
#      Possible Solutions: 
#       1. Consider change in CPI (CPI_Chg) in the VAR model
#       2. If CPI is the data we want, continue tweeking the Granger test. 
#       3. Cry. 


