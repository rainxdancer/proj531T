# Estimation of threshold time series models using efficient jump MCMC
# Goldman & Agbeyegbe 2005
# v1: Market yield on U.S. Treasury securities at 3-month   constant maturity, quoted on investment basis
# v2: 3-month Treasury bill secondary market rate   discount basis
# http://econ.hunter.cuny.edu/wp-content/uploads/2013/09/HunterEconWP406.pdf

library(tseries)
library(astsa)
library(ggplot2)
library(ggfortify)
library(zoo)
library(forecast)

rawdata = read.csv("FRB_H15.csv")
rate.ts = ts(rawdata$marketrate,frequency = 12, start = c(1962, 1))
plot(rate.ts)
dec = decompose(rate.ts)
# large variance spikes in randomness at a specific segment
plot(dec$random)
acf(na.remove(dec$random))

log.ts = ts(log(rate.ts),frequency = 12, start = c(1962, 1))
plot(log.ts)
dec.log = decompose(log.ts)
plot(dec.log)

drate = diff(rate.ts)
lag1.plot(drate)

# 1979-1982 high volatility
# estimated threshold:  r = 10.03%
# Regime 2, y_t > r
small.ts = ts(rate.ts[205:252],frequency=12,start = c(1979, 1))
plot(decompose(small.ts))
dsmall = diff(small.ts)
lag1.plot(dsmall)
resid = decompose(small.ts)$random
acf(na.remove(resid))
acf(dsmall)

AR <- arima(small.ts, order = c(1,0,1))
print(AR)
plot(small.ts)
AR_fit <- small.ts - residuals(AR)
points(AR_fit, type = "l", col = 2)

# Regime 1. y_t < r
seg1 = ts(rate.ts[1:204],frequency=12,start = c(1962, 1))
plot(decompose(seg1))
d1 = diff(seg1)
lag1.plot(d1)
acf(d1)

AR1 <- arima(seg1, order = c(1,0,1))
print(AR1)
plot(seg1)
AR_fit <- seg1 - residuals(AR1)
points(AR_fit, type = "l", col = 2)


seg2 = ts(rate.ts[253:528],frequency=12,start = c(1980, 1))
plot(decompose(seg2))
d2 = diff(seg2)
lag1.plot(d2)
acf(d2)

g3 = arima(small.ts,order=c(1, 0, 1))
autoplot(forecast(g3)) + autolayer(seg2, color="red")