# assumes that file is in the current working directory
# change working directory with setwd("~/My Documents/..,")
# confirm working directory with getwd()

library(forecast)
library(tseries)
library(fGarch)

btin <- read.table("F:\\Time Series\\Bath Tissue Movement.csv")
bt <- ts(btin)

pp.test(bt)
kpss.test(bt)

plot(diff(bt))
#acf(bt)
#pacf(bt)

(bt_fcst <- auto.arima(bt))
plot(forecast(bt_fcst))

Res1 = bt_fcst$residuals
Randomness.tests(Res1)

#########################

bt <- ts(btin,frequency=7)

# bests:
# ARIMA(1,0,1)(2,1,1) aic 4710.18
# ARIMA(2,0,1)(2,1,1) aic 4706.89
# ARIMA(2,0,1)(2,1,2) aic 4704.29 <-
# ARIMA(2,0,1)(3,1,3) aic 4710.02

(bt_fcst <- arima(bt,order=c(2,0,1),seasonal=list(order=c(2,1,2),period=7)))

plot(forecast(bt_fcst))

res3 = bt_fcst$residuals
Randomness.tests(res3)

plot(res3)

################
bt.log <- log(ts(btin,frequency=7))

#f.bt.log = auto.arima(bt.log,d=1,D=1)
(f.bt.log = arima(bt.log,order=c(0,1,3),seasonal=list(order=c(0,1,1),period=7)))

plot(forecast(f.bt.log))

res2 = f.bt.log$residuals
Randomness.tests(res2)

plot(res2)


################ GARCH

out <- garchFit(~ garch(1,1),data = res2, cond.dist = "norm", include.mean = FALSE, trace = FALSE)


coef(out)
res <- res2/out@sigma.t

Randomness.tests(res)

plot(res)
