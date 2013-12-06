mydata = read.table("J:\\Time Series\\Project\\Bath Tissue Movement.csv",header=F,sep=",") 
mydata = ts(mydata,c(1,1),freq=1)

plot(mydata, type='o')


acf.orig <- acf    # keep the default acf() function before it is overwritten by TSA package
library(TSA)
acf <- acf.orig    # overwrite acf() by TSA package with default acf()
library(forecast)
library(tseries)

acf(mydata)

pacf(mydata)

auto.arima(mydata)

auto.arima(mydata,seasonal=T)


# augmented dickey-fuller test
adf.test(mydata)

# The data appears to be stationary.





# ignore this stuff
Est = arima(mydata, order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
Est
plot(Est)
Randomness.tests(Est$residuals)


auto.arima(mydata,D=1,seasonal=TRUE)