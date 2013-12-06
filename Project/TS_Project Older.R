# assumes that file is in the current working directory
# change working directory with setwd("~/My Documents/..,")
# confirm working directory with getwd()

library(forecast)
library(tseries)

bt <- read.table("F:\\Time Series\\Bath Tissue Movement.csv")
bt <- as.ts(bt)

adf.test(bt)
pp.test(bt)

plot(bt)
acf(bt)
pacf(bt)

(bt_fcst <- auto.arima(bt))
plot(forecast(bt_fcst))

###########################
# so far, my favorite
# day over day difference
bt_d1 <- diff(bt)

adf.test(bt_d1)
pp.test(bt_d1)

plot(bt_d1)
acf(bt_d1)
pacf(bt_d1)

(bt_d1_fcst <- auto.arima(bt_d1))
plot(forecast(bt_d1_fcst))
#############################

# week over week difference
bt_d7 <- diff(bt, d=7)

plot(bt_d7)
acf(bt_d7)
pacf(bt_d7)

adf.test(bt_d7)
pp.test(bt_d7)

(bt_d7_fcst <- auto.arima(bt_d7))
plot(forecast(bt_d7_fcst))

# day over day, week over week difference
bt_d1_d7 <- diff(bt)
bt_d1_d7 <- diff(bt_d1_d7, d=7)

adf.test(bt_d1_d7)
pp.test(bt_d1_d7)

plot(bt_d1_d7)
acf(bt_d1_d7)
pacf(bt_d1_d7)

(bt_d1_d7_fcst <- auto.arima(bt_d1_d7))
plot(forecast(bt_d1_d7_fcst))

#########################################
# building on the day over day difference
print(bt_fcst <- auto.arima(bt, d = 1))
plot(forecast(bt_fcst))

#### Here's what I'm seeing so far -- seems to even follow a decent path
# Series: bt 
# ARIMA(2,1,4)                    
# 
# Coefficients:
#   ar1      ar2      ma1     ma2      ma3      ma4
# 1.2460  -0.9955  -1.8567  1.4964  -0.3318  -0.2025
# s.e.  0.0046   0.0045   0.0516  0.1122   0.1146   0.0543
# 
# sigma^2 estimated as 11415:  log likelihood=-2430.03
# AIC=4864.04   AICc=4864.33   BIC=4891.96


######### taking a peek at 7-day moving average
bt_ma7 <- ma(bt, order = 7)

plot(bt_ma7)
acf(bt_ma7, na.action = na.pass)
pacf(bt_ma7, na.action = na.pass)

(bt_ma7_fcst <- auto.arima(bt_ma7))
plot(forecast(bt_ma7_fcst))


######### taking a peek at 7-day moving average w/ diff
bt_ma7_d1 <- diff(bt_ma7)

plot(bt_ma7_d1)
acf(bt_ma7_d1, na.action = na.pass)
pacf(bt_ma7_d1, na.action = na.pass)

### ...this might be just right
(bt_ma7_d1_fcst <- auto.arima(bt_ma7_d1))
plot(forecast(bt_ma7_d1_fcst))


### I think this overdid it
######### taking a peek at 7-day moving average w/ diff = 7
bt_ma7_d7 <- diff(bt_ma7, d = 7)

plot(bt_ma7_d7)
acf(bt_ma7_d7, na.action = na.pass)
pacf(bt_ma7_d7, na.action = na.pass)

(bt_ma7_d7_fcst <- auto.arima(bt_ma7_d7))
plot(forecast(bt_ma7_d7_fcst))




#### Bootstrapping ####

## Bootstrap for ARIMA(2,1,4) <- this is the one we liked best
# mean is zero

  Y  <- bt
  n = length(Y)


  Est2 <- arima(Y, order=c(2,1,4))
  Res2 <- Est2$residuals
  MLE2 <- Est2$coef

  MLE3  <- matrix(0,100,length(MLE2))  #matrix to record estimated values
  Vars3 <- MLE3

  for (i in 1:100) {

    et <- sample(Res2, n, replace=TRUE) # Bootstrapping Residuals
    Y2  <- arima.sim(n=n, list(ar = c(MLE2[1],MLE2[2]), ma = c(MLE2[3],MLE2[4],MLE2[5],MLE2[6])), innov=et  )

    Est3 <- arima(Y2, order=c(2,1,4))

    MLE3[i,]  <- Est3$coef
    Vars3[i,] <- c(Est3$var.coef[1,1], Est3$var.coef[2,2], Est3$var.coef[3,3],Est3$var.coef[4,4],Est3$var.coef[5,5],Est3$var.coef[6,6])

  }

# Note: sometimes get error: "Error in arima(Y2, order = c(2, 1, 4)) : non-stationary AR part from CSS"

#### Results ##########

#--- Estimate for phi1 ---
c(mean(MLE3[,1]), var(MLE3[,1]) )

#--- Estimate for phi2 ---
c(mean(MLE3[,2]), var(MLE3[,2]) )

#--- Estimate for theta1 ---
c(mean(MLE3[,3]), var(MLE3[,3]) )

#--- Estimate for theta2 ---
c(mean(MLE3[,4]), var(MLE3[,4]) )

#--- Estimate for theta3 ---
c(mean(MLE3[,5]), var(MLE3[,5]) )

#--- Estimate for theta4 ---
c(mean(MLE3[,6]), var(MLE3[,6]) )

#######################


 


