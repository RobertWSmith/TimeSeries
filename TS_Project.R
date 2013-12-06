# assumes that file is in the current working directory
# change working directory with setwd("~/My Documents/..,")
# confirm working directory with getwd()

library(forecast)
library(tseries)

bt <- read.table("Bath Tissue Movement.csv")
bt <- ts(bt, frequency = 7)

adf.test(bt)
pp.test(bt)

plot(bt)
acf(bt)
pacf(bt)

####################################
### we like this one so far the best
(bt_fcst <- auto.arima(bt, d = 1, D = 1))
plot(forecast(bt_fcst))

# arima_mdl <- Arima(bt, order = c(2, 0, 1), seasonal = list(order = c(2, 1, 2), period = 7))

(bt.log <- log(bt))
# (bt.log.fcts <- auto.arima(bt.log, d=1, D=1))
f.bt.log <- arima(
  bt.log, 
  order = c(0,1,3), 
  seasonal = list(order = c(0,1,1), period = 7)
)

plot(forecast(f.bt.log))
Randomness.tests(f.bt.log$residuals, FALSE)

gf <- garchFit(~garch(1,1), data = f.bt.log$residuals, cond.dist = "norm", include.mean = FALSE, trace = FALSE)
print(gf)

coef(gf)
res <- f.bt.log$residuals/gf@sigma.t
plot(res)
Randomness.tests(res, FALSE)

ORDER <- c(2, 0, 1)
SEASONAL <- list(order = c(2, 1, 2), period = 7)

mc.estimate <- function(TS, Order, Seasonal, reps) {
  library(fGarch) # for skew-t distribution
  library(forecast) #for simulate.Arima function
  stopifnot(length(reps) == 1) 
  
  Arima_est <- Arima(TS, order = Order, seasonal = Seasonal)
  
  for (i  in 1:reps) {
    X <- simulate(Arima_est, bootstrap = TRUE) 
    Est <- Arima(X, order = Order, seasonal = Seasonal)
    if (i == 1) {
      Vars <- MLE <- matrix(0, reps, length(Est$coef))
    }
    MLE[i, ] <- Est$coef
    Vars[i, ] <- diag(Est$var.coef)
  }
  
  df <- as.data.frame(cbind(MLE, Vars))
  colnames(df) <- c(paste0("MLE", names(Est$coef)), paste0("VAR", names(diag(Est$var.coef))))
  
  return(df)
}

bootstrap.estimate <- function(TS, Order, Seasonal, reps) {
  library(fGarch) # for skew-t distribution
  library(forecast) #for simulate.Arima function
  stopifnot(length(reps) == 1) 
  
  Arima_est <- Arima(TS, order = Order, seasonal = Seasonal)
  res <- Arima_est$residuals
  mle <- Arima_est$coef
  
  for (i  in 1:reps) {
    et <- sample(res, length(res), replace = TRUE)
    est2 <- simulate(Arima_est, innov = et)
    est3 <- Arima(est2, order = Order, seasonal = Seasonal)
    
    X <- simulate(Arima_est, bootstrap = TRUE) 
    Est <- Arima(X, order = Order, seasonal = Seasonal)
    
    if (i == 1) {
      Vars <- MLE <- matrix(0, reps, length(Est$coef))
    }
    MLE[i, ] <- Est$coef
    Vars[i, ] <- diag(Est$var.coef)
  }
  
  df <- as.data.frame(cbind(MLE, Vars))
  colnames(df) <- c(paste0("MLE", names(Est$coef)), paste0("VAR", names(diag(Est$var.coef))))
  
  return(df)
}

Order <- 0:2
Seasonal <- list(order = Order, period = 7)

mc <- mc.estimate(bt, Order, Seasonal, 1000)

boot <- bootstrap.estimate(bt, Order, Seasonal, 1000)

for (i in 1:ncol(mc)) {
  print(hist(mc[[i]], main = paste("Histogram of", names(mc)[i])))
}

for (i in 1:ncol(boot)) {
  print(hist(boot[[i]], main = paste("Histogram of", names(boot)[i])))
}

library(ggplot2)
library(gridExtra)
library(reshape)

monte <- melt(mc)

p1 <- ggplot(mc, aes(x = MLEma1)) + 
  geom_histogram(aes(y= ..density..), binwidth = 0.01) + 
  stat_density(col = "red", geom = "line") + 
  geom_vline(xintercept = bt_fcst$coef[1], color = "blue") + 
  geom_vline(xintercept = mean(mc[[1]]), color = "green" ) + 
  labs(title = "Monte Carlo MLE MA(1)", x = "MLE MA(1)")

p2 <- ggplot(mc, aes(x = MLEma2)) + 
  geom_histogram(aes(y= ..density..), binwidth = 0.01) + 
  stat_density(col = "red", geom = "line") +
  geom_vline(xintercept = bt_fcst$coef[2], color = "blue") + 
  geom_vline(xintercept = mean(mc[[2]]), color = "green" ) + 
  labs(title = "Monte Carlo MLE MA(2)", x = "MLE MA(2)")

p3 <- ggplot(mc, aes(x = MLEsma1)) + 
  geom_histogram(aes(y= ..density..), binwidth = 0.01) + 
  stat_density(col = "red", geom = "line") +
  geom_vline(xintercept = bt_fcst$coef[3], color = "blue") + 
  geom_vline(xintercept = mean(mc[[3]]), color = "green" ) + 
  labs(title = "Monte Carlo MLE SMA(1)", x = "MLE SMA(1)")

p4 <- ggplot(mc, aes(x = MLEsma2)) + 
  geom_histogram(aes(y= ..density..), binwidth = 0.01) + 
  stat_density(col = "red", geom = "line") +
  geom_vline(xintercept = bt_fcst$coef[4], color = "blue") + 
  geom_vline(xintercept = mean(mc[[4]]), color = "green" ) + 
  labs(title = "Monte Carlo MLE SMA(2)", x = "MLE SMA(2)")

grid.arrange(p1, p2, p3, p4, nrow = 2)




p1 <- ggplot(boot, aes(x = MLEma1)) + 
  geom_histogram(aes(y= ..density..), binwidth = 0.01) + 
  stat_density(col = "red", geom = "line") + 
  geom_vline(xintercept = bt_fcst$coef[1], color = "blue") + 
  geom_vline(xintercept = mean(boot[[1]]), color = "green" ) + 
  labs(title = "Boostrap MLE MA(1)", x = "MLE MA(1)")

p2 <- ggplot(boot, aes(x = MLEma2)) + 
  geom_histogram(aes(y= ..density..), binwidth = 0.01) + 
  stat_density(col = "red", geom = "line") +
  geom_vline(xintercept = bt_fcst$coef[2], color = "blue") + 
  geom_vline(xintercept = mean(boot[[2]]), color = "green" ) + 
  labs(title = "Boostrap MLE MA(2)", x = "MLE MA(2)")

p3 <- ggplot(boot, aes(x = MLEsma1)) + 
  geom_histogram(aes(y= ..density..), binwidth = 0.01) + 
  stat_density(col = "red", geom = "line") +
  geom_vline(xintercept = bt_fcst$coef[3], color = "blue") + 
  geom_vline(xintercept = mean(boot[[3]]), color = "green" ) + 
  labs(title = "Boostrap MLE SMA(1)", x = "MLE SMA(1)")

p4 <- ggplot(boot, aes(x = MLEsma2)) + 
  geom_histogram(aes(y= ..density..), binwidth = 0.01) + 
  stat_density(col = "red", geom = "line") +
  geom_vline(xintercept = bt_fcst$coef[4], color = "blue") + 
  geom_vline(xintercept = mean(boot[[4]]), color = "green" ) + 
  labs(title = "Boostrap MLE SMA(2)", x = "MLE SMA(2)")

grid.arrange(p1, p2, p3, p4, nrow = 2)


################ GARCH

bt_diff <- diff(bt, 1, 7)

adf.test(bt_diff)
pp.test(bt_diff)

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(bt_diff)
acf(bt_diff)
pacf(bt_diff)

# testing from library(fGarch)
garchFit(data = bt_fcst$residuals)
out <- garchFit(data = bt_fcst$residuals, cond.dist = "norm", include.mean = FALSE, trace = FALSE)


coef(out)
res <- bt_fcst$residuals/out@sigma.t

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(res)
acf(res)
pacf(res)


# mont <- ggplot(monte, aes(x = value, y = ..density..)) + geom_histogram(binwidth = 0.01) + stat_density(col = "red", geom = "line") 











Randomness.tests <- function( A, plott=TRUE) {
  
  library(tseries) 
  
  L1 <- Box.test(A,   lag = 15, type = "Ljung-Box")
  L2 <- Box.test(A,   lag = 20, type = "Ljung-Box")
  L3 <- Box.test(A,   lag = 25, type = "Ljung-Box")
  L4 <- Box.test(A^2, lag = 15, type = "Ljung-Box")
  L5 <- Box.test(A^2, lag = 20, type = "Ljung-Box")
  L7 <- jarque.bera.test(A)
  S1 <- sd(A)
  
  if (plott) {
    layout( matrix(c(rep(1,8),2,3,4,8,5,6,7,8), 4, 4, byrow=T) )
    plot(A, type='l')
    acf(A)
    pacf(A)
    plot(density(A, bw="SJ-ste"), main= "")
    acf(abs(A))
    acf(A^2)
    qqnorm(A)
    
    plot(c(-1,-1), xlim=c(0,1), ylim=c(0,1), ann=F, axes=F)
    
    text( 0.5,0.95, paste("Box-Ljung test"), cex=1.3 )
    text( 0.5,0.9,  paste("H=15:p= ", round(L1$p.value, 4)), cex=1 )
    text( 0.5,0.85, paste("H=20:p= ", round(L2$p.value, 4)), cex=1 )
    text( 0.5,0.8,  paste("H=25:p= ", round(L3$p.value, 4)), cex=1 )
    text( 0.5,0.7,  paste("McLeod-Li test") , cex=1.3 )
    text( 0.5,0.65, paste("H=15:p= ", round(L4$p.value, 4)), cex=1 )
    text( 0.5,0.6,  paste("H=20:p= ", round(L5$p.value, 4)), cex=1 )
    text( 0.5,0.35, paste("Jaque-Bera test") , cex=1.3 )
    names(L7$p.value) <- ""
    text( 0.5,0.3,  paste("p= ", round(L7$p.value, 4)), cex=1 )
    text( 0.5,0.2,  paste("sample SD ", round(S1, 4)), cex=1 )
    
    layout( matrix(1, 1, 1) )
  }
  
  return( t(t(c("BL15"=round(L1$p.value, 3), "BL20"=round(L2$p.value,3),
                "BL25"=round(L3$p.value,3), "ML15"=round(L4$p.value,3),
                "ML20"=round(L5$p.value,3), "JB"=round(L7$p.value,3), 
                "SD" = round(S1, 3) ))))
  
}
