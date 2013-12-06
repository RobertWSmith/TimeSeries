#--Produces a data.frame with the Source Data + Training Data, Fitted Values + Forecast Values, forecast data Confidence Intervals
funggcast <- function(dn, fcast){ 
  require(zoo) #needed for the 'as.yearmon()' function
  
  en <- max(time(fcast$mean)) #extract the max date used in the forecast
  
  #Extract Source and Training Data
  ds <- as.data.frame(window(dn, end = en))
  names(ds) <- 'observed'
  ds$date <- as.Date(time(window(dn, end = en)))
  
  #Extract the Fitted Values (need to figure out how to grab confidence intervals)
  dfit <- as.data.frame(fcast$fitted)
  dfit$date <- as.Date(time(fcast$fitted))
  names(dfit)[1] <- 'fitted'
  
  ds <- merge(ds, dfit, all.x = T) #Merge fitted values with source and training data
  
  #Exract the Forecast values and confidence intervals
  dfcastn <- as.data.frame(fcast)
  dfcastn$date <- as.Date(as.yearmon(row.names(dfcastn)))
  names(dfcastn) <- c('forecast', 'lo80', 'hi80', 'lo95', 'hi95', 'date')
  
  pd <- merge(ds, dfcastn, all.x = T) #final data.frame for use in ggplot
  return(pd)
  
}

#----------Simulate an Arima (2, 1, 1) Process-------------
library(forecast)

set.seed(1234)  
y <- arima.sim(model = list(order = c(2, 1, 1), ar = c(0.5, .3), ma = 0.3), n = 144)
y <- ts(y, freq = 12, start = c(2000, 1))

#-- Extract Training Data, Fit the Wrong Model, and Forecast
yt <- window(y, end = 2009.99)

yfit <- Arima(yt, order = c(1, 0, 1))

yfor <- forecast(yfit)

#---Extract the Data for ggplot using funggcast()
pd <- funggcast(y, yfor)

#---Plot in ggplot2 0.9
library(ggplot2)
library(scales)


(p1a <- ggplot(data = pd, aes(x = date, y = observed)) + 
  geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = 'orange', alpha =  0.25) + 
  geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = 'orange', alpha = 0.5) + 
  geom_line(col = 'red') + 
  geom_line(aes(y = fitted), col = 'blue') + 
  geom_line(aes(y = forecast)) + 
  scale_x_date(name = '', breaks = '1 year', minor_breaks = '1 month', 
               labels = date_format("%b-%y"), expand = c(0, 0)) + 
  scale_y_continuous(name = 'Units of Y') + 
  ggtitle('Arima Fit to Simulated Data'))
