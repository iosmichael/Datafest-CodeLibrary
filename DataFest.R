function()
  input, parameters, output, mechanism (opt)
# time series analysis
  
# prediction and accuracy
# removing commas from numbers
data$subset <- as.numeric(gsub(",","", data$Earned.Premium))
  # testing accuracy of models
  # creating time series from Jan 2010 - Dec 2016
  ts.nbif <- ts(as.numeric(as.character(apex$New.Bodily.Injury..Frequency)), 
                start = c(2010, 1), end = c(2016, 12), frequency = 12)
  # exponential models
  # 4 year time span (MAPE: 12.02, 11.54, 7.71----10.42% avg error)
  fit6.1 <- HoltWinters(window(ts.nbif, 2010, c(2013,12)), gamma = FALSE) # exponential model 2010-13
  fit6.2 <- HoltWinters(window(ts.nbif, 2011, c(2014,12)), gamma = FALSE) # exponential model 2011-14
  fit6.3 <- HoltWinters(window(ts.nbif, 2012, c(2015,12)), gamma = FALSE) # exponential model 2012-15
  y <- window(ts.nbif, 2016, c(2016,12)) # observed values
  accuracy(forecast(fit6.3), y) # accuracy of forecasted vs observed
  # seasonal decomposition
  fit1 <- stl(window(ts.nbif, 2011, c(2015,12)), s.window="periodic")
  x <- window(ts.nbif, 2016, c(2016,12)) # observed values
  accuracy(forecast(fit1), x)
  # ARIMA models
  # 4 year time span (MAPE: 7.31, 7.29, 8.55----7.72% avg error)
  fit3.1 <- arima(window(ts.nbif, 2010, c(2013,12)), order=c(3, 1, 3)) # avg over 3 months, autoregressive 
  fit3.2 <- arima(window(ts.nbif, 2011, c(2014,12)), order=c(3, 1, 3)) # right 3 avg months for moving average
  fit3.3 <- arima(window(ts.nbif, 2012, c(2015,12)), order=c(3, 1, 3)) 
  x <- window(ts.nbif, 2014, c(2014,12)) # observed values 
  accuracy(forecast(fit3.3), x) # accuracy of forecasted vs observed
  # average method 9.35, 7.95, 7.52, 7.57, 6.44
  fit3.6 <- meanf(window(ts.nbif, 2014, c(2015,12)), 12)
  x <- window(ts.nbif, 2016, c(2016,12))
  accuracy(forecast(fit3.6, x))
  # forecast 2017
  # creating time series from Jan 2010 - Dec 2016
  fit3 <- arima(window(ts.nbif, 2013, c(2016,12)), order=c(3, 1, 3)) 
  plot(forecast(fit3)) # plotting predicted forecast from 2013-2016 data
  nbifpred <- forecast(fit3) # forecasted values for 2017
  
  fit3.7 <- meanf(window(ts.nbif, 2015, c(2016,12)), 12)
  plot(forecast(fit3.7))
  nbifpred <- forecast(fit3.7)
  # testing accuracy of models
  # creating time series from Jan 2010 - Dec 2016
  ts.ep <- ts(as.numeric(apex$Earned.Premium), 
              start = c(2010, 1), end = c(2016, 12), frequency = 12)
  
  # 4 year time span (MAPE: 3.08, 2.07, 1.30----2.15% avg error)
  fit37.1 <- HoltWinters(window(ts.ep, 2010, c(2013,12)), gamma = FALSE) # exponential model 2010-13
  fit37.2 <- HoltWinters(window(ts.ep, 2011, c(2014,12)), gamma = FALSE) # exponential model 2011-14
  fit37.3 <- HoltWinters(window(ts.ep, 2012, c(2015,12)), gamma = FALSE) # exponential model 2012-15
  y <- window(ts.ep, 2014, c(2014,12)) # observed values
  accuracy(forecast(fit37.1), y) # accuracy of forecasted vs observed
  # predicting 2017 with 5 year data
  fit37 <-  HoltWinters(window(ts.ep, 2012, c(2016,12)), gamma = FALSE)
  plot(forecast(fit37)) # plotting predicted forecast from 2012-2016 data
  eppred <- forecast(fit37) # forecasted values for 2017
  
  # chisq?
  
  
  # spline, gam (also stepwise)
  
  
  
  
