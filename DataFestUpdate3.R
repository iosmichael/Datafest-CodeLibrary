# Time Series Accuracy

# http://www.statmethods.net/advstats/timeseries.html
# https://www.otexts.org/fpp/8/7
# frequency: 1=annual, 4=quartly, 12=monthly
# ARIMA info: order=c(p,d,q) p=order (number of time lags), d=degree of differencing (the 
  # number of times the data have had past values subtracted), q=order of the
  # moving-average model, c(3,1,3) worked best for me 

# Input: datatable, variable, tsStart (desired starting year of time series), tsEnd (desired
# ending year of time series), timespan (number of years to use to fit), method (exp - 
# exponential, seasonal - seasonal decomposition, arima - ARIMA), p, d, q (if using arima)
# Output: error

tsaccuracy <- function(data, varname, tsStart, tsEnd, timespan, methodname, p, d, q){
  #data=apex; varname="New.Bodily.Injury..Frequency"; tsStart=2010; tsEnd=2016; timespan=4; methodname="exp"
  library(forecast)
  # creating time series from Jan tsStart year to Dec tsEnd year
  ts.tmp <- ts(data[, varname], 
           start = c(tsStart, 1), end = c(tsEnd, 12), frequency = 12)
  switch(methodname,
         "exp" = {
             fitexp <- HoltWinters(window(ts.tmp, c(tsStart, 1), c(tsStart+timespan, 12)), 
                                   gamma = FALSE)
             y <- window(ts.tmp, tsStart, c(tsEnd,12)) # observed values
             return(accuracy(forecast(fitexp), y)) # might need to make this into datatable
           #return(avg(x)) # need to return avg MAPE
         }, "seasonal" = {
             fitseas <- stl(window(ts.tmp, tsStart, c(tsEnd,12)), 
                            s.window="periodic")
             y <- window(ts.tmp, tsStart, c(tsEnd,12))
             return(accuracy(forecast(fitseas), y)) # might need to make this into datatable
         }, "arima" = {  
             fitarima <- arima(window(ts.tmp, tsStart, c(tsEnd,12)), 
                               order=c(p, d, q)) 
             y <- window(ts.tmp, tsStart, c(tsEnd,12))
             return(accuracy(forecast(fitarima), y)) # might need to make this into datatable
         }, print("Please specify time series method"))
}



# TESTING TS
#### APEX.R for reference #######
##### New Bodily Injury Frequency ########
# reading in data
apex = read.csv("APEX2017data.csv", stringsAsFactors = FALSE)

# cleaning data
library(data.table)
apex$X = NULL
apex <- apex[-c(1, 59, 79, 81), ] # taking out empty rows
apex <- t(apex) # switching rows and columns
for(i in 1:77) { # assigning variable names
  colnames(apex)[i] <- apex[1,i]
}
apex <- apex[-c(1),]
apex <- data.frame(apex) # making it into dataframe
for (i in 1:length(apex)) {
  apex[, i] <- as.numeric(gsub(",", "", as.character(apex[, i])))
}

apex$date <- rownames(apex)
# apex <- setDT(apex, keep.rownames = TRUE)[] # making row names into one vector column
# apex <- setDT(apex) # making row names into one vector column
# colnames(apex)[which(names(apex) == "rn")] <- "date" # renaming date variable

tsaccuracy(apex, "New.Bodily.Injury..Frequency", 
           2010, 2016, timespan=4, method="exp")

# Predicting with Time Series

# frequency: 1=annual, 4=quartly, 12=monthly
# ARIMA info: order=c(p,d,q) p=order (number of time lags), d=degree of differencing (the 
  # number of times the data have had past values subtracted), q=order of the 
  # moving-average model, c(3,1,3) worked best for me 

# Input: datatable, variable, tsStart (desired starting year of time series), tsEnd (desired
# ending year of time series), start year, end year (timespan of data used for prediction), 
# method (exp - exponential, seasonal - seasonal decomposition, arima - ARIMA), p, d, q (if
# using arima)
# Output: predicted values and forecast plot

tspredict <- function(data, variable, tsStart, tsEnd, startYear, endYear, method, p, d, q){
  library(forecast)
  # creating time series from Jan tsStart year to Dec tsEnd year
  # this is just naming the data with timeframe
  # window() will divide timeseries data into deisired time span
    ts <- ts(as.numeric(as.character(data$variable)), 
             start = c(tsStart, 1), end = c(tsEnd, 12), frequency = 12)
    switch(method,
           exp = {
             fitexp <-  HoltWinters(window(ts, startYear, c(endYear,12)), gamma = FALSE)
             return(plot(forecast(fitexp))) 
             exppred <- forecast(fitexp)
             return(exppred)
           }, seasonal = {
             fitseas <- stl(window(ts, startYear, c(endYear,12)), s.window="periodic")
             return(plot(forecast(fitseas)))
             seaspred <- forecast(fitseas)
             return(seaspred)
           }, arima = {
             fitarima <- arima(window(ts, startYear, c(endYear,12)), order=c(p, d, q)) 
             return(plot(forecast(fitarima)))
             arimapred <- forecast(fitarima)
             return(arimapred)
           }, print("Please specify time series method"))
  }
  

# Covert many predictors as single string for model fitting purposes
# Input: vector of predictors 
# Output: string of predictors summed together ("pred1+pred2+...+predn")
predictors <- function(vector){
  return(paste(vector[1:length(vector)], collapse = " + "))
}

test <- c("weight", "height", "water")
predictors(test)

# Stepwise Generalized Additive Model (GAM) with Smoothing Splines

# family=gaussian if proportion, family=poisson if count
# Input:datatable, variable, predictors ("pred1+pred2+...+predn")
# Output: best combo of splines and predictors to fit variable with AIC
stepgam <- function(data, variable, predictor){
  library(splines)
  library("gam")
  sgam <- gam(paste(variable, "~", predictor), data = data, family = gaussian)
  sgam1 <- step.gam(sgam, scope = list("predictor1" = c("1", "predictor1", "s(predictor1)"),
                                       "predictor2" = c("1", "predictor2", "s(predictor2)")))
  return(sgam1)
}

# Generalized Additive Model (GAM) with Smoothing Splines

# family=gaussian if proportion, family=poisson if count
# Input:datatable, variable, significant predictors from step.gam ("pred1+pred2+...+predn")
# Output: plot and summary of GAM model
gam <- function(data, variable, sigpredict){
  library(splines)
  library("gam")
  gam <- gam(paste(variable, "~", sigpredict), data = data, family = gaussian)
  par(mfrow = c(1,2)) # multiple plots - 1 row 2 column
  return(plot.gam(gam, rugplot = FALSE, se = TRUE))
  return(summary(gam))
  
}

testvars <- c("sanitation", "water", "electricity", "GDP", "population")
test.gam <- gam(epipre.dp, "death.p", predictors(testvars))
 
# Goodness of Fit of GAM

# testing against null hypothesis model with no predictors to see whether the models are
# actually significant than not using the predictors at all
# Input: datatable, variable, fitted gam model
# Output: anova summary
gamgoodness <- function(data, variable, gam){
  mod0 <- gam(variable ~ 1, data = data, family = gaussian)
  return(anova(mod0, gam, test = "Chisq"))
}


# Predicting with GAM

# probabily doesn't show standard errors
# Input: fitted GAM model, data of year we want to predict
# Output: predicted values
gampred <- function(data, gam){
  pred <- predict(gam, newdata = data, se.fit = TRUE)
  return(pred)
}


# REMEMBER AMAZON EXAMPLE


