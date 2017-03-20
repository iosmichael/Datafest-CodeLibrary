##linear and multiple regression (also stepwise), and lasso
library(foreign)

## Linear Regressions:
# Is there a relationship? F stats
# How strong is the relationship? R^2
# Which predictor contributes to the response? p value
# How much do each predictor contribute? confidence interval
# Accuracy of prediction? prediction/confidence interval
# Linear? residual plot
# Synergy? interaction terms
#
## Simple Linear Regression
# Returns the simple linear regression model
# Don't call this function, just copy/paste it
simpleLinear <- function(response, predictor, bigData){
  return(lm(paste(response, "~", predictor) , data = bigData))
}

# Tests significancy of predictor in simple linear regression (p value)
simpleSignificant <- function(model){
  if(summary(model)$coef[2, 4] > 0.05){
    print("High p value, insignificant")
  }else{
    print("Low p value, significant")
  }
}

# Simple linear predict(predicting confidence/prediction intervals)
simpleLmPredict <- function(model, predictor, value){
  print("Confidence interval:")
  newData <- data.frame(predictor = value)
  predict(model, newData, interval="confidence")
  print("Prediction interval:")
  predict(model, newData, interval="predict")
}

## Simple Linear Residual Plot
# A disirable residual plot:
# (1) symmetrically distributed, tending to cluster towards the middle of the plot
# (2) clustered around the lower single digits of the y-axis (e.g., 0.5 or 1.5, not 30 or 150)
# (3) in general there no clear patterns
simpleLmResidualPlot <- function(bigData, model, predictor = ""){
  res = resid(model)
  plot(bigData$predictor, res, ylab="Residuals", xlab=predictor, 
       main="Residual Plot") 
  abline(0, 0)                 
}


## Multiple Linear Regression
# Returns the multiple linear regression model
multiLinear <- function(formula, bigData){
  return(lm(formula, data = bigData))
}

# Multiple linear predict(predicting confidence/prediction intervals)
multiLmPredict <- function(model, predictors, values){
  print("Confidence interval:")
  newData <- data.frame(predictor1 = value1, predictor2 = value2, predictor3 = value3) ## etc...
  predict(model, newData, interval="confidence")
  print("Prediction interval:")
  predict(model, newData, interval="predict")
}

# Tests significancy of predictors in multiple linear regression (p value)
multipleSignificant <- function(model){
  #???
}

# Goodness 
# anova with ~1(nothing)
# The goodness of fit of a no categorical model is 
gofLinear <- function(model, bigData){
  library(MASS)
  chisq.test(model)
}


## Stepwise
# Using the provided model and scope, and returns the stepwise regression result
stepWiseLinear <- function(response, bigData){
  form <- paste(response, "~", paste(setdiff(names(bigData), response), collapse = "+"))
  step.1 <- step(lm(form, data = bigData), scope= bigData , direction="both", trace=TRUE)
  temp <- step.1$call[2]
  return(substr(temp, 1, nchar(temp)))
}

## Lasso
# Returns a matrix containing the predictors with non-zero coeffitients
lassoFit <- function(bigData, x, y){
  # Making the coefficient plot
  lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
  plot(lasso.mod)
  
  out=glmnet(x,y,alpha=1,lambda=grid)
  lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
  return(as.matrix(lasso.coef[lasso.coef!=0]))
}

# Prediction
predictByModel <- function(modelFit, bigData){
  predict(slm.fit, data.frame(lstat = c(5,10,15)), interval = "confidence")
  predict(slm.fit, data.frame(lstat = c(5,10,15)), interval = "prediction")
  
}

# Logistic

## Prediction & Accuracy -> test
## Can retrieve local(in-function) variables by using $
## Put in loading libraries in corresponding functions
## Explanation format
## Don't forget the AMAZON EXPAMPLE!!!
