##linear and multiple regression (also stepwise), and lasso
library(foreign)


# Seperating the dataset into training/testing datasets
# Do result[1] for training set, result[2] for testing set
trainTest <- function(testStart, bigData){
  training <- bigData[1:testStart,]
  testing <-  bigData[testStart+1:nrow(bigData),]
  return(list(training, testing))
}


## Simple Linear Regression
# Returns the simple linear regression model
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

## Simple Linear Residual Plot
# A disirable residual plot:
# (1) symmetrically distributed, tending to cluster towards the middle of the plot
# (2) clustered around the lower single digits of the y-axis (e.g., 0.5 or 1.5, not 30 or 150)
# (3) in general there no clear patterns
simpleLmResidualPlot <- function(bigData, model, predictor){
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

# Tests significancy of predictors in multiple linear regression (p value)
multipleSignificant <- function(model){
  #toReturn <- 
  #for(i in 2:ncol(summary(model)$coef)){
  # if(summary(model)$coef[i, 4] > 0.05){
      
  #  }
#  }
  print(summary(model)$coef)
}

# Goodness of fit
# The goodness of fit of a regression model can be evaluated by: R squared 
# Basically prints out the r squared
gofLinear <- function(model){
  print("R-squared")
  print(summary(model)$r.squared)
}

# Logistic Regression
logistic <- function(formula, bigData){
  return(glm(formula, data = bigData, family = binomial))
}

# Goodness of fit
# The goodness of fit of a logistic model can be evaluated by: chisq test
# 
gofLogistic <- function(model, predictor, bigData){
  anova(model, test = "Chisq")
  # 
  # hoslem.test(x = predictor, y = predict(model, type = "response"), g = 10)
}

# Predict by model
predictByModel <- function(model, bigData, predictors, values){
  newData <- data.frame(predictor1 = value1, predictor2 = value2, predictor3 = value3) ## etc...
  print("Confidence interval:")
  predict(model, newData, interval="confidence")
  print("Prediction interval:")
  predict(model, newData, interval="predict")
  print("Predict response:")
  predict(model, newData, type="response")
}

# Predicting the accuracy of a certain model
predictionAccuracy <- function(testing, response, model){
  fitted.results <- predict(model,newdata=testing[1],type='response')
  if(model$family[1] == "binomial"){
    fitted.results <- ifelse(fitted.results > 0.5,1,0)
    misClasificError <- mean(fitted.results != testing$response)
  }else{
    fitted.results <- ifelse((abs(fitted.results - testing$response)/testing$response) < 0.05,1,0)
    misClasificError <- mean(fitted.results == 1)
  }
  print(paste('Accuracy',1-misClasificError))
}

## Stepwise
# Using the provided model and scope, and returns the formula
stepWiseLinear <- function(response, bigData){
  form <- paste(response, "~", paste(setdiff(names(bigData), response), collapse = "+"))
  step.1 <- step(lm(form, data = bigData), scope= form , direction="both", trace=TRUE)
  temp <- step.1$call[2]
  return(substr(temp, 1, nchar(temp)))
}

## Lasso
# http://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html glmnet package
# Returns a matrix containing the predictors with non-zero coeffitients
lassoFit <- function(bigData, x, y){
  library(glmnet)
  # Making the coefficient plot
  lasso.mod <- glmnet(x,y,alpha=1,lambda=grid)
  plot(lasso.mod)
  lasso.coef <- predict(lasso.mod,type="coefficients",s=bestlam)[1:20,]
  return(as.matrix(lasso.coef[lasso.coef!=0]))
  
  
  # Cross validation to find best lamda
  lasso.mod <- glmnet(x.train,y.train,alpha=1,lambda=grid)
  cv.glmn.1 <- cv.glmnet(x.train, y.train, alpha = 1)
  plot(cv.glmn.1)
  #### HAve  to figure out
  bestlam <- cv.out$lambda.???
  coef <- as.matrix(coef(cv.glmn.1, s=bestlam))

  # Predicting with lasso
  lasso.pred <- predict(lasso.mod, s = bestlam, newx = x.test)
  # Accuracy test -- mean square error
  mean((lasso.pred-y.test)^2)
}

## Put in loading libraries in corresponding functions
## Explanation format
# anova with ~1(nothing)

## Don't forget the AMAZON EXPAMPLE!!!
