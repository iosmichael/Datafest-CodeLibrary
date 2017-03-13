##linear and multiple regression (also stepwise), and lasso
library(foreign)
library(MASS)

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
simpleLinear <- function(response, predictor, bigData){
return(lm(c(reponse, "~",predictor ), data = bigData))
}

## Multiple Linear Regression
multiLinear <- function(formula, bigData){
return(lm(formula, data = bigData))
}


## Stepwise
# Using the provided model and scope, and returns the stepwise regression result
stepWise(model, bigData){
return(step(model, scope= paste(names(bigData)[-1], collapse=" + ")
                                , direction="both", trace=TRUE))
}


## Lasso
# Returns a matrix containing the predictors with non-zero coeffitients
lassoFit(bigData, x, y){
  # Making the coefficient plot
  lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
  plot(lasso.mod)

  out=glmnet(x,y,alpha=1,lambda=grid)
  lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
  return(as.matrix(lasso.coef[lasso.coef!=0]))
}

# Prediction
predictByModel(modelFit, bigData){
  predict(slm.fit, data.frame(lstat = c(5,10,15)), interval = "confidence")
  predict(slm.fit, data.frame(lstat = c(5,10,15)), interval = "prediction")

}


