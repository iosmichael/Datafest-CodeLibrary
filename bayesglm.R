library(arm)
library(MASS)
?bayesglm
data <- as.data.frame(iris)
head(data)
#Bayesian Rules
M1 <- bayesglm(Sepal.Length ~ Sepal.Width + Petal.Length, family = gaussian, data = data)
display(M1)
#Prior Settings
M5 <- bayesglm(Sepal.Length ~ Sepal.Width + Petal.Length, family = gaussian, data = data, 
               prior.df=Inf, prior.mean = c(-5,-5), prior.scale = c(0.5,0.2))
display(M5)
