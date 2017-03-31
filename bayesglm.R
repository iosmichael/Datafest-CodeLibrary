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
               prior.df=Inf, prior.mean = c(4.8,3.3))
display(M5)

?predict
data$predict.B <- predict.glm(M5, newdata = data, type = "response")
library(ggplot2)
ggplot(data = data, aes(x = predict.B)) + geom_histogram()
ggplot(data = data, aes(x = Sepal.Length)) + geom_histogram(bins = 120)
t.test(data$Sepal.Length, data$predict.B)