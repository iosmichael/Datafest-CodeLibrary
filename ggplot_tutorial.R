setwd("~/datafest")
housing <- read.csv("landdata-states.csv")
head(housing[1:5])
library(ggplot2)
#data
#aesthetic mapping
#geometric object
#statistical transformation
#scales
#coordinate system
#position adjustments
#faceting
ggplot(housing, aes(x = Home.Value)) + geom_histogram()

ggplot(subset(housing, State %in% c("MA", "TX")), aes(x = Date, y=Home.Value, color = State)) + geom_point()

hp2001Q1 <- subset(housing, Date == 2001.25)
ggplot(hp2001Q1, aes(y = Structure.Cost, x = Land.Value)) + geom_point()
ggplot(hp2001Q1, aes(y = Structure.Cost, x = log(Land.Value))) + geom_point()
hp2001Q1$pred.SC <- predict(lm(Structure.Cost ~ log(Land.Value), data = hp2001Q1))
p1 <- ggplot(hp2001Q1, aes(y = Structure.Cost, x = log(Land.Value)))
p1 + geom_point(aes(color = Home.Value)) + geom_line(aes(y = pred.SC))
p1 + geom_point(aes(color = Home.Value)) + geom_smooth()
p1 + geom_text(aes(label = State), size = 3)
##install.packages("ggrepel")
library(ggrepel)
p1 + geom_point() + geom_text_repel(aes(label = State), size = 3)

p1 + geom_point(aes(color = Home.Value, shape = region))
?geom_text_repel
#Excercise I
dat <- read.csv("EconomistData.csv")
head(dat)
ggplot(dat, aes(x = CPI, y = HDI)) + geom_point(size = 2,color="blue", shape = dat$Region) + geom_text_repel(aes(label=HDI.Rank))

p2 <- ggplot(housing, aes(x = Home.Value))
p2 + geom_histogram()
p2 + geom_histogram(stat = "bin", binwidth = 4000)

housing.sum <- aggregate(housing["Home.Value"], housing["State"], FUN = mean)

rbind(head(housing.sum), tail(housing.sum))
ggplot(housing.sum, aes(x=State, y = Home.Value)) + geom_bar(stat = "identity")

p3 <- ggplot(housing, aes(x = State, y = Home.Price.Index)) + theme(legend.position = "top", axis.text = element_text(size = 6))
p4 <- p3 + geom_point(aes(color = Date), alpha = 0.5, size = 1.5, position = position_jitter(width = 0.25, height = 0))
p4 + scale_x_discrete(name = "State Abbreviation") + scale_color_continuous(name = "",
                                                                            breaks = c(1976, 1994, 2013),
                                                                            labels = c("'76","'94","'13"),
                                                                            low = "blue", high = "red")
p4 + scale_color_gradient2(name="",
                        breaks = c(1976, 1994, 2013),
                        labels = c("'76", "'94", "'13"),
                        low = "blue",
                        high = "red",
                        mid = "gray60",
                        midpoint = 1994)
#Exercise III

p5 <- ggplot(housing, aes(x = Date, y = Home.Value))
p5 + geom_line(aes(color = State))
(p5 <- p5 + geom_line() +
  facet_wrap(~State, ncol = 10))
p5 + theme_minimal() + theme(text = element_text(color = "turquoise"))

theme_new <- theme_bw() +
  theme(plot.background = element_rect(size = 1, color = "blue", fill = "black"),
        text=element_text(size = 12, family = "Serif", color = "ivory"),
        axis.text.y = element_text(colour = "purple"),
        axis.text.x = element_text(colour = "red"),
        panel.background = element_rect(fill = "pink"),
        strip.background = element_rect(fill = "orange"))

p5 + theme_new


#Exercise Economists
eco <- read.csv("EconomistData.csv")
head(eco)
graph <- ggplot(eco, aes(x = CPI, y = HDI)) + labs(x = "Corruption Perceptions Index, 2011 (10 = least corrupt)",
                                                     y = "Human Development Index, 2011 (1 = best)",
                                                    title = "Corruption and Human Development",
                                                   size = 5, family = "Helvetic Neue")
p1 <- graph + theme(legend.position = "top", axis.text = element_text(size = 9)) + geom_point(pch=21, aes(color = Region), size = 2) + geom_smooth(color = "red", size = 1, method = "glm", formula = y ~ splines::bs(x, 3), se=FALSE)
p1 + geom_text_repel(label = c(rep("",14),as.character(eco$Country[15:20]), rep("", 153))) + scale_color_manual(values = c("#E53935","#673AB7","#009688","#01579B","#1B5E20","#FF8F00"))

?prcomp
head(eco)

autoplot(pam(eco[c(3,4,5)],6))
a <- autoplot(prcomp(eco[c(3,4,5)]), data = eco, colour = 'Region',loadings = TRUE, loadings.colour = '#E53935', loadings.label = TRUE, loadings.label.size = 4)
a + geom_text_repel(label = eco$Country)

?geom_text
graph

#http://rpubs.com/sinhrks/plot_pca
#Visualization of PCA
#install.packages("ggfortify")
library(ggfortify)
df <- iris[c(1, 2, 3, 4)]
prcomp(df)
autoplot(prcomp(df), data = iris, colour = 'Species') 
autoplot(prcomp(df), data = iris, colour = 'Species', label = TRUE, label.size = 3)
autoplot(prcomp(df), data = iris, colour = 'Species', loadings = TRUE, loadings.colour = 'pink', loadings.label = TRUE, loadings.label.size = 4)

#Visualization of K-Means
autoplot(kmeans(USArrests, 3), data = USArrests)
autoplot(kmeans(USArrests, 3), data = USArrests, label = TRUE, label.size = 3)

#Visualization of Factor Analysis
d.factanal <- factanal(state.x77, factors = 4, scores = 'regression')
?factanal
autoplot(d.factanal, data = state.x77, colour = 'Income')
autoplot(d.factanal, label = TRUE, label.size = 3,
         loadings = TRUE, loadings.label = TRUE, loadings.label.size  = 3)

#Visualization of Cluster Analysis
library(cluster)
?clara

levels(iris$Species)
# K-Means Cluster Analysis
?autoplot
autoplot(kmeans(iris[-5], 3), data = iris) 
autoplot(clara(iris[-5], 3), loadings = TRUE, loadings.colour = 'pink', loadings.label = TRUE, loadings.label.size = 4)
autoplot(fanny(iris[-5], 3), frame = TRUE, loadings = TRUE, loadings.colour = 'pink', loadings.label = TRUE, loadings.label.size = 4)
autoplot(pam(iris[-5], 3), frame = TRUE, frame.type = 'norm', loadings = TRUE, loadings.colour = 'pink', loadings.label = TRUE, loadings.label.size = 4)
