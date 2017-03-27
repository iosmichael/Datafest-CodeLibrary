setwd("~/Desktop/datafest")
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
                                                     title = "Corruption and Human Development")
graph <- graph + theme(legend.position = "top", axis.text = element_text(size = 12)) + geom_point(aes(color = Region), size = 10, alpha = 0.5)
graph 
