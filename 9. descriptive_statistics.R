data <- iris

summary(iris)

#mean
sum(iris$Sepal.Length) / length(iris)
mean(iris$Sepal.Length)

mean(iris[iris$Species=="setosa",]$Sepal.Length)
mean(iris[iris$Species=="versicolor",]$Sepal.Length)
mean(iris[iris$Species=="virginica",]$Sepal.Length)

quantile(iris$Sepal.Length)


#median
v1 <- c(5,10,2,3,7)
v1[order(v1)] #nilai tengahnya adalah 5

median(c(5,10,2,3,7))

v2 <- c(5,10,2,6,8,8)
median(v2)


#mean vs. median
nilai <- c(45, 30, 35, 56, 45, 100)
mean(nilai)
median(nilai)

gaji <- c(6, 8, 7.5, 10, 12, 95)
mean(gaji)
median(gaji)

#mode
num <- c(2,2,3,1,0,4,2,5,1,2,4)

most <- function(x){
  as.integer(names(sort(-table(x)))[1])
}

most(num)


#standard deviation
sd(iris[iris$Species=="setosa",]$Sepal.Length)
sd(iris[iris$Species=="versicolor",]$Sepal.Length)
sd(iris[iris$Species=="virginica",]$Sepal.Length)

#range & interquartile range
range(iris[iris$Species=="setosa",]$Petal.Width)
IQR(iris[iris$Species=="setosa",]$Petal.Width)

medians <- aggregate(Sepal.Length ~  Species, data, median)

ggplot(data=data, aes(x=Species, y=Sepal.Length, fill=Species)) + 
  geom_boxplot() + 
  geom_text(data = medians, aes(label=Sepal.Length, y = Sepal.Length + 0.1))

ggplot(data=data, aes(x=Species, y=Sepal.Width, fill=Species)) + 
  geom_boxplot()

ggplot(data=data, aes(x=Species, y=Petal.Length, fill=Species)) + 
  geom_boxplot()

ggplot(data=data, aes(x=Species, y=Petal.Width, fill=Species)) + 
  geom_boxplot()

ggplot(data=data, aes(x=Species, y=Petal.Width, fill=Species)) + 
  geom_boxplot()

#data distribution
library(ggplot2)

plot(density(iris$Sepal.Length))
plot(density(iris[iris$Species=="setosa",]$Sepal.Length))
plot(density(iris[iris$Species=="versicolor",]$Sepal.Length))
plot(density(iris[iris$Species=="virginica",]$Sepal.Length))

ggplot(iris, aes(x=Sepal.Length, fill=Species)) +
  geom_density(alpha=0.4)

#correlation
install.packages("corrplot")
library(corrplot)
M <- cor(iris[,1:4])
corrplot(M, type="upper", order="hclust", method = c("number"))

ggplot(iris, aes(sample=Petal.Length)) + stat_qq() + geom_qq_line()


retail <- read.csv(file.choose())

ggplot(data=retail, aes(x=Segment, y=Sales, fill=Segment)) + 
  geom_boxplot() +
  ylim(0,200)

M <- cor(retail[c("Sales", "Discount", "Quantity", "Profit")])
