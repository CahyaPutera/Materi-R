x <- 175
m <- 158
sd <- 8

z <- (x-m) / sd
1-pnorm(z)

pnorm(z, lower.tail = FALSE)


x1 <- 142
m <- 147
sd <- 5

z1 <- (x1-m) / sd

x2 <- 152

z2 <- (x2-m) / sd

1-pnorm(z1)
1-pnorm(z2)

pnorm(z2) - pnorm(z1)


install.packages('NHANES')
library(NHANES)

data.nhanes <- NHANES
library(ggplot2)

data.adult <- data.nhanes[data.nhanes$Age >= 21,]

ggplot(data=data.adult, aes(x=Height)) + geom_density()

qqnorm(data.adult$Height)
qqline(data.adult$Height)


data.male <- data.nhanes[data.nhanes$Gender == 'male',]
data.female <- data.nhanes[data.nhanes$Gender == 'female',]


xbar <- mean(data.male$Height)
sd <- sd(data.male$Height)
n <- nrow(data.male)
alpha <- 0.05

SE <- qnorm(alpha/2) * (sd / sqrt(n))

batas.bawah <- xbar + SE
batas.atas <- xbar - SE

#rata rata tinggi badan pria antara 175.01cm - 175.69cm, dengan tingkat kepercayaan 95%

data.50 <- data.male [1:50,]
xbar <- mean(data.50$Height)
sd <- sd(data.50$Height)
n <- nrow(data.50)
alpha <- 0.05

SE <- qnorm(alpha/2) * (sd/sqrt(n))

batas.bawah <- xbar + SE
batas.atas <- xbar - SE

#rata rata tinggi badan pria sample 50 antara 172.72cm - 176.47cm, dengan tingkat kepercayaan 95%


m0 <- 165
sd <- 7
xbar <- 160
n <- 30

Z <- (xbar-m0)/(sd/sqrt(n))
qnorm(0.05)

xbar <- 9900
m0 <- 10000
sd <- 120
n <- 30

Z <- (xbar-m0)/(sd/sqrt(n))
qnorm(0.05)


xbar <- 9900
mu0 <- 10000
s <- 120
n <- 30

alpha <- 0.05

t <- (xbar-mu0) / (s/sqrt(n))
qt(0.05, df = n-1)

t.test(data.50$Height, mu = 160)
 
wilcox.test(data.50$Height, mu = 175)

#ztest = data ikut distribusi normal, standar deviasi populasi di ketahui
#ttest = data ikut distribusi normal, standar deviasi populasi tdk di ketahui (dari sample)
#wilcoxtest = data tdk ikut distribusi normal

