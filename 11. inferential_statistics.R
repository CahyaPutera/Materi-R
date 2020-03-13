#Normal Distribution
x = rnorm(500,0,1)
plot(density(x))

#Z-Score
#Pria dengan tinggi lebih dari 175cm
#sd = 7cm, mean = 158
z <- (175-158) / 7
p <- pnorm(z, lower.tail=F)
p

#Wanita dengan tinggi antara 142cm dan 152cm
#sd = 5cm, mean = 147
z <- (152-147) / 5
z2 <- (142-147) / 5

p <- pnorm(z) - pnorm(z2)
p


#Confidence intervals
dataset <- read.csv("../datasets/nhanes.csv")
dataset.male <- dataset[dataset$Gender=="male",]
dataset.female <- dataset[dataset$Gender=="female",]

m <- mean(dataset.male$Height)
standard.deviation <- sd(dataset.male$Height)
n <- nrow(dataset.male)
#Confidence interval 95% - alpha = 5%
SE <- qnorm(0.0025) * standard.deviation / sqrt(n)

batas.bawah <- m + SE #174.9cm
batas.atas <- m - SE #175.8cm

#50 samples
dataset.male2 <- dataset.male[1:50,]

m2 <- mean(dataset.male2$Height)
standard.deviation2 <- sd(dataset.male2$Height)
n2 <- nrow(dataset.male2)
#Confidence interval 95% - alpha = 5%
SE2 <- qnorm(0.0025) * standard.deviation2 / sqrt(n2)

batas.bawah2 <- m2 + SE2 #171.9cm
batas.atas2 <- m2 - SE2 #177.3cm

#Hypothesis Testing
mu <- 165
xbar <- 160
sdev <- 7
n3 <- 30

z <- (xbar - mu) / (sdev / sqrt(n3))
z

SE3 <- qnorm(0.025) * sdev / sqrt(n3)
batas.bawah3 <- xbar + SE3
batas.atas3 <- xbar - SE3

qnorm(0.025)
qnorm(0.975)

# z > qnorm(0.975). Null hypothesis ditolak
pnorm(z) * 2
#pvalue 0.015

?t.test

#bohlam
xbar = 9900
mu = 10000
sigma = 120
n = 30

z <- (xbar-mu) / (sigma / sqrt(n))
z

alpha = 0.05
z.alpha = qnorm(1-alpha)
-z.alpha

pval = pnorm(z)
pval



#t-test
xbar = 9900
mu0 = 10000
s = 125
n = 30
t = (xbar-mu0) / (s/sqrt(n))
t

alpha = 0.05
t.alpha = qt(1-alpha, df=n-1)
-t.alpha


pval = pt(t,df=n-1)
pval


#t-test
t.test(dataset.male[1:30,]$Height, mu=172)
