#VECTOR
vector1 <- c(10, 12, 50, 791)
vector1
is.numeric(vector1)
is.integer(vector1) #FALSE

vector2 <- c(3L, 12L, 243L, 0L)
vector2
is.numeric(vector2)
is.integer(vector2) #TRUE

vector3 <- c("a", "B", "cdef")
vector3
is.character(vector3)
is.numeric(vector3)

#can only have data with same type
vector4 <- c("a", "B", "CDe", 7)
vector4
is.character(vector4)
is.numeric(vector4)

vector5 <- c(3L, 13, 15, 20.1)
vector5
typeof(vector5)


?seq #sequence
?rep #repitition

seq(1,15)
1:15
seq(1,15,2)

rep(3,100)
x <- c('a', 'b')
rep(x, 10)



#FINANCIAL ANALYSIS
#Data
revenue <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
expenses <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)

#Solution
#pendapat setiap bulan
profit <- revenue - expenses
profit

#pendapatan setelah pajak
tax <- round(profit*0.3, 2)
tax

profit.after.tax <- profit - tax
profit.after.tax

#margin pendapatan
#berapa banyak profit yg didapat setiap pendapatan
profit.margin <- round(profit.after.tax / revenue, 2) *100
profit.margin


#bulan baik
mean.profit.after.tax <- mean(profit.after.tax)
mean.profit.after.tax

good.months <- profit.after.tax > mean(profit.after.tax)
good.months
month.name[good.months]

#bulan jelek
bad.months <- !good.months
bad.months
month.name[bad.months]

#bulan terbaik
max.profit.after.tax <- max(profit.after.tax)
best.month <- profit.after.tax == max.profit.after.tax
best.month
month.name[best.month]

#bulan terburuk
min.profit.after.tax <- min(profit.after.tax)
worst.month <- profit.after.tax == min.profit.after.tax
worst.month
month.name[worst.month]


#matrices
m <- rbind(
  profit,
  profit.after.tax,
  good.months,
  bad.months,
  best.month,
  worst.month
)
colnames(m) <- month.name
m
