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
