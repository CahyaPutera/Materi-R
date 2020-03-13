#Buat Boxplot, lihat central tendency (median)
#Range (IQR)
#Profit berdasarkan segment
#Hitung korelasi antara Sales, Discount, Quantity, Profit

#Boxplot

ggplot(data=data.retail, 
       aes(x=Segment,y=Profit, col=Segment)) +
  geom_boxplot() + ylim(-50,50)

#Range
iqr <- IQR(data.retail$Profit, na.rm = TRUE)

iqr(data.retail$Profit, na.rm = TRUE)
q1 <- quantile(data.retail$Profit, na.rm = TRUE) [2]
q3 <- quantile(data.retail$Profit, na.rm = TRUE) [4]

min <- q1 -(1.5 * iqr)
max <- q3 + (1.5 * iqr)
min
max


#Hitung korelasi antara Sales, Discount, Quantity, Profit
data <- data.retail[,c('Sales', 'Discount', 'Quantity', 'Profit')]

filter.sales <- !is.na(data$Sales)
filter.discount <- !is.na(data$Discount)
filter.quantity <- !is.na(data$Quantity)
filter.profit <- !is.na(data$Profit)

data <- data.retail[filter.sales & filter.discount & filter.quantity & filter.profit, ]


