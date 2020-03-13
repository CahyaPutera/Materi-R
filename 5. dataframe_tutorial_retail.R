#Baca data
#Mac:
setwd("/Users/davidchristian/Documents/OneTwoCode/DataScience/Week 1 v2")
retail <- read.csv("datasets/retail.csv")

head(retail)

#Produk dengan profit paling tinggi
df1 <- retail[,c("Product.ID", "Product.Name", "Profit")]
df1_agg <- aggregate(.~Product.ID+Product.Name, df1, sum)
ordered <- df1_agg[order(-df1_agg$Profit), ]
head(ordered)

#Customer dengan pembelian terbanyak
df2 <- retail[,c("Customer.ID", "Sales")]
df2_agg <- aggregate(.~Customer.ID, df2, sum)
ordered2 <- df2_agg[order(-df2_agg$Sales), ]
head(ordered2)

#Buat barplot segment vs penjualan + bonus:warnain by category
df3 <- retail[,c("Segment", "Category","Sales")]
df3_agg <- aggregate(.~Segment+Category, df3, sum)

qplot(data=df3_agg, x=Segment, y=Sales, geom = "col", fill=Category)
qplot(data=df3_agg, x=Category, y=Sales, geom = "col", fill=Segment)

xtabs(~ Sub.Category + Category, retail)
xtabs(Sales ~ Sub.Category + Category, retail)

plot(xtabs(~ Sub.Category + Ship.Mode, retail))
