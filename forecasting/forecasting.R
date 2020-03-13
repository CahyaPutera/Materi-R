install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)


#load dataset
data("AirPassengers")
class(AirPassengers) #ts = timeseries

#start of time series
start(AirPassengers)

#end of time series
end(AirPassengers)

#frequency
frequency(AirPassengers)

plot(AirPassengers)

#fit linear regression
lin.reg <- lm(AirPassengers~time(AirPassengers))
abline(reg=lin.reg)

#cycle across years
cycle(AirPassengers)

#plot YoY trend
plot(aggregate(AirPassengers,FUN=mean))

#boxplot across months
boxplot(AirPassengers~cycle(AirPassengers))

#visualize decomposed model
decomp <- stl(AirPassengers, s.window = "periodic")
plot(decomp)

#remove unequal variance by log, remove trend by taking difference
plot(AirPassengers)
plot(diff(log(AirPassengers)))
adf.test(diff(log(AirPassengers)), k=0)

#MA, q=2
acf(diff(log(AirPassengers)))

#AR, p=1
pacf(diff(log(AirPassengers)))

#fitting arima
fit <- arima(log(AirPassengers), c(1,1,2),seasonal = list(order=c(1,1,2), period=12))
summary(fit)
pred <- predict(fit, n.ahead = 10*12)
ts.plot(AirPassengers,2.718282^pred$pred, log="y", lty = c(1,3))

#cara gampang
fit.auto <- auto.arima(log(AirPassengers))
pred.auto <- predict(fit.auto, n.ahead = 10*12)
ts.plot(AirPassengers, 2.718282^pred.auto$pred, log="y", lty = c(1,3))

#ubah plot jadi ggplot
library(ggplot2)
df <- data.frame(Year = time(AirPassengers),
                 Month = c(cycle(AirPassengers)),
                 Value = c(AirPassengers))

cols <- c("LINE1"="ARIMA (1, 1, 2)","LINE2"="AUTO ARIMA")
ggplot() + geom_line(aes(x=time(AirPassengers), y=AirPassengers)) +
  geom_line(aes(x=time(pred$pred), y=2.718282^pred$pred, col="ARIMA(1,1,2)")) +
  geom_line(aes(x=time(pred.auto$pred), y=2.718282^pred.auto$pred, col="AUTO ARIMA")) +
  scale_x_continuous(breaks = round(seq(1949, 1970, by = 5),1))

autoplot(forecast(fit)) + geom_forecast()
autoplot(forecast(fit.auto)) + geom_forecast()
summary(fit)
summary(fit.auto)

?arima
