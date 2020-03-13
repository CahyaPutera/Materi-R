#split dataset training & testing
#split ratio 4/5

split <- sample.split(data.startups$Profit, SplitRatio = 4/5)
split

data.training <- data.startups[split,]
data.testing <- data.startups[!split,]

#fitting linear model dengan menggunakan training set
#prediksi Profit berdasarkan Marketing.Spend
fit.lm <- lm(formula = Profit~Marketing.Spend, data = data.training)
fit.lm

summary(fit.lm)

#prediksi test set
y_pred <- predict(fit.lm, newdata = data.testing)
y_pred

#visualisasi, scatter plot + garis regresi 
ggplot() + geom_point(data = data.testing, aes(x=data.testing$Marketing.Spend, y=data.testing$Profit),
       color="red") + 
  geom_point(data=data.training, aes(x=data.training$Marketing.Spend, y=data.training$Profit),
       color="blue") +
  geom_line(data=data.training, aes(x=data.training$Marketing.Spend,
                                   y=predict(fit.lm, newdata = data.training)))

#hitung R2 data testing
R2 <- function(actual, pred) {
  SSres <- sum((actual-pred)^2)
  SStot <- sum((pred-mean(actual))^2)
  return(1-(SSres-SStot))
}

R2(data.testing$Profit, y_pred)


#multiple linear regression

fit.multiple <- lm(formula = Profit~R.D.Spend + Administration + Marketing.Spend + 
                      State, data= data.training)
summary(fit.multiple)

y_pred.multiple <- predict(fit.multiple, newdata = data.testing)
data.testing$y_pred.multiple <- y_pred.multiple

#backward formula

#forward formula
forward.1 <- lm(formula = Profit ~ R.D.Spend, data = data.training)
summary(forward.1)
forward.1 <- lm(formula = Profit ~ R.D.Spend + Administration, data = data.training)
summary(forward.1)

#step
#backward
full.model <- lm(formula = Profit~R.D.Spend + Administration + Marketing.Spend + 
                   State, data= data.training)
backward <- step(full.model, direction = "backward")
summary(backward)

#bidirectional
both <- step(full.model, direction = "both")
summary(both)

#forward
min.model <- lm(formula = Profit ~1, data = data.training)
forward <- step(min.model, direction = "forward", scope = ~Marketing.Spend+Administration+State+R.D.Spend)
summary(forward)

