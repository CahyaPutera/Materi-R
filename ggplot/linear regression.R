install.packages('caTools')
library(caTools)

split <- sample.split(data.salary$Salary, 
                      SplitRatio = 2/3)
split

data.training <- data.salary[split,]
data.testing <- data.salary[!split,]

#data training untuk cari garis regresi
#data testing untuk prediksi

?lm
fit.lm <- lm(formula = Salary~YearsExperience, 
             data=data.training)
summary(fit.lm)

y_pred <- predict(fit.lm, newdata = data.testing)
y_pred

data.testing$y_pred <- y_pred
data.testing


#Visualisasi

library(ggplot2)

ggplot() + 
  geom_point(data=data.testing, aes(x=data.testing$YearsExperience, 
                                    y=data.testing$Salary), color='red') +
  geom_point(data=data.training, aes(x=data.training$YearsExperience, 
                                    y=data.training$Salary), color='blue') +
  geom_line(data=data.testing, aes(x=data.testing$YearsExperience,
                                   y=predict(fit.lm, newdata = data.testing)))

