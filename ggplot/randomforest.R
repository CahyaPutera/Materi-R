library(ggplot)
p<-ggplot() + geom_point(data = data.position, 
                      aes(x=Level, y=Salary), color="red")

fit.lm <- lm(formula= Salary~Level,
             data=data.position)
p + geom_line(data = data.position, 
              aes(x=Level, y=predict(fit.lm, newdata = data.position)))
summary(fit.lm)

data.position$Level2 <- data.position$Level^2 

fit.polynomial <- lm(formula = Salary~Level+Level2,
                     data=data.position)
summary(fit.polynomial)

p+geom_line(data = data.position,
            aes(x=Level, y=predict(fit.polynomial)))


#SVM

install.packages("e1071")
library(e1071)

?svm
fit.svm <- svm(formula=Salary~Level, data=data.position,
               type="eps-regression")

predict(fit.svm, newdata = data.frame(Level=3.5))

p+geom_line(data = data.position,
            aes(x=Level, y=predict(fit.svm)))


#Random Forest

install.packages("randomForest")
library(randomForest)


fit.rf <- randomForest(formula=Salary~Level,
                       ntree=500, data= data.position)
y_pred <- predict(fit.rf, newdata = data.frame(Level = 3.5))

x_grid <- seq(min(data.position$Level), max(data.position$Level), 0.01)

p+geom_line(aes(x=x_grid, 
                y=predict(fit.rf, newdata = data.frame(Level=x_grid))))


#data.startup

#fit ke linear model, svm, random forest
fit.lm <- lm(formula= Profit~R.D.Spend + Administration + Marketing.Spend + State,
             data=data.startup)

fit.svm <- svm(formula=Profit~R.D.Spend + Administration + Marketing.Spend + State, 
       data=data.startup, type="eps-regression")

fit.rf <- randomForest(formula=Profit~R.D.Spend + Administration + Marketing.Spend + State,
                       ntree=500, data= data.startup)

#visualisasi
v1 <- ggplot() + geom_point(data=data.startup, aes(x= data.startup$Profit, 
                                                  y=predict(fit.lm, newdata = data.startup)
v2 <- ggplot() + geom_line(data = data.startup,
                           aes(x=data.startup$Profit, y=predict(fit.svm, newdata = data.startup)))
v2 <- ggplot() + geom

#model mana yang paling bagus ?


#R2

