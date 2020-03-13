# Import Dataset
dataset <- read.csv("datasets/Position_Salaries.csv")
dataset <- dataset[,2:3]

#Fitting linear
lin_reg <- lm(formula = Salary ~. , 
              data = dataset)
summary(lin_reg)

#Polynomial
dataset$Level2 <- dataset$Level^2
dataset$Level3 <- dataset$Level^3
dataset$Level4 <- dataset$Level^4
# dataset$Level4 <- dataset$Level^5
poly_reg <- lm(formula = Salary ~. ,
               data = dataset)
summary(poly_reg)

#Visualisasi Linear Regression
library(ggplot2)
ggplot() +
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             colour="red") +
  geom_line(aes(x=dataset$Level, y=predict(lin_reg, newdata = dataset)),
            colour="blue") +
  ggtitle("Level vs Salary - Simple Regression") +
  xlab("Level") +
  ylab("Salary")

#Visualisasi Polynomial Regression
ggplot() +
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             colour="red") +
  geom_line(aes(x=dataset$Level, y=predict(poly_reg, newdata = dataset)),
            colour="blue") +
  ggtitle("Level vs Salary - Polynomial Regression") +
  xlab("Level") +
  ylab("Salary")

#Predicting
y_pred.linear <- predict(lin_reg, data.frame(Level=6.5))
y_pred.polynomial <- predict(poly_reg, data.frame(Level=6.5, 
                                                  Level2=6.5^2, 
                                                  Level3=6.5^3,
                                                  Level4=6.5^4))

#SVM
library(e1071)
dataset <- read.csv("datasets/Position_Salaries.csv")
dataset <- dataset[,2:3]
svm_reg <- svm(formula = Salary ~. ,
               data = dataset,
               type = "eps-regression")

y_pred <- predict(svm_reg, data.frame(Level = 6.5))

library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y=dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y=predict(svm_reg, dataset)),
            colour = 'blue')


#Decision Tree
#install.packages('rpart')
library(rpart)
dataset <- read.csv("datasets/Position_Salaries.csv")
dataset <- dataset[,2:3]
dt_reg <- rpart(formula = Salary ~. ,
               data = dataset,
               control = rpart.control(minsplit = 1))

y_pred <- predict(dt_reg, data.frame(Level = 6.5))

library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y=dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y=predict(dt_reg, dataset)),
            colour = 'blue')

#Higher resolution
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y=dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y=predict(dt_reg, data.frame(Level=x_grid))),
            colour = 'blue')


#Random Forest
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
dataset <- read.csv("datasets/Position_Salaries.csv")
dataset <- dataset[,2:3]
rf_reg <- randomForest(x = dataset[1],
                y = dataset$Salary,
                ntree = 500)

y_pred <- predict(rf_reg, data.frame(Level = 6.5))

#Visualization
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y=dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y=predict(rf_reg, data.frame(Level=x_grid))),
            colour = 'blue')
