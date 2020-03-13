#Importing dataset
dataset <- read.csv("datasets/Salary_Data.csv")
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = dataset[split,]
test_set = dataset[!split,]

#Fitting simple linear regression
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)
summary(regressor)

#Prediction
y_pred = predict(regressor, newdata = test_set)

#Visualizing
library(ggplot2)
p <- ggplot() + 
  geom_point(aes(x=training_set$YearsExperience, y=training_set$Salary),
                           color = 'red') +
  geom_line(aes(x=training_set$YearsExperience, y=predict(regressor, newdata = training_set)),
            color = 'blue') +
  ggtitle('Salary vs Experience (Training Set)') +
  xlab('Years of experience') +
  ylab('Salary')
p


q <- ggplot() + 
  geom_point(aes(x=test_set$YearsExperience, y=test_set$Salary),
             color = 'red') +
  geom_line(aes(x=training_set$YearsExperience, y=predict(regressor, newdata = training_set)),
            color = 'blue') +
  ggtitle('Salary vs Experience (Test Set)') +
  xlab('Years of experience') +
  ylab('Salary')
q
