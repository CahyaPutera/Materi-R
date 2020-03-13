#Multiple Linear Regression

#Importing dataset
dataset = read.csv("datasets/50_Startups.csv")

#Encoding categorial variable
dataset$State = factor(dataset$State,
                       levels = c("New York", "California", "Florida"),
                       labels = c(1,2,3))

#Splitting
library(caTools)
set.seed(123)
split <- sample.split(dataset$Profit, SplitRatio = 0.8)
training_set <- dataset[split,]
test_set <- dataset[!split,]

#Fitting - All variables
regressor <- lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
                data=training_set)

regressor <- lm(formula = Profit ~., data=training_set) #penulisan alternatif

# regressor <- lm(formula = Profit ~ R.D.Spend, data=training_set)
# summary(regressor)

#Predicting
y_pred <- predict(regressor, newdata = test_set)


#Backward Elimination
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, data = dataset)
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend, data = dataset)
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend, data = dataset)
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend, data = training_set)
summary(regressor)

#Automatic stepwise
full.model <- lm(formula = Profit ~. , data=dataset)

# untuk p value = 0.05
# k = qchisq(0.05,1,lower.tail = FALSE) 
backward <- step(regressor, direction = "backward", trace = FALSE)
summary(backward)
