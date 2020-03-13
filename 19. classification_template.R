#Logistic Regression
dataset = read.csv("../datasets/Social_Network_Ads.csv")

#predict only based on gender & estimated salary
dataset = dataset[,3:5]

library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = dataset[split,]
test_set = dataset[!split,]

#Feature Scaling
training_set[,1:2] = scale(training_set[,1:2])
test_set[,1:2] = scale(test_set[,1:2])

#Fitting Logistic Regression
classifier = 

#Predicting
prob_pred = predict(classifier, type = "response", newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#Confusion Matrix
cm = table(test_result[,3], y_pred)

#Visualizing Test Set
install.packages("ElemStatLearn")
library(ElemStatLearn)

set = training_set
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.01)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
prob_set = predict(classifier, type="response", newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[,-3], main = "Logistic Regression (Training Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))
