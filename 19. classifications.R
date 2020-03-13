#Logistic Regression
dataset = read.csv("../datasets/Social_Network_Ads.csv")

#predict only based on gender & estimated salary
dataset = dataset[,3:5]
dataset$Purchased <- as.factor(dataset$Purchased)

library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = dataset[split,]
test_set = dataset[!split,]

#Feature Scaling
training_set[,1:2] = scale(training_set[,1:2])
test_set[,1:2] = scale(test_set[,1:2])

ggplot(data=dataset, aes(x=Age, y=EstimatedSalary, col=Purchased)) + geom_point()

#Fitting Logistic Regression
classifier = glm(formula = Purchased ~ ., data=training_set,
                family = binomial)
summary(classifier)

#Predicting
prob_pred = predict(classifier, type = "response", newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#Confusion Matrix
cm = table(test_set[,3], y_pred)
cm

#Visualizing Test Set
#install.packages("ElemStatLearn")
library(ElemStatLearn)

set = training_set
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
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

#Fitting K-NN & Prediction
library(class)
y_pred = knn(train = training_set[,-3],
             test = test_set[,-3],
             cl = training_set[,3],
             k=5)

#Confusion Matrix
cm = table(test_set[,3], y_pred)
cm


set = training_set
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
y_grid = knn(train = training_set[,-3],
             test = grid_set[,-3],
             cl = training_set[,3],
             k=5)

plot(set[,-3], main = "KNN (Training Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))



#Fitting SVM
library(e1071)
classifier = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = "C-classification",
                 kernel = "linear")
y_pred = predict(classifier, newdata = test_set[-3])

#Confusion Matrix
cm = table(test_set[,3], y_pred)
cm


#Visualising Training
set = test_set
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
y_grid = predict(classifier, newdata = grid_set)

plot(set[,-3], main = "SVM (Test Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))




#KERNEL SVM
library(e1071)
classifier = svm(formula = Purchased ~ ., data = training_set,
                 type = "C-classification",
                 kernel = "radial")
y_pred = predict(classifier, newdata = test_set[-3])


#Confusion Matrix
cm = table(test_set[,3], y_pred)
cm

#Visualising Training
set = test_set
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
y_grid = predict(classifier, newdata = grid_set)

plot(set[,-3], main = "SVM (Test Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))



#Decision Tree
library(rpart)
classifier = rpart(formula = Purchased ~ ., 
                   data = training_set)

y_pred = predict(classifier, newdata = test_set[-3])
y_pred = ifelse(y_pred<=0.5, 0, 1)

cm = table(test_set[,3], y_pred)
cm

#Visualising Training
set = training_set
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
y_grid = predict(classifier, newdata = grid_set)
y_grid = ifelse(y_grid<=0.5, 0, 1)

plot(set[,-3], main = "SVM (Training Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))




#Random Forest
library(randomForest)

classifier = randomForest(x=training_set[-3],
                 y=factor(training_set$Purchased),
                 ntree=300)
y_pred = predict(classifier, newdata = test_set[-3])

#Confusion Matrix
cm = table(test_set[,3], y_pred)
cm

#Visualising Training
set = training_set
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
y_grid = predict(classifier, newdata = grid_set)

plot(set[,-3], main = "SVM (Training Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))

