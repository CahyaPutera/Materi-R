data <- read.csv("../datasets/Social_Network_Ads.csv")
str(data)
summary(data)

#install.packages("caTools")
library(caTools)
set.seed(123)

split <- sample.split(data$Purchased, SplitRatio = 3/4)
data.training <- data[split,]
data.testing <- data[!split,]

data.training$Purchased <- as.factor(data.training$Purchased)
data.testing$Purchased <- as.factor(data.testing$Purchased)

data.training[,3:4] <- scale(data.training[,3:4])
data.testing[,3:4] <- scale(data.testing[,3:4])

ggplot(data=data.training, aes(x=Age, y=EstimatedSalary, col=Purchased)) +
  geom_point()

logit <- glm(formula = Purchased ~ Age + EstimatedSalary, data = data.training,
             family = binomial)
summary(logit)

prob_pred <- predict(logit, newdata = data.testing, type = "response")
prob_pred

y_pred <- ifelse(prob_pred > 0.5, 1, 0)
y_pred

data.testing$Purchased
y_pred
#confusion matrix
cm <- table(data.testing$Purchased, y_pred)
cm

#Visualisasi
#install.packages("ElemStatLearn")
#Visualizing Test Set
#install.packages("ElemStatLearn")
library(ElemStatLearn)

set = data.training[,3:5]
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
prob_set = predict(logit, type="response", newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)

plot(set[,-3], main = "Logistic Regression (Training Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))


#K-NN
#install.packages("class")
library(class)

y_pred <- knn(train=data.training[,3:4],
              test=data.testing[,3:4],
              cl=data.training$Purchased,
              k=5)
#confusion matrix
cm.knn <- table(data.testing$Purchased, y_pred)
cm.knn


#visualisasi
set = data.training[,3:5]
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
y_grid = knn(train = set[,1:2],
             test = grid_set,
             cl = set[,3],
             k=5)

plot(set[,-3], main = "KNN Regression (Training Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))


#SVM
library(e1071)
svm.model.linear <- svm(formula = Purchased ~ Age + EstimatedSalary,
                 data = data.training,
                 type = "C-classification",
                 kernel="linear")
svm.model.radial <- svm(formula = Purchased ~ Age + EstimatedSalary,
                        data = data.training,
                        type = "C-classification",
                        kernel="radial")
y_pred.linear <- predict(svm.model.linear, newdata = data.testing[,3:4])
y_pred.radial <- predict(svm.model.radial, newdata = data.testing[,3:4])

#confusion matrix
cm.svm.linear <- table(data.testing$Purchased, y_pred.linear)
cm.svm.radial <- table(data.testing$Purchased, y_pred.radial)
cm.svm.linear
cm.svm.radial


#Visualisasi SVM Linear
set = data.training[,3:5]
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
y_grid = predict(svm.model.linear, newdata = grid_set)

plot(set[,-3], main = "SVM Linear (Training Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))

#Visualisasi SVM Linear
set = data.training[,3:5]
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
y_grid = predict(svm.model.radial, newdata = grid_set)

plot(set[,-3], main = "SVM Radial Kernel (Training Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))

set = data.testing[,3:5]
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
y_grid = predict(svm.model.radial, newdata = grid_set)

plot(set[,-3], main = "SVM Radial Kernel (Testing Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))


library(rpart)
dt.model <- rpart(formula = Purchased ~ Age + EstimatedSalary,
                  data = data.training)

prob_pred <- predict(dt.model, newdata = data.testing)
y_pred <- ifelse(prob_pred[,2] > 0.5, 1, 0)
y_pred

install.packages("rpart.plot")

cm.dt <- table(data.testing$Purchased, y_pred)
cm.dt

#Visualisasi Decision Tree
set = data.testing[,3:5]
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
y_grid <- predict(dt.model, newdata = grid_set)
y_grid <- ifelse(y_grid<=0.5, 0, 1)

plot(set[,-3], main = "Decision Tree (Testing Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid[,2] == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))

par(mar=c(0.2,2.1,2.1,4.1))
plot(dt.model, uniform=TRUE)
text(dt.model, use.n=TRUE)
?rpart

cm.dt


#RandomForest
#install.packages("randomForest")
library(randomForest)

rf.model <- randomForest(formula=Purchased~Age+EstimatedSalary,
                         data = data.training,
                         ntree = 500)
y_pred.rf <- predict(rf.model, data.testing)
y_pred.rf

#cm rf
cm.rf <- table(data.testing$Purchased, y_pred.rf)
cm.rf


#Visualisasi Decision Tree
set = data.testing[,3:5]
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
y_grid <- predict(rf.model, newdata = grid_set)

plot(set[,-3], main = "Random Forest (Testing Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))


cm.rf
precision <- 30 / (9 + 30)
precision
recall <- 30 / (30+6)
recall

table(data$Purchased)
257 / (257 + 143)

table(data.testing$Purchased)
64 / (64 + 36)

