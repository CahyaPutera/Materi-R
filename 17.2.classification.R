data <- read.csv("../datasets/Social_Network_Ads.csv")
str(data)
summary(data)

data$Purchased <- as.factor(data$Purchased)
data <- data[,3:5]

#install.packages("caTools")
library(caTools)
set.seed(123)

split <- sample.split(data$Purchased, SplitRatio = 3/4)

training_set <- data[split,]
test_set <- data[!split,]


# Feature Scaling
training_set[,1:2] <- scale(training_set[,1:2])
test_set[,1:2] <- scale(test_set[,1:2])

# Buat scatter plot x=Age y=EstimatedSalary
# warnai berdasarkan Purchased
library(ggplot2)
p <- ggplot(data = training_set,
            aes(x=Age, y=EstimatedSalary, col=Purchased)) +
  geom_point()
p

logit <- glm(formula = Purchased ~ Age + EstimatedSalary,
             data = training_set, family = binomial)
summary(logit)

prob_pred.logit <- predict(logit, newdata = test_set,
                     type = "response")
prob_pred.logit

y_pred.logit <- ifelse(prob_pred.logit>=0.5, 1, 0)
y_pred.logit

#Confusion Matrix
cm.logit <- table(test_set$Purchased, y_pred.logit)

#Visualisasi
#install.packages("ElemStatLearn")
library(ElemStatLearn)

set = test_set
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
prob_set = predict(logit, type="response", newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)

plot(set[,-3], main = "Logistic Regression (Test Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))


#install.packages("class")
library(class)
y_pred.knn <- knn(train = training_set[,-3],
              test = test_set[,-3],
              cl = training_set[,3],
              k = 5)
y_pred.knn

cm.knn <- table(test_set$Purchased, y_pred.knn)
cm.knn

#Visualisasi
set = test_set
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
y_grid = knn(train = training_set[,-3],
             test = grid_set[,-3],
             cl = training_set[,3],
             k = 5)

plot(set[,-3], main = "KNN k=5 (Test Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))

#install.packages(e1071)
library(e1071)
svm.model.linear <- svm(formula = Purchased ~ Age + EstimatedSalary,
                 data=training_set,
                 type="C-classification",
                 kernel = "linear")
y_pred.svm.linear <- predict(svm.model.linear, newdata=test_set)
y_pred.svm.linear

cm.svm.linear <- table(test_set$Purchased, y_pred.svm.linear)
cm.svm.linear

#Visualisasi
set = test_set
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
y_grid = predict(svm.model.linear, newdata=grid_set)

plot(set[,-3], main = "SVMN Linear (Test Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))




svm.model.radial <- svm(formula = Purchased ~ Age + EstimatedSalary,
                        data=training_set,
                        type="C-classification",
                        kernel = "radial")
y_pred.svm.radial <- predict(svm.model.radial, newdata = test_set)
y_pred.svm.radial
cm.svm.radial <- table(test_set$Purchased, y_pred.svm.radial)
cm.svm.radial

#Visualisasi
set = test_set
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
y_grid = predict(svm.model.radial, newdata=grid_set)

plot(set[,-3], main = "SVMN Radial (Test Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))

#install.packages("rpart")
library(rpart)

dt.model <- rpart(formula=Purchased ~ Age + EstimatedSalary,
                  data = training_set)

prob_pred.dt <- predict(dt.model, test_set)
y_pred.dt <- ifelse(prob_pred.dt[,2] >= 0.5, 1, 0)
y_pred.dt

#confusion matrix
cm.dt <- table(test_set$Purchased, y_pred.dt)
cm.dt

#Visualisasi
set = test_set
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
prob_grid = predict(dt.model, newdata=grid_set)
y_grid = ifelse(prob_grid[,2] >= 0.5, 1, 0)

plot(set[,-3], main = "Decision Tree (Test Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))

par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
plot(dt.model)
text(dt.model, use.n = TRUE)

#install.packages("randomForest")
library(randomForest)

rf.model <- randomForest(formula=Purchased~Age+EstimatedSalary,
                         data = training_set,
                         ntree=300)
prob_pred.rf <- 
y_pred.rf <- predict(rf.model, test_set)
y_pred.rf

#Confusion Matrix
cm.rf <- table(test_set$Purchased, y_pred.rf)
cm.rf

#Visualisasi
set = test_set
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
y_grid = predict(rf.model, grid_set)

plot(set[,-3], main = "Random Forest ntree=300 (Test Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))

cm.logit
acc.logit <- (60 + 21) / (60+4+15+21)
acc.knn <- (58+32)/100
acc.svm.radial <- (91/100)
acc.dt <- 94/100
acc.rf <- 92/100

acc.logit
acc.knn
acc.svm.radial
acc.dt
acc.rf

prec.logit <- 21 / 25
prec.dt <- 36 / 42

recall.dt <- 36 / 36 

#install.packages("pROC")
library(pROC)

ROC.logit <- roc(test_set$Purchased,
                 prob_pred.logit)
AUC.logit <- auc(ROC.logit)
AUC.logit

ROC.dt <- roc(test_set$Purchased,
              prob_pred.dt[,2])
AUC.dt <- auc(ROC.dt)
AUC.dt

plot(ROC.logit, col="green")
lines(ROC.dt, col="red")

table(test_set$Purchased)
baseline <- 64 / 100
baseline
