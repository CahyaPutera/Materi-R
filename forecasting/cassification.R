#explore data
str(data.ads)

#ubah ke factorial
data.ads$User.ID <- as.factor(data.ads$User.ID)
data.ads$Purchased <- as.factor(data.ads$Purchased)
str(data.ads)

#split data ke training & testing
library(caTools)
set.seed(123)
split <- sample.split(data.ads$Purchased, SplitRatio = 3/4)
split

data.training <- data.ads[split,]
data.testing <- data.ads[!split,]

#future scaling
data.training[,3:4] <- scale(data.training[,3:4])
data.testing[,3:4] <- scale(data.testing[,3:4])
summary(data.training)

#buat plot
ggplot(data = data.training, aes(x=Age, y=EstimatedSalary, 
                                 col=Purchased))+ geom_point()

#Logistic Regression (logit)
logit <- glm(formula = Purchased~Age+EstimatedSalary,
             data = data.training, family = binomial)
summary(logit)       

#buat prediksi
pred <- predict(logit, newdata = data.testing, type = "response")
pred

prob_pred <- predict(logit, newdata = data.testing,
                     type = "response")
y_pred <- ifelse(prob_pred>=0.5,1,0)
y_pred

#conclussion matrix
cm.logit <- table(data.testing$Purchased, y_pred)
cm.logit #accuracy = 83/100

#Visualisasi
install.packages("ElemStatLearn")
library(ElemStatLearn)

set = data.testing[,3:5]
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

#K-NN
install.packages("class")
library(class)

prob_pred.knn <- knn(train = data.training[,3:4],
              data.testing[,3:4],
              k=3,
              cl = data.training$Purchased, 
              prob = TRUE)
prob_pred.knn

y_pred.knn <-  knn(train = data.training[,3:4],
                   data.testing[,3:4],
                   k=3,
                   cl = data.training$Purchased, 
                   prob = FALSE)
y_pred.knn

cm.knn <- table(data.testing$Purchased, y_pred.knn)
cm.knn #accuracy 89/100

set = data.testing[,3:5]
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
prob_set = predict(logit, type="response", newdata = grid_set)
y_grid = knn(train = data.training[,3:4],
             test = grid_set,
             k=3,
             cl = data.training$Purchased, 
             prob = FALSE)

plot(set[,-3], main = "K-NN, k=3 (Test Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))


#SVM Linear
library(e1071)
svm.linear <- svm(formula=Purchased~Age + EstimatedSalary,
                  data = data.training,
                  type = "C-classification",
                  kernel = "linear")

y_pred.svm <- predict(svm.linear, newdata = data.testing)
cm.svm <- table(data.testing$Purchased, y_pred.svm)
cm.svm #Accuracy 80/100

set = data.testing[,3:5]
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
prob_set = predict(logit, type="response", newdata = grid_set)
y_grid = predict(svm.linear, newdata = grid_set)

plot(set[,-3], main = "SVM Linear (Test Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))

#SVM Radial
svm.radial <- svm(formula=Purchased~Age + EstimatedSalary,
                  data = data.training,
                  type = "C-classification",
                  kernel = "radial")

y_pred.svm.radial <- predict(svm.radial, newdata = data.testing)
cm.svm.radial <- table(data.testing$Purchased, y_pred.svm.radial)
cm.svm.radial #Accuracy 90/100

set = data.testing[,3:5]
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
prob_set = predict(logit, type="response", newdata = grid_set)
y_grid = predict(svm.radial, newdata = grid_set)

plot(set[,-3], main = "SVM Radial (Test Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))

#Decision Tree & Random Forest
library(randomForest)

set.seed(123)
rf <- randomForest(formula = Purchased~Age + EstimatedSalary,
                   data = data.training, ntree = 300)
prob_pred.rf <- predict(rf, newdata = data.testing,
                     type = "prob")
prob_pred.rf


y_pred.rf <- predict(rf, newdata = data.testing,
                        type = "response")
y_pred.rf

cm.rf <- table(data.testing$Purchased, y_pred.rf)
cm.rf #Accuracy 86/100

set = data.testing[,3:5]
X1 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
X2 = seq(min(set[,1]) - 1, max(set[,1] + 1), by=0.05)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c("Age", "EstimatedSalary")
prob_set = predict(logit, type="response", newdata = grid_set) 
y_grid = predict(rf, newdata = grid_set)

plot(set[,-3], main = "Random Forest, ntree = 300 (Test Set)",
     xlab = "Age", ylab="Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch='.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch=21, bg = ifelse(set[,3]==1, "green4", "red3"))

#Baseline
table(data.testing$Purchased) #baseline = 64%

#ROC
install.packages("pROC")
library(pROC)

#logit
ROC_logit <- roc(data.testing$Purchased, prob_pred)
plot(ROC_logit)
AUC_logit <- auc(ROC_logit)
AUC_logit #0.91

#KNN
ROC_knn <- roc(data.testing$Purchased, as.factor(prob_pred.knn))
plot(ROC_knn)
AUC_knn <- auc(ROC_knn)
AUC_knn #0.88

#Random FOrest
ROC_rf <- roc(data.testing$Purchased, prob_pred.rf[,2])
plot(ROC_rf)
AUC_rf <- auc(ROC_rf)
AUC_rf #0.91
