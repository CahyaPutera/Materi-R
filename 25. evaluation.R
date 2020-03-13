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
classifier = glm(formula = Purchased ~ ., data=training_set,
                 family = binomial)
summary(classifier)

#Predicting
lm_prob_pred = predict(classifier, type = "response", newdata = test_set[-3])
lm_y_pred = ifelse(lm_prob_pred > 0.5, 1, 0)

#Fitting RF
library(randomForest)

classifier = randomForest(x=training_set[-3],
                          y=factor(training_set$Purchased),
                          ntree=400)
rf_y_pred = predict(classifier, newdata = test_set[-3], type="prob")


#Confusion Matrix
lm_cm = table(test_set[,3], lm_y_pred)
lm_cm

rf_cm = table(test_set[,3], rf_y_pred[,2])
rf_cm

#install.packages("pROC")
library(pROC)
ROC_rf <- roc(test_set$Purchased, rf_y_pred[,2])
ROC_lr <- roc(test_set$Purchased, lm_prob_pred)

ROC_rf_auc <- auc(ROC_rf)
ROC_lr_auc <- auc(ROC_lr)
ROC_rf_auc
ROC_lr_auc


#SVM
svm.model.radial <- svm(formula = Purchased ~ Age + EstimatedSalary,
                        data = training_set,
                        type = "eps-regression",
                        kernel="radial")
svm_pred <- predict(svm.model.radial, newdata = test_set)
svm_pred

install.packages(caret)

ROC_svm <- roc(test_set$Purchased, as.numeric(svm_pred))
AUC_svm <- auc(ROC_svm)
AUC_svm

# plot ROC curves
plot(ROC_rf, col = "green", main = "ROC For Random Forest (GREEN) vs Logistic Regression (RED)")
lines(ROC_lr, col = "red")
lines(ROC_svm, col = "blue")


# print the performance of each model
paste("Accuracy % of random forest: ", mean(test_set$Purchased == round(rf_y_pred[,2], digits = 0)))
paste("Accuracy % of logistic regression: ", mean(test_set$Purchased == round(lm_y_pred, digits = 0))) 
paste("Accuracy % of svm: ", mean(test_set$Purchased == round(svm_pred, digits = 0)))   
paste("Area under curve of random forest: ", ROC_rf_auc)
paste("Area under curve of logistic regression: ", ROC_lr_auc)
paste("Area under curve of svm: ", AUC_svm)

