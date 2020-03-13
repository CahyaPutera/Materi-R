data.credit <- read.csv("~/Documents/OneTwoCode/DataScience/Week 4/datasets/credit_risk.csv")

#Explore data, bersih2 data (ubah nama kolom, ubah tipe data, etc)
str(data.credit)
data.credit$not_paid <- as.factor(data.credit$not_paid)
data.credit$verified <- as.factor(data.credit$verified)
data.credit$grdCtoA <- as.factor(data.credit$grdCtoA)
str(data.credit)

#Training vs Testing (Split Ratio = 3/4)
library(caTools)
split <- sample.split(data.credit$not_paid, SplitRatio = 3/4)
data.training <- data.credit[split,]
data.testing <- data.credit[!split,]

#Fitting ke beberapa model (logit, SVM, random forest)
#LOGIT
logit.model <- glm(formula = not_paid ~., data = data.training,
                   family = binomial)
summary(logit.model)
logit.model2 <- step(logit.model, direction = "both")
summary(logit.model2)
fm <- logit.model2$formula

prob.pred.logit <- predict(logit.model2, data.testing, type="response")
y.pred.logit <- ifelse(prob.pred.logit >=0.5, 1, 0)

#RandomForest
library(randomForest)
rf.model <- randomForest(formula = fm, data = data.training)
prob.pred.rf <- predict(rf.model, data.testing, type="prob")
y.pred.rf <- ifelse(prob.pred.rf[,2] >=0.5, 1, 0)

#SVM
svm.model <- svm(formula = as.numeric(not_paid) ~ installment + grdCtoA + annual_inc + verified, 
                 data = data.training, type = "eps-regression",
                 kernel = "radial")
prob.pred.svm <- predict(svm.model, data.testing) - 1
prob.pred.svm <- ifelse(prob.pred.svm >= 1, 1, prob.pred.svm)
prob.pred.svm <- ifelse(prob.pred.svm <= 0, 0, prob.pred.svm)

y.pred.svm <- ifelse(prob.pred.svm >= 0.5, 1, 0)


#Tentukan baseline
table(data.testing$not_paid) #50%

#Evaluasi model (Accuracy, ROC, AUC)
table(data.testing$not_paid, y.pred.logit) #57.98%
table(data.testing$not_paid, y.pred.rf) #58.5%
table(data.testing$not_paid, y.pred.svm) #57.21%

library(pROC)
ROC.logit <- roc(data.testing$not_paid, prob.pred.logit)
ROC.rf <- roc(data.testing$not_paid, prob.pred.rf[,2])
ROC.svm <- roc(data.testing$not_paid, prob.pred.svm)
plot(ROC.logit, col="red")
lines(ROC.rf, col="blue")
lines(ROC.svm, col="green")

auc(ROC.logit)
auc(ROC.rf)
auc(ROC.svm)
