data <- read.csv("../datasets/credit_risk.csv")
summary(data)
data <- data[,-1]
data$not_paid <- as.factor(data$not_paid)
data$verified <- as.factor(data$verified)
data$home_ownership <- as.factor(data$home_ownership)
data$grdCtoA <- as.factor(data$grdCtoA)
summary(data)

data[,c("installment", "int_rate",
            "annual_inc")] <- scale(data[,c("installment", "int_rate",
                      "annual_inc")])

set.seed(417)
intrain <- sample(nrow(data), nrow(data)*0.8)
loans.train <- data[intrain, ]
loans.test <- data[-intrain, ]
?step

logit <- glm(not_paid ~ verified + purpose + installment + int_rate + 
                    home_ownership + grdCtoA + annual_inc, loans.train, family="binomial")
logit2 <- step(logit, direction = "both")
fm <- logit2$formula

summary(logit)
summary(logit2)



y_pred <- predict(logit, type = "response", newdata = loans.test)
y_pred

y_pred.2 <- predict(logit2, type = "response", newdata = loans.test)
y_pred.2

table(loans.test$not_paid, ifelse(y_pred>=0.5,1,0))
table(loans.test$not_paid, ifelse(y_pred.2>=0.5,1,0))


rf.model = randomForest(formula=fm, data = loans.train, ntree=300)
rf_pred <- predict(rf.model, type = "prob", newdata = loans.test)

y_pred.rf <- ifelse(rf_pred[,2] >= 0.5, 1, 0)
table(loans.test$not_paid, y_pred.rf)


loans.train$not_paid <- as.numeric(loans.train$not_paid) - 1
svm.model <- svm(formula=fm, data=loans.train, type="eps-regression", kernel="radial")
svm_pred <- predict(svm.model, type = "response", newdata = loans.test)
svm_pred
y_pred.svm <- ifelse(svm_pred >= 0.5, 1, 0)
table(loans.test$not_paid, y_pred.svm)


ROC_lm <- roc(loans.test$not_paid, y_pred)
AUC_lm <- auc(ROC_lm)
AUC_lm
ROC_lm.2 <- roc(loans.test$not_paid, y_pred)
AUC_lm.2 <- auc(ROC_lm.2)
AUC_lm.2
ROC_rf <- roc(loans.test$not_paid, rf_pred[,2])
AUC_rf <- auc(ROC_rf)
AUC_rf
ROC_svm <- roc(loans.test$not_paid, svm_pred)
AUC_svm <- auc(ROC_svm)
AUC_svm

plot(ROC_lm, col = "green", main = "ROC For Random Forest (GREEN) vs Logistic Regression (RED)")
lines(ROC_rf, col = "red")
lines(ROC_svm, col="blue")

baseline <- prop.table(table(loans.test$not_paid))
baseline
paste("Accuracy % of random forest: ", mean(loans.test$not_paid == round(rf_pred, digits = 0)))
paste("Accuracy % of logit regression: ", mean(loans.test$not_paid == round(y_pred, digits = 0)))
paste("Accuracy % of svm: ", mean(loans.test$not_paid == round(y_pred.svm, digits = 0)))

#baseline
table(loans.test$not_paid)
161 / (161+151)









