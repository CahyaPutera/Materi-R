bank.data <- read.csv("../datasets/bank_marketing_weka_dataset.csv")
str(bank.data)
summary(bank.data)

set.seed(123)
bank.split = sample.split(bank.data$y, SplitRatio = 0.75)
bank.training = bank.data[split,]
bank.test = bank.data[!split,]

bank.lm <- glm(formula = y~., data = bank.training,
               family = binomial)
summary(bank.lm)

bank.lm.fs <- step(bank.lm, direction = "both")
summary(bank.lm.fs)
fm <- bank.lm.fs$formula

svm.model <- svm(formula = fm, data = bank.training,
                 type="C-classification",
                 kernel="linear")

rf.model <- randomForest(formula=fm, data = bank.training,
                         ntree = 500)

prob_pred <- predict(rf.model, newdata=bank.test,
                     type = "prob")
y_pred <- ifelse(prob_pred[,2] >= 0.5, 1, 0)

table(bank.test$y, y_pred)
table(bank.test$y)
