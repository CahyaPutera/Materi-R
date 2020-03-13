install.packages("caret")
library(caret)

train.control <- trainControl(method = "cv", number = 10)
model <- train(Purchased ~ Age + EstimatedSalary, data = data.training, method = "glm", 
               family = "binomial", trControl = train.control)
summary(model)
predict(model, data.testing)

print(model)


