#Randomly shuffle the data
dataset<-dataset[sample(nrow(dataset)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(dataset)),breaks=10,labels=FALSE)
folds

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- dataset[testIndexes, ]
  trainData <- dataset[-testIndexes, ]
  
}

#install.packages("caret")
library(caret)
# Define training control
set.seed(123)
data <- read.csv("../datasets/Social_Network_Ads.csv")
data$Purchased <- as.factor(data$Purchased)
train.control <- trainControl(method = "cv", 
                              number = 10)
# Train the model
model <- train(Purchased ~Age+EstimatedSalary, data = data, method = "rf",
               trControl = train.control)
# Summarize the results
print(model)

predict(model, test_set, type="prob")


data(iris)
# define training control
train_control <- trainControl(method="cv", number=10)
# fix the parameters of the algorithm
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# train the model
model <- train(Species~., data=iris, trControl=train_control, method="nb", tuneGrid=grid)
# summarize results
print(model)