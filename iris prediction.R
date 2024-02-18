install.packages(c("tidyverse", "caret"))
library (tidyverse)
library (caret)
if (!require(randomForest)) {
  +   install.packages("randomForest")
}
library(randomforest)

####importing the data set
iris_dataset <- read.csv("C:/Users/user/OneDrive/Documents/IRIS.csv")
view(iris_dataset)
head(iris_dataset)
str(iris_dataset)
summary(iris_dataset)

#### data visualization
par(mfrow=c(2, 2))
for (i in 1:4) {
  hist(iris$Sepal.Length[iris$Species == "setosa"], col="purple", xlab="Sepal Length", main="Setosa")
  hist(iris$Sepal.Length[iris$Species == "versicolor"], col="yellow", xlab="Sepal Length", main="Versicolor")
  hist(iris$Sepal.Length[iris$Species == "virginica"], col="blue", xlab="Sepal Length", main="Virginica")
}
par(mfrow=c(1, 1))

pairs(iris[, 1:4], col = iris_dataset$Species)



### Split the data set into training and testing sets
set.seed(100) 
# for reproducibility
iris_dataset_trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
iris_dataset_trainIndex
data_train <- iris[iris_data_trainIndex, ]
data_train
data_test <- iris[-iris_dataset_trainIndex, ]
data_test

# Train a machine learning model
model <- train(Species ~ ., data = data_train, method = "rf")
model

predictions <- predict(model, newdata = data_test)
predictions

confusionMatrix(predictions, data_test $Species)