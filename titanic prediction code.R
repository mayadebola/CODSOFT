###TITANICDATASET PREDICTION CODE

install.packages(c("tidyverse", "caret"))
library (tidyverse)
library (caret)
if (!require(randomForest)) {
  +   install.packages("randomForest")
}
library(randomforest)

## IMPORTING THE TITANIC DATASET
#load the titanic dataset
titanic_dataset <- read.csv("C:/Users/user/Downloads/Titanic-Dataset.csv")

View(data)
head(data)
str(data)

## DATA CLEANING
colSums(is.na(data))
data = data %>%
  mutate(Age = ifelse(is.na(Age),mean(Age,na.rm =TRUE),Age))
colSums(is.na(data))
data = data %>%
  
  select(-PassengerId,-Name,-Ticket,-Cabin)

## DATA VISUALIZATION

library(ggplot2)
ggplot(data, aes(x = Survived)) +
  geom_bar(stat = 'count')
ggplot(data, aes(x = Sex)) +
  geom_bar(stat = 'count')
library(dplyr)
ggplot(data, aes(x = Age)) +
  geom_histogram(fill = 'lightblue', color='black')


## DATA PREPROCESSING
table(data$Sex)
data$Sex = ifelse(data$Sex == 'male',1,0)
data$Embarked = as.factor(data$Embarked)
data$Survived = as.factor(data$Survived)
data$Age = as.integer(data$Age)
str(data)


## DATA SPLITTING

set.seed(100)
index = createDataPartition(data$Survived, p=0.8, list= FALSE)

data_shape = dim(data)
print(data_shape)

train_data = data[index,]
test_data = data[-index,]

train_data_shape = dim(train_data)
test_data_shape = dim(test_data)

print(train_data_shape)
print(test_data_shape)

## RANDOM FOREST ALGORITHM

install.packages('randomForest')
library(randomForest)
## BUILDING THE MODEL

rf_model = randomForest(Survived~., data = train_data, ntree= 500, mtrye=4, importance=TRUE)

## MODEL PREDICTION

pred = predict(rf_model,test_data)
pred

## CONFUSION MATRIX AND ACCURACY

conf_matrix = table(pred, test_data$Survived)
conf_matrix
accuracy = sum(diag(conf_matrix))/sum(conf_matrix)
print(paste('Accuracy is :',accuracy))
print(paste('Accuracy is :',accuracy*100))

