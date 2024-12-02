set.seed(2024)

vet1_train <- read.table(file = "/Users/gmarrero/Desktop/MKT/vet1_train.csv", sep=",", header = TRUE);
vet1_test <- read.table(file = "/Users/gmarrero/Desktop/MKT/vet1_test.csv", sep=",", header = TRUE);

vet2_train <- read.table(file = "/Users/gmarrero/Desktop/MKT/vet2_train.csv", sep=",", header = TRUE);
vet2_test <- read.table(file = "/Users/gmarrero/Desktop/MKT/vet2_test.csv", sep=",", header = TRUE);

vet3_train <- read.table(file = "/Users/gmarrero/Desktop/MKT/vet3_train.csv", sep=",", header = TRUE);
vet3_test <- read.table(file = "/Users/gmarrero/Desktop/MKT/vet3_test.csv", sep=",", header = TRUE);

# Unknown values for any NA
#vet1_train$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(vet1_train$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"
#vet1_test$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(vet1_test$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"
#vet2_train$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(vet2_train$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"
#vet2_test$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(vet2_test$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"
#vet3_train$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(vet3_train$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"
#vet3_test$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(vet3_test$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"

library(caret)
library(dplyr)
library(e1071)
library(randomForest)

# Drop the column "X"
vet1_train <- vet1_train %>% select(-X)
vet1_test <- vet1_test %>% select(-X)
vet2_train <- vet2_train %>% select(-X)
vet2_test <- vet2_test %>% select(-X)
vet3_train <- vet3_train %>% select(-X)
vet3_test <- vet3_test %>% select(-X)

# SUS_FLAG as Factor
vet1_train$SUS_FLAG <- as.factor(vet1_train$SUS_FLAG)
vet1_test$SUS_FLAG <- as.factor(vet1_test$SUS_FLAG)
vet2_train$SUS_FLAG <- as.factor(vet2_train$SUS_FLAG)
vet2_test$SUS_FLAG <- as.factor(vet2_test$SUS_FLAG)
vet3_train$SUS_FLAG <- as.factor(vet3_train$SUS_FLAG)
vet3_test$SUS_FLAG <- as.factor(vet3_test$SUS_FLAG)

# rf_model is our Random Forest model for Vet1 Data. 
# Fit random forest model for Vet1
rf_model <- randomForest(SUS_FLAG ~ ., data = vet1_train, importance = TRUE)
importance(rf_model)
#varImpPlot(rf_model)

# Make predictions on the test set
rf_predictions <- predict(rf_model, newdata = vet1_test)

rf_predictions
table(rf_predictions)
table(vet1_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions <- as.factor(rf_predictions)
actual_values <- as.factor(vet1_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix <- table(Predicted = rf_predictions, Actual = actual_values)

# Print the confusion matrix
print(conf_matrix)


### Calculating Training Error for Vet1 using rf_model

# Predictions on the training data
train_predictions <- predict(rf_model, newdata = vet1_train)

# Confusion matrix for training data
train_conf_matrix <- table(Predicted = train_predictions, Actual = vet1_train$SUS_FLAG)
print(train_conf_matrix)

# Calculate Training Accuracy
train_accuracy <- sum(diag(train_conf_matrix)) / sum(train_conf_matrix)

# Calculate Training Error
train_error <- 1 - train_accuracy

# Print Training Error
cat("Training Error:", train_error, "\n")

### Calculating Testing Error on Vet1_Test data using Vet1_Train
test_predictions <- predict(rf_model, newdata = vet1_test)
test_predictions <- as.factor(test_predictions)
actual_values_test <- as.factor(vet1_test$SUS_FLAG)
test_conf_matrix <- table(Predicted = test_predictions, Actual = actual_values_test)

test_accuracy <- sum(diag(test_conf_matrix)) / sum(test_conf_matrix)
test_error <- 1 - test_accuracy
cat("Testing Error:", test_error, "\n")

### Running rf_model (Vet1_Train) on Vet3_Test data. 

# Make predictions on the test set
rf_predictions <- predict(rf_model, newdata = vet3_test)

rf_predictions
table(rf_predictions)
table(vet3_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions <- as.factor(rf_predictions)
actual_values <- as.factor(vet3_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix <- table(Predicted = rf_predictions, Actual = actual_values)

# Print the confusion matrix
print(conf_matrix)


### Calculating Testing Error
test_predictions <- predict(rf_model, newdata = vet3_test)
test_predictions <- as.factor(test_predictions)
actual_values_test <- as.factor(vet3_test$SUS_FLAG)
test_conf_matrix <- table(Predicted = test_predictions, Actual = actual_values_test)

test_accuracy <- sum(diag(test_conf_matrix)) / sum(test_conf_matrix)
test_error <- 1 - test_accuracy
cat("Testing Error:", test_error, "\n")


### Creating Random Forest Models for Vet2_Train

sample_vet_split <- sample(c(TRUE, FALSE), nrow(vet2_train), replace=TRUE, prob=c(0.45,0.55))
vet2_train_45  <- vet2_train[sample_vet_split, ]
vet2_train_other_55   <- vet2_train[!sample_vet_split, ]


################################

### Variable Selection based on for 70% Cumulative Importance

# Importance scores based on the original RF model from 1
importance_scores <- importance(rf_model, type = 1)

# Sort features by importance in descending order
sorted_importance <- sort(importance_scores[, 1], decreasing = TRUE)

# Calculate cumulative importance
cumulative_importance <- cumsum(sorted_importance) / sum(sorted_importance)

# Select features contributing up to 70% of cumulative importance
selected_features <- names(cumulative_importance[cumulative_importance <= 0.7])

# Subset the dataset to keep only selected features and target variable
vet2_train_45_reduced <- vet2_train_45[, c(selected_features, "SUS_FLAG")]

# Fit the random forest model on the reduced feature set
rf_model2_45_reduced_0.7 <- randomForest(SUS_FLAG ~ ., data = vet2_train_45_reduced, importance = TRUE)


### Variable Selection based on for 80% Cumulative Importance

# Select features contributing up to 80% of cumulative importance
selected_features <- names(cumulative_importance[cumulative_importance <= 0.8])

# Subset the dataset to keep only selected features and target variable
vet2_train_45_reduced <- vet2_train_45[, c(selected_features, "SUS_FLAG")]

# Fit the random forest model on the reduced feature set
rf_model2_45_reduced_0.8 <- randomForest(SUS_FLAG ~ ., data = vet2_train_45_reduced, importance = TRUE)


### Variable Selection based on for 90% Cumulative Importance

# Select features contributing up to 90% of cumulative importance
selected_features <- names(cumulative_importance[cumulative_importance <= 0.9])

# Subset the dataset to keep only selected features and target variable
vet2_train_45_reduced <- vet2_train_45[, c(selected_features, "SUS_FLAG")]

# Fit the random forest model on the reduced feature set
rf_model2_45_reduced_0.9 <- randomForest(SUS_FLAG ~ ., data = vet2_train_45_reduced, importance = TRUE)

###

############ Train: 45% of Vet2_Train data, Test: Vet1_Test. Cumulative Importance: 70%

### Calculating Training Error on Vet2_Train using 45% of data, using 70% Cumulative Importance

# Predictions on the training data
train_predictions2 <- predict(rf_model2_45_reduced_0.7, newdata = vet2_train_45)

# Confusion matrix for training data
train_conf_matrix2 <- table(Predicted = train_predictions2, Actual = vet2_train_45$SUS_FLAG)
print(train_conf_matrix2)

# Calculate Training Accuracy
train_accuracy2 <- sum(diag(train_conf_matrix2)) / sum(train_conf_matrix2)

# Calculate Training Error
train_error2 <- 1 - train_accuracy2

# Print Training Error
cat("Training Error:", train_error2, "\n")

### 

# Make predictions on the test set
rf_predictions2 <- predict(rf_model2_45_reduced_0.7, newdata = vet1_test)

rf_predictions2
table(rf_predictions2)
table(vet1_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions2 <- as.factor(rf_predictions2)
actual_values2 <- as.factor(vet1_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix2 <- table(Predicted = rf_predictions2, Actual = actual_values2)

# Print the confusion matrix
print(conf_matrix2)


### Calculating Testing Error
test_predictions2 <- predict(rf_model2_45_reduced_0.7, newdata = vet1_test)
test_predictions2 <- as.factor(test_predictions2)
actual_values_test2 <- as.factor(vet1_test$SUS_FLAG)
test_conf_matrix2 <- table(Predicted = test_predictions2, Actual = actual_values_test2)

test_accuracy2 <- sum(diag(test_conf_matrix2)) / sum(test_conf_matrix2)
test_error2 <- 1 - test_accuracy2
cat("Testing Error:", test_error2, "\n")


############ Train: 45% of Vet2_Train data, Test: Vet1_Test. Cumulative Importance: 80%


### Calculating Training Error on Vet2_Train using 45% of data, using 80% Cumulative Importance

# Predictions on the training data
train_predictions2 <- predict(rf_model2_45_reduced_0.8, newdata = vet2_train_45)

# Confusion matrix for training data
train_conf_matrix2 <- table(Predicted = train_predictions2, Actual = vet2_train_45$SUS_FLAG)
print(train_conf_matrix2)

# Calculate Training Accuracy
train_accuracy2 <- sum(diag(train_conf_matrix2)) / sum(train_conf_matrix2)

# Calculate Training Error
train_error2 <- 1 - train_accuracy2

# Print Training Error
cat("Training Error:", train_error2, "\n")

### 

# Make predictions on the test set
rf_predictions2 <- predict(rf_model2_45_reduced_0.8, newdata = vet1_test)

rf_predictions2
table(rf_predictions2)
table(vet1_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions2 <- as.factor(rf_predictions2)
actual_values2 <- as.factor(vet1_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix2 <- table(Predicted = rf_predictions2, Actual = actual_values2)

# Print the confusion matrix
print(conf_matrix2)


### Calculating Testing Error
test_predictions2 <- predict(rf_model2_45_reduced_0.8, newdata = vet1_test)
test_predictions2 <- as.factor(test_predictions2)
actual_values_test2 <- as.factor(vet1_test$SUS_FLAG)
test_conf_matrix2 <- table(Predicted = test_predictions2, Actual = actual_values_test2)

test_accuracy2 <- sum(diag(test_conf_matrix2)) / sum(test_conf_matrix2)
test_error2 <- 1 - test_accuracy2
cat("Testing Error:", test_error2, "\n")



############ Train: 45% of Vet2_Train data, Test: Vet1_Test. Cumulative Importance: 90%

### Calculating Training Error on Vet2_Train using 45% of data, using 90% Cumulative Importance

# Predictions on the training data
train_predictions2 <- predict(rf_model2_45_reduced_0.9, newdata = vet2_train_45)

# Confusion matrix for training data
train_conf_matrix2 <- table(Predicted = train_predictions2, Actual = vet2_train_45$SUS_FLAG)
print(train_conf_matrix2)

# Calculate Training Accuracy
train_accuracy2 <- sum(diag(train_conf_matrix2)) / sum(train_conf_matrix2)

# Calculate Training Error
train_error2 <- 1 - train_accuracy2

# Print Training Error
cat("Training Error:", train_error2, "\n")

### 

# Make predictions on the test set
rf_predictions2 <- predict(rf_model2_45_reduced_0.9, newdata = vet1_test)

rf_predictions2
table(rf_predictions2)
table(vet1_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions2 <- as.factor(rf_predictions2)
actual_values2 <- as.factor(vet1_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix2 <- table(Predicted = rf_predictions2, Actual = actual_values2)

# Print the confusion matrix
print(conf_matrix2)


### Calculating Testing Error
test_predictions2 <- predict(rf_model2_45_reduced_0.9, newdata = vet1_test)
test_predictions2 <- as.factor(test_predictions2)
actual_values_test2 <- as.factor(vet1_test$SUS_FLAG)
test_conf_matrix2 <- table(Predicted = test_predictions2, Actual = actual_values_test2)

test_accuracy2 <- sum(diag(test_conf_matrix2)) / sum(test_conf_matrix2)
test_error2 <- 1 - test_accuracy2
cat("Testing Error:", test_error2, "\n")


############ Train: 45% of Vet2_Train data, Test: Vet2_Test. Cumulative Importance: 70%


# Make predictions on the test set
rf_predictions2 <- predict(rf_model2_45_reduced_0.7, newdata = vet2_test)

rf_predictions2
table(rf_predictions2)
table(vet1_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions2 <- as.factor(rf_predictions2)
actual_values2 <- as.factor(vet2_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix2 <- table(Predicted = rf_predictions2, Actual = actual_values2)

# Print the confusion matrix
print(conf_matrix2)


### Calculating Testing Error
test_predictions2 <- predict(rf_model2_45_reduced_0.7, newdata = vet2_test)
test_predictions2 <- as.factor(test_predictions2)
actual_values_test2 <- as.factor(vet2_test$SUS_FLAG)
test_conf_matrix2 <- table(Predicted = test_predictions2, Actual = actual_values_test2)

test_accuracy2 <- sum(diag(test_conf_matrix2)) / sum(test_conf_matrix2)
test_error2 <- 1 - test_accuracy2
cat("Testing Error:", test_error2, "\n")


############ Train: 45% of Vet2_Train data, Test: Vet2_Test. Cumulative Importance: 80%

# Make predictions on the test set
rf_predictions2 <- predict(rf_model2_45_reduced_0.8, newdata = vet2_test)

rf_predictions2
table(rf_predictions2)
table(vet1_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions2 <- as.factor(rf_predictions2)
actual_values2 <- as.factor(vet2_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix2 <- table(Predicted = rf_predictions2, Actual = actual_values2)

# Print the confusion matrix
print(conf_matrix2)


### Calculating Testing Error
test_predictions2 <- predict(rf_model2_45_reduced_0.8, newdata = vet2_test)
test_predictions2 <- as.factor(test_predictions2)
actual_values_test2 <- as.factor(vet2_test$SUS_FLAG)
test_conf_matrix2 <- table(Predicted = test_predictions2, Actual = actual_values_test2)

test_accuracy2 <- sum(diag(test_conf_matrix2)) / sum(test_conf_matrix2)
test_error2 <- 1 - test_accuracy2
cat("Testing Error:", test_error2, "\n")


############ Train: 45% of Vet2_Train data, Test: Vet2_Test. Cumulative Importance: 90%

# Make predictions on the test set
rf_predictions2 <- predict(rf_model2_45_reduced_0.9, newdata = vet2_test)

rf_predictions2
table(rf_predictions2)
table(vet1_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions2 <- as.factor(rf_predictions2)
actual_values2 <- as.factor(vet2_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix2 <- table(Predicted = rf_predictions2, Actual = actual_values2)

# Print the confusion matrix
print(conf_matrix2)


### Calculating Testing Error
test_predictions2 <- predict(rf_model2_45_reduced_0.9, newdata = vet2_test)
test_predictions2 <- as.factor(test_predictions2)
actual_values_test2 <- as.factor(vet2_test$SUS_FLAG)
test_conf_matrix2 <- table(Predicted = test_predictions2, Actual = actual_values_test2)

test_accuracy2 <- sum(diag(test_conf_matrix2)) / sum(test_conf_matrix2)
test_error2 <- 1 - test_accuracy2
cat("Testing Error:", test_error2, "\n")

############ Train: 45% of Vet2_Train data, Test: Vet3_Test. Cumulative Importance: 70%


# Make predictions on the test set
rf_predictions2 <- predict(rf_model2_45_reduced_0.7, newdata = vet3_test)

rf_predictions2
table(rf_predictions2)
table(vet1_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions2 <- as.factor(rf_predictions2)
actual_values2 <- as.factor(vet3_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix2 <- table(Predicted = rf_predictions2, Actual = actual_values2)

# Print the confusion matrix
print(conf_matrix2)


### Calculating Testing Error
test_predictions2 <- predict(rf_model2_45_reduced_0.7, newdata = vet3_test)
test_predictions2 <- as.factor(test_predictions2)
actual_values_test2 <- as.factor(vet3_test$SUS_FLAG)
test_conf_matrix2 <- table(Predicted = test_predictions2, Actual = actual_values_test2)

test_accuracy2 <- sum(diag(test_conf_matrix2)) / sum(test_conf_matrix2)
test_error2 <- 1 - test_accuracy2
cat("Testing Error:", test_error2, "\n")


############ Train: 45% of Vet2_Train data, Test: Vet3_Test. Cumulative Importance: 80%

# Make predictions on the test set
rf_predictions2 <- predict(rf_model2_45_reduced_0.8, newdata = vet3_test)

rf_predictions2
table(rf_predictions2)
table(vet1_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions2 <- as.factor(rf_predictions2)
actual_values2 <- as.factor(vet3_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix2 <- table(Predicted = rf_predictions2, Actual = actual_values2)

# Print the confusion matrix
print(conf_matrix2)


### Calculating Testing Error
test_predictions2 <- predict(rf_model2_45_reduced_0.8, newdata = vet3_test)
test_predictions2 <- as.factor(test_predictions2)
actual_values_test2 <- as.factor(vet3_test$SUS_FLAG)
test_conf_matrix2 <- table(Predicted = test_predictions2, Actual = actual_values_test2)

test_accuracy2 <- sum(diag(test_conf_matrix2)) / sum(test_conf_matrix2)
test_error2 <- 1 - test_accuracy2
cat("Testing Error:", test_error2, "\n")


############ Train: 45% of Vet2_Train data, Test: Vet3_Test. Cumulative Importance: 90%

# Make predictions on the test set
rf_predictions2 <- predict(rf_model2_45_reduced_0.9, newdata = vet3_test)

rf_predictions2
table(rf_predictions2)
table(vet1_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions2 <- as.factor(rf_predictions2)
actual_values2 <- as.factor(vet3_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix2 <- table(Predicted = rf_predictions2, Actual = actual_values2)

# Print the confusion matrix
print(conf_matrix2)


### Calculating Testing Error
test_predictions2 <- predict(rf_model2_45_reduced_0.9, newdata = vet3_test)
test_predictions2 <- as.factor(test_predictions2)
actual_values_test2 <- as.factor(vet3_test$SUS_FLAG)
test_conf_matrix2 <- table(Predicted = test_predictions2, Actual = actual_values_test2)

test_accuracy2 <- sum(diag(test_conf_matrix2)) / sum(test_conf_matrix2)
test_error2 <- 1 - test_accuracy2
cat("Testing Error:", test_error2, "\n")


### Now running RF models using 20% of the Vet2_Train data, using all variables

sample_vet_split <- sample(c(TRUE, FALSE), nrow(vet2_train), replace=TRUE, prob=c(0.20,0.80))
vet2_train_twenty  <- vet2_train[sample_vet_split, ]
vet2_train_other_80   <- vet2_train[!sample_vet_split, ]

# Fit random forest model usuing 20% of the Vet2_Train Data
# This was the largest amount of Vet2_Train data that we could achieve computationally. 

rf_model2_20 <- randomForest(SUS_FLAG ~ ., data = vet2_train_twenty, importance = TRUE)
importance(rf_model2)
varImpPlot(rf_model)

### Calculating Training Error on rf_model2_20

# Predictions on the training data
train_predictions2 <- predict(rf_model2_20, newdata = vet2_train_twenty)

# Confusion matrix for training data
train_conf_matrix2 <- table(Predicted = train_predictions2, Actual = vet2_train_twenty$SUS_FLAG)
print(train_conf_matrix2)

# Calculate Training Accuracy
train_accuracy2 <- sum(diag(train_conf_matrix2)) / sum(train_conf_matrix2)

# Calculate Training Error
train_error2 <- 1 - train_accuracy2

# Print Training Error
cat("Training Error:", train_error2, "\n")


### Calculating Model Performance in Vet1_Test

# Make predictions on the test set
rf_predictions2 <- predict(rf_model2_20, newdata = vet1_test)

rf_predictions2
table(rf_predictions2)
table(vet1_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions2 <- as.factor(rf_predictions2)
actual_values2 <- as.factor(vet1_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix2 <- table(Predicted = rf_predictions2, Actual = actual_values2)

# Print the confusion matrix
print(conf_matrix2)

### Calculating Testing Error
test_predictions2 <- predict(rf_model2_20, newdata = vet1_test)
test_predictions2 <- as.factor(test_predictions2)
actual_values_test2 <- as.factor(vet1_test$SUS_FLAG)
test_conf_matrix2 <- table(Predicted = test_predictions2, Actual = actual_values_test2)

test_accuracy2 <- sum(diag(test_conf_matrix2)) / sum(test_conf_matrix2)
test_error2 <- 1 - test_accuracy2
cat("Testing Error:", test_error2, "\n")


###
### Calculating Model Performance in Vet2_Test
###

# Make predictions on the test set
rf_predictions2 <- predict(rf_model2_20, newdata = vet2_test)

rf_predictions2
table(rf_predictions2)
table(vet2_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions2 <- as.factor(rf_predictions2)
actual_values2 <- as.factor(vet2_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix2 <- table(Predicted = rf_predictions2, Actual = actual_values2)

# Print the confusion matrix
print(conf_matrix2)

### Calculating Testing Error
test_predictions2 <- predict(rf_model2_20, newdata = vet2_test)
test_predictions2 <- as.factor(test_predictions2)
actual_values_test2 <- as.factor(vet2_test$SUS_FLAG)
test_conf_matrix2 <- table(Predicted = test_predictions2, Actual = actual_values_test2)

test_accuracy2 <- sum(diag(test_conf_matrix2)) / sum(test_conf_matrix2)
test_error2 <- 1 - test_accuracy2
cat("Testing Error:", test_error2, "\n")


###
### Calculating Model Performance in Vet3_Test
###

# Make predictions on the test set
rf_predictions2 <- predict(rf_model2_20, newdata = vet3_test)

rf_predictions2
table(rf_predictions2)
table(vet3_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions2 <- as.factor(rf_predictions2)
actual_values2 <- as.factor(vet3_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix2 <- table(Predicted = rf_predictions2, Actual = actual_values2)

# Print the confusion matrix
print(conf_matrix2)

### Calculating Testing Error
test_predictions2 <- predict(rf_model2_20, newdata = vet3_test)
test_predictions2 <- as.factor(test_predictions2)
actual_values_test2 <- as.factor(vet3_test$SUS_FLAG)
test_conf_matrix2 <- table(Predicted = test_predictions2, Actual = actual_values_test2)

test_accuracy2 <- sum(diag(test_conf_matrix2)) / sum(test_conf_matrix2)
test_error2 <- 1 - test_accuracy2
cat("Testing Error:", test_error2, "\n")


###### Creating a Random Forest Model for Vet3_Train
###### Creating a Random Forest Model for Vet3_Train
###### Creating a Random Forest Model for Vet3_Train

# 90% of Vet3_Train was the most amount of data that we could computational withstand. 

sample_vet_split <- sample(c(TRUE, FALSE), nrow(vet3_train), replace=TRUE, prob=c(0.9,0.1))
vet3_train_0.9  <- vet3_train[sample_vet_split, ]
vet3_train_other_0.1   <- vet3_train[!sample_vet_split, ]

# Fit random forest model
rf_model3_0.9 <- randomForest(SUS_FLAG ~ ., data = vet3_train_0.9, importance = TRUE)
importance(rf_model3_0.9)
varImpPlot(rf_model3_0.9)


### Calculating Training Error on rf_model3_0.9

# Predictions on the training data
train_predictions3 <- predict(rf_model3_0.9, newdata = vet3_train_0.9)

# Confusion matrix for training data
train_conf_matrix3 <- table(Predicted = train_predictions3, Actual = vet3_train_0.9$SUS_FLAG)
print(train_conf_matrix3)

# Calculate Training Accuracy
train_accuracy3 <- sum(diag(train_conf_matrix3)) / sum(train_conf_matrix3)

# Calculate Training Error
train_error3 <- 1 - train_accuracy3

# Print Training Error
cat("Training Error:", train_error3, "\n")


# Testing Vet3_Train Random Forest on Vet1_Test 
# Make predictions on the test set
rf_predictions3 <- predict(rf_model3_0.9, newdata = vet1_test)

rf_predictions3
table(rf_predictions3)
table(vet1_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions3 <- as.factor(rf_predictions3)
actual_values3 <- as.factor(vet1_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix3 <- table(Predicted = rf_predictions3, Actual = actual_values3)

# Print the confusion matrix
print(conf_matrix3)


### Calculating Testing Error
test_predictions3 <- predict(rf_model3_0.9, newdata = vet1_test)
test_predictions3 <- as.factor(test_predictions3)
actual_values_test3 <- as.factor(vet1_test$SUS_FLAG)
test_conf_matrix3 <- table(Predicted = test_predictions3, Actual = actual_values_test3)

test_accuracy3 <- sum(diag(test_conf_matrix3)) / sum(test_conf_matrix3)
test_error3 <- 1 - test_accuracy3
cat("Testing Error:", test_error3, "\n")


# Testing Vet3_Train Random Forest on Vet2_Test 
# Make predictions on the test set
rf_predictions3 <- predict(rf_model3_0.9, newdata = vet2_test)

rf_predictions3
table(rf_predictions3)
table(vet2_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions3 <- as.factor(rf_predictions3)
actual_values3 <- as.factor(vet2_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix3 <- table(Predicted = rf_predictions3, Actual = actual_values3)

# Print the confusion matrix
print(conf_matrix3)


### Calculating Testing Error on Vet2_Test
test_predictions3 <- predict(rf_model3_0.9, newdata = vet2_test)
test_predictions3 <- as.factor(test_predictions3)
actual_values_test3 <- as.factor(vet2_test$SUS_FLAG)
test_conf_matrix3 <- table(Predicted = test_predictions3, Actual = actual_values_test3)

test_accuracy3 <- sum(diag(test_conf_matrix3)) / sum(test_conf_matrix3)
test_error3 <- 1 - test_accuracy3
cat("Testing Error:", test_error3, "\n")


# Testing Vet3_Train Random Forest on Vet2_Test 
# Make predictions on the test set
rf_predictions3 <- predict(rf_model3_0.9, newdata = vet3_test)

rf_predictions3
table(rf_predictions3)
table(vet3_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions3 <- as.factor(rf_predictions3)
actual_values3 <- as.factor(vet3_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix3 <- table(Predicted = rf_predictions3, Actual = actual_values3)

# Print the confusion matrix
print(conf_matrix3)


### Calculating Testing Error
test_predictions3 <- predict(rf_model3_0.9, newdata = vet3_test)
test_predictions3 <- as.factor(test_predictions3)
actual_values_test3 <- as.factor(vet3_test$SUS_FLAG)
test_conf_matrix3 <- table(Predicted = test_predictions3, Actual = actual_values_test3)

test_accuracy3 <- sum(diag(test_conf_matrix3)) / sum(test_conf_matrix3)
test_error3 <- 1 - test_accuracy3
cat("Testing Error:", test_error3, "\n")



####
#### Naive Bayes Modeling
####

# Training Naive Bayes Model on Vet1_Train data
nb_model_vet <- naiveBayes(SUS_FLAG ~ ., data = vet1_train)
nb_predictions_vet <- predict(nb_model_vet, newdata = vet1_test)

# Confusion matrix
nb_conf_matrix_vet <- table(Predicted = nb_predictions_vet, Actual = vet1_test$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_vet)

# Testing Error
test_accuracy <- sum(diag(nb_conf_matrix_vet)) / sum(nb_conf_matrix_vet)
test_error <- 1 - test_accuracy
cat("Testing Error:", test_error, "\n")

# Calculating Training Error
nb_predictions_vet_train <- predict(nb_model_vet, newdata = vet1_train)
nb_conf_matrix_vet_train <- table(Predicted = nb_predictions_vet_train, Actual = vet1_train$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_vet_train)

train_accuracy <- sum(diag(nb_conf_matrix_vet_train)) / sum(nb_conf_matrix_vet_train)
train_error <- 1 - train_accuracy
cat("Training Error:", train_error, "\n")


# Creating a Naive Bayes Model on Vet1_Train data USING LAPLACE SMOOTHING
nb_model_vet_lap <- naiveBayes(SUS_FLAG ~ ., data = vet1_train, laplace = 1)
nb_predictions_vet_lap <- predict(nb_model_vet_lap, newdata = vet1_test)

# Confusion matrix
nb_conf_matrix_vet_lap <- table(Predicted = nb_predictions_vet_lap, Actual = vet1_test$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_vet_lap)

# Testing Error
test_accuracy_lap <- sum(diag(nb_conf_matrix_vet_lap)) / sum(nb_conf_matrix_vet_lap)
test_error_lap <- 1 - test_accuracy_lap
cat("Testing Error:", test_error_lap, "\n")

# Calculating Training Error
nb_predictions_vet_train_lap <- predict(nb_model_vet_lap, newdata = vet1_train)
nb_conf_matrix_vet_train_lap <- table(Predicted = nb_predictions_vet_train_lap, Actual = vet1_train$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_vet_train_lap)

train_accuracy_lap <- sum(diag(nb_conf_matrix_vet_train_lap)) / sum(nb_conf_matrix_vet_train_lap)
train_error_lap <- 1 - train_accuracy_lap
cat("Training Error:", train_error_lap, "\n")


#### Naive Bayes Modeling for Vet2_Data

# Training Naive Bayes Model on Vet2_Train data
nb_model_vet2 <- naiveBayes(SUS_FLAG ~ ., data = vet2_train)
nb_predictions_vet2 <- predict(nb_model_vet2, newdata = vet2_test)

# Confusion matrix
nb_conf_matrix_vet2 <- table(Predicted = nb_predictions_vet2, Actual = vet2_test$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_vet2)

# Testing Error
test_accuracy <- sum(diag(nb_conf_matrix_vet2)) / sum(nb_conf_matrix_vet2)
test_error <- 1 - test_accuracy
cat("Testing Error:", test_error, "\n")

# Calculating Training Error
nb_predictions_vet_train2 <- predict(nb_model_vet2, newdata = vet2_train)
nb_conf_matrix_vet_train2 <- table(Predicted = nb_predictions_vet_train2, Actual = vet2_train$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_vet_train2)

train_accuracy <- sum(diag(nb_conf_matrix_vet_train2)) / sum(nb_conf_matrix_vet_train2)
train_error <- 1 - train_accuracy
cat("Training Error:", train_error, "\n")


### Creating a Naive Bayes Model on Vet2_Train data USING LAPLACE SMOOTHING
nb_model_vet_lap2 <- naiveBayes(SUS_FLAG ~ ., data = vet2_train, laplace = 1)
nb_predictions_vet_lap2 <- predict(nb_model_vet_lap2, newdata = vet2_test)

# Confusion matrix
nb_conf_matrix_vet_lap2 <- table(Predicted = nb_predictions_vet_lap2, Actual = vet2_test$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_vet_lap2)

# Testing Error
test_accuracy_lap <- sum(diag(nb_conf_matrix_vet_lap2)) / sum(nb_conf_matrix_vet_lap2)
test_error_lap <- 1 - test_accuracy_lap
cat("Testing Error:", test_error_lap, "\n")

# Calculating Training Error
nb_predictions_vet_train_lap2 <- predict(nb_model_vet_lap2, newdata = vet2_train)
nb_conf_matrix_vet_train_lap2 <- table(Predicted = nb_predictions_vet_train_lap2, Actual = vet2_train$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_vet_train_lap2)

train_accuracy_lap <- sum(diag(nb_conf_matrix_vet_train_lap2)) / sum(nb_conf_matrix_vet_train_lap2)
train_error_lap <- 1 - train_accuracy_lap
cat("Training Error:", train_error_lap, "\n")


#### Naive Bayes Modeling for Vet3_Data

# Training Naive Bayes Model on Vet3_Train data
nb_model_vet3 <- naiveBayes(SUS_FLAG ~ ., data = vet3_train)
nb_predictions_vet3 <- predict(nb_model_vet3, newdata = vet3_test)

# Confusion matrix
nb_conf_matrix_vet3 <- table(Predicted = nb_predictions_vet3, Actual = vet3_test$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_vet3)

# Testing Error
test_accuracy <- sum(diag(nb_conf_matrix_vet3)) / sum(nb_conf_matrix_vet3)
test_error <- 1 - test_accuracy
cat("Testing Error:", test_error, "\n")

# Calculating Training Error
nb_predictions_vet_train3 <- predict(nb_model_vet3, newdata = vet3_train)
nb_conf_matrix_vet_train3 <- table(Predicted = nb_predictions_vet_train3, Actual = vet3_train$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_vet_train3)

train_accuracy <- sum(diag(nb_conf_matrix_vet_train3)) / sum(nb_conf_matrix_vet_train3)
train_error <- 1 - train_accuracy
cat("Training Error:", train_error, "\n")


### Creating a Naive Bayes Model on Vet3_Train data USING LAPLACE SMOOTHING
nb_model_vet_lap3 <- naiveBayes(SUS_FLAG ~ ., data = vet3_train, laplace = 1)
nb_predictions_vet_lap3 <- predict(nb_model_vet_lap3, newdata = vet3_test)

# Confusion matrix
nb_conf_matrix_vet_lap3 <- table(Predicted = nb_predictions_vet_lap3, Actual = vet3_test$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_vet_lap3)

# Testing Error
test_accuracy_lap <- sum(diag(nb_conf_matrix_vet_lap3)) / sum(nb_conf_matrix_vet_lap3)
test_error_lap <- 1 - test_accuracy_lap
cat("Testing Error:", test_error_lap, "\n")

# Calculating Training Error
nb_predictions_vet_train_lap3 <- predict(nb_model_vet_lap3, newdata = vet3_train)
nb_conf_matrix_vet_train_lap3 <- table(Predicted = nb_predictions_vet_train_lap3, Actual = vet3_train$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_vet_train_lap3)

train_accuracy_lap <- sum(diag(nb_conf_matrix_vet_train_lap3)) / sum(nb_conf_matrix_vet_train_lap3)
train_error_lap <- 1 - train_accuracy_lap
cat("Training Error:", train_error_lap, "\n")