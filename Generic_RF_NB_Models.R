# Importing Data
Generic_no_out_train <- read.table(file = "/Users/gmarrero/Desktop/MKT/Reduced_Data_Split/generic_data_no_out_reduced_flagged_test.csv", sep=",", header = TRUE);
Generic_no_out_test <- read.table(file = "/Users/gmarrero/Desktop/MKT/Reduced_Data_Split/generic_data_no_out_reduced_flagged_train.csv", sep=",", header = TRUE);

Generic_out_train <- read.table(file = "/Users/gmarrero/Desktop/MKT/Reduced_Data_Split/generic_data_out_reduced_flagged_train.csv", sep=",", header = TRUE);
Generic_out_test <- read.table(file = "/Users/gmarrero/Desktop/MKT/Reduced_Data_Split/generic_data_out_reduced_flagged_test.csv", sep=",", header = TRUE);

# Setting Seed
set.seed(2024)

colSums(is.na(Generic_no_out_train))

#unique(Generic_no_out_train$MOST_COMMON_GIFT_CHANNEL_PRO)
#unique(Generic_no_out_train$MEMBER)

# Load dplyr package
library(dplyr)

### Preparing Generic_no_out_train data - Our Generic Data without Outliers

# Drop the column "X"
Generic_no_out_train <- Generic_no_out_train %>% select(-X)

# Drop the column "ACCOUNTID"
Generic_no_out_train <- Generic_no_out_train %>% select(-ACCOUNTID)

# Drop the column "outlier_flag"
Generic_no_out_train <- Generic_no_out_train %>% select(-outlier_flag)

# Replace NA with 0
Generic_no_out_train$MEMBER[is.na(Generic_no_out_train$MEMBER)] <- 0

# Replace "Y" with 1
Generic_no_out_train$MEMBER[Generic_no_out_train$MEMBER == "Y"] <- 1

# Unknown values for any NA
Generic_no_out_train$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(Generic_no_out_train$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"

# Creating a binary response for sustainer (1 = Sustainer, 0 = Not)
Generic_no_out_train$SUS_FLAG[Generic_no_out_train$SUS_FLAG == "Yes"] <- 1
Generic_no_out_train$SUS_FLAG[Generic_no_out_train$SUS_FLAG == "No"] <- 0

# SUS_FLAG as Factor
Generic_no_out_train$SUS_FLAG <- as.factor(Generic_no_out_train$SUS_FLAG)


##### Preparing Generic_no_out_test data - Our Generic Data without Outliers

# Drop the column "X"
Generic_no_out_test <- Generic_no_out_test %>% select(-X)

# Drop the column "ACCOUNTID"
Generic_no_out_test <- Generic_no_out_test %>% select(-ACCOUNTID)

# Drop the column "outlier_flag"
Generic_no_out_test <- Generic_no_out_test %>% select(-outlier_flag)

# Replace NA with 0
Generic_no_out_test$MEMBER[is.na(Generic_no_out_test$MEMBER)] <- 0

# Replace "Y" with 1
Generic_no_out_test$MEMBER[Generic_no_out_test$MEMBER == "Y"] <- 1

# Unknown values for any NA
Generic_no_out_test$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(Generic_no_out_test$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"

# Creating a binary response for sustainer (1 = Sustainer, 0 = Not)
Generic_no_out_test$SUS_FLAG[Generic_no_out_test$SUS_FLAG == "Yes"] <- 1
Generic_no_out_test$SUS_FLAG[Generic_no_out_test$SUS_FLAG == "No"] <- 0

# SUS_FLAG as Factor
Generic_no_out_test$SUS_FLAG <- as.factor(Generic_no_out_test$SUS_FLAG)


#######################################################

##### Preparing Generic_out_test data - Our Generic Data with Outliers

# Drop the column "X"
Generic_out_test <- Generic_out_test %>% select(-X)

# Drop the column "ACCOUNTID"
Generic_out_test <- Generic_out_test %>% select(-ACCOUNTID)

# Replace NA with 0
Generic_out_test$MEMBER[is.na(Generic_out_test$MEMBER)] <- 0

# Replace "Y" with 1
Generic_out_test$MEMBER[Generic_out_test$MEMBER == "Y"] <- 1

# Unknown values for any NA
Generic_out_test$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(Generic_out_test$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"

# Creating a binary response for sustainer (1 = Sustainer, 0 = Not)
Generic_out_test$SUS_FLAG[Generic_out_test$SUS_FLAG == "Yes"] <- 1
Generic_out_test$SUS_FLAG[Generic_out_test$SUS_FLAG == "No"] <- 0

Generic_out_test$SUS_FLAG <- as.factor(Generic_out_test$SUS_FLAG)

colSums(is.na(Generic_out_test))

Generic_out_test$STATE[is.na(Generic_out_test$STATE)] <- "Unknown"
Generic_out_test$TOP_5_COUNT_FLAG[is.na(Generic_out_test$TOP_5_COUNT_FLAG)] <- "Unknown"
Generic_out_test$TOP_5_FLAG[is.na(Generic_out_test$TOP_5_FLAG)] <- "Unknown"

unique(Generic_out_test$TOP_5_COUNT_FLAG)
unique(Generic_out_test$MEMBER)

##### Preparing Generic_out_train data - Our Generic Data with Outliers

# Drop the column "X"
Generic_out_train <- Generic_out_train %>% select(-X)

# Drop the column "ACCOUNTID"
Generic_out_train <- Generic_out_train %>% select(-ACCOUNTID)

# Replace NA with 0
Generic_out_train$MEMBER[is.na(Generic_out_train$MEMBER)] <- 0

# Replace "Y" with 1
Generic_out_train$MEMBER[Generic_out_train$MEMBER == "Y"] <- 1

Generic_out_train$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(Generic_out_train$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"
Generic_out_train$STATE[is.na(Generic_out_train$STATE)] <- "Unknown"
Generic_out_train$TOP_5_COUNT_FLAG[is.na(Generic_out_train$TOP_5_COUNT_FLAG)] <- "Unknown"
Generic_out_train$TOP_5_FLAG[is.na(Generic_out_train$TOP_5_FLAG)] <- "Unknown"

# Creating a binary response for sustainer (1 = Sustainer, 0 = Not)
Generic_out_train$SUS_FLAG[Generic_out_train$SUS_FLAG == "Yes"] <- 1
Generic_out_train$SUS_FLAG[Generic_out_train$SUS_FLAG == "No"] <- 0

# SUS_FLAG as Factor
Generic_out_train$SUS_FLAG <- as.factor(Generic_out_train$SUS_FLAG)

# Fit random forest model - Rf_model is our Random Forest model made with our No Outlier data
library(randomForest)
rf_model <- randomForest(SUS_FLAG ~ ., data = Generic_no_out_train, importance = TRUE)
importance(rf_model)
#varImpPlot(rf_model)

# Make predictions on the test set
rf_predictions_no_out <- predict(rf_model, newdata = Generic_no_out_test)

rf_predictions_no_out
table(rf_predictions_no_out)
table(Generic_no_out_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions_no_out <- as.factor(rf_predictions_no_out)
actual_values <- as.factor(Generic_no_out_test$SUS_FLAG)

# Create a confusion matrix - No outlier train data & no outlier test data
conf_matrix <- table(Predicted = rf_predictions_no_out, Actual = actual_values)

# Print the confusion matrix
print(conf_matrix)


### Calculating Training Error on rf_model - Built on No Outlier Data

# Predictions on the training data
train_predictions <- predict(rf_model, newdata = Generic_no_out_train)

# Confusion matrix for training data
train_conf_matrix <- table(Predicted = train_predictions, Actual = Generic_no_out_train$SUS_FLAG)
print(train_conf_matrix)

# Calculate Training Accuracy
train_accuracy <- sum(diag(train_conf_matrix)) / sum(train_conf_matrix)

# Calculate Training Error
train_error <- 1 - train_accuracy

# Print Training Error
cat("Training Error:", train_error, "\n")


### Calculating Testing Error on No Outliers Test & No Outliers Train
test_predictions <- predict(rf_model, newdata = Generic_no_out_test)
test_predictions <- as.factor(test_predictions)
actual_values_test <- as.factor(Generic_no_out_test$SUS_FLAG)
test_conf_matrix <- table(Predicted = test_predictions, Actual = actual_values_test)

test_accuracy <- sum(diag(test_conf_matrix)) / sum(test_conf_matrix)
test_error <- 1 - test_accuracy
cat("Testing Error:", test_error, "\n")


####### REPEATING THE SAME AS ABOVE BUT FOR DATA WITH OUTLIERS - Model name "rf_model_out"

# Fit random forest model
library(randomForest)
rf_model_out <- randomForest(SUS_FLAG ~ ., data = Generic_out_train, importance = TRUE)
importance(rf_model_out)
#varImpPlot(rf_model_out)

# Make predictions on the test set - With Outlier Data
rf_predictions_out <- predict(rf_model_out, newdata = Generic_out_test)

rf_predictions_out
table(rf_predictions_out)
table(Generic_out_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions_out <- as.factor(rf_predictions_out)
actual_values_out <- as.factor(Generic_out_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix_out <- table(Predicted = rf_predictions_out, Actual = actual_values_out)

# Print the confusion matrix
print(conf_matrix_out)

### Calculating Training Error on With Outliers Model 

# Predictions on the training data
train_predictions_out <- predict(rf_model_out, newdata = Generic_out_train)

# Confusion matrix for training data
train_conf_matrix_out <- table(Predicted = train_predictions_out, Actual = Generic_out_train$SUS_FLAG)

# Calculate Training Accuracy
train_accuracy_out <- sum(diag(train_conf_matrix_out)) / sum(train_conf_matrix_out)

# Calculate Training Error
train_error_out <- 1 - train_accuracy_out

# Print Training Error
cat("Training Error:", train_error_out, "\n")



### Calculating Testing Error on With Outliers Train vs. With Outliers Test
test_predictions_out <- predict(rf_model_out, newdata = Generic_out_test)
test_predictions_out <- as.factor(test_predictions_out)
actual_values_test_out <- as.factor(Generic_out_test$SUS_FLAG)
test_conf_matrix_out <- table(Predicted = test_predictions_out, Actual = actual_values_test_out)

test_accuracy_out <- sum(diag(test_conf_matrix_out)) / sum(test_conf_matrix_out)
test_error_out <- 1 - test_accuracy_out
cat("Testing Error:", test_error_out, "\n")




###### Using Model from "No Outliers" to predict values for "With Outliers"

#rf_model

# Make predictions on the test set
rf_predictions_1 <- predict(rf_model, newdata = Generic_out_test)

rf_predictions_1
table(rf_predictions_1)
# table(Generic_out_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions_1 <- as.factor(rf_predictions_1)
actual_values <- as.factor(Generic_out_test$SUS_FLAG)

# Create a confusion matrix
conf_matrix <- table(Predicted = rf_predictions_1, Actual = actual_values)

# Print the confusion matrix
print(conf_matrix)


###### Using Model from "Outliers" to predict values for "Without Outliers"

# No_Out must be prepared while maintaining the values of Outlier_Flag in order to make predictions. 

Generic_no_out_test123 <- read.table(file = "/Users/gmarrero/Desktop/MKT/Reduced_Data_Split/generic_data_no_out_reduced_flagged_train.csv", sep=",", header = TRUE);

# Drop the column "X"
Generic_no_out_test123 <- Generic_no_out_test123 %>% select(-X)

# Drop the column "ACCOUNTID"
Generic_no_out_test123 <- Generic_no_out_test123 %>% select(-ACCOUNTID)

# Replace NA with 0
Generic_no_out_test123$MEMBER[is.na(Generic_no_out_test123$MEMBER)] <- 0

# Replace "Y" with 1
Generic_no_out_test123$MEMBER[Generic_no_out_test123$MEMBER == "Y"] <- 1

# Replacing NA with Unknown
Generic_no_out_test123$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(Generic_no_out_test123$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"

# Creating a binary response for sustainer (1 = Sustainer, 0 = Not)
Generic_no_out_test123$SUS_FLAG[Generic_no_out_test123$SUS_FLAG == "Yes"] <- 1
Generic_no_out_test123$SUS_FLAG[Generic_no_out_test123$SUS_FLAG == "No"] <- 0

Generic_no_out_test123$SUS_FLAG <- as.factor(Generic_no_out_test123$SUS_FLAG)


# Make predictions on the test set
rf_predictions_2 <- predict(rf_model_out, newdata = Generic_no_out_test123)

rf_predictions_2
table(rf_predictions_2)
# table(Generic_out_test$SUS_FLAG)

# Ensure predictions and actual values are factors with matching levels
rf_predictions_2 <- as.factor(rf_predictions_2)
actual_values <- as.factor(Generic_no_out_test123$SUS_FLAG)

# Create a confusion matrix
conf_matrix <- table(Predicted = rf_predictions_2, Actual = actual_values)

# Print the confusion matrix
print(conf_matrix)


### Naive Bayes Modeling on No Outliers Data

library(e1071)

nb_model_no_out <- naiveBayes(SUS_FLAG ~ ., data = Generic_no_out_train)
nb_predictions_no_out <- predict(nb_model_no_out, newdata = Generic_no_out_test)

# Confusion matrix
nb_conf_matrix_no_out <- table(Predicted = nb_predictions_no_out, Actual = Generic_no_out_test$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_no_out)

# Calculating Training Error
nb_predictions_no_out_train_err <- predict(nb_model_no_out, newdata = Generic_no_out_train)
nb_conf_matrix_no_out_train_err <- table(Predicted = nb_predictions_no_out_train_err, Actual = Generic_no_out_train$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_no_out_train_err)

test_accuracy_out <- sum(diag(nb_conf_matrix_no_out_train_err)) / sum(nb_conf_matrix_no_out_train_err)
test_error_out <- 1 - test_accuracy_out
cat("Testing Error:", test_error_out, "\n")

### Changing Parameters on Naive Bayes Modeling to include Laplace Smoothing
nb_model_no_out1 <- naiveBayes(SUS_FLAG ~ ., data = Generic_no_out_train, laplace = 1)
nb_predictions_no_out1 <- predict(nb_model_no_out1, newdata = Generic_no_out_test)

# Confusion matrix
nb_conf_matrix_no_out1 <- table(Predicted = nb_predictions_no_out1, Actual = Generic_no_out_test$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_no_out1)

# Calculating Training Error
nb_predictions_no_out1_train_err <- predict(nb_model_no_out1, newdata = Generic_no_out_train)
nb_conf_matrix_no_out1_train_err <- table(Predicted = nb_predictions_no_out1_train_err, Actual = Generic_no_out_train$SUS_FLAG)

# Print the confusion matrix
print(nb_conf_matrix_no_out1_train_err)

test_accuracy_out <- sum(diag(nb_conf_matrix_no_out1_train_err)) / sum(nb_conf_matrix_no_out1_train_err)
test_error_out <- 1 - test_accuracy_out
cat("Testing Error:", test_error_out, "\n")

