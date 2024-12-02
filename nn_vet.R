set.seed(2024)
library(nnet);
library(e1071);
library(caret)
library(dplyr)

train.df <- vet_train
test.df <- vet_test
train.df$SUS_FLAG <- as.factor(train.df$SUS_FLAG)
test.df$SUS_FLAG <- as.factor(test.df$SUS_FLAG)
true.y.test <- test.df$SUS_FLAG
train.df <- train.df %>%
  mutate(
    MOST_COMMON_GIFT_CHANNEL_PRO = ifelse(is.na(MOST_COMMON_GIFT_CHANNEL_PRO),"unknown", MOST_COMMON_GIFT_CHANNEL_PRO)
  )
test.df <- test.df %>%
  mutate(
    MOST_COMMON_GIFT_CHANNEL_PRO = ifelse(is.na(MOST_COMMON_GIFT_CHANNEL_PRO),"unknown", MOST_COMMON_GIFT_CHANNEL_PRO)
  )

##neural network with 2 hidden layers
fit.nnet1 <- nnet(SUS_FLAG ~ ., data= train.df,  size =2);

# Predict probabilities for training data
train_prob1 <- predict(fit.nnet1, train.df, type = "raw")

# Predict class labels for training data
train_preds1 <- ifelse(train_prob1 > 0.5, 1, 0)
train_preds1 <- as.factor(train_preds1)

# Calculate training error
training_error1 <- mean(train_preds1 != train.df$SUS_FLAG)
cat("Training Error:", training_error1, "\n") ##0.04880668

# Predict probabilities for testing data
test_prob1 <- predict(fit.nnet1, test.df[,-1], type = "raw")

# Predict class labels for testing data
test_preds1<- ifelse(test_prob1 > 0.5, 1, 0)
test_preds1 <- as.factor(test_preds1)

# Calculate testing error
testing_error1 <- mean(test_preds1 != test.df$SUS_FLAG)
cat("Testing Error:", testing_error1, "\n") #0.05194064

# Compute confusion matrix for testing data
conf_matrix1 <- confusionMatrix(test_preds1, test.df$SUS_FLAG)

# Extract confusion matrix metrics (Testing Set)
test1_TP <- conf_matrix1$table[2,2]/length(true.y.test)  # True Positives
test1_FP <- conf_matrix1$table[1,2]/length(true.y.test)  # False Positives
test1_TN <- conf_matrix1$table[1,1]/length(true.y.test)  # True Negatives
test1_FN <- conf_matrix1$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test1_TP, "FP:", test1_FP, "TN:", test1_TN, "FN:", test1_FN, "\n")
#Testing Set - TP: 0.258317 FP: 0.04435747 TN: 0.6897423 FN: 0.00758317 

##5 hidden layers and 0.1 decay
fit.nnet2 <- nnet(SUS_FLAG ~ ., data= train.df,  size =5, decay= 0.1);

# Predict probabilities for training data
train_prob2 <- predict(fit.nnet2, train.df, type = "raw")

# Predict class labels for training data
train_preds2 <- ifelse(train_prob2 > 0.5, 1, 0)
train_preds2 <- as.factor(train_preds2)

# Calculate training error
training_error2 <- mean(train_preds2 != train.df$SUS_FLAG)
cat("Training Error:", training_error2, "\n") ##0.04374935

# Predict probabilities for testing data
test_prob2 <- predict(fit.nnet2, test.df[,-1], type = "raw")

# Predict class labels for testing data
test_preds2<- ifelse(test_prob2 > 0.5, 1, 0)
test_preds2 <- as.factor(test_preds2)

# Calculate testing error
testing_error2 <- mean(test_preds2 != test.df$SUS_FLAG)
cat("Testing Error:", testing_error2, "\n") #0.0460698

# Compute confusion matrix for testing data
conf_matrix2 <- confusionMatrix(test_preds2, test.df$SUS_FLAG)

# Extract confusion matrix metrics (Testing Set)
test2_TP <- conf_matrix2$table[2,2]/length(true.y.test)  # True Positives
test2_FP <- conf_matrix2$table[1,2]/length(true.y.test)  # False Positives
test2_TN <- conf_matrix2$table[1,1]/length(true.y.test)  # True Negatives
test2_FN <- conf_matrix2$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test2_TP, "FP:", test2_FP, "TN:", test2_TN, "FN:", test2_FN, "\n")
#Testing Set - TP: 0.271771 FP: 0.03090346 TN: 0.6821592 FN: 0.01516634 

##10 hidden layers and 0.1 decay and 200 maxit
fit.nnet3 <- nnet(SUS_FLAG ~ ., data= train.df,  size =10, decay= 0.1, maxit = 200);

# Predict probabilities for training data
train_prob3 <- predict(fit.nnet3, train.df, type = "raw")

# Predict class labels for training data
train_preds3 <- ifelse(train_prob3 > 0.5, 1, 0)
train_preds3 <- as.factor(train_preds3)

# Calculate training error
training_error3 <- mean(train_preds3 != train.df$SUS_FLAG)
cat("Training Error:", training_error3, "\n") ##0.03709862

# Predict probabilities for testing data
test_prob3 <- predict(fit.nnet3, test.df[,-1], type = "raw")

# Predict class labels for testing data
test_preds3<- ifelse(test_prob3 > 0.5, 1, 0)
test_preds3 <- as.factor(test_preds3)

# Calculate testing error
testing_error3 <- mean(test_preds3 != test.df$SUS_FLAG)
cat("Testing Error:", testing_error3, "\n") #0.03938356

# Compute confusion matrix for testing data
conf_matrix3 <- confusionMatrix(test_preds3, test.df$SUS_FLAG)

# Extract confusion matrix metrics (Testing Set)
test3_TP <- conf_matrix3$table[2,2]/length(true.y.test)  # True Positives
test3_FP <- conf_matrix3$table[1,2]/length(true.y.test)  # False Positives
test3_TN <- conf_matrix3$table[1,1]/length(true.y.test)  # True Negatives
test3_FN <- conf_matrix3$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test3_TP, "FP:", test3_FP, "TN:", test3_TN, "FN:", test3_FN, "\n")
#Testing Set - TP: 0.2738095 FP: 0.02886497 TN: 0.6868069 FN: 0.01051859

##remove outliers and use size=10, decay = 0.1, maxit=500
train.df.nn4 <- train.df[train.df$outlier_flag == 0, ]
train.df.nn4 <- train.df.nn4[,-23]
fit.nnet4 <- nnet(SUS_FLAG ~ ., data= train.df.nn4,  size =10, decay= 0.1, maxit = 500);

# Predict probabilities for training data
train_prob4 <- predict(fit.nnet4, train.df.nn4, type = "raw")

# Predict class labels for training data
train_preds4 <- ifelse(train_prob4 > 0.5, 1, 0)
train_preds4 <- as.factor(train_preds4)

# Calculate training error
training_error4 <- mean(train_preds4 != train.df.nn4$SUS_FLAG)
cat("Training Error:", training_error4, "\n") ##0.02239399

# Predict probabilities for testing data
test_prob4 <- predict(fit.nnet4, test.df[,-1], type = "raw")

# Predict class labels for testing data
test_preds4<- ifelse(test_prob4 > 0.5, 1, 0)
test_preds4 <- as.factor(test_preds4)

# Calculate testing error
testing_error4 <- mean(test_preds4 != test.df$SUS_FLAG)
cat("Testing Error:", testing_error4, "\n") #0.03302348

# Compute confusion matrix for testing data
conf_matrix4 <- confusionMatrix(test_preds4, test.df$SUS_FLAG)

# Extract confusion matrix metrics (Testing Set)
test4_TP <- conf_matrix4$table[2,2]/length(true.y.test)  # True Positives
test4_FP <- conf_matrix4$table[1,2]/length(true.y.test)  # False Positives
test4_TN <- conf_matrix4$table[1,1]/length(true.y.test)  # True Negatives
test4_FN <- conf_matrix4$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test4_TP, "FP:", test4_FP, "TN:", test4_TN, "FN:", test4_FN, "\n")
#Testing Set - TP: 0.279925 FP: 0.02274951 TN: 0.6870515 FN: 0.01027397
