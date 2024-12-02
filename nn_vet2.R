set.seed(2024)
library(nnet);
library(e1071);
library(caret)
library(dplyr)

train.df <- vet2_train
test.df <- vet2_test
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
fit.nnet1 <- nnet(SUS_FLAG ~ ., data= train.df,  size =10, decay=0.1, maxit = 500);

# Predict probabilities for training data
train_prob1 <- predict(fit.nnet1, train.df, type = "raw")

# Predict class labels for training data
train_preds1 <- ifelse(train_prob1 > 0.5, 1, 0)
train_preds1 <- as.factor(train_preds1)

# Calculate training error
training_error1 <- mean(train_preds1 != train.df$SUS_FLAG)
cat("Training Error:", training_error1, "\n") ##0.1125032

# Predict probabilities for testing data
test_prob1 <- predict(fit.nnet1, test.df[,-1], type = "raw")

# Predict class labels for testing data
test_preds1<- ifelse(test_prob1 > 0.5, 1, 0)
test_preds1 <- as.factor(test_preds1)

# Calculate testing error
testing_error1 <- mean(test_preds1 != test.df$SUS_FLAG)
cat("Testing Error:", testing_error1, "\n") #0.1122225

# Compute confusion matrix for testing data
conf_matrix1 <- confusionMatrix(test_preds1, test.df$SUS_FLAG)

# Extract confusion matrix metrics (Testing Set)
test1_TP <- conf_matrix1$table[2,2]/length(true.y.test)  # True Positives
test1_FP <- conf_matrix1$table[1,2]/length(true.y.test)  # False Positives
test1_TN <- conf_matrix1$table[1,1]/length(true.y.test)  # True Negatives
test1_FN <- conf_matrix1$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test1_TP, "FP:", test1_FP, "TN:", test1_TN, "FN:", test1_FN, "\n")
#Testing Set - TP: 0.4362034 FP: 0.06391606 TN: 0.4515741 FN: 0.04830644 

