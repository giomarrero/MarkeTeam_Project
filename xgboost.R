install.packages("xgboost")
install.packages("caret")
install.packages("e1071")  # caret dependency
library(xgboost)
library(caret)

# Simulated data for demonstration (replace this with your actual data)
# set.seed(2024)
# data <- twoClassSim(1000)  # Simulated binary classification dataset
# X <- data[, -ncol(data)]   # Features
# y <- as.numeric(data$Class) - 1  # Target variable (convert factor to numeric 0/1)
# 
# # Split the data into training and testing sets
# set.seed(42)
# trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- train_generic[,-1]
X_test <- test_generic[,-1]
y_train <- train_generic[,1]
y_test <- test_generic[,1]

# Convert data to xgboost DMatrix format
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

# Train the XGBoost model
params <- list(
  objective = "binary:logistic",  # For binary classification
  eval_metric = "error"          # Use error rate for evaluation
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,          # Number of boosting rounds
  watchlist = list(train = dtrain, test = dtest),
  print_every_n = 10
)

# Predictions
y_train_pred <- predict(xgb_model, dtrain)
y_test_pred <- predict(xgb_model, dtest)

# Convert probabilities to binary class predictions
y_train_pred_binary <- ifelse(y_train_pred > 0.5, 1, 0)
y_test_pred_binary <- ifelse(y_test_pred > 0.5, 1, 0)

# Calculate training and testing errors
train_error <- mean(y_train_pred_binary != y_train)
test_error <- mean(y_test_pred_binary != y_test)

# Confusion matrix for testing data
conf_matrix <- table(Predicted = y_test_pred_binary, Actual = y_test)
tp <- conf_matrix[2, 2]  # True positives
fp <- conf_matrix[2, 1]  # False positives
tn <- conf_matrix[1, 1]  # True negatives
fn <- conf_matrix[1, 2]  # False negatives

# Output metrics
cat("Training Error:", train_error, "\n")
cat("Testing Error:", test_error, "\n")
cat("True Positives (TP):", tp, "\n")
cat("False Positives (FP):", fp, "\n")
cat("True Negatives (TN):", tn, "\n")
cat("False Negatives (FN):", fn, "\n")
