# Load the packages
library(gbm)
library(caret)

set.seed(2024)

X_train <- train_generic[,-1]
X_test <- test_generic[,-1]
y_train <- train_generic[,1]
y_test <- test_generic[,1]

#table(y_train, useNA = "ifany")  # Check unique values and NA counts


categorical_var <- c("STATE", "MOST_COMMON_GIFT_CHANNEL_PRO")

# Use dummyVars to create dummy variables for categorical features
dummy_model <- dummyVars(~ ., data = X_train[, categorical_var])

# Convert the categorical variables to dummy variables
X_train_dummy <- predict(dummy_model, newdata = X_train)
X_test_dummy <- predict(dummy_model, newdata = X_test)

# # Convert target variable to factor
# y_train <- as.factor(y_train)
# y_test <- as.factor(y_test)

#sum(is.na(y_train))  # Count missing values in y_train

# Ensure the data is a data.frame
X_train_df <- as.data.frame(X_train_dummy)
X_test_df <- as.data.frame(X_test_dummy)

# # Convert target variable to numeric
# y_train <- as.numeric(as.character(y_train))  # Convert target to numeric
# y_test <- as.numeric(as.character(y_test))    # Same for testing target
# 
# # Combine the predictors and the target variable into a single training data.frame
# train_data <- cbind(X_train_df, y_train = as.numeric(as.character(y_train)) - 1)
# train_data <- cbind(X_train_df, y_train = y_train)

#train_data <- cbind(X_train_df, y_train = y_train)


train_data <- cbind(X_train_df, y_train )
test_data <- cbind(X_test_df, y_test )


# Verify the structure
# str(train_data)
# str(test_data)


# Train the GBM model
gbm_model <- gbm(
  formula = SUS_FLAG ~ .,       # Specify the formula
  distribution = "bernoulli",  # For binary classification
  data = train_data,           # Training data
  n.trees = 100,               # Number of boosting iterations
  interaction.depth = 3,       # Tree depth
  shrinkage = 0.1,             # Learning rate
  cv.folds = 5,                # Cross-validation
  verbose = FALSE
)


