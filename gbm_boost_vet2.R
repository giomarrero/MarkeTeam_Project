library(gbm)
library(dplyr)
set.seed(2024)
df.train <- train_vet
df.train$STATE <- as.factor(df.train$STATE)
df.test <- test_vet
df.test$STATE <- as.factor(df.test$STATE)
true.y.train <- df.train$SUS_FLAG
true.y.test <- df.test$SUS_FLAG
df.test <- df.test[,-1]
df.train <- df.train %>%
  mutate(
    MOST_COMMON_GIFT_CHANNEL_PRO = ifelse(is.na(MOST_COMMON_GIFT_CHANNEL_PRO),"unknown", MOST_COMMON_GIFT_CHANNEL_PRO)
  )
df.test <- df.test %>%
  mutate(
    MOST_COMMON_GIFT_CHANNEL_PRO = ifelse(is.na(MOST_COMMON_GIFT_CHANNEL_PRO),"unknown", MOST_COMMON_GIFT_CHANNEL_PRO)
  )
df.train$MOST_COMMON_GIFT_CHANNEL_PRO <- as.factor(df.train$MOST_COMMON_GIFT_CHANNEL_PRO)
df.test$MOST_COMMON_GIFT_CHANNEL_PRO <- as.factor(df.test$MOST_COMMON_GIFT_CHANNEL_PRO)

#gbm with 100 trees
gbm.generic1 <- gbm(SUS_FLAG ~ .,data=df.train,
                 distribution = 'bernoulli',
                 n.trees = 100, 
                 shrinkage = 0.01, 
                 interaction.depth = 3,
                 cv.folds = 5)
## Model Inspection 
## Find the estimated optimal number of iterations
perf_gbm1 = gbm.perf(gbm.generic1, method="cv") 
perf_gbm1


## summary model
## Which variances are important
summary(gbm.generic1)
##variables with high rel.inf : RESPONSE_RATE, AVG_DAYS_ACROSS_APPEALCODES, MOST_COMMON_GIFT_CHANNEL_PRO, DONATION_COUNT,AGE.
#AGE_NA, AGE_70PLUS, ONE_TIME_FLAG

## Training error
pred1gbm.train <- predict(gbm.generic1,newdata = df.train[,-1], n.trees=perf_gbm1, type="response")
y1hat.train <- ifelse(pred1gbm.train < 0.5, 0, 1)
sum(y1hat.train != true.y.train)/length(true.y.train)  ##Training error =  0.1465008

## Testing Error
y1hat.test <- ifelse(predict(gbm.generic1,newdata = df.test, n.trees=perf_gbm1, type="response") < 0.5, 0, 1)
mean(y1hat.test != true.y.test) 
## Testing error = 0.1459078
conf.matrix1 <- confusionMatrix(as.factor(y1hat.test), as.factor(true.y.test) )
# Extract confusion matrix metrics (Testing Set)
test1_TP <- conf.matrix1$table[2,2]/length(true.y.test)  # True Positives
test1_FP <- conf.matrix1$table[1,2]/length(true.y.test)  # False Positives
test1_TN <- conf.matrix1$table[1,1]/length(true.y.test)  # True Negatives
test1_FN <- conf.matrix1$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test1_TP, "FP:", test1_FP, "TN:", test1_TN, "FN:", test1_FN, "\n")
#Testing Set - TP: 0.4152493 FP: 0.0848701 TN: 0.4388429 FN: 0.06103773

