library(gbm)
library(dplyr)
set.seed(2024)
df.train <- vet_train
df.train$STATE <- as.factor(df.train$STATE)
df.test <- vet_test
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

#gbm with 500 trees
gbm.generic2 <- gbm(SUS_FLAG ~ .,data=df.train,
                    distribution = 'bernoulli',
                    n.trees = 500, 
                    shrinkage = 0.01, 
                    interaction.depth = 3,
                    cv.folds = 5)
perf_gbm2 = gbm.perf(gbm.generic2, method="cv") 
perf_gbm2

#gbm with 1000 trees
gbm.generic3 <- gbm(SUS_FLAG ~ .,data=df.train,
                    distribution = 'bernoulli',
                    n.trees = 1000, 
                    shrinkage = 0.01, 
                    interaction.depth = 3,
                    cv.folds = 5)
perf_gbm3 = gbm.perf(gbm.generic3, method="cv") 
perf_gbm3

#gbm with 5000 trees
gbm.generic4 <- gbm(SUS_FLAG ~ .,data=df.train,
                    distribution = 'bernoulli',
                    n.trees = 5000, 
                    shrinkage = 0.01, 
                    interaction.depth = 3,
                    cv.folds = 5)
perf_gbm4 = gbm.perf(gbm.generic4, method="cv") 
perf_gbm4
## summary model
## Which variances are important
summary(gbm.generic1)
##variables with high rel.inf : AVG_DAYS_ACROSS_APPEALCODES, AGE_NA, RESPONSE_RATE, AMOUNT_GIFTED_LAST_YEAR,
#DONATION_COUNT, TOTAL_AMOUNT_GIFTED_PRO, DISTINCT_APPEAL_COUNT, MAX_GIFT

summary(gbm.generic2)
##variables with high rel.inf : AVG_DAYS_ACROSS_APPEALCODES, AGE_NA,TOTAL_AMOUNT_GIFTED_PRO, RESPONSE_RATE,
#AMOUNT_GIFTED_LAST_YEAR, DISTINCT_APPEAL_COUNT, DONATION_COUNT, MAX_GIFT, AGE, STATE, AVG_GIFT, TOTAL_GIFT

summary(gbm.generic3)
##variables with high rel.inf : AVG_DAYS_ACROSS_APPEALCODES, AGE_NA, RESPONSE_RATE, TOTAL_AMOUNT_GIFTED_PRO, 
#DISTINCT_APPEAL_COUNT, AMOUNT_GIFTED_LAST_YEAR, DONATION_COUNT, MAX_GIFT, STATE, AGE, AVG_GIFT, TOTAL_GIFT,
#AGE_60_69, MALE, outlier_flag

summary(gbm.generic4)
##variables with high rel.inf : AVG_DAYS_ACROSS_APPEALCODES, AGE_NA,TOTAL_AMOUNT_GIFTED_PRO, RESPONSE_RATE, 
#STATE, AMOUNT_GIFTED_LAST_YEAR, DONATION_COUNT, DISTINCT_APPEAL_COUNT, MAX_GIFT, AGE, AVG_GIFT, TOTAL_GIFT, 
#MALE,COUPLE, outlier_flag,FEMALE, AGE_70PLUS, AGE_UNDER50,  AGE_50_59, AGE_60_69 

#gbm1
## Training error
pred1gbm.train <- predict(gbm.generic1,newdata = df.train[,-1], n.trees=perf_gbm1, type="response")
y1hat.train <- ifelse(pred1gbm.train < 0.5, 0, 1)
sum(y1hat.train != true.y.train)/length(true.y.train)  ##Training error =  0.0422945

## Testing Error
y1hat.test <- ifelse(predict(gbm.generic1,newdata = df.test, n.trees=perf_gbm1, type="response") < 0.5, 0, 1)
mean(y1hat.test != true.y.test) 
## Testing error = 0.04288976
conf.matrix1 <- confusionMatrix(as.factor(y1hat.test), as.factor(true.y.test) )
# Extract confusion matrix metrics (Testing Set)
test1_TP <- conf.matrix1$table[2,2]/length(true.y.test)  # True Positives
test1_FP <- conf.matrix1$table[1,2]/length(true.y.test)  # False Positives
test1_TN <- conf.matrix1$table[1,1]/length(true.y.test)  # True Negatives
test1_FN <- conf.matrix1$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test1_TP, "FP:", test1_FP, "TN:", test1_TN, "FN:", test1_FN, "\n")
#Testing Set - TP: 0.2597847 FP: 0.04288976 TN: 0.6973255 FN: 0 

#gbm2
## Training error
pred2gbm.train <- predict(gbm.generic2,newdata = df.train[,-1], n.trees=perf_gbm2, type="response")
y2hat.train <- ifelse(pred2gbm.train < 0.5, 0, 1)
sum(y2hat.train != true.y.train)/length(true.y.train)  ##Training error =  0.03768748

## Testing Error
y2hat.test <- ifelse(predict(gbm.generic2,newdata = df.test, n.trees=perf_gbm2, type="response") < 0.5, 0, 1)
mean(y2hat.test != true.y.test) 
## Testing error = 0.03962818
conf.matrix2 <- confusionMatrix(as.factor(y2hat.test), as.factor(true.y.test) )
# Extract confusion matrix metrics (Testing Set)
test2_TP <- conf.matrix2$table[2,2]/length(true.y.test)  # True Positives
test2_FP <- conf.matrix2$table[1,2]/length(true.y.test)  # False Positives
test2_TN <- conf.matrix2$table[1,1]/length(true.y.test)  # True Negatives
test2_FN <- conf.matrix2$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test2_TP, "FP:", test2_FP, "TN:", test2_TN, "FN:", test2_FN, "\n")
#Testing Set - TP: 0.2647586 FP: 0.03791585 TN: 0.6956132 FN: 0.001712329 

#gbm3
## Training error
pred3gbm.train <- predict(gbm.generic3,newdata = df.train[,-1], n.trees=perf_gbm3, type="response")
y3hat.train <- ifelse(pred3gbm.train < 0.5, 0, 1)
sum(y3hat.train != true.y.train)/length(true.y.train)  ##Training error =  0.03356542

## Testing Error
y3hat.test <- ifelse(predict(gbm.generic3,newdata = df.test, n.trees=perf_gbm3, type="response") < 0.5, 0, 1)
mean(y3hat.test != true.y.test) 
## Testing error = 0.03644814
conf.matrix3 <- confusionMatrix(as.factor(y3hat.test), as.factor(true.y.test) )
# Extract confusion matrix metrics (Testing Set)
test3_TP <- conf.matrix3$table[2,2]/length(true.y.test)  # True Positives
test3_FP <- conf.matrix3$table[1,2]/length(true.y.test)  # False Positives
test3_TN <- conf.matrix3$table[1,1]/length(true.y.test)  # True Negatives
test3_FN <- conf.matrix3$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test3_TP, "FP:", test3_FP, "TN:", test3_TN, "FN:", test3_FN, "\n")
#Testing Set - TP: 0.2699772 FP: 0.03269733 TN: 0.6935747 FN: 0.003750815 

#gbm4
## Training error
pred4gbm.train <- predict(gbm.generic4,newdata = df.train[,-1], n.trees=perf_gbm4, type="response")
y4hat.train <- ifelse(pred4gbm.train < 0.5, 0, 1)
sum(y4hat.train != true.y.train)/length(true.y.train)  ##Training error =  0.02511344

## Testing Error
y4hat.test <- ifelse(predict(gbm.generic4,newdata = df.test, n.trees=perf_gbm4, type="response") < 0.5, 0, 1)
mean(y4hat.test != true.y.test) 
## Testing error = 0.03432811
conf.matrix4 <- confusionMatrix(as.factor(y4hat.test), as.factor(true.y.test) )
# Extract confusion matrix metrics (Testing Set)
test4_TP <- conf.matrix4$table[2,2]/length(true.y.test)  # True Positives
test4_FP <- conf.matrix4$table[1,2]/length(true.y.test)  # False Positives
test4_TN <- conf.matrix4$table[1,1]/length(true.y.test)  # True Negatives
test4_FN <- conf.matrix4$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test4_TP, "FP:", test4_FP, "TN:", test4_TN, "FN:", test4_FN, "\n")
#Testing Set - TP: 0.2743803 FP: 0.02829419 TN: 0.6912916 FN: 0.00603392 

#gbm5 with 5000 trees and influential variables from gbm4
##variables with high rel.inf : AVG_DAYS_ACROSS_APPEALCODES, AGE_NA,TOTAL_AMOUNT_GIFTED_PRO, RESPONSE_RATE, 
#STATE, AMOUNT_GIFTED_LAST_YEAR, DONATION_COUNT, DISTINCT_APPEAL_COUNT, MAX_GIFT, AGE, AVG_GIFT, TOTAL_GIFT, 
#MALE,COUPLE, outlier_flag,FEMALE, AGE_70PLUS, AGE_UNDER50,  AGE_50_59, AGE_60_69 
df.train.gbm5 <- df.train[,-c(13,17,24)]
df.test.gbm5 <- df.test[,-c(12,16,23)]
true.y.train.gbm5 <- df.train.gbm5$SUS_FLAG
true.y.test.gbm5 <- vet_test$SUS_FLAG
gbm.generic5 <- gbm(SUS_FLAG ~ .,
                    data=df.train.gbm5,
                    distribution = 'bernoulli',
                    n.trees = 5000, 
                    shrinkage = 0.01, 
                    interaction.depth = 3,
                    cv.folds = 5)
perf_gbm5 = gbm.perf(gbm.generic5, method="cv") 
perf_gbm5
## summary model
## Which variances are important
summary(gbm.generic5)
##variables with high rel.inf : AVG_DAYS_ACROSS_APPEALCODES, AGE_NA, RESPONSE_RATE, TOTAL_AMOUNT_GIFTED_PRO, 
#STATE, AMOUNT_GIFTED_LAST_YEAR, DONATION_COUNT, DISTINCT_APPEAL_COUNT, MAX_GIFT, AGE, AVG_GIFT, TOTAL_GIFT, 
#MALE, COUPLE, outlier_flag,FEMALE, AGE_UNDER50, AGE_70PLUS, AGE_60_69, AGE_50_59

## Training error
pred5gbm.train <- predict(gbm.generic5,newdata = df.train.gbm5[,-1], n.trees=perf_gbm5, type="response")
y5hat.train <- ifelse(pred5gbm.train < 0.5, 0, 1)
sum(y5hat.train != true.y.train)/length(true.y.train)  ##Training error =  0.025252

## Testing Error
y5hat.test <- ifelse(predict(gbm.generic5,newdata = df.test.gbm5, n.trees=perf_gbm5, type="response") < 0.5, 0, 1)
mean(y5hat.test != true.y.test) 
## Testing error = 0.03432811
conf.matrix5 <- confusionMatrix(as.factor(y5hat.test), as.factor(true.y.test) )
# Extract confusion matrix metrics (Testing Set)
test5_TP <- conf.matrix5$table[2,2]/length(true.y.test)  # True Positives
test5_FP <- conf.matrix5$table[1,2]/length(true.y.test)  # False Positives
test5_TN <- conf.matrix5$table[1,1]/length(true.y.test)  # True Negatives
test5_FN <- conf.matrix5$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test5_TP, "FP:", test5_FP, "TN:", test5_TN, "FN:", test5_FN, "\n")
#Testing Set - TP: 0.2743803 FP: 0.02829419 TN: 0.6912916 FN: 0.00603392 

#gbm7 with 10,000 trees and all variables, outliers excluded and outlier flag
# Subset df.train to keep only rows where outlier = 0
df.train.gbm7 <- df.train[df.train$outlier_flag == 0, ]
df.train.gbm7 <- df.train.gbm7[,-c(23)]
df.test.gbm7 <- vet_test[vet_test$outlier_flag ==0,]
df.test.gbm7$STATE <- as.factor(df.test.gbm7$STATE) 
df.test.gbm7$MOST_COMMON_GIFT_CHANNEL_PRO <- as.factor(df.test.gbm7$MOST_COMMON_GIFT_CHANNEL_PRO)
true.y.test.gbm7 <- df.test.gbm7$SUS_FLAG
df.test.gbm7 <- df.test.gbm7[,-c(1,23)]
true.y.train.gbm7 <- df.train.gbm7$SUS_FLAG

gbm.generic7 <- gbm(SUS_FLAG ~ .,
                    data=df.train.gbm7,
                    distribution = 'bernoulli',
                    n.trees = 10000, 
                    shrinkage = 0.01, 
                    interaction.depth = 3,
                    cv.folds = 5)
perf_gbm7 = gbm.perf(gbm.generic7, method="cv") 
perf_gbm7
## summary model
## Which variances are important
summary(gbm.generic7)
##variables with high rel.inf : AVG_DAYS_ACROSS_APPEALCODES, AGE_NA, STATE, RESPONSE_RATE, TOTAL_AMOUNT_GIFTED_PRO, 
#DONATION_COUNT, AMOUNT_GIFTED_LAST_YEAR, DISTINCT_APPEAL_COUNT, MAX_GIFT, TOTAL_GIFT, AVG_GIFT, AGE,
#MALE, COUPLE, FEMALE, AGE_60_69, AGE_70PLUS,  AGE_50_59, AGE_UNDER50

## Training error
pred7gbm.train <- predict(gbm.generic7,newdata = df.train.gbm7[,-1], n.trees=perf_gbm7, type="response")
y7hat.train <- ifelse(pred7gbm.train < 0.5, 0, 1)
sum(y7hat.train != true.y.train.gbm7)/length(true.y.train.gbm7)  ##Training error =  0.02029907

## Testing Error
y7hat.test <- ifelse(predict(gbm.generic7,newdata = df.test.gbm7, n.trees=perf_gbm7, type="response") < 0.5, 0, 1)
mean(y7hat.test != true.y.test.gbm7) 
## Testing error = 0.0330269
conf.matrix7 <- confusionMatrix(as.factor(y7hat.test), as.factor(true.y.test.gbm7) )
# Extract confusion matrix metrics (Testing Set)
test7_TP <- conf.matrix7$table[2,2]/length(true.y.test.gbm7)  # True Positives
test7_FP <- conf.matrix7$table[1,2]/length(true.y.test.gbm7)  # False Positives
test7_TN <- conf.matrix7$table[1,1]/length(true.y.test.gbm7)  # True Negatives
test7_FN <- conf.matrix7$table[2,1]/length(true.y.test.gbm7)  # False Negatives
cat("Testing Set - TP:", test7_TP, "FP:", test7_FP, "TN:", test7_TN, "FN:", test7_FN, "\n")
#Testing Set - TP: 0.2750255 FP: 0.02621723 TN: 0.6919476 FN: 0.00680967 


