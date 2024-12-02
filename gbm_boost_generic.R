library(gbm)
set.seed(2024)
df.train <- general_train
df.train$STATE <- as.factor(df.train$STATE)
df.train$MOST_COMMON_GIFT_CHANNEL_PRO <- as.factor(df.train$MOST_COMMON_GIFT_CHANNEL_PRO)
df.test <- general_test
df.test$STATE <- as.factor(df.test$STATE)
df.test$MOST_COMMON_GIFT_CHANNEL_PRO <- as.factor(df.test$MOST_COMMON_GIFT_CHANNEL_PRO)
true.y.train <- df.train$SUS_FLAG
true.y.test <- df.test$SUS_FLAG
df.test <- df.test[,-1]

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
##variables with high rel.inf : AVG_DAYS_ACROSS_APPEALCODES, AGE_NA, MOST_COMMON_GIFT_CHANNEL_PRO,
#DONATION_COUNT, MEMBER

summary(gbm.generic2)
##variables with high rel.inf : AVG_DAYS_ACROSS_APPEALCODES, AGE_NA,TOTAL_AMOUNT_GIFTED_PRO, MOST_COMMON_GIFT_CHANNEL_PRO,
#DONATION_COUNT,DISTINCT_APPEAL_COUNT, MEMBER, TOTAL_GIFT, RESPONSE_RATE, STATE, AVG_GIFT, AMOUNT_GIFTED_LAST_YEAR, MAX_GIFT

summary(gbm.generic3)
##variables with high rel.inf : AVG_DAYS_ACROSS_APPEALCODES, AGE_NA,TOTAL_AMOUNT_GIFTED_PRO, MOST_COMMON_GIFT_CHANNEL_PRO,
#DONATION_COUNT,DISTINCT_APPEAL_COUNT, MEMBER, TOTAL_GIFT,STATE, AMOUNT_GIFTED_LAST_YEAR,  AVG_GIFT,RESPONSE_RATE, MAX_GIFT,
#AGE, AGE_70PLUS, ONE_TIME_FLAG, COUPLE

summary(gbm.generic4)
##variables with high rel.inf : AVG_DAYS_ACROSS_APPEALCODES, AGE_NA,TOTAL_AMOUNT_GIFTED_PRO, MOST_COMMON_GIFT_CHANNEL_PRO,
#DONATION_COUNT,STATE,DISTINCT_APPEAL_COUNT, MEMBER, TOTAL_GIFT, AMOUNT_GIFTED_LAST_YEAR,  AVG_GIFT,RESPONSE_RATE, MAX_GIFT,
#AGE,COUPLE, AGE_70PLUS, ONE_TIME_FLAG, MALE, outlier_flag, AGE_60_69, FEMALE, AGE_UNDER50, AGE_50_59

#gbm1
## Training error
pred1gbm.train <- predict(gbm.generic1,newdata = df.train[,-1], n.trees=perf_gbm1, type="response")
pred1gbm.train[1:10]
y1hat.train <- ifelse(pred1gbm.train < 0.5, 0, 1)
y1hat.train[1:10]
sum(y1hat.train != true.y.train)/length(true.y.train)  ##Training error =  0.01898171

## Testing Error
y1hat.test <- ifelse(predict(gbm.generic1,newdata = df.test, n.trees=perf_gbm1, type="response") < 0.5, 0, 1)
mean(y1hat.test != true.y.test) 
## Testing error = 0.01897131
conf.matrix1 <- confusionMatrix(as.factor(y1hat.test), as.factor(true.y.test) )
# Extract confusion matrix metrics (Testing Set)
test1_TP <- conf.matrix1$table[2,2]/length(true.y.test)  # True Positives
test1_FP <- conf.matrix1$table[1,2]/length(true.y.test)  # False Positives
test1_TN <- conf.matrix1$table[1,1]/length(true.y.test)  # True Negatives
test1_FN <- conf.matrix1$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test1_TP, "FP:", test1_FP, "TN:", test1_TN, "FN:", test1_FN, "\n")

#gbm2
## Training error
pred2gbm.train <- predict(gbm.generic2,newdata = df.train[,-1], n.trees=perf_gbm2, type="response")
y2hat.train <- ifelse(pred2gbm.train < 0.5, 0, 1)
sum(y2hat.train != true.y.train)/length(true.y.train)  ##Training error =  0.01856792

## Testing Error
y2hat.test <- ifelse(predict(gbm.generic2,newdata = df.test, n.trees=perf_gbm2, type="response") < 0.5, 0, 1)
mean(y2hat.test != true.y.test) 
## Testing error = 0.01827095
conf.matrix2 <- confusionMatrix(as.factor(y2hat.test), as.factor(true.y.test) )
# Extract confusion matrix metrics (Testing Set)
test2_TP <- conf.matrix2$table[2,2]/length(true.y.test)  # True Positives
test2_FP <- conf.matrix2$table[1,2]/length(true.y.test)  # False Positives
test2_TN <- conf.matrix2$table[1,1]/length(true.y.test)  # True Negatives
test2_FN <- conf.matrix2$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test2_TP, "FP:", test2_FP, "TN:", test2_TN, "FN:", test2_FN, "\n")

#gbm3
## Training error
pred3gbm.train <- predict(gbm.generic3,newdata = df.train[,-1], n.trees=perf_gbm3, type="response")
y3hat.train <- ifelse(pred3gbm.train < 0.5, 0, 1)
sum(y3hat.train != true.y.train)/length(true.y.train)  ##Training error =  0.01785478

## Testing Error
y3hat.test <- ifelse(predict(gbm.generic3,newdata = df.test, n.trees=perf_gbm3, type="response") < 0.5, 0, 1)
mean(y3hat.test != true.y.test) 
## Testing error = 0.01827095
conf.matrix3 <- confusionMatrix(as.factor(y3hat.test), as.factor(true.y.test) )
# Extract confusion matrix metrics (Testing Set)
test3_TP <- conf.matrix3$table[2,2]/length(true.y.test)  # True Positives
test3_FP <- conf.matrix3$table[1,2]/length(true.y.test)  # False Positives
test3_TN <- conf.matrix3$table[1,1]/length(true.y.test)  # True Negatives
test3_FN <- conf.matrix3$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test3_TP, "FP:", test3_FP, "TN:", test3_TN, "FN:", test3_FN, "\n")

#gbm4
## Training error
pred4gbm.train <- predict(gbm.generic4,newdata = df.train[,-1], n.trees=perf_gbm4, type="response")
y4hat.train <- ifelse(pred4gbm.train < 0.5, 0, 1)
sum(y4hat.train != true.y.train)/length(true.y.train)  ##Training error =  0.01499344

## Testing Error
y4hat.test <- ifelse(predict(gbm.generic4,newdata = df.test, n.trees=perf_gbm4, type="response") < 0.5, 0, 1)
mean(y4hat.test != true.y.test) 
## Testing error = 0.01691145
conf.matrix4 <- confusionMatrix(as.factor(y4hat.test), as.factor(true.y.test) )
# Extract confusion matrix metrics (Testing Set)
test4_TP <- conf.matrix4$table[2,2]/length(true.y.test)  # True Positives
test4_FP <- conf.matrix4$table[1,2]/length(true.y.test)  # False Positives
test4_TN <- conf.matrix4$table[1,1]/length(true.y.test)  # True Negatives
test4_FN <- conf.matrix4$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test4_TP, "FP:", test4_FP, "TN:", test4_TN, "FN:", test4_FN, "\n")

#gbm5 with 5000 trees and influential variables from gbm4
##variables with high rel.inf : AVG_DAYS_ACROSS_APPEALCODES, AGE_NA,TOTAL_AMOUNT_GIFTED_PRO, MOST_COMMON_GIFT_CHANNEL_PRO,
#DONATION_COUNT, STATE, DISTINCT_APPEAL_COUNT, MEMBER, TOTAL_GIFT, AMOUNT_GIFTED_LAST_YEAR,  AVG_GIFT,RESPONSE_RATE, MAX_GIFT,
#AGE, COUPLE, AGE_70PLUS, ONE_TIME_FLAG, MALE, outlier_flag, AGE_60_69, FEMALE, AGE_UNDER50, AGE_50_59
df.train.gbm <- df.train[,-c(9,26)]
df.test.gbm5 <- df.test[,-c(8,25)]
true.y.train.gbm5 <- df.train.gbm5$SUS_FLAG
true.y.test.gbm5 <- general_test$SUS_FLAG
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
##variables with high rel.inf : AVG_DAYS_ACROSS_APPEALCODES, AGE_NA, TOTAL_AMOUNT_GIFTED_PRO, MOST_COMMON_GIFT_CHANNEL_PRO,
#DONATION_COUNT, STATE, DISTINCT_APPEAL_COUNT, MEMBER, TOTAL_GIFT, AMOUNT_GIFTED_LAST_YEAR,  AVG_GIFT, RESPONSE_RATE, MAX_GIFT,
#AGE, COUPLE, AGE_70PLUS, ONE_TIME_FLAG, AGE_60_69, MALE,  FEMALE, AGE_50_59, outlier_flag

##gbm6 with 10,000 trees
gbm.generic6 <- gbm(SUS_FLAG ~ .,
                    data=df.train.gbm5,
                    distribution = 'bernoulli',
                    n.trees = 10000, 
                    shrinkage = 0.01, 
                    interaction.depth = 3,
                    cv.folds = 5)
perf_gbm6 = gbm.perf(gbm.generic6, method="cv") 
perf_gbm6
## summary model
## Which variances are important
summary(gbm.generic6)
##variables with high rel.inf : AVG_DAYS_ACROSS_APPEALCODES, AGE_NA, TOTAL_AMOUNT_GIFTED_PRO, MOST_COMMON_GIFT_CHANNEL_PRO,
#DONATION_COUNT, STATE, DISTINCT_APPEAL_COUNT, MEMBER, TOTAL_GIFT, AMOUNT_GIFTED_LAST_YEAR,  AVG_GIFT, RESPONSE_RATE, MAX_GIFT,
#AGE, COUPLE, AGE_70PLUS, ONE_TIME_FLAG, AGE_60_69, MALE, AGE_50_59, FEMALE,  outlier_flag

#gbm5
## Training error
pred5gbm.train <- predict(gbm.generic5,newdata = df.train.gbm5[,-1], n.trees=perf_gbm5, type="response")
y5hat.train <- ifelse(pred5gbm.train < 0.5, 0, 1)
sum(y5hat.train != true.y.train)/length(true.y.train)  ##Training error =  0.01488779

## Testing Error
y5hat.test <- ifelse(predict(gbm.generic5,newdata = df.test.gbm5, n.trees=perf_gbm5, type="response") < 0.5, 0, 1)
mean(y5hat.test != true.y.test) 
## Testing error = 0.01699384
conf.matrix5 <- confusionMatrix(as.factor(y5hat.test), as.factor(true.y.test) )
# Extract confusion matrix metrics (Testing Set)
test5_TP <- conf.matrix5$table[2,2]/length(true.y.test)  # True Positives
test5_FP <- conf.matrix5$table[1,2]/length(true.y.test)  # False Positives
test5_TN <- conf.matrix5$table[1,1]/length(true.y.test)  # True Negatives
test5_FN <- conf.matrix5$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test5_TP, "FP:", test5_FP, "TN:", test5_TN, "FN:", test5_FN, "\n")

#gbm6
## Training error
pred6gbm.train <- predict(gbm.generic6,newdata = df.train.gbm5[,-1], n.trees=perf_gbm6, type="response")
y6hat.train <- ifelse(pred6gbm.train < 0.5, 0, 1)
sum(y6hat.train != true.y.train)/length(true.y.train)  ##Training error =  0.01365521

## Testing Error
y6hat.test <- ifelse(predict(gbm.generic6,newdata = df.test.gbm5, n.trees=perf_gbm6, type="response") < 0.5, 0, 1)
mean(y6hat.test != true.y.test) 
## Testing error = 0.01670546
conf.matrix6 <- confusionMatrix(as.factor(y6hat.test), as.factor(true.y.test) )
# Extract confusion matrix metrics (Testing Set)
test6_TP <- conf.matrix6$table[2,2]/length(true.y.test)  # True Positives
test6_FP <- conf.matrix6$table[1,2]/length(true.y.test)  # False Positives
test6_TN <- conf.matrix6$table[1,1]/length(true.y.test)  # True Negatives
test6_FN <- conf.matrix6$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test6_TP, "FP:", test6_FP, "TN:", test6_TN, "FN:", test6_FN, "\n")

#gbm7 with 10,000 trees and all variables, outliers excluded and outlier flag
# Subset df.train to keep only rows where outlier = 0
df.train.gbm7 <- df.train[df.train$outlier_flag == 0, ]
df.train.gbm7 <- df.train.gbm7[,-c(25)]
df.test.gbm7 <- general_test[general_test$outlier_flag ==0,]
df.test.gbm7$STATE <- as.factor(df.test.gbm7$STATE) 
df.test.gbm7$MOST_COMMON_GIFT_CHANNEL_PRO <- as.factor(df.test.gbm7$MOST_COMMON_GIFT_CHANNEL_PRO)
true.y.test.gbm7 <- df.test.gbm7$SUS_FLAG
df.test.gbm7 <- df.test.gbm7[,-c(1,25)]
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
##variables with high rel.inf : AVG_DAYS_ACROSS_APPEALCODES, AGE_NA, TOTAL_AMOUNT_GIFTED_PRO, MOST_COMMON_GIFT_CHANNEL_PRO,
#STATE,DONATION_COUNT,  DISTINCT_APPEAL_COUNT, TOTAL_GIFT, AMOUNT_GIFTED_LAST_YEAR, AVG_GIFT, MEMBER, RESPONSE_RATE, MAX_GIFT,
#AGE, COUPLE, AGE_70PLUS,FEMALE, AGE_60_69, AGE_50_59,MALE, ONE_TIME_FLAG, AGE_UNDER50

## Training error
pred7gbm.train <- predict(gbm.generic7,newdata = df.train.gbm7[,-1], n.trees=perf_gbm7, type="response")
y7hat.train <- ifelse(pred7gbm.train < 0.5, 0, 1)
sum(y7hat.train != true.y.train.gbm7)/length(true.y.train.gbm7)  ##Training error =  0.01278408

## Testing Error
y7hat.test <- ifelse(predict(gbm.generic7,newdata = df.test.gbm7, n.trees=perf_gbm7, type="response") < 0.5, 0, 1)
mean(y7hat.test != true.y.test.gbm7) 
## Testing error = 0.01563263
conf.matrix7 <- confusionMatrix(as.factor(y7hat.test), as.factor(true.y.test.gbm7) )
# Extract confusion matrix metrics (Testing Set)
test7_TP <- conf.matrix7$table[2,2]/length(true.y.test.gbm7)  # True Positives
test7_FP <- conf.matrix7$table[1,2]/length(true.y.test.gbm7)  # False Positives
test7_TN <- conf.matrix7$table[1,1]/length(true.y.test.gbm7)  # True Negatives
test7_FN <- conf.matrix7$table[2,1]/length(true.y.test.gbm7)  # False Negatives
cat("Testing Set - TP:", test7_TP, "FP:", test7_FP, "TN:", test7_TN, "FN:", test7_FN, "\n")

##test the model on dataset with outliers
## Testing Error
y8hat.test <- ifelse(predict(gbm.generic7,newdata = df.test[,-24], n.trees=perf_gbm7, type="response") < 0.5, 0, 1)
mean(y8hat.test != true.y.test) 
## Testing error =  0.01701444
conf.matrix8 <- confusionMatrix(as.factor(y8hat.test), as.factor(true.y.test) )
# Extract confusion matrix metrics (Testing Set)
test8_TP <- conf.matrix8$table[2,2]/length(true.y.test)  # True Positives
test8_FP <- conf.matrix8$table[1,2]/length(true.y.test)  # False Positives
test8_TN <- conf.matrix8$table[1,1]/length(true.y.test)  # True Negatives
test8_FN <- conf.matrix8$table[2,1]/length(true.y.test)  # False Negatives
cat("Testing Set - TP:", test8_TP, "FP:", test8_FP, "TN:", test8_TN, "FN:", test8_FN, "\n")
#Testing Set - TP: 0.2903578 FP: 0.01446021 TN: 0.6926278 FN: 0.002554226 
