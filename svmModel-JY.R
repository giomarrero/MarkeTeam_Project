library(tidyverse)
library(dplyr)
library(e1071)
library(purrr)
library(caret)

datasets <- c("clean_generic_model","Dataset1_Vet")
mode<-("test")
set.seed(2024)

make_model <- function(onedataset) {
  
  # Read the dataset
  file1 <- sprintf("./Final_Data/Reduced_Data/%s.csv", onedataset)
  initial_data <- read.csv(file1, header = TRUE, fill = TRUE)%>%
    dplyr::select(-any_of(c("X","ACCOUNTID","MAX_GIFT","COUPLE","ORG","ONE_TIME_FLAG","MEMBER")))%>%
    filter(outlier_flag==0)
   
  

  sample_data <- sample(c(TRUE, FALSE), nrow(initial_data), replace = TRUE, prob = c(0.7, 0.3))
  initial_data_train <- initial_data[sample_data, ]
  initial_data_test <- initial_data[!sample_data, ]
  
  if(onedataset=="clean_generic_model"){
    # hello1
    print("hello1")
    
    initial_data_train$SUS_FLAG[initial_data_train$SUS_FLAG == "Yes"] <- 1
    initial_data_train$SUS_FLAG[initial_data_train$SUS_FLAG == "No"] <- 0
    initial_data_train$SUS_FLAG <- as.factor(initial_data_train$SUS_FLAG)
    
    print("hello2")
    
    initial_data_test$SUS_FLAG[initial_data_test$SUS_FLAG == "Yes"] <- 1
    initial_data_test$SUS_FLAG[initial_data_test$SUS_FLAG == "No"] <- 0
    initial_data_test$SUS_FLAG <- as.factor(initial_data_test$SUS_FLAG)
  }
  initial_data_train$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(initial_data_train$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"
  initial_data_test$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(initial_data_test$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"
  
  print(colSums(is.na(initial_data_train)))
  # Train SVM model
  model2 <- svm(SUS_FLAG ~., data = initial_data_train) 
  print(summary(model2))
  
  if(mode=="test"){
    predicted_values<-predict(model2,type="response",newdata=initial_data_test)
    actual_values<-initial_data_test$SUS_FLAG==1
  }
  
  else if(mode=="train"){
    predicted_values<-predict(model2,type="response",newdata=initial_data_train)
    actual_values<-initial_data_train$SUS_FLAG==1
  }

  
  print(length(predicted_values))
  print(length(actual_values))
  # Predictions on the training data
  cm<-confusionMatrix(table(predicted_values>=0.5,actual_values))
  cm1<-as.table(cm)


  # Write confusion matrix to CSV
  write.csv(cm1, sprintf("./svm_reducedMulti_%s_%s_no_out_output.csv", onedataset,mode))
}

walk(datasets, make_model)
