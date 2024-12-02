library(tidyverse)
library(dplyr)
library(caret)
library(purrr)
library(stringr)

set.seed(2024)

datasets<-c("clean_generic_model","Dataset1_Vet")
mode<-("test")
make_model<-function(onedataset){
  
  
  initial_data <- read.csv(sprintf("./Final-Data-Repo/%s.csv",onedataset),header=TRUE,fill=TRUE)%>%
    select(-any_of(c("X","ACCOUNTID")))
  
  sample_data <- sample(c(TRUE, FALSE), nrow(initial_data), replace = TRUE, prob = c(0.7, 0.3))
  initial_data_train <- initial_data[sample_data, ]
  initial_data_test <- initial_data[!sample_data, ]
  
  if(onedataset=="clean_generic_model"){
    # hello1
    print("hello1")
    
    initial_data_train$MEMBER[is.na(initial_data_train$MEMBER)] <- 0
    # Replace "Y" with 1
    initial_data_train$MEMBER[initial_data_train$MEMBER == "Y"] <- 1
    initial_data_train$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(initial_data_train$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"
    initial_data_train$SUS_FLAG[initial_data_train$SUS_FLAG == "Yes"] <- 1
    initial_data_train$SUS_FLAG[initial_data_train$SUS_FLAG == "No"] <- 0
    initial_data_train$SUS_FLAG <- as.factor(initial_data_train$SUS_FLAG)
    
    print("hello2")
    
    initial_data_test$MEMBER[is.na(initial_data_test$MEMBER)] <- 0
    # Replace "Y" with 1
    initial_data_test$MEMBER[initial_data_test$MEMBER == "Y"] <- 1
    initial_data_test$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(initial_data_test$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"
    initial_data_test$SUS_FLAG[initial_data_test$SUS_FLAG == "Yes"] <- 1
    initial_data_test$SUS_FLAG[initial_data_test$SUS_FLAG == "No"] <- 0
    initial_data_test$SUS_FLAG <- as.factor(initial_data_test$SUS_FLAG)
  }
  initial_data_train$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(initial_data_train$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"
  initial_data_test$MOST_COMMON_GIFT_CHANNEL_PRO[is.na(initial_data_test$MOST_COMMON_GIFT_CHANNEL_PRO)] <- "Unknown"
  
  print(colSums(is.na(initial_data_train)))
  model1<-glm(formula = SUS_FLAG~., family = binomial, data = initial_data_train) 
  results_df<-summary(model1)$coefficients
  
  
  
  print(colSums(is.na(initial_data_train)))
  
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
  cm<-confusionMatrix(table(predicted_values>=0.5,actual_values))
  newcm<-as.table(cm)
  write.csv(newcm,sprintf("./logistic_full_%s_%s_output_accu.csv",onedataset,mode))
}

walk(datasets,make_model)